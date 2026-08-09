import { ApiError, RunServerClient } from "./index.js";

const ELEMENT_NAME = "run-code";
const DEFAULT_ENDPOINT = "https://run.gilray.net";
const TURNSTILE_SCRIPT =
  "https://challenges.cloudflare.com/turnstile/v0/api.js?render=explicit";
const DEFAULT_TURNSTILE_LOAD_TIMEOUT_MS = 15_000;
const TURNSTILE_POLL_INTERVAL_MS = 250;
const DEFAULT_TURNSTILE_VERIFICATION_TIMEOUT_MS = 120_000;
const LANGUAGE_ALIASES = new Map([
  ["c++", "cpp20"],
  ["cpp", "cpp20"],
  ["cpp20", "cpp20"],
  ["haskell", "haskell"],
  ["hs", "haskell"],
  ["lean", "lean"],
  ["lean4", "lean"],
  ["ml", "ocaml"],
  ["ocaml", "ocaml"],
  ["racket", "racket"],
  ["rkt", "racket"],
  ["rust", "rust2024"],
  ["rust2024", "rust2024"],
]);

let settings = defaultSettings();
let coordinator = null;
const turnstileLoads = new WeakMap();

/** Configure every <run-code> element on this page while the page is loading. */
export function configureRunCode(options = {}) {
  if (!options || typeof options !== "object" || Array.isArray(options)) {
    throw new TypeError("RunCode configuration must be an object.");
  }
  const allowed = new Set([
    "endpoint",
    "eagerSession",
    "attestation",
    "clientFactory",
    "clientOptions",
    "turnstileScriptNonce",
    "turnstileTheme",
    "turnstileLoadTimeoutMs",
    "turnstileVerificationTimeoutMs",
  ]);
  const unknown = Object.keys(options).find((key) => !allowed.has(key));
  if (unknown) throw new TypeError(`Unknown RunCode configuration option: ${unknown}.`);
  if (options.attestation !== undefined && typeof options.attestation !== "function") {
    throw new TypeError("attestation must be a function when provided.");
  }
  if (options.eagerSession !== undefined && typeof options.eagerSession !== "boolean") {
    throw new TypeError("eagerSession must be a boolean when provided.");
  }
  if (options.clientFactory !== undefined && typeof options.clientFactory !== "function") {
    throw new TypeError("clientFactory must be a function when provided.");
  }
  if (
    options.clientOptions !== undefined &&
    (!options.clientOptions ||
      typeof options.clientOptions !== "object" ||
      Array.isArray(options.clientOptions))
  ) {
    throw new TypeError("clientOptions must be an object when provided.");
  }
  if (
    options.turnstileTheme !== undefined &&
    !["auto", "light", "dark"].includes(options.turnstileTheme)
  ) {
    throw new TypeError("turnstileTheme must be auto, light, or dark.");
  }
  for (const name of ["turnstileLoadTimeoutMs", "turnstileVerificationTimeoutMs"]) {
    const value = options[name];
    if (value !== undefined && (!Number.isInteger(value) || value < 1 || value > 300_000)) {
      throw new RangeError(`${name} must be an integer from 1 through 300000.`);
    }
  }

  coordinator?.dispose();
  coordinator = null;
  settings = {
    ...defaultSettings(),
    ...Object.fromEntries(Object.entries(options).filter(([, value]) => value !== undefined)),
    clientOptions: { ...(options.clientOptions ?? {}) },
  };
  return Object.freeze({ ...settings, clientOptions: { ...settings.clientOptions } });
}

/** Return the effective page-level configuration without exposing mutable state. */
export function getRunCodeConfiguration() {
  return Object.freeze({ ...settings, clientOptions: { ...settings.clientOptions } });
}

export class RunCodeElement extends (globalThis.HTMLElement ?? class {}) {
  static get observedAttributes() {
    return [
      "button-label",
      "label",
      "language",
      "line-numbers",
      "name",
      "no-run",
      "readonly",
      "resizable",
      "rows",
    ];
  }

  constructor() {
    super();
    this._rendered = false;
    this._initialCode = "";
    this._abort = null;
    this._running = false;
    this._onRun = () => {
      void this.run().catch(() => {
        // run() already rendered and dispatched the error for button-driven runs.
      });
    };
    this._onInput = () => this._updateLineNumbers();
    this._onScroll = () => {
      if (this._gutter && this._textarea) this._gutter.scrollTop = this._textarea.scrollTop;
    };
    this._onKeyDown = (event) => this._handleEditorKey(event);
  }

  connectedCallback() {
    if (this._rendered) return;
    this._initialCode = normalizeCode(this.textContent ?? "");
    this._render();
    if (!this.hasAttribute("no-run")) {
      queueMicrotask(() => {
        if (this.isConnected) pageCoordinator().prepare(this);
      });
    }
  }

  disconnectedCallback() {
    this._abort?.abort(createAbortError("The code box left the page."));
  }

  attributeChangedCallback() {
    if (this._rendered) this._syncAttributes();
  }

  get value() {
    return this._textarea?.value ?? this._initialCode;
  }

  set value(code) {
    const value = String(code ?? "");
    this._initialCode = value;
    if (this._textarea) {
      this._textarea.value = value;
      this._updateLineNumbers();
    }
  }

  get runtime() {
    return parseLanguage(this.getAttribute("language"));
  }

  get mode() {
    const mode = (this.getAttribute("mode") ?? "main").trim().toLowerCase();
    if (!["main", "stmt", "expr"].includes(mode)) {
      throw new TypeError(`Unsupported run-code mode: ${mode || "(empty)"}.`);
    }
    return mode;
  }

  get dependencyNames() {
    return parseNames(this.getAttribute("include") ?? "");
  }

  /** Build the strict v1 JobRequest represented by this box and its named dependencies. */
  createJob(config) {
    const runtime = this.runtime;
    const capability = runtimeCapability(config, runtime);
    const mode = this.mode;
    if (!capability.modes.includes(mode)) {
      throw new TypeError(`${runtime} does not support ${mode} mode.`);
    }
    const files = collectDependencyFiles(this, runtime);

    if (mode === "main") {
      const filename = this._filename(capability.default_entrypoint, false);
      assertUniqueFilename(files, filename, this);
      files.push({ path: filename, content: this.value });
      return {
        runtime,
        mode,
        entrypoint: this.getAttribute("entrypoint")?.trim() || filename,
        files,
      };
    }

    return { runtime, mode, snippet: this.value, files };
  }

  /** Run this box and display its result. Resolves to the raw JobResponse. */
  async run(options = {}) {
    if (this._running) throw new Error("This code box is already running.");
    if (this.hasAttribute("no-run")) throw new Error("This code box has no run control.");
    this._running = true;
    this._abort = new AbortController();
    const linked = linkSignals(options.signal, this._abort.signal);
    this._setBusy(true);
    this._showPending();
    dispatch(this, "runcode-start", { element: this });

    try {
      const result = await pageCoordinator().run(this, { signal: linked.signal });
      this._renderResult(result);
      dispatch(this, "runcode-result", { element: this, result });
      return result;
    } catch (error) {
      const message = friendlyError(error);
      this._showResult("Could not run", message, "error");
      dispatch(this, "runcode-error", { element: this, error });
      throw error;
    } finally {
      linked.cleanup();
      this._abort = null;
      this._running = false;
      this._setBusy(false);
      this._hideAttestation();
    }
  }

  _render() {
    const document = this.ownerDocument;
    this.replaceChildren();

    const frame = element(document, "div", "runcode");
    const header = element(document, "div", "runcode__header");
    const identity = element(document, "div", "runcode__identity");
    this._label = element(document, "span", "runcode__label");
    this._language = element(document, "span", "runcode__language");
    identity.append(this._label, this._language);
    this._button = element(document, "button", "runcode__run");
    this._button.type = "button";
    this._button.addEventListener("click", this._onRun);
    header.append(identity, this._button);

    const editor = element(document, "div", "runcode__editor");
    this._gutter = element(document, "pre", "runcode__lines");
    this._gutter.setAttribute("aria-hidden", "true");
    this._textarea = element(document, "textarea", "runcode__code");
    this._textarea.value = this._initialCode;
    this._textarea.wrap = "off";
    this._textarea.spellcheck = false;
    this._textarea.autocomplete = "off";
    this._textarea.autocapitalize = "off";
    this._textarea.setAttribute("aria-label", "Code");
    this._textarea.addEventListener("input", this._onInput);
    this._textarea.addEventListener("scroll", this._onScroll);
    this._textarea.addEventListener("keydown", this._onKeyDown);
    editor.append(this._gutter, this._textarea);

    this._attestation = element(document, "div", "runcode__attestation");
    this._attestation.hidden = true;
    this._attestationWidget = element(document, "div", "runcode__attestation-widget");
    this._attestation.append(this._attestationWidget);

    this._result = element(document, "section", "runcode__result");
    this._result.hidden = true;
    this._result.setAttribute("aria-live", "polite");
    this._spinner = element(document, "div", "runcode__spinner");
    this._spinner.hidden = true;
    this._spinner.setAttribute("role", "status");
    this._spinner.setAttribute("aria-label", "Running code");
    this._resultHeader = element(document, "div", "runcode__result-header");
    this._resultBody = element(document, "pre", "runcode__result-body");
    this._result.append(this._spinner, this._resultHeader, this._resultBody);

    frame.append(header, editor, this._attestation, this._result);
    this.append(frame);
    this._frame = frame;
    this._rendered = true;
    this._syncAttributes();
    this._updateLineNumbers();
  }

  _syncAttributes() {
    if (!this._textarea) return;
    const name = this.getAttribute("name")?.trim();
    const label = this.getAttribute("label")?.trim() || name || "Code";
    this._label.textContent = label;
    this._textarea.setAttribute("aria-label", `${label} code`);
    let runtime = this.getAttribute("language")?.trim() || "language required";
    try {
      runtime = this.runtime;
    } catch (_) {
      // Keep the supplied value visible; run() will return the useful error.
    }
    this._language.textContent = runtime;
    this._button.textContent = this.getAttribute("button-label")?.trim() || "Run";
    this._button.hidden = this.hasAttribute("no-run");
    this._textarea.readOnly = this.hasAttribute("readonly");

    const lines = lineCount(this._textarea.value);
    this._textarea.rows = parseRows(this.getAttribute("rows"), lines);
    const resize = this.hasAttribute("resizable")
      ? this.getAttribute("resizable")?.trim().toLowerCase() === "both"
        ? "both"
        : "vertical"
      : "none";
    this._textarea.dataset.resize = resize;
    this._frame.dataset.readonly = String(this._textarea.readOnly);
    this._gutter.hidden = !this.hasAttribute("line-numbers");
    this._frame.dataset.lineNumbers = String(this.hasAttribute("line-numbers"));
  }

  _updateLineNumbers() {
    if (!this._textarea || !this._gutter) return;
    const count = lineCount(this._textarea.value);
    this._gutter.textContent = Array.from({ length: count }, (_, index) => index + 1).join("\n");
  }

  _handleEditorKey(event) {
    if (event.key !== "Tab" || this._textarea.readOnly) return;
    event.preventDefault();
    const start = this._textarea.selectionStart;
    const end = this._textarea.selectionEnd;
    this._textarea.setRangeText("  ", start, end, "end");
    const EventConstructor = this.ownerDocument.defaultView?.Event ?? globalThis.Event;
    this._textarea.dispatchEvent(new EventConstructor("input", { bubbles: true }));
  }

  _filename(defaultEntrypoint, dependency) {
    const explicit = this.getAttribute("filename")?.trim();
    if (explicit) return explicit;
    const name = this.getAttribute("name")?.trim();
    if (name?.includes(".")) return name;
    if (!dependency) return defaultEntrypoint;
    throw new TypeError(
      `Included code box "${name || "(unnamed)"}" needs a filename attribute or a name with an extension.`,
    );
  }

  _mountAttestation() {
    delete this._attestation.dataset.interactive;
    this._attestation.hidden = false;
    this._attestationWidget.replaceChildren();
    return this._attestationWidget;
  }

  _showAttestationInteraction() {
    this._attestation.dataset.interactive = "true";
  }

  _hideAttestation() {
    if (!this._attestation) return;
    this._attestation.hidden = true;
    delete this._attestation.dataset.interactive;
    this._attestationWidget.replaceChildren();
  }

  _setBusy(busy) {
    this._button.disabled = busy;
    this._textarea.setAttribute("aria-busy", String(busy));
    this._frame.dataset.running = String(busy);
  }

  _showPending() {
    this._result.hidden = false;
    this._result.dataset.kind = "pending";
    this._spinner.hidden = false;
    this._resultHeader.hidden = true;
    this._resultBody.hidden = true;
    this._resultHeader.textContent = "";
    this._resultBody.textContent = "";
  }

  _showResult(title, body, kind) {
    this._result.hidden = false;
    this._result.dataset.kind = kind;
    this._spinner.hidden = true;
    this._resultHeader.hidden = false;
    this._resultBody.hidden = false;
    this._resultHeader.textContent = title;
    this._resultBody.textContent = body;
  }

  _renderResult(result) {
    this._showResult(statusLabel(result.status), resultText(result), result.status);
  }
}

class PageCoordinator {
  constructor(configuration) {
    this.configuration = configuration;
    this.clientPromise = null;
    this.activeElement = null;
    this.preparationElement = null;
    this.gate = Promise.resolve();
  }

  prepare(element) {
    if (!this.configuration.eagerSession || !(element instanceof RunCodeElement)) return;
    this.preparationElement ??= element;
    void this._client().catch(() => {
      // Preparation is opportunistic. A click retries and renders any failure.
    });
  }

  async run(element, options) {
    let release;
    const previous = this.gate;
    this.gate = new Promise((resolve) => {
      release = resolve;
    });
    await previous;
    this.activeElement = element;
    try {
      const client = await this._client();
      const job = element.createJob(client.config);
      return await client.run(job, options);
    } finally {
      this.activeElement = null;
      release();
    }
  }

  dispose() {
    void this.clientPromise?.then((client) => client.dispose?.(), () => {});
  }

  _client() {
    if (!this.clientPromise) {
      const configuration = this.configuration;
      const attestation =
        configuration.attestation ??
        ((context) =>
          defaultAttestation(
            this.activeElement ?? this.preparationElement,
            context,
            configuration,
          ));
      const options = {
        ...configuration.clientOptions,
        endpoint: configuration.endpoint,
        attestation,
        eagerSession: configuration.eagerSession,
      };
      this.clientPromise = configuration.clientFactory
        ? Promise.resolve(configuration.clientFactory(options))
        : RunServerClient.create(options);
      this.clientPromise = this.clientPromise.catch((error) => {
        this.clientPromise = null;
        throw error;
      });
    }
    return this.clientPromise;
  }
}

function pageCoordinator() {
  if (!coordinator) coordinator = new PageCoordinator({ ...settings });
  return coordinator;
}

function defaultSettings() {
  return {
    endpoint: DEFAULT_ENDPOINT,
    eagerSession: true,
    attestation: undefined,
    clientFactory: undefined,
    clientOptions: {},
    turnstileScriptNonce: undefined,
    turnstileTheme: "auto",
    turnstileLoadTimeoutMs: DEFAULT_TURNSTILE_LOAD_TIMEOUT_MS,
    turnstileVerificationTimeoutMs: DEFAULT_TURNSTILE_VERIFICATION_TIMEOUT_MS,
  };
}

async function defaultAttestation(element, context, configuration) {
  if (!(element instanceof RunCodeElement)) {
    throw new Error("No active code box is available for browser verification.");
  }
  const host = element._mountAttestation();
  let turnstile;
  try {
    turnstile = await loadTurnstile(element.ownerDocument, configuration, context.signal);
  } catch (error) {
    element._hideAttestation();
    throw error;
  }

  return new Promise((resolve, reject) => {
    let widgetId = null;
    let removeAfterRender = false;
    let settled = false;
    const timeout = setTimeout(
      () => fail("The browser check did not finish. Check content blockers and try again."),
      configuration.turnstileVerificationTimeoutMs,
    );
    const cleanup = () => {
      if (settled) return false;
      settled = true;
      clearTimeout(timeout);
      context.signal?.removeEventListener("abort", onAbort);
      if (widgetId !== null) {
        try {
          turnstile.remove(widgetId);
        } catch (_) {
          // The token is already complete; widget cleanup must not strand the run.
        }
      } else {
        removeAfterRender = true;
      }
      element._hideAttestation();
      return true;
    };
    const succeed = (token) => {
      if (cleanup()) resolve(token);
    };
    const fail = (message) => {
      if (cleanup()) reject(new Error(message));
    };
    const onAbort = () => {
      if (cleanup()) reject(abortReason(context.signal));
    };
    context.signal?.addEventListener("abort", onAbort, { once: true });
    if (context.signal?.aborted) {
      onAbort();
      return;
    }

    try {
      widgetId = turnstile.render(host, {
        sitekey: context.config.turnstile_sitekey,
        action: context.config.turnstile_action,
        appearance: "interaction-only",
        size: "flexible",
        theme: configuration.turnstileTheme,
        callback: succeed,
        "before-interactive-callback": () => element._showAttestationInteraction(),
        "unsupported-callback": () => {
          fail("This browser is not supported by the browser check.");
        },
        "error-callback": (code) => {
          const detail = typeof code === "string" && code ? ` (${code})` : "";
          fail(`The browser check could not complete${detail}. Please try again.`);
          return true;
        },
        "expired-callback": () => fail("The browser check expired. Please run again."),
        "timeout-callback": () => fail("The browser check timed out. Please run again."),
      });
      if (removeAfterRender && widgetId !== null) turnstile.remove(widgetId);
    } catch (error) {
      fail(error instanceof Error ? error.message : "The browser check could not start.");
    }
  });
}

function loadTurnstile(document, configuration, signal) {
  if (document.defaultView?.turnstile) return Promise.resolve(document.defaultView.turnstile);
  let turnstileLoad = turnstileLoads.get(document);
  if (!turnstileLoad) {
    turnstileLoad = new Promise((resolve, reject) => {
      const existing = document.querySelector("script[data-runcode-turnstile]");
      const script = existing ?? document.createElement("script");
      let settled = false;
      const settle = (callback, value) => {
        if (settled) return;
        settled = true;
        clearTimeout(timeout);
        clearInterval(watch);
        callback(value);
      };
      const finish = () => {
        const api = document.defaultView?.turnstile;
        if (!api) return;
        clearInterval(watch);
        api.ready(() => settle(resolve, api));
      };
      const giveUp = (message) => {
        // A dead script tag would silence the load event for every later attempt, so the
        // failed request is dropped and the next attempt fetches Turnstile again.
        script.remove();
        settle(reject, new Error(message));
      };
      const timeout = setTimeout(
        () =>
          giveUp(
            "Cloudflare Turnstile did not load. Check content blockers or network policy and try again.",
          ),
        configuration.turnstileLoadTimeoutMs,
      );
      // An inherited script tag may have loaded before these listeners were attached, so the
      // global is polled too; its load event would never fire a second time.
      const watch = setInterval(
        finish,
        Math.max(
          1,
          Math.min(TURNSTILE_POLL_INTERVAL_MS, Math.floor(configuration.turnstileLoadTimeoutMs / 4)),
        ),
      );
      script.addEventListener("load", finish, { once: true });
      script.addEventListener("error", () => giveUp("Cloudflare Turnstile could not be loaded."), {
        once: true,
      });
      if (!existing) {
        script.src = TURNSTILE_SCRIPT;
        script.async = true;
        script.defer = true;
        script.dataset.runcodeTurnstile = "";
        if (configuration.turnstileScriptNonce) {
          script.nonce = configuration.turnstileScriptNonce;
        }
        document.head.append(script);
      }
      finish();
    }).catch((error) => {
      turnstileLoads.delete(document);
      throw error;
    });
    turnstileLoads.set(document, turnstileLoad);
  }
  if (!signal) return turnstileLoad;
  return Promise.race([
    turnstileLoad,
    new Promise((_, reject) => {
      if (signal.aborted) reject(abortReason(signal));
      else signal.addEventListener("abort", () => reject(abortReason(signal)), { once: true });
    }),
  ]);
}

function collectDependencyFiles(root, runtime) {
  const files = [];
  const visited = new Set();
  const visiting = new Set([root]);

  const visit = (box) => {
    for (const name of box.dependencyNames) {
      const dependency = findNamedBox(root.ownerDocument, name);
      if (visiting.has(dependency)) {
        throw new TypeError(`Code box dependency cycle detected at "${name}".`);
      }
      if (visited.has(dependency)) continue;
      const dependencyRuntime = dependency.getAttribute("language")?.trim()
        ? dependency.runtime
        : runtime;
      if (dependencyRuntime !== runtime) {
        throw new TypeError(
          `Included code box "${name}" uses ${dependencyRuntime}; expected ${runtime}.`,
        );
      }
      visiting.add(dependency);
      visit(dependency);
      visiting.delete(dependency);
      visited.add(dependency);
      const filename = dependency._filename("", true);
      assertUniqueFilename(files, filename, dependency);
      files.push({ path: filename, content: dependency.value });
    }
  };

  visit(root);
  return files;
}

function findNamedBox(document, name) {
  const matches = Array.from(document.querySelectorAll(`${ELEMENT_NAME}[name]`)).filter(
    (element) => element.getAttribute("name")?.trim() === name,
  );
  if (matches.length === 0) throw new TypeError(`No code box is named "${name}".`);
  if (matches.length > 1) throw new TypeError(`More than one code box is named "${name}".`);
  const box = matches[0];
  if (!(box instanceof RunCodeElement)) {
    throw new TypeError(`The element named "${name}" is not an initialized run-code box.`);
  }
  return box;
}

function assertUniqueFilename(files, filename, box) {
  if (files.some((file) => file.path === filename)) {
    throw new TypeError(
      `More than one included code box resolves to filename "${filename}" near "${box.getAttribute("name") || "code"}".`,
    );
  }
}

function runtimeCapability(config, runtime) {
  const capability = config?.runtimes?.find((candidate) => candidate.runtime === runtime);
  if (!capability) throw new Error(`The server did not describe the ${runtime} runtime.`);
  return capability;
}

function parseLanguage(value) {
  const language = value?.trim().toLowerCase();
  const runtime = LANGUAGE_ALIASES.get(language);
  if (!runtime) {
    throw new TypeError(
      `Unsupported or missing run-code language: ${language || "(missing)"}. Use cpp, rust, racket, haskell, ocaml, or lean.`,
    );
  }
  return runtime;
}

function parseNames(value) {
  const names = value
    .split(/[\s,]+/u)
    .map((name) => name.trim())
    .filter(Boolean);
  if (new Set(names).size !== names.length) {
    throw new TypeError("A run-code include list must not repeat a box name.");
  }
  return names;
}

function normalizeCode(source) {
  const normalized = String(source).replaceAll("\r\n", "\n").replaceAll("\r", "\n");
  const lines = normalized.split("\n");
  while (lines[0]?.trim() === "") lines.shift();
  while (lines.at(-1)?.trim() === "") lines.pop();
  const contentLines = lines.filter((line) => line.trim() !== "");
  const indent = contentLines.length
    ? Math.min(...contentLines.map((line) => line.match(/^[\t ]*/u)[0].length))
    : 0;
  return lines.map((line) => line.slice(indent)).join("\n");
}

function lineCount(value) {
  return String(value).split("\n").length;
}

function parseRows(value, lines) {
  if (value !== null && value !== "") {
    const parsed = Number(value);
    if (Number.isInteger(parsed) && parsed >= 1 && parsed <= 1000) return parsed;
  }
  return Math.max(2, Math.min(lines, 24));
}

function resultText(result) {
  const sections = [];
  if (result.error?.message) sections.push(result.error.message);
  for (const [phaseName, phase] of [
    ["Build", result.build],
    ["Output", result.run],
  ]) {
    if (!phase) continue;
    if (phase.stdout) sections.push(`${phaseName}:\n${phase.stdout.replace(/\n$/u, "")}`);
    if (phase.stderr) sections.push(`${phaseName} errors:\n${phase.stderr.replace(/\n$/u, "")}`);
  }
  return sections.join("\n\n") || "(no output)";
}

function statusLabel(status) {
  const label = String(status ?? "finished").replaceAll("_", " ");
  return label.charAt(0).toUpperCase() + label.slice(1);
}

function friendlyError(error) {
  if (error instanceof ApiError) {
    const retry = error.retryAfterSeconds === null ? "" : ` Try again in ${error.retryAfterSeconds}s.`;
    return `${error.message}${retry}`;
  }
  return error instanceof Error ? error.message : "The code could not be run.";
}

function element(document, tagName, className) {
  const node = document.createElement(tagName);
  node.className = className;
  return node;
}

function dispatch(element, type, detail) {
  const CustomEventConstructor = element.ownerDocument.defaultView?.CustomEvent ?? globalThis.CustomEvent;
  if (CustomEventConstructor) {
    element.dispatchEvent(new CustomEventConstructor(type, { bubbles: true, detail }));
  }
}

function linkSignals(...signals) {
  const active = signals.filter(Boolean);
  const controller = new AbortController();
  const listeners = [];
  const abortFrom = (signal) => controller.abort(abortReason(signal));
  for (const signal of active) {
    if (signal.aborted) abortFrom(signal);
    else {
      const listener = () => abortFrom(signal);
      signal.addEventListener("abort", listener, { once: true });
      listeners.push([signal, listener]);
    }
  }
  return {
    signal: controller.signal,
    cleanup() {
      for (const [signal, listener] of listeners) signal.removeEventListener("abort", listener);
    },
  };
}

function abortReason(signal) {
  return signal?.reason ?? createAbortError("The operation was aborted.");
}

function createAbortError(message) {
  return new DOMException(message, "AbortError");
}

if (globalThis.customElements && !globalThis.customElements.get(ELEMENT_NAME)) {
  globalThis.customElements.define(ELEMENT_NAME, RunCodeElement);
}

const globalApi = Object.freeze({
  configure: configureRunCode,
  getConfiguration: getRunCodeConfiguration,
  RunCodeElement,
});
if (typeof globalThis.RunCode === "undefined") globalThis.RunCode = globalApi;
