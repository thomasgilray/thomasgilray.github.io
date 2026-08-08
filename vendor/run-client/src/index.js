const INTERNAL = Symbol("RunServerClient.internal");
const RUNTIMES = new Set(["cpp20", "rust2024", "racket", "haskell", "ocaml", "lean"]);
const MODES = new Set(["main", "stmt", "expr"]);
const SESSION_RENEWAL_SKEW_MS = 15_000;
const RESERVED_FILE_NAMES = new Set([
  "__run_server_main.cpp",
  "__run_server_main.rs",
  "__run_server_main.rkt",
  "__run_server_bootstrap.rkt",
  "__run_server_main.hs",
  "RunServerMain.lean",
  "lakefile.toml",
  "__run_server_program",
]);
const U64_MAX_DECIMAL = "18446744073709551615";
let nextSolveId = 0;

/** An HTTP error returned by the run-server API. */
export class ApiError extends Error {
  constructor(status, code, message, options = {}) {
    super(message, options.cause === undefined ? undefined : { cause: options.cause });
    this.name = "ApiError";
    this.status = status;
    this.code = code;
    this.retryAfterSeconds = options.retryAfterSeconds ?? null;
    this.requestId = options.requestId ?? null;
  }
}

/** A response that did not match the advertised v1 protocol. */
export class ProtocolError extends Error {
  constructor(message, options = {}) {
    super(message, options.cause === undefined ? undefined : { cause: options.cause });
    this.name = "ProtocolError";
    this.requestId = options.requestId ?? null;
  }
}

/** A transport failure before a valid API response was received. */
export class NetworkError extends Error {
  constructor(message, options = {}) {
    super(message, options.cause === undefined ? undefined : { cause: options.cause });
    this.name = "NetworkError";
  }
}

/** A local proof worker or proof deadline failure. */
export class ProofError extends Error {
  constructor(code, message, options = {}) {
    super(message, options.cause === undefined ? undefined : { cause: options.cause });
    this.name = "ProofError";
    this.code = code;
  }
}

/** The client was used after dispose(). */
export class ClientDisposedError extends Error {
  constructor() {
    super("The run-server client has been disposed.");
    this.name = "ClientDisposedError";
  }
}

/**
 * Create a proof solver backed by the module worker shipped with this package.
 * A worker factory can be supplied for bundlers with custom worker handling.
 */
export function createWorkerProofSolver(options = {}) {
  if (!options || typeof options !== "object") {
    throw new TypeError("Worker proof solver options must be an object.");
  }
  if (options.workerFactory !== undefined && typeof options.workerFactory !== "function") {
    throw new TypeError("workerFactory must be a function.");
  }
  const maxSolveMs = options.maxSolveMs ?? 28_000;
  if (!Number.isInteger(maxSolveMs) || maxSolveMs < 1 || maxSolveMs > 28_000) {
    throw new RangeError("maxSolveMs must be an integer from 1 through 28000.");
  }
  const workerUrl = options.workerUrl ?? new URL("./proof-worker.js", import.meta.url);

  return Object.freeze({
    solve(challenge, solveOptions = {}) {
      const signal = solveOptions.signal;
      if (signal?.aborted) {
        return Promise.reject(abortReason(signal));
      }

      const remainingMs = challenge.expires_at * 1000 - Date.now() - 250;
      const deadlineMs = Math.floor(Math.min(maxSolveMs, remainingMs));
      if (deadlineMs <= 0) {
        return Promise.reject(
          new ProofError("challenge_expired", "The proof challenge expired before solving began."),
        );
      }

      return new Promise((resolve, reject) => {
        let worker;
        try {
          if (options.workerFactory) {
            worker = options.workerFactory();
          } else {
            const WorkerConstructor = globalThis.Worker;
            if (typeof WorkerConstructor !== "function") {
              throw new ProofError(
                "worker_unavailable",
                "This environment has no Worker implementation; provide workerFactory or proofSolver.",
              );
            }
            worker = new WorkerConstructor(workerUrl, {
              type: "module",
              name: "run-server-proof",
              ...options.workerOptions,
            });
          }
        } catch (error) {
          reject(
            error instanceof ProofError
              ? error
              : new ProofError("worker_start_failed", "The proof worker could not start.", {
                  cause: error,
                }),
          );
          return;
        }

        const id = ++nextSolveId;
        let settled = false;
        const cleanup = () => {
          if (settled) return false;
          settled = true;
          signal?.removeEventListener("abort", onAbort);
          worker.onmessage = null;
          worker.onerror = null;
          try {
            worker.terminate();
          } catch {
            // A worker is already unusable once this solve is settled.
          }
          return true;
        };
        const fail = (error) => {
          if (cleanup()) reject(error);
        };
        const onAbort = () => fail(abortReason(signal));

        worker.onmessage = ({ data }) => {
          if (!data || data.id !== id || settled) return;
          if (data.type === "progress") {
            try {
              solveOptions.onProgress?.({
                tries: data.tries,
                elapsedMs: data.elapsedMs,
              });
            } catch (error) {
              fail(
                new ProofError("progress_callback_failed", "The proof progress callback failed.", {
                  cause: error,
                }),
              );
            }
            return;
          }
          if (data.type === "solved") {
            if (!isCanonicalU64(data.nonce)) {
              fail(new ProofError("invalid_worker_result", "The proof worker returned an invalid nonce."));
              return;
            }
            if (cleanup()) resolve(data.nonce);
            return;
          }
          if (data.type === "error") {
            fail(
              new ProofError(
                typeof data.code === "string" ? data.code : "worker_failed",
                typeof data.message === "string" ? data.message : "The proof worker failed.",
              ),
            );
          }
        };
        worker.onerror = (event) => {
          fail(
            new ProofError(
              "worker_failed",
              typeof event?.message === "string" ? event.message : "The proof worker failed.",
            ),
          );
        };
        signal?.addEventListener("abort", onAbort, { once: true });
        if (signal?.aborted) {
          onAbort();
          return;
        }

        try {
          worker.postMessage({
            type: "solve",
            id,
            challenge: challenge.challenge,
            difficulty: challenge.difficulty,
            deadlineMs,
          });
        } catch (error) {
          fail(new ProofError("worker_start_failed", "The proof worker could not start.", { cause: error }));
        }
      });
    },
  });
}

/** A DOM-free client for the run-server v1 browser API. */
export class RunServerClient {
  static async create(options = {}) {
    const client = new this(INTERNAL, options);
    try {
      await client.refreshConfig({ signal: options.signal });
      if (client._attestation && options.eagerSession !== false) {
        await client.refreshSession({ signal: options.signal });
      }
      return client;
    } catch (error) {
      client.dispose();
      throw error;
    }
  }

  constructor(internal, options) {
    if (internal !== INTERNAL) {
      throw new TypeError("Use RunServerClient.create() to construct a client.");
    }
    this.endpoint = normalizeEndpoint(options.endpoint ?? "https://run.gilray.net");
    this._fetch = options.fetch ?? globalThis.fetch?.bind(globalThis);
    if (typeof this._fetch !== "function") {
      throw new TypeError("No fetch implementation is available; provide options.fetch.");
    }
    if (options.attestation !== undefined && typeof options.attestation !== "function") {
      throw new TypeError("options.attestation must be a function.");
    }
    this._attestation = options.attestation ?? null;
    this._proofSolver = normalizeProofSolver(options.proofSolver ?? createWorkerProofSolver());
    this._maxProofAttempts = options.maxProofAttempts ?? 2;
    if (!Number.isInteger(this._maxProofAttempts) || this._maxProofAttempts < 1 || this._maxProofAttempts > 5) {
      throw new RangeError("maxProofAttempts must be an integer from 1 through 5.");
    }
    this._config = null;
    this._session = null;
    this._sessionAbsoluteDeadlineAt = 0;
    this._sessionLastUsedAt = 0;
    this._disposed = false;
    this._lifetime = new AbortController();
    this._runGate = Promise.resolve();
  }

  get config() {
    return this._config;
  }

  get session() {
    if (!this._session) return null;
    return Object.freeze({
      expiresAt: this._session.expires_at,
      expiresIn: this._session.expires_in,
      idleExpiresIn: this._session.idle_expires_in,
    });
  }

  get disposed() {
    return this._disposed;
  }

  async refreshConfig(options = {}) {
    this._assertActive();
    const linked = linkSignals(options.signal, this._lifetime.signal);
    try {
      const config = await this._request("/v1/config", { method: "GET", signal: linked.signal });
      validateConfig(config);
      this._config = deepFreeze(config);
      return this._config;
    } finally {
      linked.cleanup();
    }
  }

  async createSession(turnstileToken, options = {}) {
    this._assertActive();
    if (typeof turnstileToken !== "string" || turnstileToken.length < 1 || turnstileToken.length > 2048) {
      throw new TypeError("turnstileToken must be a non-empty string of at most 2048 characters.");
    }
    if (
      this._config?.limits &&
      utf8Length(JSON.stringify({ turnstile_token: turnstileToken })) >
        this._config.limits.max_session_body_bytes
    ) {
      throw new RangeError("The encoded session request exceeds the server's body limit.");
    }
    const linked = linkSignals(options.signal, this._lifetime.signal);
    try {
      const session = await this._request("/v1/sessions", {
        method: "POST",
        body: { turnstile_token: turnstileToken },
        signal: linked.signal,
      });
      validateSession(session);
      this._session = session;
      this._sessionLastUsedAt = Date.now();
      this._sessionAbsoluteDeadlineAt = this._sessionLastUsedAt + session.expires_in * 1000;
      return this.session;
    } finally {
      linked.cleanup();
    }
  }

  async refreshSession(options = {}) {
    this._assertActive();
    if (!this._attestation) {
      throw new ProtocolError(
        "No attestation provider is configured; call createSession(token) or provide options.attestation.",
      );
    }
    if (!this._config) {
      await this.refreshConfig(options);
    }
    const linked = linkSignals(options.signal, this._lifetime.signal);
    try {
      const token = await this._attestation({
        config: this._config,
        endpoint: this.endpoint,
        signal: linked.signal,
      });
      throwIfAborted(linked.signal);
      return await this.createSession(token, { signal: linked.signal });
    } finally {
      linked.cleanup();
    }
  }

  async run(job, options = {}) {
    this._assertActive();
    validateJob(job, this._config);
    let stableJob;
    try {
      stableJob = JSON.parse(JSON.stringify(job));
    } catch (error) {
      throw new TypeError("job must be JSON-serializable.", { cause: error });
    }
    validateJob(stableJob, this._config);
    const linked = linkSignals(options.signal, this._lifetime.signal);
    let release;
    try {
      release = await this._acquireRun(linked.signal);
      await this._ensureSession({ signal: linked.signal });
      const runtime = stableJob.runtime;

      for (let attempt = 1; attempt <= this._maxProofAttempts; attempt += 1) {
        const issued = await this._createChallenge(runtime, linked.signal);
        const challenge = issued.challenge;
        let nonce;
        try {
          nonce = await this._proofSolver.solve(challenge, {
            signal: linked.signal,
            onProgress: options.onProofProgress,
          });
        } catch (error) {
          if (
            error instanceof ProofError &&
            error.code === "challenge_expired" &&
            attempt < this._maxProofAttempts &&
            !linked.signal.aborted
          ) {
            continue;
          }
          throw error;
        }
        throwIfAborted(linked.signal);
        if (!isCanonicalU64(nonce)) {
          throw new ProofError("invalid_solver_result", "The proof solver returned an invalid nonce.");
        }

        try {
          const result = await this._request("/v1/jobs", {
            method: "POST",
            body: {
              runtime,
              challenge: challenge.challenge,
              nonce,
              job: stableJob,
            },
            signal: linked.signal,
            token: issued.sessionToken,
          });
          this._markSessionUsed(issued.sessionToken);
          validateJobResponse(result);
          return result;
        } catch (error) {
          if (error instanceof ApiError && error.code === "invalid_session") {
            this._clearSessionIfToken(issued.sessionToken);
          }
          throw error;
        }
      }
      throw new ProofError("challenge_expired", "The proof challenge repeatedly expired.");
    } finally {
      release?.();
      linked.cleanup();
    }
  }

  dispose() {
    if (this._disposed) return;
    this._disposed = true;
    this._clearSession();
    this._lifetime.abort(new ClientDisposedError());
    this._proofSolver.dispose?.();
  }

  async _createChallenge(runtime, signal) {
    for (let attempt = 0; attempt < 2; attempt += 1) {
      if (!this._session) throw new ProtocolError("No active session is available.");
      const sessionToken = this._session.session_token;
      try {
        const challenge = await this._request("/v1/challenges", {
          method: "POST",
          body: { runtime },
          signal,
          token: sessionToken,
        });
        this._markSessionUsed(sessionToken);
        validateChallenge(challenge);
        return { challenge: deepFreeze(challenge), sessionToken };
      } catch (error) {
        if (error instanceof ApiError && error.code === "invalid_session") {
          this._clearSessionIfToken(sessionToken);
          if (attempt === 0) {
            if (!this._session && this._attestation) await this.refreshSession({ signal });
            if (this._session) continue;
          }
        }
        throw error;
      }
    }
    throw new ProtocolError("Unable to establish a valid session.");
  }

  async _ensureSession(options) {
    if (this._sessionIsUsable()) return;
    this._clearSession();
    await this.refreshSession(options);
  }

  _sessionIsUsable() {
    if (!this._session) return false;
    const now = Date.now();
    const idleDeadline = this._sessionLastUsedAt + this._session.idle_expires_in * 1000;
    return now + SESSION_RENEWAL_SKEW_MS < Math.min(this._sessionAbsoluteDeadlineAt, idleDeadline);
  }

  _clearSession() {
    this._session = null;
    this._sessionAbsoluteDeadlineAt = 0;
    this._sessionLastUsedAt = 0;
  }

  _clearSessionIfToken(token) {
    if (this._session?.session_token === token) this._clearSession();
  }

  _markSessionUsed(token) {
    if (this._session?.session_token === token) this._sessionLastUsedAt = Date.now();
  }

  async _request(path, options) {
    throwIfAborted(options.signal);
    const headers = { Accept: "application/json" };
    if (options.method !== "GET") headers["Content-Type"] = "application/json";
    if (options.token) headers.Authorization = `Bearer ${options.token}`;

    let response;
    try {
      response = await this._fetch(`${this.endpoint}${path}`, {
        method: options.method,
        headers,
        body: options.body === undefined ? undefined : JSON.stringify(options.body),
        cache: "no-store",
        signal: options.signal,
      });
    } catch (error) {
      if (options.signal?.aborted) throw abortReason(options.signal);
      throw new NetworkError(`The request to ${path} failed.`, { cause: error });
    }

    const requestId = response.headers?.get?.("X-Request-Id") ?? null;
    let body;
    try {
      body = await response.json();
    } catch (error) {
      if (!response.ok) {
        throw new ApiError(response.status, "unreadable_response", "The server returned an unreadable error response.", {
          requestId,
          retryAfterSeconds: parseRetryAfter(response.headers?.get?.("Retry-After")),
          cause: error,
        });
      }
      throw new ProtocolError("The server returned an unreadable success response.", {
        requestId,
        cause: error,
      });
    }

    if (!response.ok) {
      const apiError = body && typeof body === "object" ? body.error : null;
      throw new ApiError(
        response.status,
        typeof apiError?.code === "string" ? apiError.code : "request_failed",
        typeof apiError?.message === "string"
          ? apiError.message
          : `The request failed with HTTP ${response.status}.`,
        {
          requestId,
          retryAfterSeconds:
            parseNonnegativeInteger(apiError?.retry_after_seconds) ??
            parseRetryAfter(response.headers?.get?.("Retry-After")),
        },
      );
    }
    return body;
  }

  async _acquireRun(signal) {
    let release;
    const previous = this._runGate;
    this._runGate = new Promise((resolve) => {
      release = resolve;
    });
    try {
      await waitFor(previous, signal);
      return release;
    } catch (error) {
      previous.then(release, release);
      throw error;
    }
  }

  _assertActive() {
    if (this._disposed) throw new ClientDisposedError();
  }
}

function normalizeEndpoint(value) {
  let url;
  try {
    url = new URL(value);
  } catch (error) {
    throw new TypeError("endpoint must be an absolute HTTP or HTTPS URL.", { cause: error });
  }
  if (url.protocol !== "https:" && url.protocol !== "http:") {
    throw new TypeError("endpoint must use HTTP or HTTPS.");
  }
  if (url.search || url.hash) {
    throw new TypeError("endpoint must not include a query string or fragment.");
  }
  return url.href.replace(/\/+$/, "");
}

function normalizeProofSolver(solver) {
  if (typeof solver === "function") return Object.freeze({ solve: solver });
  if (!solver || typeof solver.solve !== "function") {
    throw new TypeError("proofSolver must be a function or an object with solve().");
  }
  return solver;
}

function validateConfig(config) {
  if (!config || typeof config !== "object" || config.api_version !== "v1") {
    throw new ProtocolError("The server does not advertise the run-server v1 API.");
  }
  if (
    !config.service ||
    typeof config.service.version !== "string" ||
    typeof config.service.revision !== "string" ||
    typeof config.turnstile_sitekey !== "string" ||
    config.turnstile_sitekey.length === 0 ||
    config.turnstile_action !== "run_server_session" ||
    typeof config.execution_enabled !== "boolean"
  ) {
    throw new ProtocolError("The server returned invalid v1 service metadata.");
  }
  if (config.challenge_algorithm !== "sha256-leading-zero-v1") {
    throw new ProtocolError("The server uses an unsupported proof algorithm.");
  }
  if (config.challenge?.nonce_encoding !== "decimal-u64-string") {
    throw new ProtocolError("The server uses an unsupported proof nonce encoding.");
  }
  if (!Array.isArray(config.enabled_runtimes) || config.enabled_runtimes.some((value) => !RUNTIMES.has(value))) {
    throw new ProtocolError("The server returned invalid runtime capabilities.");
  }
  if (new Set(config.enabled_runtimes).size !== config.enabled_runtimes.length) {
    throw new ProtocolError("The server returned duplicate runtime capabilities.");
  }
  if (!Array.isArray(config.runtimes) || config.runtimes.length === 0) {
    throw new ProtocolError("The server returned no runtime capability descriptions.");
  }
  const describedRuntimes = new Set();
  for (const capability of config.runtimes) {
    if (
      !capability ||
      typeof capability !== "object" ||
      !RUNTIMES.has(capability.runtime) ||
      describedRuntimes.has(capability.runtime) ||
      !Array.isArray(capability.modes) ||
      capability.modes.length === 0 ||
      capability.modes.some((mode) => !MODES.has(mode)) ||
      new Set(capability.modes).size !== capability.modes.length ||
      !isCapabilityPath(capability.default_entrypoint) ||
      !validExtensionList(capability.file_extensions) ||
      !validExtensionList(capability.entrypoint_extensions) ||
      capability.entrypoint_extensions.some(
        (extension) => !capability.file_extensions.includes(extension),
      ) ||
      !hasExtension(capability.default_entrypoint, capability.entrypoint_extensions) ||
      (capability.generated_support_file !== null &&
        (!isCapabilityPath(capability.generated_support_file) ||
          !hasExtension(capability.generated_support_file, capability.file_extensions)))
    ) {
      throw new ProtocolError("The server returned an invalid runtime capability description.");
    }
    describedRuntimes.add(capability.runtime);
  }
  if (describedRuntimes.size !== RUNTIMES.size) {
    throw new ProtocolError("The server did not describe every v1 runtime.");
  }
  if (
    !config.session ||
    !isPositiveInteger(config.session.absolute_lifetime_seconds) ||
    !isPositiveInteger(config.session.idle_lifetime_seconds) ||
    !config.challenge ||
    !Number.isInteger(config.challenge.difficulty) ||
    config.challenge.difficulty < 1 ||
    config.challenge.difficulty > 32 ||
    !isPositiveInteger(config.challenge.lifetime_seconds)
  ) {
    throw new ProtocolError("The server returned invalid session or challenge policy.");
  }
  const positiveLimits = [
    "max_files",
    "max_file_bytes",
    "max_total_source_bytes",
    "max_snippet_bytes",
    "max_stdin_bytes",
    "max_args",
    "max_arg_bytes",
    "max_total_arg_bytes",
    "max_path_bytes",
    "max_session_body_bytes",
    "max_job_body_bytes",
    "request_deadline_ms",
    "default_build_timeout_ms",
    "submitted_code_timeout_ms",
    "max_output_bytes_per_stream",
  ];
  if (
    !config.limits ||
    positiveLimits.some((name) => !isPositiveInteger(config.limits[name])) ||
    !Number.isSafeInteger(config.limits.queue_wait_ms) ||
    config.limits.queue_wait_ms < 0
  ) {
    throw new ProtocolError("The server returned invalid execution limits.");
  }
}

function validateSession(session) {
  if (
    !session ||
    typeof session !== "object" ||
    typeof session.session_token !== "string" ||
    !/^[A-Za-z0-9_-]{43}$/.test(session.session_token) ||
    !Number.isSafeInteger(session.expires_at) ||
    session.expires_at < 0 ||
    !isPositiveInteger(session.expires_in) ||
    !isPositiveInteger(session.idle_expires_in)
  ) {
    throw new ProtocolError("The server returned an invalid session response.");
  }
}

function validateChallenge(challenge) {
  if (
    !challenge ||
    typeof challenge !== "object" ||
    typeof challenge.challenge !== "string" ||
    !/^[A-Za-z0-9_-]{104}$/.test(challenge.challenge) ||
    !Number.isInteger(challenge.difficulty) ||
    challenge.difficulty < 1 ||
    challenge.difficulty > 32 ||
    !Number.isSafeInteger(challenge.expires_at) ||
    challenge.expires_at < 0 ||
    challenge.nonce_encoding !== "decimal-u64-string"
  ) {
    throw new ProtocolError("The server returned an invalid proof challenge.");
  }
}

function validateJob(job, config) {
  if (!job || typeof job !== "object" || Array.isArray(job)) {
    throw new TypeError("job must be an object.");
  }
  rejectUnknownKeys(job, ["runtime", "mode", "entrypoint", "snippet", "files", "stdin", "args"], "job");
  if (!RUNTIMES.has(job.runtime)) throw new TypeError("job.runtime is not supported by API v1.");
  if (!MODES.has(job.mode)) throw new TypeError("job.mode is not supported by API v1.");
  if (!config || !config.limits) throw new ProtocolError("The server configuration is unavailable.");
  if (!config.enabled_runtimes.includes(job.runtime)) {
    throw new TypeError(`The server does not currently enable ${job.runtime}.`);
  }

  const limits = config.limits;
  const capability = config.runtimes.find(({ runtime }) => runtime === job.runtime);
  if (!capability) throw new ProtocolError(`The server did not describe ${job.runtime}.`);
  if (!capability.modes.includes(job.mode)) {
    throw new TypeError(`${job.runtime} does not support ${job.mode} mode.`);
  }
  const files = job.files ?? [];
  const stdin = job.stdin ?? "";
  const args = job.args ?? [];
  if (!Array.isArray(files)) throw new TypeError("job.files must be an array.");
  if (files.length > limits.max_files) {
    throw new RangeError(`job.files exceeds the ${limits.max_files}-file limit.`);
  }
  if (typeof stdin !== "string") throw new TypeError("job.stdin must be a string.");
  if (utf8Length(stdin) > limits.max_stdin_bytes) {
    throw new RangeError(`job.stdin exceeds the ${limits.max_stdin_bytes}-byte limit.`);
  }
  if (stdin.includes("\0")) throw new TypeError("job.stdin must not contain NUL bytes.");
  if (!Array.isArray(args)) throw new TypeError("job.args must be an array.");
  if (args.length > limits.max_args) {
    throw new RangeError(`job.args exceeds the ${limits.max_args}-argument limit.`);
  }

  let totalArgBytes = 0;
  for (const argument of args) {
    if (typeof argument !== "string") throw new TypeError("Each job argument must be a string.");
    const bytes = utf8Length(argument);
    if (bytes > limits.max_arg_bytes) {
      throw new RangeError(`A job argument exceeds the ${limits.max_arg_bytes}-byte limit.`);
    }
    if (argument.includes("\0")) throw new TypeError("Job arguments must not contain NUL bytes.");
    totalArgBytes += bytes;
  }
  if (totalArgBytes > limits.max_total_arg_bytes) {
    throw new RangeError(`job.args exceeds the ${limits.max_total_arg_bytes}-byte aggregate limit.`);
  }

  let totalSourceBytes = 0;
  const names = new Set();
  for (const file of files) {
    if (!file || typeof file !== "object" || Array.isArray(file)) {
      throw new TypeError("Each source file must be an object.");
    }
    rejectUnknownKeys(file, ["path", "content"], "source file");
    validatePath(file.path, limits.max_path_bytes);
    if (names.has(file.path)) throw new TypeError(`Duplicate source file path: ${file.path}.`);
    if (RESERVED_FILE_NAMES.has(file.path)) throw new TypeError(`Reserved source file path: ${file.path}.`);
    names.add(file.path);
    if (typeof file.content !== "string") throw new TypeError("Source file content must be a string.");
    const bytes = utf8Length(file.content);
    if (bytes > limits.max_file_bytes) {
      throw new RangeError(`${file.path} exceeds the ${limits.max_file_bytes}-byte per-file limit.`);
    }
    totalSourceBytes += bytes;
    if (!hasExtension(file.path, capability.file_extensions)) {
      throw new TypeError(`The file extension is not supported by ${job.runtime}: ${file.path}.`);
    }
  }

  if (job.mode === "main") {
    if (!Object.hasOwn(job, "files") || !Array.isArray(job.files)) {
      throw new TypeError("main-mode jobs require files.");
    }
    if (Object.hasOwn(job, "snippet") && job.snippet !== null) {
      throw new TypeError("main-mode jobs do not accept snippet.");
    }
    if (job.entrypoint !== undefined && job.entrypoint !== null && typeof job.entrypoint !== "string") {
      throw new TypeError("job.entrypoint must be a string or null when provided.");
    }
    const entrypoint = job.entrypoint ?? capability.default_entrypoint;
    validatePath(entrypoint, limits.max_path_bytes);
    if (!names.has(entrypoint)) throw new TypeError(`The entrypoint is not present in files: ${entrypoint}.`);
    if (!hasExtension(entrypoint, capability.entrypoint_extensions)) {
      throw new TypeError(`The entrypoint extension is not supported by ${job.runtime}: ${entrypoint}.`);
    }
  } else {
    if (Object.hasOwn(job, "entrypoint") && job.entrypoint !== null) {
      throw new TypeError("Generated jobs do not accept entrypoint.");
    }
    if (typeof job.snippet !== "string" || job.snippet.length === 0) {
      throw new TypeError("stmt- and expr-mode jobs require a non-empty snippet.");
    }
    const snippetBytes = utf8Length(job.snippet);
    if (snippetBytes > limits.max_snippet_bytes) {
      throw new RangeError(`job.snippet exceeds the ${limits.max_snippet_bytes}-byte limit.`);
    }
    totalSourceBytes += snippetBytes;
    if (
      capability.generated_support_file !== null &&
      files.length > 0 &&
      !names.has(capability.generated_support_file)
    ) {
      throw new TypeError(
        `Generated ${job.runtime} jobs with support files must include ${capability.generated_support_file}.`,
      );
    }
    if (job.runtime === "ocaml" && job.mode === "expr" && (stdin.length > 0 || args.length > 0)) {
      throw new TypeError(
        "Generated OCaml expressions do not accept stdin or command-line arguments; use main mode.",
      );
    }
  }

  if (totalSourceBytes > limits.max_total_source_bytes) {
    throw new RangeError(`Job source exceeds the ${limits.max_total_source_bytes}-byte aggregate limit.`);
  }
  const largestEnvelope = {
    runtime: job.runtime,
    challenge: "A".repeat(104),
    nonce: U64_MAX_DECIMAL,
    job,
  };
  if (utf8Length(JSON.stringify(largestEnvelope)) > limits.max_job_body_bytes) {
    throw new RangeError("The encoded job request exceeds the server's body limit.");
  }
}

function rejectUnknownKeys(value, permitted, name) {
  const allowed = new Set(permitted);
  const unknown = Object.keys(value).find((key) => !allowed.has(key));
  if (unknown !== undefined) throw new TypeError(`Unknown ${name} field: ${unknown}.`);
}

function validatePath(path, maxBytes) {
  if (
    typeof path !== "string" ||
    utf8Length(path) < 1 ||
    utf8Length(path) > maxBytes ||
    path === "." ||
    path === ".." ||
    path.startsWith("-") ||
    !/^[A-Za-z0-9._-]+$/.test(path)
  ) {
    throw new TypeError(
      `Source paths must be flat ASCII names of 1 to ${maxBytes} bytes using letters, digits, dot, underscore, or dash.`,
    );
  }
}

function hasExtension(path, allowed) {
  const extension = path.includes(".") ? path.slice(path.lastIndexOf(".") + 1) : "";
  return allowed.includes(extension);
}

function validExtensionList(value) {
  return (
    Array.isArray(value) &&
    value.length > 0 &&
    value.every((extension) => typeof extension === "string" && /^[a-z0-9]+$/.test(extension)) &&
    new Set(value).size === value.length
  );
}

function isCapabilityPath(value) {
  return (
    typeof value === "string" &&
    value.length <= 100 &&
    value !== "." &&
    value !== ".." &&
    !value.startsWith("-") &&
    /^[A-Za-z0-9._-]+$/.test(value)
  );
}

function utf8Length(value) {
  return new TextEncoder().encode(value).byteLength;
}

function isCanonicalU64(value) {
  if (typeof value !== "string" || !/^(0|[1-9][0-9]{0,19})$/.test(value)) return false;
  return (
    value.length < U64_MAX_DECIMAL.length ||
    (value.length === U64_MAX_DECIMAL.length && value <= U64_MAX_DECIMAL)
  );
}

function validateJobResponse(response) {
  const statuses = new Set([
    "ok",
    "invalid_request",
    "build_error",
    "build_timeout",
    "run_error",
    "run_timeout",
    "output_limit",
    "sandbox_terminated",
    "internal_error",
  ]);
  if (!response || typeof response !== "object" || !statuses.has(response.status)) {
    throw new ProtocolError("The server returned an invalid job response.");
  }
}

function parseRetryAfter(value) {
  if (typeof value !== "string" || !/^\d+$/.test(value)) return null;
  return parseNonnegativeInteger(Number(value));
}

function parseNonnegativeInteger(value) {
  return Number.isSafeInteger(value) && value >= 0 ? value : null;
}

function isPositiveInteger(value) {
  return Number.isSafeInteger(value) && value > 0;
}

function deepFreeze(value) {
  if (!value || typeof value !== "object" || Object.isFrozen(value)) return value;
  for (const child of Object.values(value)) deepFreeze(child);
  return Object.freeze(value);
}

function throwIfAborted(signal) {
  if (signal?.aborted) throw abortReason(signal);
}

function abortReason(signal) {
  if (signal?.reason !== undefined) return signal.reason;
  if (typeof DOMException === "function") return new DOMException("The operation was aborted.", "AbortError");
  const error = new Error("The operation was aborted.");
  error.name = "AbortError";
  return error;
}

function linkSignals(...signals) {
  const active = signals.filter(Boolean);
  const controller = new AbortController();
  const listeners = [];
  for (const signal of active) {
    if (signal.aborted) {
      controller.abort(abortReason(signal));
      break;
    }
    const listener = () => controller.abort(abortReason(signal));
    signal.addEventListener("abort", listener, { once: true });
    listeners.push([signal, listener]);
  }
  return {
    signal: controller.signal,
    cleanup() {
      for (const [signal, listener] of listeners) signal.removeEventListener("abort", listener);
    },
  };
}

function waitFor(promise, signal) {
  if (!signal) return promise;
  throwIfAborted(signal);
  return new Promise((resolve, reject) => {
    const onAbort = () => reject(abortReason(signal));
    signal.addEventListener("abort", onAbort, { once: true });
    promise.then(
      (value) => {
        signal.removeEventListener("abort", onAbort);
        resolve(value);
      },
      (error) => {
        signal.removeEventListener("abort", onAbort);
        reject(error);
      },
    );
  });
}
