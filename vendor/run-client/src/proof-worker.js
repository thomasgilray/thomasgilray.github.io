const SHA256_CONSTANTS = new Uint32Array([
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4,
  0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe,
  0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f,
  0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
  0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc,
  0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
  0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116,
  0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7,
  0xc67178f2,
]);

const INITIAL_HASH = new Uint32Array([
  0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
  0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
]);

const DOMAIN = new TextEncoder().encode("run-server-pow-v1\0");
const WORDS = new Uint32Array(64);
const CHALLENGE_BYTES = 78;
const MESSAGE_BYTES = DOMAIN.length + CHALLENGE_BYTES + 8;
const NONCE_OFFSET = DOMAIN.length + CHALLENGE_BYTES;
const BATCH_SIZE = 8192;
let activeSolve = null;

self.onmessage = ({ data }) => {
  if (data?.type === "cancel") {
    activeSolve = null;
    return;
  }
  if (data?.type !== "solve") return;

  try {
    const challenge = decodeChallenge(data.challenge);
    const difficulty = Number(data.difficulty);
    if (!Number.isInteger(difficulty) || difficulty < 1 || difficulty > 32) {
      throw new Error("Unsupported proof difficulty.");
    }
    const deadlineMs = Number(data.deadlineMs);
    if (!Number.isFinite(deadlineMs) || deadlineMs <= 0 || deadlineMs > 28_000) {
      throw new Error("Invalid proof deadline.");
    }
    const message = paddedMessage(challenge);
    const solve = {
      id: data.id,
      difficulty,
      deadline: performance.now() + deadlineMs,
      message,
      view: new DataView(message.buffer),
      nonce: 0,
      started: performance.now(),
    };
    activeSolve = solve;
    searchBatch(solve);
  } catch (error) {
    self.postMessage({
      type: "error",
      code: "invalid_challenge",
      id: data.id,
      message: error instanceof Error ? error.message : "Invalid proof challenge.",
    });
  }
};

function searchBatch(solve) {
  if (activeSolve !== solve) return;
  if (performance.now() >= solve.deadline) {
    activeSolve = null;
    self.postMessage({
      type: "error",
      code: "challenge_expired",
      id: solve.id,
      message: "The proof challenge expired.",
    });
    return;
  }
  const end = Math.min(solve.nonce + BATCH_SIZE, Number.MAX_SAFE_INTEGER);
  for (; solve.nonce < end; solve.nonce += 1) {
    setNonce(solve.view, solve.nonce);
    const firstWord = sha256FirstWord(solve.message);
    if (Math.clz32(firstWord) >= solve.difficulty) {
      const elapsedMs = Math.round(performance.now() - solve.started);
      activeSolve = null;
      self.postMessage({
        type: "solved",
        id: solve.id,
        nonce: solve.nonce.toString(),
        tries: solve.nonce + 1,
        elapsedMs,
      });
      return;
    }
  }
  self.postMessage({
    type: "progress",
    id: solve.id,
    tries: solve.nonce,
    elapsedMs: Math.round(performance.now() - solve.started),
  });
  setTimeout(() => searchBatch(solve), 0);
}

function decodeChallenge(encoded) {
  if (typeof encoded !== "string" || encoded.length !== 104) {
    throw new Error("Malformed proof challenge.");
  }
  const standard = encoded.replaceAll("-", "+").replaceAll("_", "/");
  const decoded = atob(standard);
  if (decoded.length !== CHALLENGE_BYTES) throw new Error("Malformed proof challenge.");
  return Uint8Array.from(decoded, (character) => character.charCodeAt(0));
}

function paddedMessage(challenge) {
  const padded = new Uint8Array(128);
  padded.set(DOMAIN, 0);
  padded.set(challenge, DOMAIN.length);
  padded[MESSAGE_BYTES] = 0x80;
  new DataView(padded.buffer).setUint32(124, MESSAGE_BYTES * 8, false);
  return padded;
}

function setNonce(view, nonce) {
  view.setUint32(NONCE_OFFSET, nonce >>> 0, true);
  view.setUint32(NONCE_OFFSET + 4, Math.floor(nonce / 0x100000000), true);
}

function rotateRight(value, count) {
  return (value >>> count) | (value << (32 - count));
}

function sha256FirstWord(message) {
  let h0 = INITIAL_HASH[0];
  let h1 = INITIAL_HASH[1];
  let h2 = INITIAL_HASH[2];
  let h3 = INITIAL_HASH[3];
  let h4 = INITIAL_HASH[4];
  let h5 = INITIAL_HASH[5];
  let h6 = INITIAL_HASH[6];
  let h7 = INITIAL_HASH[7];
  const view = new DataView(message.buffer, message.byteOffset, message.byteLength);

  for (let offset = 0; offset < message.length; offset += 64) {
    for (let index = 0; index < 16; index += 1) {
      WORDS[index] = view.getUint32(offset + index * 4, false);
    }
    for (let index = 16; index < 64; index += 1) {
      const before15 = WORDS[index - 15];
      const before2 = WORDS[index - 2];
      const sigma0 = rotateRight(before15, 7) ^ rotateRight(before15, 18) ^ (before15 >>> 3);
      const sigma1 = rotateRight(before2, 17) ^ rotateRight(before2, 19) ^ (before2 >>> 10);
      WORDS[index] = (WORDS[index - 16] + sigma0 + WORDS[index - 7] + sigma1) >>> 0;
    }

    let a = h0;
    let b = h1;
    let c = h2;
    let d = h3;
    let e = h4;
    let f = h5;
    let g = h6;
    let h = h7;

    for (let index = 0; index < 64; index += 1) {
      const sum1 = rotateRight(e, 6) ^ rotateRight(e, 11) ^ rotateRight(e, 25);
      const choice = (e & f) ^ (~e & g);
      const temporary1 = (h + sum1 + choice + SHA256_CONSTANTS[index] + WORDS[index]) >>> 0;
      const sum0 = rotateRight(a, 2) ^ rotateRight(a, 13) ^ rotateRight(a, 22);
      const majority = (a & b) ^ (a & c) ^ (b & c);
      const temporary2 = (sum0 + majority) >>> 0;
      h = g;
      g = f;
      f = e;
      e = (d + temporary1) >>> 0;
      d = c;
      c = b;
      b = a;
      a = (temporary1 + temporary2) >>> 0;
    }

    h0 = (h0 + a) >>> 0;
    h1 = (h1 + b) >>> 0;
    h2 = (h2 + c) >>> 0;
    h3 = (h3 + d) >>> 0;
    h4 = (h4 + e) >>> 0;
    h5 = (h5 + f) >>> 0;
    h6 = (h6 + g) >>> 0;
    h7 = (h7 + h) >>> 0;
  }
  return h0;
}
