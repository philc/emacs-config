#!/usr/bin/env -S deno run --allow-run --allow-read --allow-write --allow-env
// Benchmarks markdown-lite-mode at a git revision against the working tree's version, and prints a
// table comparing the two. `make bench` runs it.
//
// It byte-compiles both versions, starts a throwaway Emacs daemon for each, and runs
// `mlm-bench-measure/measure' (in mlm-bench-measure.el) on each file in both, for several rounds,
// alternating between the two so that both see the same machine conditions. The ROUNDS environment
// variable sets the number of rounds (default 3). The table shows each operation's median time, and
// its range over the rounds; differences within the range are noise. It never touches the live
// Emacs, and it shuts the daemons down afterwards.
//
// Usage: scripts/bench/mlm-bench.js REV [FILE...]
//
// Without FILEs, it runs on the synthetic files from write-synthetic-files.js.

import { writeSyntheticFiles } from "./write-synthetic-files.js";

const emacs = Deno.env.get("EMACS") ?? "/opt/homebrew/bin/emacs";
const emacsclient = emacs.slice(0, emacs.lastIndexOf("/") + 1) + "emacsclient";
const benchDir = decodeURIComponent(new URL(".", import.meta.url).pathname);
const repo = decodeURIComponent(new URL("../..", import.meta.url).pathname);

// Runs `program` with `args`, and returns its output. Throws an error with the output if it fails.
async function run(program, args, options = {}) {
  const command = new Deno.Command(program, { args, ...options, stdout: "piped", stderr: "piped" });
  const output = await command.output();
  const decoder = new TextDecoder();
  const stdout = decoder.decode(output.stdout);
  if (!output.success) {
    const stderr = decoder.decode(output.stderr);
    throw new Error(`${program} ${args.join(" ")} failed:\n${stdout}${stderr}`);
  }
  return stdout;
}

// Evaluates the elisp `form` in `daemon`, and returns the result as emacsclient prints it.
async function evalInDaemon(daemon, form) {
  return (await run(emacsclient, ["-s", daemon, "-e", form])).trim();
}

// Writes markdown-lite-mode.el at `rev` to tmp/old/, and the working tree's version to tmp/new/,
// and byte-compiles both.
async function compileVersions(rev, tmp) {
  const oldFile = `${tmp}/old/markdown-lite-mode.el`;
  const newFile = `${tmp}/new/markdown-lite-mode.el`;
  await Deno.mkdir(`${tmp}/old`);
  await Deno.mkdir(`${tmp}/new`);
  // Revisions from before the config moved to the repo root have it under .emacs.d/.
  let source;
  try {
    source = await run("git", ["-C", repo, "show", `${rev}:elisp/markdown-lite-mode.el`]);
  } catch {
    source = await run("git", ["-C", repo, "show", `${rev}:.emacs.d/elisp/markdown-lite-mode.el`]);
  }
  await Deno.writeTextFile(oldFile, source);
  await Deno.copyFile(`${repo}elisp/markdown-lite-mode.el`, newFile);
  await run(`${repo}scripts/compile-elisp.sh`, ["--install", oldFile, newFile], {
    env: { EMACS: emacs },
  });
}

// Starts a daemon called `name`, with the compiled markdown-lite-mode `elc` and
// mlm-bench-measure.el loaded.
async function startDaemon(name, elc) {
  const init = `(progn
    (load ${JSON.stringify(repo + "scripts/package-load-path.el")} nil t)
    (setq global-leader-prefix ";")
    (push ${JSON.stringify(repo + "elisp")} load-path)
    (load ${JSON.stringify(elc)} nil t)
    (load ${JSON.stringify(benchDir + "mlm-bench-measure.el")} nil t))`;
  await run(emacs, ["-Q", `--daemon=${name}`, "--eval", init]);
  // Check that the daemon runs compiled code, not the .el.
  const type = await evalInDaemon(name, "(type-of (symbol-function 'mlm/outline-level))");
  if (type != "byte-code-function") {
    throw new Error(
      `The daemon ${name} isn't running compiled code: mlm/outline-level is a ${type}`,
    );
  }
}

// Times the operations on `file` in `daemon`. Returns [name, ms] pairs.
//
// JSON.stringify gives a valid elisp string literal, since JSON's string escapes mean the same in
// elisp. mlm-bench-measure/measure returns an alist with symbol keys, which json-serialize turns
// into a JSON object. emacsclient prints that JSON as an elisp string literal, which is also a JSON
// string literal, hence the two JSON.parse calls.
async function measure(daemon, file) {
  const form = `(json-serialize (mlm-bench-measure/measure ${JSON.stringify(file)}))`;
  return Object.entries(JSON.parse(JSON.parse(await evalInDaemon(daemon, form))));
}

function formatMs(ms) {
  if (ms >= 1000) {
    return `${(ms / 1000).toFixed(2)}s`;
  }
  if (ms >= 10) {
    return `${ms.toFixed(0)}ms`;
  }
  if (ms >= 1) {
    return `${ms.toFixed(1)}ms`;
  }
  return `${ms.toFixed(2)}ms`;
}

function median(times) {
  const sorted = times.toSorted((a, b) => a - b);
  return sorted[Math.floor((sorted.length - 1) / 2)];
}

// The median and range of `times`, e.g. "3.60s (3.56s-3.72s)".
function summary(times) {
  if (times == null) {
    return "-";
  }
  if (times.length == 1) {
    return formatMs(times[0]);
  }
  const min = formatMs(Math.min(...times));
  const max = formatMs(Math.max(...times));
  return `${formatMs(median(times))} (${min}-${max})`;
}

// Prints the comparison table. `ops` maps each file to its operation names, in order, and `times`
// maps "version file op" keys to that operation's times.
function printTable(rev, files, ops, times) {
  const row = (op, old, new_, change) =>
    `  ${op.padEnd(30)} ${old.padEnd(28)} ${new_.padEnd(28)} ${change}`;
  for (const file of files) {
    console.log(`\n${file.slice(file.lastIndexOf("/") + 1)}`);
    console.log(row("", rev, "working tree", "change"));
    for (const op of ops.get(file)) {
      const old = times.get(`old ${file} ${op}`);
      const new_ = times.get(`new ${file} ${op}`);
      let change = "";
      if (old && new_ && median(old) > 0) {
        const percent = 100 * (median(new_) - median(old)) / median(old);
        change = `${percent >= 0 ? "+" : ""}${percent.toFixed(0)}%`;
      }
      console.log(row(op, summary(old), summary(new_), change));
    }
  }
}

async function main() {
  const [rev, ...fileArgs] = Deno.args;
  if (rev == null) {
    throw new Error("Usage: mlm-bench.js REV [FILE...]");
  }
  let files = fileArgs.map((file) => {
    try {
      return Deno.realPathSync(file);
    } catch {
      throw new Error(`No such file: ${file}`);
    }
  });
  const rounds = Number(Deno.env.get("ROUNDS") ?? 3);
  const tmp = await Deno.makeTempDir({ prefix: "mlm-bench" });
  const daemons = { old: `mlm-bench-${Deno.pid}-old`, new: `mlm-bench-${Deno.pid}-new` };
  // Each file's operation names, in the order they were run.
  const ops = new Map();
  // "version file op" -> the operation's times, one per round.
  const times = new Map();
  try {
    if (files.length == 0) {
      await Deno.mkdir(`${tmp}/files`);
      files = await writeSyntheticFiles(`${tmp}/files`);
    }
    await compileVersions(rev, tmp);
    for (const [version, daemon] of Object.entries(daemons)) {
      await startDaemon(daemon, `${tmp}/${version}/markdown-lite-mode.elc`);
    }
    for (let i = 1; i <= rounds; i++) {
      console.error(`Round ${i} of ${rounds}`);
      for (const file of files) {
        for (const [version, daemon] of Object.entries(daemons)) {
          for (const [op, ms] of await measure(daemon, file)) {
            if (!ops.has(file)) {
              ops.set(file, []);
            }
            if (!ops.get(file).includes(op)) {
              ops.get(file).push(op);
            }
            const key = `${version} ${file} ${op}`;
            if (!times.has(key)) {
              times.set(key, []);
            }
            times.get(key).push(ms);
          }
        }
      }
    }
    printTable(rev, files, ops, times);
  } finally {
    for (const daemon of Object.values(daemons)) {
      await run(emacsclient, ["-s", daemon, "-e", "(kill-emacs)"]).catch(() => {});
    }
    await Deno.remove(tmp, { recursive: true });
  }
}

try {
  await main();
} catch (e) {
  console.error(e.message);
  Deno.exit(1);
}
