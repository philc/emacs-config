#!/usr/bin/env -S deno run --allow-read --allow-run
// Usage:
// jump_to_symbol.js filePath line column
// Outputs the file and line number of the match:
// line:column filePath
// Line is one-based and column is zero-based, as is the Unix convention.
//
// This doesn't handle variable shadowing.
//
// NOTE(philc): For more robust inspection of a specific js file, the npm:acorn parser could be
// used.

let file = Deno.args[0];
let query = Deno.args[1];
let projectRoot = Deno.args[2];

async function runRipgrep(query, file, projectRoot) {
  if (!query) throw new Error("query is required.");
  if (!file && !projectRoot) throw new Error("file or projectRoot is required.");

  const regexpArgs = [
    // function foo(a, b) {
    "-e",
    // "function\\s+\\w+\\s*\\(",
    `function\\s+${query}\\s*\\(`,
    // const theFn = function(a, b) {
    "-e",
    `(const|let|var)\\s+${query}\\s*=\\s*function\\s*\\(`,
    // let theFn = (a, b) => {
    "-e",
    `(const|let|var)\\s+${query}\\s*=\\s*\\([^)]*\\)\\s*=>`,
    // // theFn(a, b) {
    "-e",
    `^\\s*${query}\\s*\\([^)]*\\)\\s*\\{`,
  ];

  let args = [
    // --no-unicode makes rg's character classes simpler. Do I want this?
    // "--no-unicode",
    "--line-number",
    "--column",
    "--with-filename",
    "--no-heading",
  ];

  if (file) {
    args.push(file);
  } else {
    args = args.concat([
      "--glob",
      "*.js",
      projectRoot,
    ]);
  }

  args = args.concat(regexpArgs);
  const command = new Deno.Command("rg", {
    args,
    stdout: "piped",
    stderr: "piped",
  });

  const { code, stdout, stderr } = await command.output(); // code is an int.
  if (code == 1) {
    // No matches found.
    return [];
  }
  if (code > 1) {
    const err = new TextDecoder().decode(stderr).trim();
    throw new Error("rg failed: " + err);
  }
  const str = new TextDecoder().decode(stdout).trim();
  let lines = str.split("\n");
  // Remove the matched string, and just retain path:line:column. Also, adjust the column to where
  // the query occurs, rather than where the regexp first matched.
  lines = lines.map((line) => {
    const [path, lineNum, _, matchText] = line.split(":", 4);
    const col = matchText.indexOf(query);
    return [path, lineNum, col].join(":");
  });
  return lines;
}

export async function search(query, startingFile, projectRoot) {
  let lines = await runRipgrep(query, startingFile, null);
  if (lines.length == 0 && projectRoot != null) {
    lines = await runRipgrep(query, null, projectRoot);
  }
  return lines;
}

const test = false;
if (test) {
  file = "/Users/phil/projects/vimium/pages/vomnibar_page.js";
  query = "init";
  const lines = await search(query, file, projectRoot);
  console.log("lines:", lines);
}

const isUnitTesting = import.meta.url != Deno.mainModule;

if (!isUnitTesting) {
  try {
    const lines = await search(query, file, projectRoot);
    if (!test && lines.length == 0) {
      Deno.exit(1);
    }
    console.log(lines.join("\n"));
  } catch (e) {
    console.log("Error:", e);
  }
}
