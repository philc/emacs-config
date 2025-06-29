#!/usr/bin/env -S deno run --allow-read
// Outputs the JavaScript function names in a given file, in this format:
//   line:column functionName
// Line and column numbers are one-based.

const regexps = [
  // E.g.: async function add(a, b) {
  /^\s*(?:async )?function ([^( ]+)\([^()]*\) {\s*}?$/,
  // Syntax for declaring members in a class.
  // E.g.: add(a, b) {
  /^\s*(?:async )?([^( ]+)\([^()]*\) {\s*}?$/,
];

export function getSymbols(text) {
  const lines = text.split("\n");
  const results = [];
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    for (const re of regexps) {
      const groups = line.match(re);
      if (groups == null) continue;
      const symbol = groups[1];
      const lineNum = i + 1;
      const column = line.indexOf(symbol) + 1;
      results.push([lineNum, column, symbol]);
    }
  }
  return results;
}

const isUnitTesting = import.meta.url != Deno.mainModule;
if (!isUnitTesting) {
  const file = Deno.args[0];
  const text = await Deno.readTextFile(file);
  const results = getSymbols(text);
  for (const [line, column, symbol] of results) {
    console.log(`${line}:${column}`, symbol);
  }
}
