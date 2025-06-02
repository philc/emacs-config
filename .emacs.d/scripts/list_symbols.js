#!/usr/bin/env -S deno run --allow-read
// Outputs the JavaScript function names in a given file, in this format:
//   line:column functionName
// Line and column numbers are one-based.

const file = Deno.args[0];
const text = await Deno.readTextFile(file);
const lines = text.split("\n");

const regexps = [
  // E.g.: function add(a, b) {
  /^\s*function ([^( ]+)\([^()]*\) {$/,
  // Syntax for declaring members in a class.
  // E.g.: add(a, b) {
  /^\s*([^( ]+)\([^()]*\) {$/,
];

for (let i = 0; i < lines.length; i++) {
  const line = lines[i];
  for (const re of regexps) {
    const groups = line.match(re);
    if (groups == null) continue;
    const symbol = groups[1];
    const lineNum = i + 1;
    const column = line.indexOf(symbol) + 1;
    console.log(`${lineNum}:${column}`, symbol);
  }
}
