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

import * as stdPath from "@std/path";

// Parses `fileContents` for all named imports and returns an object of symbolName => moduleName.
export function getModuleImports(fileContents) {
  // Syntax: `import * as fs from "path"`
  const importAsRegexp = /import\s+\*\s+as\s+(\S+)\s+from\s+"(.+)"/;
  // Syntax: `import {a, b} from "path"`
  const importSpecificRegexp = /import\s+{(.+)}\s+from\s+"(.+)"/;
  const importLines = fileContents.split("\n").filter((line) => line.startsWith("import "));
  const importMap = {};
  for (const line of importLines) {
    let matches = line.match(importAsRegexp);
    if (matches != null) {
      const symbol = matches[1];
      const path = matches[2];
      importMap[symbol] = path;
    } else {
      matches = line.match(importSpecificRegexp);
      if (matches == null) continue;

      // `names` is the content inside the import statement's curly braces.
      const names = matches[1].split(",").map((s) => s.trim());
      const path = matches[2];
      for (const s of names) {
        // There are two possible syntaxes for names inside the curly braces:
        // * import {a, b}
        // * import {a, b as aliasName, c}
        if (s.includes(" as ")) {
          const name = s.split(" as ")[1].trim();
          importMap[name] = path;
        } else {
          importMap[s] = path;
        }
      }
    }
  }
  return importMap;
}

async function runRipgrep(query, args, file, projectRoot) {
  if (!query) throw new Error("query is required.");
  if (!file && !projectRoot) throw new Error("file or projectRoot is required.");

  let additionalArgs = [
    // --no-unicode makes rg's character classes simpler. Do I want this?
    // "--no-unicode",
    "--line-number",
    "--column",
    "--with-filename",
    "--no-heading",
  ];

  args = [].concat(args, additionalArgs);

  if (file) {
    args.push(file);
  } else {
    args = args.concat([
      "--glob",
      "*.js",
      projectRoot,
    ]);
  }

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
  // TODO(philc): Consider doing this output munging in the caller.
  lines = lines.map((line) => {
    const [path, lineNum, _, matchText] = line.split(":", 4);
    const col = matchText.indexOf(query);
    return [path, lineNum, col].join(":");
  });
  return lines;
}

export async function search(query, startingFile, projectRoot) {
  // If the query contains multiple symbols, see if the first is a module pointing to a local file,
  // and if so, search in that file. If not, then fall back to searching for just the symbol that's
  // immediately under the cursor.
  const queryParts = query.split(".");
  const isMultipartQuery = queryParts.length > 1;
  const isLocalPath = (s) => ["./", "../", "file:///"].find((prefix) => s.startsWith(prefix));
  const importMap = getModuleImports(await Deno.readTextFile(startingFile));
  if (isMultipartQuery) {
    let modulePath = importMap[queryParts[0]];
    if (modulePath && isLocalPath(modulePath)) {
      // Resolve the module path relative to `startingFile`.
      modulePath = stdPath.join(stdPath.dirname(startingFile), modulePath);
      startingFile = modulePath;
    }
    // TODO(philc): Make this be the symbol that's under the cursor, rather than the
    // last part of the dot chain.
    query = queryParts[queryParts.length - 1];
  }

  // These are the valid syntaxes for function declarations.
  const rgArgs = [
    // function foo(a, b) {
    "-e",
    `function\\s+${query}\\s*\\(`,
    // const foo = function(a, b) {
    "-e",
    `(const|let|var)\\s+${query}\\s*=\\s*function\\s*\\(`,
    // let foo = (a, b) => {
    "-e",
    `(const|let|var)\\s+${query}\\s*=\\s*\\([^)]*\\)\\s*=>`,
    // foo(a, b) {
    "-e",
    `^\\s*${query}\\s*\\([^)]*\\)\\s*\\{`,
  ];

  // Search the current file.
  let lines = await runRipgrep(query, rgArgs, startingFile, null);
  if (lines.length > 0) return lines;

  // If the query is a single word, see if it's a symbol imported from the module import statements.
  if (!isMultipartQuery) {
    // We didn't find a match in the local file. Check to see if the query matches an imported
    // symbol.
    let modulePath = importMap[queryParts[0]];
    if (modulePath && isLocalPath(modulePath)) {
      // Resolve the module path relative to `startingFile`.
      modulePath = stdPath.join(stdPath.dirname(startingFile), modulePath);
      startingFile = modulePath;
      lines = await runRipgrep(query, rgArgs, startingFile, null);
    }
  }
  if (lines.length > 0) return lines;

  // Search all of the project's files.
  if (projectRoot != null) {
    lines = await runRipgrep(query, rgArgs, null, projectRoot);
  }
  return lines;
}

// Returns the symbol under the cursor. If the cursor is in a property chain like "foo.bar.baz", and
// the cursor is on "bar", return "foo.bar". If there is no text under the cursor, throw an error.
export function parseQueryFromCursorPos(fileContents, lineNum, column) {
  const lines = fileContents.split("\n");
  const line = lines[lineNum - 1];
  let start = column;
  let end = column;
  // Only letters, numbers, and periods
  const word = /^[A-Za-z0-9.]+$/;
  // Extract the word surrounding the cursor.
  for (let i = column; i >= 0; i--) {
    if (!word.test(line[i])) break;
    start = i;
  }
  for (let i = column; i < line.length; i++) {
    // When traversing right, don't step over periods. In the case of f_oo.bar, return foo.
    if (!word.test(line[i]) || line[i] == ".") break;
    end = i;
  }
  const query = line.substring(start, end + 1);

  // Reject queries which are just single, non-alphanumeric characters.
  if (query.length <= 1 && (query == "." || !word.test(query))) {
    throw new Error(`Query \"${query}\" isn't the name of a symbol.`);
  }
  return query;
}

const test = false;
if (test) {
  const filenameArg = "/Users/phil/projects/vimium/pages/options.js:50:13";
  const [path, line, col] = filenameArg.split(":");
  const query = parseQueryFromCursorPos(
    await Deno.readTextFile(path),
    parseInt(line),
    parseInt(col),
  );
  const projectRoot = null;
  // file = "/Users/phil/projects/vimium/pages/vomnibar_page.js";
  // query = "UIComponentMessenger.postMessage";
  const lines = await search(query, path, projectRoot);
}

const isUnitTesting = import.meta.url != Deno.mainModule;

if (!isUnitTesting) {
  try {
    const filenameArg = Deno.args[0];
    const [path, line, col] = filenameArg.split(":");
    const query = parseQueryFromCursorPos(
      await Deno.readTextFile(path),
      parseInt(line),
      parseInt(col),
    );
    const projectRoot = Deno.args[1];
    const lines = await search(query, path, projectRoot);
    if (!test && lines.length == 0) {
      console.log("Definition not found.");
      Deno.exit(1);
    }
    console.log(lines.join("\n"));
  } catch (e) {
    console.log("Error:", e);
    Deno.exit(1);
  }
}
