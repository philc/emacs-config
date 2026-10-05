#!/usr/bin/env -S deno run --allow-read --allow-write --allow-run=git
// Usage:
// rename.js filePath:line:column oldName newName projectRoot
// Renames the JavaScript variable at the given position, editing files in place. Outputs a summary:
// "6 occurrences replaced"
// Line is one-based and column is zero-based, as is the Unix convention. The column counts
// characters (Unicode code points), as Emacs does.
//
// projectRoot is the directory searched for the other files a rename can affect: modules which
// import an exported variable, and files which reference a global.
//
// The rename is limited to the variable's scope, which is determined by parsing the files:
// * A variable declared in a block or function: only that block or function.
// * A top-level variable in a module (a file with import/export): that file, plus the files which
//   import it, if it's exported.
// * A global: every file in the project which references it. Globals are top-level variables in
//   classic scripts (files without import/export, like browser extension content scripts), names
//   assigned to via `globalThis.name = ...`, and names which are used but not declared.
//
// Object properties and methods can't be renamed, because that requires knowing the types of
// objects.
//
// If there's a syntax error in any of the affected files, nothing is renamed.

import * as espree from "espree";
import * as eslintScope from "eslint-scope";
import * as fs from "@std/fs";
import * as stdPath from "@std/path";

// Objects which can be used to assign globals, e.g. `globalThis.Person = Person`.
const globalObjectNames = ["globalThis", "window", "self"];

// Parses a JS file and analyzes its scopes. Files with import or export statements are modules;
// all others are classic scripts, whose top-level declarations are globals.
export function parseFile(text, path) {
  // `range` adds each node's [start, end] character offsets in the text. `loc` adds its start and
  // end line and column.
  const options = { ecmaVersion: "latest", range: true, loc: true };
  let ast;
  try {
    ast = espree.parse(text, { ...options, sourceType: "module" });
  } catch (moduleError) {
    // Some valid classic scripts aren't valid modules, because modules are always in strict mode.
    try {
      ast = espree.parse(text, { ...options, sourceType: "script" });
    } catch {
      const e = moduleError;
      throw new Error(`Syntax error at ${path}:${e.lineNumber}:${e.column}: ${e.message}`);
    }
  }
  const isModule = ast.body.some((node) =>
    [
      "ImportDeclaration",
      "ExportNamedDeclaration",
      "ExportDefaultDeclaration",
      "ExportAllDeclaration",
    ]
      .includes(node.type)
  );
  if (!isModule && ast.sourceType == "module") {
    ast = espree.parse(text, { ...options, sourceType: "script" });
  }
  const sourceType = isModule ? "module" : "script";
  const scopeManager = eslintScope.analyze(ast, {
    ecmaVersion: espree.latestEcmaVersion,
    sourceType,
  });

  // Map each node to its parent, for inspecting the context that an identifier appears in.
  const parents = new Map();
  const walk = (node, parent) => {
    parents.set(node, parent);
    for (const key of Object.keys(node)) {
      if (key == "parent") {
        continue;
      }
      const value = node[key];
      const children = Array.isArray(value) ? value : [value];
      for (const child of children) {
        if (child && typeof child.type == "string") {
          walk(child, node);
        }
      }
    }
  };
  walk(ast, null);

  return { path, text, ast, isModule, scopeManager, parents };
}

// Converts a one-based line and a zero-based column of characters into an offset into `text`.
// JS strings index by UTF-16 code units, so characters above U+FFFF (e.g. emoji) count as two.
export function positionToOffset(text, line, column) {
  const lines = text.split("\n");
  if (line < 1 || line > lines.length) {
    throw new Error(`Line ${line} is out of range.`);
  }
  let offset = 0;
  for (let i = 0; i < line - 1; i++) {
    offset += lines[i].length + 1;
  }
  const prefix = Array.from(lines[line - 1]).slice(0, column).join("");
  return offset + prefix.length;
}

// Returns the identifier at `offset`. There can be more than one: in the shorthand property
// `{ foo }`, the property name and the variable are separate nodes. Prefer the variable.
function findIdentifierAt(file, offset) {
  const matches = Array.from(file.parents.keys()).filter((node) =>
    node.type == "Identifier" && node.range[0] <= offset && offset <= node.range[1]
  );
  return matches.find((node) => variableForIdentifier(file, node) !== undefined) ?? matches[0];
}

// Returns the variable which `identifier` is a declaration of or a reference to, or null if the
// identifier refers to an undeclared global.
function variableForIdentifier(file, identifier) {
  for (const scope of file.scopeManager.scopes) {
    for (const variable of scope.variables) {
      if (variable.identifiers.includes(identifier)) {
        return variable;
      }
    }
    for (const ref of scope.references) {
      if (ref.identifier == identifier) {
        return ref.resolved;
      }
    }
  }
  return undefined;
}

// True if `identifier` is a reference via a global object, like `HUD` in `globalThis.HUD`.
function isGlobalObjectProperty(file, identifier) {
  const parent = file.parents.get(identifier);
  return parent?.type == "MemberExpression" && parent.property == identifier &&
    !parent.computed && parent.object.type == "Identifier" &&
    globalObjectNames.includes(parent.object.name);
}

// Returns the names which this module exports under their own name, e.g. `export function foo` or
// `export { foo }`, but not `export { foo as bar }`.
function getExportedNames(file) {
  const names = new Set();
  for (const node of file.ast.body) {
    if (node.type != "ExportNamedDeclaration" || node.source) {
      continue;
    }
    const declaration = node.declaration;
    if (declaration?.id) {
      names.add(declaration.id.name);
    }
    for (const d of declaration?.declarations ?? []) {
      if (d.id.type == "Identifier") {
        names.add(d.id.name);
      }
    }
    for (const s of node.specifiers) {
      if (s.local.name == s.exported.name) {
        names.add(s.local.name);
      }
    }
  }
  return names;
}

function isAssignedToGlobalObject(file, name) {
  for (const node of file.parents.keys()) {
    if (node.type == "Identifier" && node.name == name && isGlobalObjectProperty(file, node)) {
      const member = file.parents.get(node);
      const assignment = file.parents.get(member);
      if (assignment?.type == "AssignmentExpression" && assignment.left == member) {
        return true;
      }
    }
  }
  return false;
}

// Resolves an import statement's source, like "../lib/utils.js", to an absolute path. Returns null
// for imports of packages and URLs.
function resolveImportPath(importingFile, source) {
  if (!source.startsWith("./") && !source.startsWith("../")) {
    return null;
  }
  return stdPath.resolve(stdPath.dirname(importingFile), source);
}

// A rename of one variable within one file: the identifiers to replace, plus the scope they're in,
// for detecting naming collisions.
function localRenameSites(file, variable) {
  // Some declarations create a second variable with the same declaration. A class declaration's
  // name is also a variable inside the class body, and references from within the body refer to
  // that second variable.
  const variables = file.scopeManager.scopes
    .flatMap((scope) => scope.variables)
    .filter((v) => v.identifiers.some((id) => variable.identifiers.includes(id)));
  const identifiers = new Set(variables.flatMap((v) => v.identifiers));
  // An array of eslint-scope Reference objects: one for each place these variables are used. Each
  // has these properties:
  // - identifier: the Identifier AST node where the variable is used.
  // - from: the Scope that use is in.
  // - resolved: the Variable it refers to.
  // The naming-collision checks use `from` and `resolved`. A declaration with an initializer, like
  // `let x = 1`, is also a Reference, whose identifier is the declaration's; those are filtered out.
  const refs = variables.flatMap((v) => v.references)
    .filter((ref) => !identifiers.has(ref.identifier));
  for (const ref of refs) {
    identifiers.add(ref.identifier);
  }
  return {
    identifiers: Array.from(identifiers),
    refs,
    declaringScope: variable.scope,
  };
}

// If `identifier` is `foo` in `ns.foo`, where `ns` is from `import * as ns from "./file.js"`,
// returns the absolute path of the imported file.
function namespaceImportPath(file, identifier) {
  const member = file.parents.get(identifier);
  if (member?.type != "MemberExpression" || member.property != identifier || member.computed) {
    return null;
  }
  if (member.object.type != "Identifier") {
    return null;
  }
  const def = variableForIdentifier(file, member.object)?.defs[0];
  if (def?.type != "ImportBinding" || def.node.type != "ImportNamespaceSpecifier") {
    return null;
  }
  return resolveImportPath(file.path, def.parent.source.value);
}

// Returns the identifiers in `file` which refer to the global named `name`.
function globalRenameSites(file, name) {
  const globalScope = file.scopeManager.globalScope;
  const identifiers = new Set();
  const refs = [];
  for (const ref of globalScope.through) {
    if (ref.identifier.name == name) {
      identifiers.add(ref.identifier);
      refs.push(ref);
    }
  }
  // In modules, a top-level declaration which is then assigned to globalThis is the global itself,
  // e.g. `const HUD = {...}; globalThis.HUD = HUD;`.
  const topLevelScope = file.isModule ? globalScope.childScopes[0] : globalScope;
  const declared = topLevelScope.set.get(name);
  if (declared && (!file.isModule || isAssignedToGlobalObject(file, name))) {
    const sites = localRenameSites(file, declared);
    for (const identifier of sites.identifiers) {
      identifiers.add(identifier);
    }
    refs.push(...sites.refs);
  }
  for (const node of file.parents.keys()) {
    if (node.type == "Identifier" && node.name == name && isGlobalObjectProperty(file, node)) {
      identifiers.add(node);
    }
  }
  return {
    identifiers: Array.from(identifiers),
    refs,
    declaringScope: globalScope,
  };
}

// Throws an error if renaming the given sites to `newName` would change the meaning of the code:
// either a renamed reference would resolve to a different, existing variable named `newName`, or an
// existing reference to `newName` would resolve to the renamed variable.
function checkCollisions(file, sites, newName) {
  const location = (node) => `${file.path}:${node.loc.start.line}:${node.loc.start.column}`;
  const isWithin = (scope, ancestor) => {
    for (let s = scope; s; s = s.upper) {
      if (s == ancestor) {
        return true;
      }
    }
    return false;
  };
  for (const ref of sites.refs) {
    for (let scope = ref.from; scope; scope = scope.upper) {
      const existing = scope.set.get(newName);
      if (existing) {
        throw new Error(
          `"${newName}" is already declared at ${location(existing.identifiers[0])}, which would ` +
            `change the meaning of the reference at ${location(ref.identifier)}.`,
        );
      }
      if (scope == sites.declaringScope) {
        break;
      }
    }
  }
  const declaringScope = sites.declaringScope;
  if (declaringScope.set.get(newName)) {
    const existing = declaringScope.set.get(newName);
    throw new Error(`"${newName}" is already declared at ${location(existing.identifiers[0])}.`);
  }
  for (const scope of file.scopeManager.scopes) {
    for (const ref of scope.references) {
      if (ref.identifier.name != newName || !isWithin(ref.from, declaringScope)) {
        continue;
      }
      const resolvedOutside = !ref.resolved || !isWithin(ref.resolved.scope, declaringScope);
      if (resolvedOutside) {
        throw new Error(
          `The existing reference to "${newName}" at ${location(ref.identifier)} would refer to ` +
            `the renamed variable.`,
        );
      }
    }
  }
}

// Returns the replacement text for `identifier`. Usually that's `newName`, but shorthand properties
// need to be expanded so that the object's property name doesn't change: `{ foo }` becomes
// `{ foo: newName }`.
function replacementFor(file, identifier, newName) {
  let parent = file.parents.get(identifier);
  // A shorthand property with a default value: `const { foo = 1 } = obj`.
  if (parent?.type == "AssignmentPattern" && parent.left == identifier) {
    parent = file.parents.get(parent);
  }
  const isShorthandValue = parent?.type == "Property" && parent.shorthand &&
    parent.key.range[0] == identifier.range[0];
  if (isShorthandValue) {
    return `${identifier.name}: ${newName}`;
  }
  // `import { foo } from "..."` where only the local name is being renamed.
  if (parent?.type == "ImportSpecifier" && parent.imported == parent.local) {
    return `${identifier.name} as ${newName}`;
  }
  return newName;
}

// Returns the JS files in the project. Uses git to skip ignored files, if it's a git repo.
async function listProjectFiles(projectRoot) {
  const command = new Deno.Command("git", {
    args: ["ls-files", "--cached", "--others", "--exclude-standard", "--", "*.js", "*.mjs"],
    cwd: projectRoot,
    stdout: "piped",
    stderr: "piped",
  });
  let output;
  try {
    output = await command.output();
  } catch (e) {
    // git isn't installed. Fall through to walking the directory.
    if (!(e instanceof Deno.errors.NotFound)) {
      throw e;
    }
  }
  // A nonzero exit code means projectRoot isn't in a git repo.
  if (output?.code == 0) {
    const str = new TextDecoder().decode(output.stdout).trim();
    return str == "" ? [] : str.split("\n").map((p) => stdPath.join(projectRoot, p));
  }
  // Matches paths inside a node_modules directory or a hidden directory (e.g. .git), at any depth.
  // node_modules holds installed dependencies, which aren't the project's own code. In a git repo,
  // .gitignore usually excludes these, but this fallback has no ignore rules to go by.
  const skippedDirRegexp = /(^|\/)(node_modules|\.[^/]+)\//;
  const files = [];
  for await (const entry of fs.walk(projectRoot, { exts: [".js", ".mjs"] })) {
    // The path is made relative so that a projectRoot which is itself inside a hidden directory
    // (e.g. ~/.emacs.d) isn't skipped.
    const relativePath = stdPath.relative(projectRoot, entry.path);
    if (skippedDirRegexp.test(relativePath)) {
      continue;
    }
    if (entry.isFile) {
      files.push(entry.path);
    }
  }
  return files;
}

// Returns a map of path => list of edits, where each edit is { range, replacement }. Edits are
// computed for every affected file before any are written, so that a syntax error or naming
// collision anywhere leaves all files unchanged.
export async function computeEdits(path, line, column, oldName, newName, projectRoot) {
  const reservedWords = new Set(
    ("break case catch class const continue debugger default delete do else enum export extends " +
      "false finally for function if import in instanceof new null return super switch this throw " +
      "true try typeof var void while with yield let static implements interface package private " +
      "protected public await arguments eval").split(" "),
  );

  // A letter, _ or $, followed by any number of letters, digits, _ or $. This is stricter than JS,
  // which also allows non-ASCII letters in identifiers.
  const identifierRegexp = /^[A-Za-z_$][\w$]*$/;
  if (!identifierRegexp.test(newName) || reservedWords.has(newName)) {
    throw new Error(`"${newName}" isn't a valid variable name.`);
  }
  if (oldName == newName) {
    throw new Error("The new name is the same as the old name.");
  }
  path = stdPath.resolve(path);
  projectRoot = stdPath.resolve(projectRoot);

  const files = new Map(); // path => parsed file
  const getFile = async (p) => {
    if (!files.has(p)) {
      files.set(p, parseFile(await Deno.readTextFile(p), p));
    }
    return files.get(p);
  };

  const file = await getFile(path);
  const identifier = findIdentifierAt(file, positionToOffset(file.text, line, column));
  if (identifier?.name != oldName) {
    throw new Error(`There's no variable named "${oldName}" at ${path}:${line}:${column}.`);
  }

  // Each element is { file, sites }.
  const renames = [];
  const renameGlobal = async () => {
    const containsName = new RegExp(`(^|[^\\w$])${oldName.replace(/\$/g, "\\$")}([^\\w$]|$)`);
    const paths = await listProjectFiles(projectRoot);
    if (!paths.includes(path)) {
      paths.push(path);
    }
    for (const p of paths) {
      // Only parse the files which mention the name. This is faster, and a syntax error in an
      // unrelated file won't prevent the rename.
      if (p != path && !containsName.test(await Deno.readTextFile(p))) {
        continue;
      }
      const f = await getFile(p);
      renames.push({ file: f, sites: globalRenameSites(f, oldName) });
    }
  };

  // Renames an exported variable in its module, and in the modules which import it by name.
  const renameExport = async (exportingFile, variable) => {
    renames.push({ file: exportingFile, sites: localRenameSites(exportingFile, variable) });
    for (const p of await listProjectFiles(projectRoot)) {
      if (p == exportingFile.path || !(await Deno.readTextFile(p)).includes(oldName)) {
        continue;
      }
      const f = await getFile(p);
      for (const node of f.ast.body) {
        if (node.type != "ImportDeclaration") {
          continue;
        }
        if (resolveImportPath(f.path, node.source.value) != exportingFile.path) {
          continue;
        }
        for (const s of node.specifiers) {
          if (s.type == "ImportSpecifier" && s.imported.name == oldName) {
            if (s.imported == s.local) {
              // `import { foo }`: rename the import and all of its uses.
              const v = f.scopeManager.getDeclaredVariables(s)[0];
              renames.push({ file: f, sites: localRenameSites(f, v), renameImport: true });
            } else {
              // `import { foo as bar }`: only the imported name changes.
              const sites = { identifiers: [s.imported], refs: [], declaringScope: null };
              renames.push({ file: f, sites, renameImport: true });
            }
          } else if (s.type == "ImportNamespaceSpecifier") {
            // `import * as ns`: rename uses of `ns.foo`.
            const v = f.scopeManager.getDeclaredVariables(s)[0];
            const identifiers = v.references
              .map((ref) => f.parents.get(ref.identifier))
              .filter((m) =>
                m?.type == "MemberExpression" && !m.computed && m.property.name == oldName
              )
              .map((m) => m.property);
            renames.push({ file: f, sites: { identifiers, refs: [], declaringScope: null } });
          }
        }
      }
    }
  };

  // Renames the variable named `oldName` which is exported from the module at `sourcePath`.
  const renameExportFrom = async (sourcePath) => {
    const sourceFile = await getFile(sourcePath);
    const sourceVariable = sourceFile.scopeManager.globalScope.childScopes[0]?.set.get(oldName);
    if (!sourceVariable || !getExportedNames(sourceFile).has(oldName)) {
      throw new Error(`Couldn't find the export of "${oldName}" in ${sourcePath}.`);
    }
    await renameExport(sourceFile, sourceVariable);
  };

  const variable = variableForIdentifier(file, identifier);
  const scopeType = variable?.scope.type;
  const importDef = variable?.defs.find((d) => d.type == "ImportBinding");
  const isImportedByName = importDef?.node.type == "ImportSpecifier" &&
    importDef.node.imported == importDef.node.local &&
    resolveImportPath(path, importDef.parent.source.value);

  if (variable === undefined) {
    // The identifier isn't a variable, so it's a property, unless it's a reference to a module's
    // export via a namespace import (`ns.foo`) or a global (`globalThis.foo`).
    const sourcePath = namespaceImportPath(file, identifier);
    if (sourcePath) {
      await renameExportFrom(sourcePath);
    } else if (isGlobalObjectProperty(file, identifier)) {
      await renameGlobal();
    } else {
      throw new Error(
        `"${oldName}" at ${path}:${line}:${column} is a property, not a variable. Renaming ` +
          `properties isn't supported.`,
      );
    }
  } else if (variable == null || scopeType == "global") {
    await renameGlobal();
  } else if (scopeType == "module" && isAssignedToGlobalObject(file, oldName)) {
    await renameGlobal();
  } else if (scopeType == "module" && getExportedNames(file).has(oldName)) {
    await renameExport(file, variable);
  } else if (isImportedByName) {
    // The cursor is on a name imported from another file in the project. Rename it at its source.
    await renameExportFrom(resolveImportPath(path, importDef.parent.source.value));
  } else {
    renames.push({ file, sites: localRenameSites(file, variable) });
  }

  const edits = new Map();
  for (const { file: f, sites, renameImport } of renames) {
    if (sites.declaringScope) {
      checkCollisions(f, sites, newName);
    }
    if (!edits.has(f.path)) {
      edits.set(f.path, new Map());
    }
    const fileEdits = edits.get(f.path);
    for (const identifier of sites.identifiers) {
      // When renaming an import along with the export, `import { foo }` becomes
      // `import { newName }` rather than `import { foo as newName }`.
      const parent = f.parents.get(identifier);
      const replacement = renameImport && parent?.type == "ImportSpecifier"
        ? newName
        : replacementFor(f, identifier, newName);
      // An identifier can appear in more than one site, e.g. `export { foo }`, where the local and
      // exported names are the same node.
      fileEdits.set(identifier.range[0], { range: identifier.range, replacement });
    }
  }
  const result = new Map();
  for (const [p, fileEdits] of edits) {
    if (fileEdits.size > 0) {
      result.set(p, { text: files.get(p).text, edits: [...fileEdits.values()] });
    }
  }
  return result;
}

// Applies the edits to the text. Edits are applied from the end of the file backwards, so that
// earlier offsets remain valid.
export function applyEdits(text, edits) {
  const sorted = edits.toSorted((a, b) => b.range[0] - a.range[0]);
  for (const { range, replacement } of sorted) {
    text = text.slice(0, range[0]) + replacement + text.slice(range[1]);
  }
  return text;
}

// Renames the variable and writes the changes to disk. Returns the number of occurrences and files
// changed.
export async function rename(path, line, column, oldName, newName, projectRoot) {
  const edits = await computeEdits(path, line, column, oldName, newName, projectRoot);
  let occurrences = 0;
  for (const [p, { text, edits: fileEdits }] of edits) {
    await Deno.writeTextFile(p, applyEdits(text, fileEdits));
    occurrences += fileEdits.length;
  }
  return { occurrences, files: edits.size };
}

const isUnitTesting = import.meta.url != Deno.mainModule;

if (!isUnitTesting) {
  try {
    const [filenameArg, oldName, newName, projectRoot] = Deno.args;
    if (!filenameArg || !oldName || !newName || !projectRoot) {
      throw new Error("Usage: rename.js filePath:line:column oldName newName projectRoot");
    }
    const [path, line, col] = filenameArg.split(":");
    const result = await rename(path, parseInt(line), parseInt(col), oldName, newName, projectRoot);
    const plural = result.occurrences == 1 ? "occurrence" : "occurrences";
    const inFiles = result.files > 1 ? ` in ${result.files} files` : "";
    console.log(`${result.occurrences} ${plural} replaced${inFiles}`);
  } catch (e) {
    console.log("Error:", e.message);
    Deno.exit(1);
  }
}
