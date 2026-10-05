import { assert, context, setup, should, teardown } from "@philc/shoulda";
import { positionToOffset, rename } from "../rename.js";
import * as stdPath from "@std/path";

let projectDir;

// Writes the given files (an object of relativePath => contents) to a fresh project directory.
async function writeProject(files) {
  for (const [relativePath, contents] of Object.entries(files)) {
    const path = stdPath.join(projectDir, relativePath);
    await Deno.mkdir(stdPath.dirname(path), { recursive: true });
    await Deno.writeTextFile(path, contents);
  }
}

// Like assert.throwsError, but for async functions.
async function assertRejects(fn) {
  try {
    await fn();
  } catch {
    return;
  }
  assert.fail("Expected an error, but none was thrown.");
}

async function readProjectFile(relativePath) {
  return await Deno.readTextFile(stdPath.join(projectDir, relativePath));
}

// Renames the occurrence of `oldName` marked by a "|" in `marker`, e.g. "let |foo", which must
// appear exactly once in the file.
async function renameAt(relativePath, marker, oldName, newName) {
  const path = stdPath.join(projectDir, relativePath);
  const text = await Deno.readTextFile(path);
  const offset = text.indexOf(marker.replace("|", "")) + marker.indexOf("|");
  const lines = text.slice(0, offset).split("\n");
  const line = lines.length;
  const column = lines[lines.length - 1].length;
  return await rename(path, line, column, oldName, newName, projectDir);
}

context("positionToOffset", () => {
  should("count lines and columns", () => {
    assert.equal(6, positionToOffset("abc\ndef", 2, 2));
  });

  should("count characters outside the BMP as one column", () => {
    // "😀" is two UTF-16 code units, but one character in Emacs.
    assert.equal(3, positionToOffset("😀 a", 1, 2));
  });
});

context("rename", () => {
  setup(async () => {
    projectDir = await Deno.makeTempDir();
  });

  teardown(async () => {
    await Deno.remove(projectDir, { recursive: true });
  });

  should("rename a local variable only within its function", async () => {
    await writeProject({
      "a.js": "function f() { let x = 1; return x; }\nfunction g() { let x = 2; return x; }\n",
    });
    const result = await renameAt("a.js", "let |x = 1", "x", "y");
    assert.equal({ occurrences: 2, files: 1 }, result);
    assert.equal(
      "function f() { let y = 1; return y; }\nfunction g() { let x = 2; return x; }\n",
      await readProjectFile("a.js"),
    );
  });

  should("rename from a reference, not just the declaration", async () => {
    await writeProject({ "a.js": "function f(x) { return x + 1; }\n" });
    await renameAt("a.js", "return |x", "x", "count");
    assert.equal("function f(count) { return count + 1; }\n", await readProjectFile("a.js"));
  });

  should("not rename a shadowing variable in a nested block", async () => {
    await writeProject({ "a.js": "let x = 1;\n{ let x = 2; x++; }\nx++;\n" });
    await renameAt("a.js", "let |x = 1", "x", "y");
    assert.equal("let y = 1;\n{ let x = 2; x++; }\ny++;\n", await readProjectFile("a.js"));
  });

  should("not rename properties with the same name", async () => {
    await writeProject({ "a.js": "function f(x) { return obj.x + x; }\n" });
    await renameAt("a.js", "f(|x)", "x", "y");
    assert.equal("function f(y) { return obj.x + y; }\n", await readProjectFile("a.js"));
  });

  should("expand shorthand properties so that the property name doesn't change", async () => {
    await writeProject({ "a.js": "function f(x) { return { x }; }\n" });
    await renameAt("a.js", "f(|x)", "x", "y");
    assert.equal("function f(y) { return { x: y }; }\n", await readProjectFile("a.js"));
  });

  should("rename when the cursor is on a shorthand property", async () => {
    await writeProject({ "a.js": "function f(x) { return { x }; }\n" });
    await renameAt("a.js", "{ |x }", "x", "y");
    assert.equal("function f(y) { return { x: y }; }\n", await readProjectFile("a.js"));
  });

  should("rename a class, including references inside its body", async () => {
    await writeProject({
      "a.js": "export class Foo { static make() { return new Foo(); } }\n",
    });
    await renameAt("a.js", "class |Foo", "Foo", "Bar");
    assert.equal(
      "export class Bar { static make() { return new Bar(); } }\n",
      await readProjectFile("a.js"),
    );
  });

  should("rename a non-exported top-level variable in a module only in that file", async () => {
    await writeProject({
      "a.js": 'import "./c.js";\nconst x = 1;\nconsole.log(x);\n',
      "b.js": 'import "./c.js";\nconst x = 2;\n',
    });
    const result = await renameAt("a.js", "const |x", "x", "y");
    assert.equal({ occurrences: 2, files: 1 }, result);
    assert.equal('import "./c.js";\nconst x = 2;\n', await readProjectFile("b.js"));
  });

  should("rename globals across classic scripts", async () => {
    await writeProject({
      "hud.js": "const HUD = { show() {} };\nglobalThis.HUD = HUD;\n",
      "mode.js": "function f() { HUD.show(); }\nfunction g(HUD) { return HUD; }\n",
    });
    const result = await renameAt("mode.js", "{ |HUD.show", "HUD", "Hud");
    assert.equal({ occurrences: 4, files: 2 }, result);
    assert.equal(
      "const Hud = { show() {} };\nglobalThis.Hud = Hud;\n",
      await readProjectFile("hud.js"),
    );
    // The parameter named HUD in g() shadows the global, so it's unchanged.
    assert.equal(
      "function f() { Hud.show(); }\nfunction g(HUD) { return HUD; }\n",
      await readProjectFile("mode.js"),
    );
  });

  should("rename globals which modules assign to globalThis", async () => {
    await writeProject({
      "utils.js": "const Utils = {};\nglobalThis.Utils = Utils;\n",
      "main.js": 'import "./utils.js";\nUtils.foo();\n',
    });
    const result = await renameAt("utils.js", "const |Utils", "Utils", "Util");
    assert.equal({ occurrences: 4, files: 2 }, result);
    assert.equal('import "./utils.js";\nUtil.foo();\n', await readProjectFile("main.js"));
  });

  should("rename an export and its named imports", async () => {
    await writeProject({
      "lib.js": "export function foo() {}\n",
      "main.js": 'import { foo } from "./lib.js";\nfoo();\n',
      "other.js": 'import { foo as f } from "./lib.js";\nf();\n',
    });
    const result = await renameAt("main.js", "|foo();", "foo", "bar");
    assert.equal({ occurrences: 4, files: 3 }, result);
    assert.equal("export function bar() {}\n", await readProjectFile("lib.js"));
    assert.equal('import { bar } from "./lib.js";\nbar();\n', await readProjectFile("main.js"));
    assert.equal(
      'import { bar as f } from "./lib.js";\nf();\n',
      await readProjectFile("other.js"),
    );
  });

  should("rename an export used via a namespace import", async () => {
    await writeProject({
      "lib.js": "export function foo() {}\n",
      "main.js": 'import * as lib from "./lib.js";\nlib.foo();\n',
    });
    await renameAt("main.js", "lib.|foo", "foo", "bar");
    assert.equal("export function bar() {}\n", await readProjectFile("lib.js"));
    assert.equal(
      'import * as lib from "./lib.js";\nlib.bar();\n',
      await readProjectFile("main.js"),
    );
  });

  should("refuse to rename properties", async () => {
    await writeProject({ "a.js": "obj.foo();\n" });
    await assertRejects(() => renameAt("a.js", "obj.|foo", "foo", "bar"));
  });

  should("refuse a new name which is already declared in scope", async () => {
    const original = "function f() { let x = 1; let y = 2; return x + y; }\n";
    await writeProject({ "a.js": original });
    await assertRejects(() => renameAt("a.js", "let |x", "x", "y"));
    assert.equal(original, await readProjectFile("a.js"));
  });

  should("refuse a new name which would capture an outer variable", async () => {
    const original = "let y = 1;\nfunction f() { let x = 2; return x + y; }\n";
    await writeProject({ "a.js": original });
    await assertRejects(() => renameAt("a.js", "let |x", "x", "y"));
    assert.equal(original, await readProjectFile("a.js"));
  });

  should("refuse to rename when a file has a syntax error, and change no files", async () => {
    await writeProject({
      "a.js": "globalThis.Foo = 1;\n",
      "b.js": "Foo(;\n",
    });
    await assertRejects(() => renameAt("a.js", "globalThis.|Foo", "Foo", "Bar"));
    assert.equal("globalThis.Foo = 1;\n", await readProjectFile("a.js"));
  });

  should("refuse an invalid new name", async () => {
    await writeProject({ "a.js": "let x = 1;\n" });
    await assertRejects(() => renameAt("a.js", "let |x", "x", "class"));
    await assertRejects(() => renameAt("a.js", "let |x", "x", "a-b"));
  });
});
