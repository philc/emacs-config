import { assert, context, setup, should, teardown } from "@philc/shoulda";
import { getModuleImports, parseQueryFromCursorPos, search } from "../goto_def.js";
import * as fs from "jsr:@std/fs";
import * as stdPath from "@std/path";

const fixturesDir = "tests/tmpFixtures";
const fixture1 = fixturesDir + "/goto_def_fixture1.js";
const fixture2 = fixturesDir + "/goto_def_fixture2.js";

async function writeFixture(contents, path = fixture1) {
  if (!await fs.exists(fixturesDir)) {
    await Deno.mkdir(fixturesDir);
  }
  await Deno.writeTextFile(path, contents);
}

async function deleteFixtures() {
  if (await fs.exists(fixturesDir)) {
    await Deno.remove(fixturesDir, { recursive: true });
  }
}

context("parseQueryFromCursorPos", () => {
  should("find word boundaries", () => {
    assert.equal("bar.baz", parseQueryFromCursorPos("foo bar.baz end", 1, 8));
  });

  should("return only the symbol under the cursor, not the full property chain", () => {
    assert.equal("foo.bar", parseQueryFromCursorPos("foo.bar.baz", 1, 5));
  });

  should("throw error when the query isn't a valid symbol", () => {
    assert.throwsError(() => parseQueryFromCursorPos("!!!", 1, 0));
    assert.throwsError(() => parseQueryFromCursorPos(".", 1, 0));
  });
});

context("getModuleImports", () => {
  should("find imports using import * syntax", () => {
    const file = 'import * as foo   from "pathA" \n' +
      'import * as bar from "pathB"';
    const result = getModuleImports(file);
    assert.equal({ "foo": "pathA", "bar": "pathB" }, result);
  });

  should("find imports using import { a, b} syntax", () => {
    const file = 'import { a , b as  alias, c} from "pathA"';
    const result = getModuleImports(file);
    assert.equal({ "a": "pathA", "alias": "pathA", "c": "pathA" }, result);
  });
});

context("goto_def_test", () => {
  should("return nothing when definition is not found", async () => {
    await writeFixture("");
    const got = await search("not-found", fixture1);
    assert.equal([], got);
  });

  should("include a filename, line, and column", async () => {
    await writeFixture("function foo(a, b) {}");
    const got = await search("foo", fixture1);
    const [path, line, col] = got[0].split(":", 3);
    assert.equal(fixture1, path);
    assert.equal("1", line);
    assert.equal("9", col);
  });

  should("handle const foo = function() syntax", async () => {
    await writeFixture("const foo = function (a, b) {}");
    const got = await search("foo", fixture1);
    assert.equal(1, got.length);
    const line = got[0].split(":")[1];
    assert.equal("1", line);
  });

  should("handle const foo = () => syntax", async () => {
    await writeFixture("const foo = (a, b) =>");
    const got = await search("foo", fixture1);
    assert.equal(1, got.length);
    const line = got[0].split(":")[1];
    assert.equal("1", line);
  });

  should("handle class syntax: foo(a, b) {", async () => {
    await writeFixture("foo(a, b) {");
    const got = await search("foo", fixture1);
    assert.equal(1, got.length);
    const line = got[0].split(":")[1];
    assert.equal("1", line);
  });

  should("search for the last part when no earlier part refers to a module", async () => {
    await writeFixture("function foo() {}", fixture1);
    let got = await search("this.foo", fixture1);
    assert.equal(1, got.length);
    assert.equal(fixture1 + ":1:9", got[0]);
  });

  should("resolve references to on-disk imported modules", async () => {
    await writeFixture("function foo() {}", fixture1);
    await writeFixture('import * as bar from "./goto_def_fixture1.js";\nbar.foo()', fixture2);
    let got = await search("bar.foo", fixture2);
    assert.equal(1, got.length);
    assert.equal(fixture1 + ":1:9", got[0]);

    // Test the alternate import syntax.
    await writeFixture(
      'import { foo } from "./goto_def_fixture1.js";\n' +
        "foo()",
      fixture2,
    );
    got = await search("foo", fixture2);
    assert.equal(1, got.length);
    assert.equal(fixture1 + ":1:9", got[0]);
  });

  should("resolve references to on-disk imported modules with shadowed variables", async () => {
    await writeFixture("function foo() {}", fixture1);
    // Here, the current file has shadowed the imported `foo`. Prefer `foo` from the current file.
    await writeFixture(
      'import { foo } from "./goto_def_fixture1.js";\n' +
        "function foo() {}",
      fixture2,
    );
    let got = await search("foo", fixture2);
    assert.equal(1, got.length);
    assert.equal(fixture2 + ":2:9", got[0]);
  });

  should("ignore references to imports on non-local file systems", async () => {
    await writeFixture('import { foo } from "@std/path";');
    let got = await search("bar.foo", fixture1);
    assert.equal(0, got.length);
  });

  should("expand search to project root if no matches in the local file", async () => {
    await writeFixture("", fixture1);
    await writeFixture("function foo() {}", fixture2);
    // There should be no matches when projectRoot isn't provided.
    let got = await search("foo", fixture1, null);
    assert.equal(0, got.length);
    // When projectRoot is provided, there should be one match.
    got = await search("foo", fixture1, fixturesDir);
    assert.equal(1, got.length);
    const line = got[0].split(":")[1];
    assert.equal("1", line);
  });

  teardown(async () => {
    await deleteFixtures();
  });
});
