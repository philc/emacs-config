import { assert, context, setup, should, teardown } from "@philc/shoulda";
import { getModuleImports, search } from "../goto_def.js";
import * as fs from "jsr:@std/fs";

const fixturesDir = "tests/tmpFixtures";
const fixtureFile = fixturesDir + "/goto_def_fixture.js";

async function writeFixture(contents, path = fixtureFile) {
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

context("getModuleImports", () => {
  should("find imports using import * syntax", () => {
    const file = 'import * as foo   from "pathA" \n' +
          'import * as bar from "pathB"';
    const result = getModuleImports(file);
    assert.equal({ "foo": "pathA",
                   "bar": "pathB"}, result);
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
    const got = await search("not-found", fixtureFile);
    assert.equal([], got);
  });

  should("include a filename, line, and column", async () => {
    await writeFixture("function foo(a, b) {}");
    const got = await search("foo", fixtureFile);
    const [path, line, col] = got[0].split(":", 3);
    assert.equal(fixtureFile, path);
    assert.equal("1", line);
    assert.equal("9", col);
  });

  should("handle const foo = function() syntax", async () => {
    await writeFixture("const foo = function (a, b) {}");
    const got = await search("foo", fixtureFile);
    assert.equal(1, got.length);
    const line = got[0].split(":")[1];
    assert.equal("1", line);
  });

  should("handle const foo = () => syntax", async () => {
    await writeFixture("const foo = (a, b) =>");
    const got = await search("foo", fixtureFile);
    assert.equal(1, got.length);
    const line = got[0].split(":")[1];
    assert.equal("1", line);
  });

  should("handle class syntax: foo(a, b) {", async () => {
    await writeFixture("foo(a, b) {");
    const got = await search("foo", fixtureFile);
    assert.equal(1, got.length);
    const line = got[0].split(":")[1];
    assert.equal("1", line);
  });

  should("expand search to project root", async () => {
    const fixture1 = fixturesDir + "/goto_def_fixture1.js";
    const fixture2 = fixturesDir + "/goto_def_fixture2.js";
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
