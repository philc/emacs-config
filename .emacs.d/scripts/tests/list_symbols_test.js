import { assert, context, setup, should, teardown } from "@philc/shoulda";
import { getSymbols } from "../list_symbols.js";
// import * as fs from "jsr:@std/fs";

context("getSymbols", () => {
  should("functions", () => {
    assert.equal([[1, 12, "example"]], getSymbols("  function example() {"));
    assert.equal([[1, 18, "example"]], getSymbols("  async function example() {"));
  });

  should("empty functions", () => {
    assert.equal([[1, 10, "example"]], getSymbols("function example() {}"));
  });

  should("class member syntax", () => {
    assert.equal([[1, 3, "example"]], getSymbols("  example() {"));
    assert.equal([[1, 8, "example"]], getSymbols(" async example() {"));
  });

  should("class member syntax empty functions", () => {
    assert.equal([[1, 3, "example"]], getSymbols("  example() { }"));
  });
});
