import { assert, context, setup, should, teardown } from "@philc/shoulda";
import { getSymbols } from "../list_symbols.js";
// import * as fs from "jsr:@std/fs";

context("getSymbols", () => {
  should("functions", () => {
    assert.equal([[1, 11, "example"]], getSymbols("  function example() {"));
    assert.equal([[1, 17, "example"]], getSymbols("  async function example() {"));
  });

  should("empty functions", () => {
    assert.equal([[1, 9, "example"]], getSymbols("function example() {}"));
  });

  should("class member syntax", () => {
    assert.equal([[1, 2, "example"]], getSymbols("  example() {"));
    assert.equal([[1, 7, "example"]], getSymbols(" async example() {"));
  });

  should("class member syntax empty functions", () => {
    assert.equal([[1, 2, "example"]], getSymbols("  example() { }"));
  });
});
