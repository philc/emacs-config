import { assert, context, should } from "@philc/shoulda";
import * as listSymbols from "../list_symbols.js";
// import * as fs from "jsr:@std/fs";

context("listSymbols.parse", () => {
  should("functions", () => {
    assert.equal([[1, 11, "example"]], listSymbols.parse("  function example() {"));
    assert.equal([[1, 17, "example"]], listSymbols.parse("  async function example() {"));
  });

  should("empty functions", () => {
    assert.equal([[1, 9, "example"]], listSymbols.parse("function example() {}"));
  });

  should("class member syntax", () => {
    assert.equal([[1, 2, "example"]], listSymbols.parse("  example() {"));
    assert.equal([[1, 7, "example"]], listSymbols.parse(" async example() {"));
  });

  should("class member syntax empty functions", () => {
    assert.equal([[1, 2, "example"]], listSymbols.parse("  example() { }"));
  });
});
