#!/usr/bin/env -S deno run --allow-write
// Generates markdown files for benchmarking markdown-lite-mode, so that `make bench` doesn't need
// real notes files. The files are the same on every run. There's one file like a typical large notes
// file, and several worst cases for highlighting, each a structure which makes some part of
// markdown-lite-mode scan far.
//
// mlm-bench.js imports this. To write the files somewhere to look at them:
// scripts/bench/write-synthetic-files.js DIR

const text = "lorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod";

// A large notes file: sections with headings, each with a list nested up to four levels deep, with
// wrapped continuation lines, fenced code blocks (inside items and on their own), lines starting
// with **bold** text, and paragraphs of prose.
function notes() {
  const lines = [];
  // Each item's depth, cycling through the items in a section.
  const depths = [0, 1, 2, 1, 2, 3, 0, 1];
  for (let section = 0; section < 200; section++) {
    lines.push(`# Section ${section}`, "");
    if (section % 3 == 0) {
      lines.push(text, text, text, "");
    }
    if (section % 4 == 1) {
      lines.push(`**Summary** of section ${section}: ${text}`, "");
    }
    for (let i = 0; i < 20; i++) {
      const indent = "  ".repeat(depths[i % depths.length]);
      lines.push(`${indent}* item ${i}: ${text}`);
      if (i % 5 == 2) {
        lines.push(`${indent}  ${text}`, `${indent}  ${text}`);
      }
      if (i % 9 == 4) {
        lines.push(
          `${indent}  \`\`\`js`,
          `${indent}  const x = f(${i}, "*", "#");`,
          `${indent}  return x * 2;`,
          `${indent}  \`\`\``,
        );
      }
    }
    lines.push("");
    if (section % 10 == 5) {
      lines.push("```", "* not a list item", "# not a heading", text, "```", "");
    }
  }
  return lines;
}

// One list item with 3000 wrapped lines, between two ordinary lists.
function longItem() {
  return [
    ...Array(100).fill("* item"),
    "* long item",
    ...Array(3000).fill(`  ${text}`),
    ...Array(100).fill("* item"),
  ];
}

// A list item followed by 3000 unindented lines without a blank line, e.g. a pasted stack trace.
// They aren't continuation lines (they're not indented), but they're in the run of lines after the
// item's marker line.
function lazyBlock() {
  return ["* error:", ...Array(3000).fill(text)];
}

// 3000 lines of prose without any blank lines or list items.
function proseBlock() {
  return Array(3000).fill(text);
}

// An outline of 20,000 one-line items, without blank lines.
function outline() {
  return Array.from({ length: 20000 }, (_, i) => `${"  ".repeat(i % 4)}* item ${text}`);
}

const generators = {
  "notes.md": notes,
  "long-item.md": longItem,
  "lazy-block.md": lazyBlock,
  "prose-block.md": proseBlock,
  "outline.md": outline,
};

// Writes the files into `dir`, which must exist, and returns their paths.
export async function writeSyntheticFiles(dir) {
  const paths = [];
  for (const [name, generate] of Object.entries(generators)) {
    const path = `${dir}/${name}`;
    await Deno.writeTextFile(path, generate().join("\n") + "\n");
    paths.push(path);
  }
  return paths;
}

if (import.meta.main) {
  const [dir] = Deno.args;
  if (dir == null) {
    console.error("Usage: write-synthetic-files.js DIR");
    Deno.exit(1);
  }
  await Deno.mkdir(dir, { recursive: true });
  for (const path of await writeSyntheticFiles(dir)) {
    console.log(path);
  }
}
