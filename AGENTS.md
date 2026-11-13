# Formatting and linting

After editing files, run `make fix` (see "Development" in README.md). It reindents the elisp files,
formats every other file with dprint, and lints the JavaScript with `deno lint`, applying automatic
fixes. Fix any lint errors it reports.
