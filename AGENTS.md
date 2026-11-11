# Formatting and linting

After editing files, run `make fix` (see "Development" in README.md). It reindents the elisp files,
formats every other file with dprint, and lints the JavaScript with `deno lint`, applying automatic
fixes. Fix any lint errors it reports.

# JavaScript style

* Name exports to read well with the module's namespace prefix, since modules are imported under a
  namespace: `cut.plan`, not `cut.planCuts`. For a module whose main export is a class, name the
  class for what it is within the module: `export class Tree` in `file_tree.js`, used as `new
  fileTree.Tree()`. Don't use default exports.
* Wrap comments at 100 columns. The formatter doesn't wrap them. A line can be longer if it's long
  because of something which can't be broken, like a URL.
