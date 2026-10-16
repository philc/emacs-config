EMACS ?= /opt/homebrew/bin/emacs
ELISP_DIR := elisp
# Elisp files to format and lint. Excludes third-party code (plugins/), and generated or data files
# (custom.el, tempel-snippets.el).
ELISP_FILES := init.el early-init.el packages.el tangotango-theme.el \
	scripts/package-load-path.el \
	scripts/bench/mlm-bench-measure.el \
	$(wildcard $(ELISP_DIR)/*.el)

# -Q skips the real init.el, loading only what the test files themselves require.
.PHONY: test
test:
	$(EMACS) -Q --batch \
		-l scripts/package-load-path.el \
		--eval '(setq load-prefer-newer t)' \
		-L $(ELISP_DIR) \
		$(foreach f,$(wildcard $(ELISP_DIR)/*-test.el),-l $(f)) \
		-f ert-run-tests-batch-and-exit

# Byte-compiles the files in elisp/ (except the tests), and puts the .elc files next to them, so Emacs
# loads them as compiled code (and natively compiles them in the background), which is much faster
# than interpreting them. early-init.el sets `load-prefer-newer', so a .el edited since its .elc was
# compiled is loaded instead of the .elc. A file with compile warnings doesn't get a new .elc.
.PHONY: compile
compile:
	@EMACS=$(EMACS) scripts/compile-elisp.sh --install $(filter-out %-test.el,$(wildcard $(ELISP_DIR)/*.el))

# Benchmarks markdown-lite-mode at REV (default HEAD) against the working tree, on FILES, e.g.
# make bench FILES='notes/a.md notes/b.md', or without FILES on generated files. See
# scripts/bench/mlm-bench.js.
REV ?= HEAD
.PHONY: bench
bench:
	@EMACS=$(EMACS) scripts/bench/mlm-bench.js $(REV) $(FILES)

# Reindents the elisp files and removes trailing whitespace.
.PHONY: fmt
fmt:
	$(EMACS) -Q --batch -L $(ELISP_DIR) -l scripts/format-elisp.el -f format-elisp-batch $(ELISP_FILES)

# Runs all checks, and fails if any of them fail.
.PHONY: check
check:
	@status=0; \
	$(MAKE) --no-print-directory check-compile || status=1; \
	$(MAKE) --no-print-directory check-regexps || status=1; \
	exit $$status

# Byte-compiles each elisp file in its own fresh Emacs, and fails on any warning. See
# scripts/compile-elisp.sh.
.PHONY: check-compile
check-compile:
	@EMACS=$(EMACS) scripts/compile-elisp.sh $(ELISP_FILES)

# Checks regular expressions for mistakes, using relint. relint is installed via `my-packages` in
# packages.el.
.PHONY: check-regexps
check-regexps:
	$(EMACS) -Q --batch \
		-l scripts/package-load-path.el \
		--eval '(setq load-prefer-newer t)' \
		--eval "(unless (require 'relint nil t) (error \"relint isn't installed. Run make sync-packages\"))" \
		-f relint-batch $(ELISP_FILES)

# Package management. Packages are installed by straight.el, at the versions in package-lock.el. See
# packages.el and elisp/package-upgrade.el.
PACKAGES := $(EMACS) -Q --batch -l packages.el -L $(ELISP_DIR) -l package-upgrade

# Installs any missing packages, and checks out the versions in package-lock.el, e.g. after pulling
# a change to it.
.PHONY: sync-packages
sync-packages:
	$(PACKAGES) -f package-upgrade/sync

# Moves each package to its latest release (or newest commit, for those in `my-packages-at-head`),
# and records the new versions in package-lock.el.
.PHONY: upgrade-packages
upgrade-packages:
	$(PACKAGES) -f package-upgrade/upgrade-all

# Prints the version of each installed package.
.PHONY: package-versions
package-versions:
	@$(PACKAGES) -f package-upgrade/print-versions
