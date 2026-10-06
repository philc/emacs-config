EMACS ?= /opt/homebrew/bin/emacs
ELISP_DIR := .emacs.d/elisp
# Elisp files to format and lint. Excludes third-party code (plugins/), and generated or data files
# (custom.el, tempel-snippets.el).
ELISP_FILES := .emacs.d/init.el .emacs.d/early-init.el .emacs.d/tangotango-theme.el \
	$(wildcard $(ELISP_DIR)/*.el)

# -Q skips the real init.el, loading only what the test files themselves require.
.PHONY: test
test:
	$(EMACS) -Q --batch \
		--eval '(setq package-user-dir (expand-file-name "~/.emacs.d/elpa"))' \
		--eval '(package-initialize)' \
		-L $(ELISP_DIR) \
		$(foreach f,$(wildcard $(ELISP_DIR)/*-test.el),-l $(f)) \
		-f ert-run-tests-batch-and-exit

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

# Byte-compiles the elisp files into a temp dir, and fails on any warning.
# - package-user-dir and package-initialize: the compiler loads each file's `require`d packages, to
#   know their macros and functions. -Q skips init.el, so the packages must be activated here.
# - byte-compile-docstring-max-column: the default is 80, but this repo uses 100 columns.
# - byte-compile-dest-file-function: writes the .elc files into $tmp, which is then deleted. .elc
#   files next to the sources would shadow them when loading.
.PHONY: check-compile
check-compile:
	@tmp=$$(mktemp -d); \
	out=$$($(EMACS) -Q --batch \
		--eval '(setq package-user-dir (expand-file-name "~/.emacs.d/elpa"))' \
		--eval '(package-initialize)' \
		--eval '(setq byte-compile-docstring-max-column 100)' \
		--eval "(setq byte-compile-dest-file-function \
		          (lambda (f) (expand-file-name (concat (file-name-nondirectory f) \"c\") \"$$tmp\")))" \
		-L $(ELISP_DIR) \
		-f batch-byte-compile $(ELISP_FILES) 2>&1); \
	status=$$?; \
	rm -rf "$$tmp"; \
	out=$$(printf '%s\n' "$$out" | grep -v '^Loading .* (source)\.\.\.$$'); \
	[ -n "$$out" ] && printf '%s\n' "$$out"; \
	[ $$status -eq 0 ] && ! printf '%s\n' "$$out" | grep -q 'Warning'

# Checks regular expressions for mistakes, using relint. relint is installed via `my-packages` in
# init.el.
.PHONY: check-regexps
check-regexps:
	$(EMACS) -Q --batch \
		--eval '(setq package-user-dir (expand-file-name "~/.emacs.d/elpa"))' \
		--eval '(package-initialize)' \
		--eval "(unless (require 'relint nil t) (error \"relint isn't installed. Restart Emacs to install it\"))" \
		-f relint-batch $(ELISP_FILES)
