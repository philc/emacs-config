EMACS ?= /opt/homebrew/bin/emacs
ELISP_DIR := .emacs.d/elisp

# -Q skips the real init.el, loading only what the test files themselves require.
# global-leader-prefix must be set before markdown-lite-mode.el loads, since loading it defines its
# keybindings (via define-leader-keys), which reads that variable; it's normally set in init.el.
.PHONY: test
test:
	$(EMACS) -Q --batch \
		--eval '(setq package-user-dir (expand-file-name "~/.emacs.d/elpa"))' \
		--eval '(package-initialize)' \
		--eval '(setq global-leader-prefix ";")' \
		-L $(ELISP_DIR) \
		$(foreach f,$(wildcard $(ELISP_DIR)/*-test.el),-l $(f)) \
		-f ert-run-tests-batch-and-exit
