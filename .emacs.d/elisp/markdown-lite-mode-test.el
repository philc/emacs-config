;;; -*- lexical-binding: t; -*-
;;
;; Unit tests for markdown-lite-mode.el.
;;
;; Run these with `make test`.

(require 'ert)
(require 'cl-lib)
(setq global-leader-prefix ";")
(require 'general)
(require 'evil)
(require 'evil-ext)
(require 'markdown-lite-mode)

(defun mlm-test/fill-at-marker (text)
  "Inserts TEXT into a new markdown-lite-mode buffer, removes the \"|\" marker in it (which marks
   where point should be), and runs evil-ext/fill-inside-paragraph-or-comment-block from there.
   Returns the buffer's contents afterwards. This mimics a user pressing `SPC` (gqip) with their
   cursor inside a paragraph."
  (with-temp-buffer
    (insert text)
    (markdown-lite-mode)
    (setq-local fill-column 15)
    (goto-char (point-min))
    (search-forward "|")
    (delete-char -1)
    (evil-ext/fill-inside-paragraph-or-comment-block)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest markdown-lite-mode-test/fill-preserves-indentation-of-code-block-in-list-item ()
  "A paragraph indented more deeply than its enclosing list item -- e.g. a code block nested inside a
   bullet point, as opposed to a plain wrapped continuation of the bullet's own text -- should keep
   that deeper indentation on every line after filling, rather than having it collapsed down to align
   with the bullet's text (2 spaces, to match \"* \")."
  (let ((result
         (mlm-test/fill-at-marker
          (concat "* one:\n"
                  "\n"
                  "        |aaa bbb ccc ddd\n"))))
    (should (equal result
                   (concat "* one:\n"
                           "\n"
                           "        aaa bbb\n"
                           "        ccc ddd\n")))))

(ert-deftest markdown-lite-mode-test/promote-last-list-item-in-buffer ()
  (with-temp-buffer
    (insert "* A\n  * B\n")
    (markdown-lite-mode)
    ;; Simulate the font-lock state of a real file buffer (with-temp-buffer's buffer name
    ;; starts with a space, so global-font-lock-mode never turns font-lock on in it, and the bug
    ;; only manifests once font-lock-flush actually attempts to refontify via jit-lock).
    (setq-local font-lock-mode t)
    (setq-local font-lock-fontified t)
    (setq-local font-lock-flush-function #'jit-lock-refontify)
    (goto-char (point-min))
    (search-forward "B")
    (mlm/markdown-promote)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "* A\n* B\n"))))

(ert-deftest markdown-lite-mode-test/fill-aligns-list-item-continuation-to-marker ()
  "Sanity/regression check for the normal case the code above also handles: a continuation paragraph
   that's indented to align with the list item's own text (not indented deeper, i.e. not a nested
   code block) should keep wrapping aligned under that text, as before."
  (let ((result
         (mlm-test/fill-at-marker
          (concat "* one:\n"
                  "\n"
                  "  |aaa bbb ccc ddd\n"))))
    (should (equal result
                   (concat "* one:\n"
                           "\n"
                           "  aaa bbb ccc\n"
                           "  ddd\n")))))

(defun mlm-test/pre-block-line-ranges (text)
  "Inserts TEXT into a new temp buffer and returns the (START . END) line ranges of every Markdown
   pre block that mlm/markdown-match-pre-blocks finds in it, in the order found. END is the last
   line included in the block (inclusive)."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let (ranges)
      (while (mlm/markdown-match-pre-blocks (point-max))
        (push (cons (line-number-at-pos (match-beginning 0))
                    (1- (line-number-at-pos (match-end 0))))
              ranges))
      (nreverse ranges))))

(ert-deftest markdown-lite-mode-test/pre-block-in-two-space-nested-list ()
  "A code block nested two list levels deep in a 2-space-indented list (dprint's convention) is
   recognized as a pre block at the CommonMark-correct indentation: the innermost item's content
   column (6, for a bullet at column 4) plus 4."
  (should (equal
           (mlm-test/pre-block-line-ranges
            (concat "* one\n"
                    "  * two\n"
                    "    * three\n"
                    "\n"
                    "          code\n"))
           '((5 . 5)))))

(ert-deftest markdown-lite-mode-test/pre-block-in-two-space-nested-list-rejects-under-indentation ()
  (should (equal
           (mlm-test/pre-block-line-ranges
            (concat "* one\n"
                    "  * two\n"
                    "    * three\n"
                    "\n"
                    ;; This code block is one column less than the required code indent level.
                    "         code\n"))
           nil)))

(ert-deftest markdown-lite-mode-test/pre-block-in-four-space-nested-list ()
  (should (equal
           (mlm-test/pre-block-line-ranges
            (concat "* one\n"
                    "    * two\n"
                    "        * three\n"
                    "\n"
                    "              code\n"))
           '((5 . 5)))))

(ert-deftest markdown-lite-mode-test/pre-block-in-four-space-nested-list-rejects-under-indentation ()
  (should (equal
           (mlm-test/pre-block-line-ranges
            (concat "* one\n"
                    "    * two\n"
                    "        * three\n"
                    "\n"
                    "             code\n"))
           nil)))

(ert-deftest markdown-lite-mode-test/pre-block ()
  "An indented code block outside of any list just needs the ordinary 4 columns."
  (should (equal
           (mlm-test/pre-block-line-ranges
            (concat "text\n"
                    "\n"
                    "    code\n"))
           '((3 . 3)))))

(ert-deftest markdown-lite-mode-test/pre-block-accounts-for-marker-width ()
  "A list item's required child/content column depends on its own marker's width, not a fixed
   constant -- an ordered-list marker like \"1. \" is wider than \"* \", so a code block nested two
   levels into an ordered list needs more indentation than the same nesting in an asterisk list."
  (should (equal
           (mlm-test/pre-block-line-ranges
            (concat "1. one\n"
                    "   1. two\n"
                    "\n"
                    "          code\n"))
           '((4 . 4))))
  (should (equal
           (mlm-test/pre-block-line-ranges
            (concat "1. one\n"
                    "   1. two\n"
                    "\n"
                    "         code\n"))
           nil)))

(defun mlm-test/visible-lines-at-level (text level)
  "Inserts TEXT into a markdown-lite-mode buffer, folds it to LEVEL with `mlm/show-level', and
   returns the lines which remain visible."
  (with-temp-buffer
    (insert text)
    (markdown-lite-mode)
    (mlm/show-level level)
    (goto-char (point-min))
    (let ((lines nil))
      (while (not (eobp))
        (unless (outline-invisible-p (line-beginning-position))
          (push (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                lines))
        (forward-line 1))
      (nreverse lines))))

(ert-deftest markdown-lite-mode-test/outline-folds-atx-headings ()
  "\"#\" and \"##\" headings are both top-level. Deeper headings, and the list items under a heading,
   are nested beneath it."
  (let ((text (concat "# Title\n"
                      "intro\n"
                      "## A\n"
                      "* a1\n"
                      "  * a2\n"
                      "### A.1\n"
                      "* a3\n"
                      "## B\n"
                      "text\n")))
    (should (equal (mlm-test/visible-lines-at-level text 1)
                   '("# Title" "## A" "## B")))
    (should (equal (mlm-test/visible-lines-at-level text 2)
                   '("# Title" "## A" "* a1" "### A.1" "## B")))
    (should (equal (mlm-test/visible-lines-at-level text 3)
                   '("# Title" "## A" "* a1" "  * a2" "### A.1" "* a3" "## B")))))

(ert-deftest markdown-lite-mode-test/outline-folds-list-items-without-headings ()
  (let ((text "* a\n  * b\n    * c\n* d\n"))
    (should (equal (mlm-test/visible-lines-at-level text 1) '("* a" "* d")))
    (should (equal (mlm-test/visible-lines-at-level text 2) '("* a" "  * b" "* d")))))

(ert-deftest markdown-lite-mode-test/outline-level-updates-after-edit ()
  "The cached heading positions used by `mlm/outline-level' are refreshed when the buffer changes."
  (with-temp-buffer
    (insert "* a\n")
    (markdown-lite-mode)
    (goto-char (point-max))
    (forward-line -1)
    (should (equal (mlm/outline-level) 1))
    (goto-char (point-min))
    (insert "### h\n")
    (should (equal (mlm/outline-level) 3))))
