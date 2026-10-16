;;; -*- lexical-binding: t; -*-
;;
;; Unit tests for markdown-lite-mode.el.
;;
;; Run these with `make test`.

(require 'ert)
(require 'cl-lib)
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

(ert-deftest markdown-lite-mode-test/move-empty-list-item-down ()
  "An empty list item (just a marker and a space) should be movable when point is after the space,
   which is where point sits right after inserting a new item."
  (with-temp-buffer
    (insert "* Hey\n  * \n  * One\n")
    (markdown-lite-mode)
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (mlm/markdown-move-list-item-down)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "* Hey\n  * One\n  * \n"))))

(ert-deftest markdown-lite-mode-test/hashtag-line-does-not-end-list-item ()
  "A line starting with a #hashtag isn't a heading, so it doesn't end the list item before it."
  (with-temp-buffer
    (insert "* a\n#tag\n* b\n")
    (markdown-lite-mode)
    (goto-char (point-min))
    (forward-line 2)
    (mlm/markdown-move-list-item-up)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "* b\n* a\n#tag\n"))))

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

(ert-deftest markdown-lite-mode-test/pre-block-after-dedent-to-sibling ()
  "When a list dedents, the deeper levels close. \"* d\" is a sibling of \"* b\", so a code block
   under it needs its content column (4) plus 4, not the content column of \"* c\" plus 4."
  (let ((list (concat "* a\n"
                      "  * b\n"
                      "    * c\n"
                      "  * d\n"
                      "\n")))
    (should (equal (mlm-test/pre-block-line-ranges (concat list "        code\n"))
                   '((6 . 6))))
    (should (equal (mlm-test/pre-block-line-ranges (concat list "       code\n"))
                   nil))))

(ert-deftest markdown-lite-mode-test/pre-block-after-dedent-to-top-level ()
  "Dedenting to a top-level item closes all of the deeper levels."
  (let ((list (concat "* a\n"
                      "  * b\n"
                      "    * c\n"
                      "* e\n"
                      "\n")))
    (should (equal (mlm-test/pre-block-line-ranges (concat list "      code\n"))
                   '((6 . 6))))
    (should (equal (mlm-test/pre-block-line-ranges (concat list "     code\n"))
                   nil))))

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

(ert-deftest markdown-lite-mode-test/outline-skips-fenced-code-blocks ()
  "Lines in a fenced code block aren't headings or list items, so they're folded with the block."
  (let ((text "## A\n```\n# comment\n* not an item\n```\n## B\n"))
    (should (equal (mlm-test/visible-lines-at-level text 1) '("## A" "## B")))))

(ert-deftest markdown-lite-mode-test/outline-level-ignores-fenced-code-blocks ()
  "A \"#\" line in a fenced code block doesn't change the level of the list items after it."
  (with-temp-buffer
    (insert "### A\n```\n# comment\n```\n* a1\n")
    (markdown-lite-mode)
    (goto-char (point-max))
    (forward-line -1)
    (should (equal (mlm/outline-level) 3))))

(ert-deftest markdown-lite-mode-test/outline-folds-list-items-without-headings ()
  (let ((text "* a\n  * b\n    * c\n* d\n"))
    (should (equal (mlm-test/visible-lines-at-level text 1) '("* a" "* d")))
    (should (equal (mlm-test/visible-lines-at-level text 2) '("* a" "  * b" "* d")))))

(ert-deftest markdown-lite-mode-test/outline-ignores-lines-starting-with-bold ()
  "A line starting with **bold** text isn't a list item, but a bare \"*\" (an empty item) is."
  (let ((text "* a\n**Bold** text\n*\n  * b\n"))
    (should (equal (mlm-test/visible-lines-at-level text 1) '("* a" "*")))))

(ert-deftest markdown-lite-mode-test/demote-includes-indented-bold-line ()
  "An indented line starting with **bold** text is part of the list item above it."
  (with-temp-buffer
    (insert "* a\n  **Bold** text\n* b\n")
    (markdown-lite-mode)
    (setq-local indent-tabs-mode nil)
    (goto-char (point-min))
    (mlm/markdown-demote)
    (should (equal (buffer-string) "  * a\n    **Bold** text\n* b\n"))))

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

(ert-deftest markdown-lite-mode-test/bisect-left ()
  (should (equal (mlm/bisect-left [] 5) 0))
  (should (equal (mlm/bisect-left [10 20 30] 5) 0))
  (should (equal (mlm/bisect-left [10 20 30] 20) 1)) ; Equal elements aren't counted as less.
  (should (equal (mlm/bisect-left [10 20 30] 25) 2))
  (should (equal (mlm/bisect-left [10 20 30] 35) 3))
  (should (equal (mlm/bisect-left [(10 . a) (20 . b)] 15 #'car) 1)))

(defun mlm-test/uncached-list-levels (pos)
  "The list levels at POS, computed without `mlm/markdown-calculate-list-levels's cache."
  (save-excursion
    (goto-char pos)
    (mlm/markdown-search-backward-baseline)
    (mlm/markdown-replay-list-levels pos nil)))

(defun mlm-test/list-levels-mismatches ()
  "Returns the line numbers where the cached and uncached list levels differ, checking every line
   in order, then in reverse order."
  (let ((line-starts nil)
        (mismatches nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (push (point) line-starts)
        (forward-line 1)))
    (dolist (pos (append (reverse line-starts) line-starts))
      (unless (equal (save-excursion (goto-char pos) (mlm/markdown-calculate-list-levels))
                     (mlm-test/uncached-list-levels pos))
        (push (line-number-at-pos pos) mismatches)))
    mismatches))

(ert-deftest markdown-lite-mode-test/list-levels-propertized-match-uncached ()
  "The list levels stored by `mlm/syntax-propertize' match the ones computed from scratch, including
   after edits, and after edits made with the change hooks inhibited (followed by
   `syntax-ppss-flush-cache')."
  (progn
    (with-temp-buffer
      (dotimes (_ 5)
        (insert (concat "* a\n"
                        "  * b\n"
                        "    * c\n"
                        "\n"
                        "        code\n"
                        "\n"
                        "  * d\n"
                        "text\n"
                        "\n"
                        "Paragraph\n"
                        "1. one\n"
                        "   1. two\n"
                        "## Heading\n"
                        "* e\n"
                        "---\n")))
      (markdown-lite-mode)
      (should (null (mlm-test/list-levels-mismatches)))
      ;; Edits which change the nesting of the list items after them.
      (goto-char (point-min))
      (search-forward "  * b")
      (beginning-of-line)
      (insert "\nBaseline\n\n")
      (should (null (mlm-test/list-levels-mismatches)))
      (search-forward "* d")
      (let ((inhibit-modification-hooks t))
        (indent-rigidly (line-beginning-position) (line-end-position) 4))
      (syntax-ppss-flush-cache (line-beginning-position))
      (should (null (mlm-test/list-levels-mismatches)))
      (goto-char (point-min))
      (delete-region (point) (progn (forward-line 3) (point)))
      (should (null (mlm-test/list-levels-mismatches))))))

(ert-deftest markdown-lite-mode-test/list-levels-updated-after-demote ()
  "Demoting a list item changes the list levels after it, although it edits with the change hooks
   inhibited."
  (with-temp-buffer
    (insert "* a\n* b\n* c\n")
    (markdown-lite-mode)
    (should (null (mlm-test/list-levels-mismatches)))
    (goto-char (point-min))
    (search-forward "* b")
    (mlm/markdown-demote)
    (should (equal (buffer-string) "* a\n  * b\n* c\n"))
    (should (null (mlm-test/list-levels-mismatches)))))

(defconst mlm-test/mixed-text
  (concat "# Heading\n"
          "\n"
          "Paragraph text\n"
          "which wraps.\n"
          "\n"
          "* one\n"
          "  wrapped\n"
          "  * two\n"
          "    wrapped\n"
          "\n"
          "        pre block\n"
          "        in an item\n"
          "\n"
          "    * three\n"
          "  * four\n"
          "**Bold** line\n"
          "\n"
          "```js\n"
          "* not an item\n"
          "# not a heading\n"
          "```\n"
          "* five\n"
          "  ```\n"
          "  fenced in an item\n"
          "  ```\n"
          "1. numbered\n"
          "   1. nested\n"
          "\n"
          "    pre block\n"
          "\n"
          "## Subheading\n"
          "* six\n"
          "---\n")
  "Text with most of the structures markdown-lite-mode highlights and tracks.")

(defun mlm-test/random-generator (seed)
  "Returns a function which takes N and returns a pseudo-random integer from 0 to N - 1. It returns
   the same sequence for the same SEED, on every machine and Emacs version, unlike `random', whose
   sequence depends on the platform's generator. It's a linear congruential generator, with glibc's
   constants."
  (let ((state seed))
    (lambda (n)
      (setq state (% (+ (* state 1103515245) 12345) 2147483648))
      ;; The low bits of an LCG cycle with a short period, so use the high ones.
      (% (/ state 65536) n))))

(ert-deftest markdown-lite-mode-test/list-levels-match-uncached-after-random-edits ()
  "The list levels stored by `mlm/syntax-propertize' match the ones computed from scratch after each
   of a series of random edits, which insert and delete lines and change their indentation. A third
   of the edits are made with the change hooks inhibited, followed by `syntax-ppss-flush-cache'. The
   edits are the same on every run."
  (with-temp-buffer
    (insert mlm-test/mixed-text)
    (markdown-lite-mode)
    (let ((random (mlm-test/random-generator 1))
          (lines ["* x\n"
                  "  * y\n"
                  "    * z\n"
                  "\n"
                  "text\n"
                  "## H\n"
                  "        code\n"
                  "1. n\n"
                  "---\n"]))
      ;; Each edit takes ~1ms to check. A bug in how the list levels are kept up to date after an
      ;; edit (e.g. `mlm/syntax-propertize' not continuing from the previous line's levels) makes
      ;; most edits fail, so 15 is plenty.
      (dotimes (i 15)
        (goto-char (point-min))
        (forward-line (funcall random (count-lines (point-min) (point-max))))
        (let ((start (point))
              (inhibit-modification-hooks (= 0 (% i 3))))
          (pcase (funcall random 4)
            (0 (insert (aref lines (funcall random (length lines)))))
            (1 (delete-region (point) (min (point-max) (1+ (line-end-position)))))
            (2 (insert "  "))
            (3 (when (looking-at "  ") (delete-char 2))))
          (when inhibit-modification-hooks
            (syntax-ppss-flush-cache start)))
        (should (null (mlm-test/list-levels-mismatches)))))))

(ert-deftest markdown-lite-mode-test/pre-block-deep-in-long-list ()
  "The list levels used to find pre blocks account for list items arbitrarily far back. Here, the
   nested list items near the end are over 100,000 characters from their parent, \"* a\". If the
   parent were ignored, they'd be indented enough to count as a pre block."
  (with-temp-buffer
    (insert "* a\n")
    (dotimes (_ 15000) (insert "    * c\n"))
    (markdown-lite-mode)
    (goto-char (point-max))
    (forward-line -2)
    (should (equal (mlm/markdown-calculate-list-levels) '((4 . 6) (0 . 2))))
    (should (null (mlm/markdown-match-pre-blocks (point-max))))))

(defun mlm-test/line-faces ()
  "Returns (LINE . FACE) for each line in the current buffer, where FACE is the face of the line's
   first non-whitespace character."
  (save-excursion
    (goto-char (point-min))
    (let (result)
      (while (not (eobp))
        (back-to-indentation)
        (push (cons (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                    (get-text-property (point) 'face))
              result)
        (forward-line 1))
      (nreverse result))))

(defmacro mlm-test/with-fontified-buffer (text &rest body)
  "Runs BODY in a markdown-lite-mode buffer containing TEXT, after fontifying all of it."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (markdown-lite-mode)
     (font-lock-ensure)
     ,@body))

(ert-deftest markdown-lite-mode-test/list-item-continuation-lines-highlighted ()
  "The continuation lines of a list item are highlighted with the item's face, when they're indented
   to the item's text. A paragraph after the list isn't, nor is a line after a blank line."
  (mlm-test/with-fontified-buffer (concat "* one\n"
                                          "  wrapped\n"
                                          "  * two\n"
                                          "    wrapped\n"
                                          "    wrapped\n"
                                          "  under-indented\n"
                                          "\n"
                                          "  after blank\n"
                                          "- dash\n"
                                          "  wrapped\n"
                                          "text\n"
                                          "text\n")
    (should (equal (mlm-test/line-faces)
                   '(("* one" . mlm-list-item-1)
                     ("  wrapped" . mlm-list-item-1)
                     ("  * two" . mlm-list-item-2)
                     ("    wrapped" . mlm-list-item-2)
                     ("    wrapped" . mlm-list-item-2)
                     ("  under-indented" . nil)
                     ("" . nil)
                     ("  after blank" . nil)
                     ("- dash" . nil)
                     ("  wrapped" . nil)
                     ("text" . nil)
                     ("text" . nil))))))

(ert-deftest markdown-lite-mode-test/atx-heading-requires-space-after-hashes ()
  "An ATX heading's \"#\"s must be followed by whitespace, so e.g. a #hashtag isn't highlighted as a
   heading."
  (mlm-test/with-fontified-buffer (concat "# Title\n"
                                          "## Section\n"
                                          "#hashtag\n"
                                          "##notaheading\n")
    (should (equal (mlm-test/line-faces)
                   '(("# Title" . mlm-header-delimiter)
                     ("## Section" . mlm-header-delimiter)
                     ("#hashtag" . nil)
                     ("##notaheading" . nil))))))

(ert-deftest markdown-lite-mode-test/list-item-faces-by-level ()
  "A \"*\" list item's marker line gets the face for its level: one level per two spaces or tab of
   indentation, up to six. An indentation which isn't a whole number of levels, or a \"-\" item, gets
   no face."
  (mlm-test/with-fontified-buffer
      (concat "* 1\n"
              "  * 2\n"
              "\t  * 3\n"
              "      * 4\n"
              "  \t  \t* 5\n"
              "          * 6\n"
              "            * 7\n"
              "   * odd\n"
              "- dash\n")
    (should (equal (mapcar #'cdr (mlm-test/line-faces))
                   '(mlm-list-item-1 mlm-list-item-2
                                     mlm-list-item-3
                                     mlm-list-item-4
                                     mlm-list-item-5
                                     mlm-list-item-6
                                     mlm-list-item-6
                                     nil
                                     nil)))))

(ert-deftest markdown-lite-mode-test/atx-heading-faces-by-level ()
  "Each ATX heading's text gets the face for its level, and its \"#\"s get the delimiter face. More
   than six \"#\"s isn't a heading."
  (mlm-test/with-fontified-buffer (concat "# h1\n" "## h2\n" "### h3\n" "#### h4\n" "##### h5\n"
                                          "###### h6 ##\n" "####### h7\n")
    (should (equal (cl-loop for text in '("h1" "h2" "h3" "h4" "h5" "h6" "h7")
                            collect (progn (goto-char (point-min))
                                           (search-forward text)
                                           (get-text-property (match-beginning 0) 'face)))
                   '(mlm-header-1 mlm-header-2 mlm-header-3
                                  mlm-header-4 mlm-header-5 mlm-header-6 nil)))
    (goto-char (point-min))
    (search-forward "h6 ")
    (should (eq (get-text-property (point) 'face) 'mlm-header-delimiter))))

(ert-deftest markdown-lite-mode-test/list-item-continuation-line-refontified-alone ()
  "A continuation line is highlighted when it's refontified without its marker line, e.g. after
   it's edited, or when it's at the start of a jit-lock chunk."
  (mlm-test/with-fontified-buffer "* one\n  two\n  three\n"
    (font-lock-unfontify-buffer)
    (goto-char (point-min))
    (forward-line 2)
    (font-lock-fontify-region (point) (line-end-position))
    (should (equal (cdr (nth 2 (mlm-test/line-faces))) 'mlm-list-item-1))))

(ert-deftest markdown-lite-mode-test/list-item-continuation-lines-not-multiline ()
  "Continuation lines are matched one line at a time. If they were matched as one multiline match,
   font-lock would refontify the whole of a long list item on every keystroke in it."
  (mlm-test/with-fontified-buffer (concat "* one\n" (apply #'concat (make-list 5 "  two\n")))
    (should (equal (mapcar #'cdr (mlm-test/line-faces))
                   (make-list 6 'mlm-list-item-1)))
    (should-not (text-property-any (point-min) (point-max) 'font-lock-multiline t))))

(ert-deftest markdown-lite-mode-test/fenced-code-block-highlighted ()
  "A fenced code block is highlighted, including its fence lines, and a line in it which would
   otherwise be a heading. The text after it isn't."
  (mlm-test/with-fontified-buffer "```bash\n# comment\n```\ntext\n"
    (should (equal (mapcar #'cdr (mlm-test/line-faces))
                   '(mlm-pre mlm-pre mlm-pre nil)))))

(ert-deftest markdown-lite-mode-test/fenced-code-block-closing-fence ()
  "A block is closed only by a fence of the same character, at least as long as its opening fence."
  (mlm-test/with-fontified-buffer "~~~~\n```\n~~~\n~~~~~\ntext\n"
    (should (equal (mapcar #'cdr (mlm-test/line-faces))
                   '(mlm-pre mlm-pre mlm-pre mlm-pre nil)))))

(ert-deftest markdown-lite-mode-test/fenced-code-block-in-list-item ()
  "A fenced code block can be indented, e.g. inside a list item."
  (mlm-test/with-fontified-buffer "* item\n  ```\n  code\n  ```\n"
    (should (equal (mapcar #'cdr (mlm-test/line-faces))
                   '(mlm-list-item-1
                     mlm-pre mlm-pre mlm-pre)))))

(ert-deftest markdown-lite-mode-test/fenced-code-block-unclosed ()
  "A block with no closing fence runs to the end of the buffer."
  (mlm-test/with-fontified-buffer "```\ncode\n"
    (should (equal (mapcar #'cdr (mlm-test/line-faces))
                   '(mlm-pre mlm-pre)))))

(ert-deftest markdown-lite-mode-test/inline-code-is-not-a-fence ()
  "A line which starts with inline code written with three backticks doesn't open a block."
  (mlm-test/with-fontified-buffer "```x``` is inline\ntext\n"
    (should (equal (cdr (nth 1 (mlm-test/line-faces))) nil))))

(ert-deftest markdown-lite-mode-test/fenced-code-block-refontified-alone ()
  "A line in the middle of a block is highlighted when it's refontified without the rest of the
   block, e.g. when it's at the start of a jit-lock chunk."
  (mlm-test/with-fontified-buffer "```\none\ntwo\n```\n"
    (font-lock-unfontify-buffer)
    (goto-char (point-min))
    (forward-line 2)
    (font-lock-fontify-region (point) (line-end-position))
    (should (equal (cdr (nth 2 (mlm-test/line-faces))) 'mlm-pre))))

(ert-deftest markdown-lite-mode-test/fenced-code-block-in-narrowed-buffer ()
  "A block is highlighted in a narrowed buffer, where the stored text properties can't be used."
  (mlm-test/with-fontified-buffer "```\none\ntwo\n```\n"
    (font-lock-unfontify-buffer)
    (goto-char (point-min))
    (forward-line 2)
    (save-restriction
      (narrow-to-region (point-min) (line-end-position))
      (font-lock-fontify-region (point) (line-end-position)))
    (should (equal (cdr (nth 2 (mlm-test/line-faces))) 'mlm-pre))))

(ert-deftest markdown-lite-mode-test/fenced-code-block-updated-after-edit ()
  "Deleting a block's closing fence extends the block, so the text after it becomes highlighted."
  (mlm-test/with-fontified-buffer "```\ncode\n```\ntext\n"
    (goto-char (point-min))
    (forward-line 2)
    (delete-region (point) (line-beginning-position 2))
    (font-lock-ensure)
    (should (equal (cdr (nth 2 (mlm-test/line-faces))) 'mlm-pre))))

(defun mlm-test/char-faces ()
  "Returns the face of each character in the current buffer."
  (mapcar (lambda (pos) (get-text-property pos 'face))
          (number-sequence (point-min) (1- (point-max)))))

(defun mlm-test/char-faces-fontified-in-chunks (text chunk-size reverse)
  "Returns the face of each character of TEXT, after fontifying it in chunks of CHUNK-SIZE
   characters, as jit-lock does when scrolling through it. If REVERSE is non-nil, the chunks are
   fontified from the end of the buffer to the start."
  (with-temp-buffer
    (insert text)
    (markdown-lite-mode)
    (let ((starts (number-sequence (point-min) (1- (point-max)) chunk-size)))
      (dolist (start (if reverse (reverse starts) starts))
        (font-lock-fontify-region start (min (point-max) (+ start chunk-size)))))
    (mlm-test/char-faces)))

(ert-deftest markdown-lite-mode-test/fontifying-in-chunks-matches-whole-buffer ()
  "Fontifying a buffer in chunks, in either order, gives the same faces as fontifying all of it at
   once. Small chunks start partway through list items, pre blocks and fenced code blocks.
   Setext headings (a line underlined with \"===\") aren't tested, because they currently get
   different faces when a chunk starts at the underline."
  (let* ((text (concat mlm-test/mixed-text mlm-test/mixed-text mlm-test/mixed-text))
         (whole (mlm-test/with-fontified-buffer text (mlm-test/char-faces))))
    (dolist (chunk-size '(7 31 100))
      (dolist (reverse '(nil t))
        (should (equal (mlm-test/char-faces-fontified-in-chunks text chunk-size reverse) whole))))))
