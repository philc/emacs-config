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
