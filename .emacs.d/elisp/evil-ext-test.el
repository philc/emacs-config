;;; -*- lexical-binding: t; -*-
;;
;; Unit tests for evil-ext.el.
;;
;; Run these with `make test`.

(require 'ert)
(require 'cl-lib)
(require 'emacs-utils)
(require 'evil)
(require 'evil-ext)

(defun evil-ext-test/fill-at-marker (text)
  "Inserts TEXT into a new emacs-lisp-mode buffer, removes the \"|\" marker in it (which marks where
   point should be), and runs evil-ext/fill-inside-paragraph-or-comment-block from there. Returns the
   buffer's contents afterwards. This mimics a user pressing `SPC` (gqip) with their cursor inside a
   comment."
  (with-temp-buffer
    (insert text)
    (emacs-lisp-mode)
    (setq-local fill-column 15)
    (goto-char (point-min))
    (search-forward "|")
    (delete-char -1)
    (evil-ext/fill-inside-paragraph-or-comment-block)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest evil-ext-test/fill-comment-block-preserves-blank-comment-line-separators ()
  "Filling a comment block that contains a blank comment-marker-only line (e.g. \";;\"), used to
   visually separate two paragraphs within the comment, should preserve that line as a separator
   instead of folding both paragraphs together into one.
   Regression test for a bug where lisp-style comments (unlike e.g. js-mode, where cc-mode already
   reconfigures paragraph-start/separate for comments) had no notion of a blank comment line as a
   paragraph break, so filling merged separate paragraphs -- and ate the separator line entirely."
  (let ((result
         (evil-ext-test/fill-at-marker
          (concat ";; |aaa bbb ccc ddd\n"
                  ";;\n"
                  ";; eee\n"))))
    (should (equal result
                   (concat ";; aaa bbb ccc\n"
                           ";; ddd\n"
                           ";;\n"
                           ";; eee\n")))))
