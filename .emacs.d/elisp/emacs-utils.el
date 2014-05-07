;;
;; Lisp utility functions to serve as building blocks for working with text in Emacs.
;;
(provide 'emacs-utils)

(require 'lisp-helpers-personal)

(defun util/line-indentation-level (line)
  "The number of space characters prefixing a line."
  (string-match "\\([ ]*\\)" line)
  (length (match-string 1 line)))

(defun util/replace-current-line (new-line)
  "Replaces hte current line with the new one."
  (save-excursion
   (delete-region (line-beginning-position) (line-end-position))
   (insert new-line)))

(defun util/get-current-line ()
  "Returns the text (without string properties) of the current line."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun define-keys (keymap &rest key-and-fn-pairs)
  "Like define-key, but takes a variable number of arguments -- two per key binding pair."
  (dolist (pair (partition key-and-fn-pairs 2))
    (define-key keymap (first pair) (second pair))))

(defun save-buffer-if-dirty ()
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer)))

(defun with-env-var (name value fn)
  "Temporarily sets an env var to the given value and excutes fn."
  (let ((original-value (getenv name)))
    (unwind-protect
        (progn
          (setenv name value)
          (funcall fn))
      (setenv name original-value))))

(defun without-confirmation (fn)
  (flet ((y-or-n-p (&rest args) t)) ; Skip the confirmation prompts.
    (funcall fn)))

(defun preserve-selected-window (f)
  "Runs the given function and then restores focus to the original window. Useful when you want to invoke
   a function (like showing documentation) but don't want to keep editing your current buffer."
  (lexical-let ((f f))
    (let ((original-window (selected-window)))
      (funcall f)
      (select-window original-window))))
