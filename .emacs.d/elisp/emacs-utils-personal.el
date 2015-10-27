;;
;; Lisp utility functions to serve as building blocks for working with text in Emacs.
;;
(provide 'emacs-utils-personal)
(require 'lisp-helpers-personal)

(defun util/call-process-with-exit-status (program stdin &rest args)
  "Runs a command and returns a list containing the status code and output string.
   E.g.: (call-process-with-exit-status 'ls' '-h' '-l' '-a') ;; => (0 '-r-w-r-- 1 ...')"
  (with-temp-buffer
    (when stdin
      (insert stdin))
    (list (apply 'call-process-region (point-min) (point-max) program t (current-buffer) nil args)
          (buffer-string))))

(defun util/call-process-and-check (program stdin &rest args)
  "Calls the given program and raises an error if the exist status is non-zero."
  (lexical-let ((result (apply 'util/call-process-with-exit-status program stdin args)))
    (if (= (first result) 0)
        (second result)
      (let ((message (concat "This command exited with status "
                             (number-to-string (first result))
                             ": `"
                             (mapconcat 'identity (append (list program) args) " ")
                             "`\n"
                             (second result)
                             )))
        (throw nil message)))))

(defun util/line-indentation-level (line)
  "The number of space characters prefixing a line."
  (string-match "\\([ ]*\\)" line)
  (length (match-string 1 line)))

(defun util/replace-current-line (new-line)
  "Replaces the current line with the new one."
  (save-excursion
   (delete-region (line-beginning-position) (line-end-position))
   (insert new-line)))

(defun util/replace-buffer-text (new-text)
  "Replaces the entire buffer with the new text."
  (save-excursion
   (delete-region (point-min) (point-max))
   (insert new-text)))

(defun util/get-current-line ()
  "Returns the text (without string properties) of the current line."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun util/define-keys (keymap &rest key-and-fn-pairs)
  "Like define-key, but takes a variable number of arguments -- two per key binding pair."
  (dolist (pair (partition key-and-fn-pairs 2))
    (define-key keymap (first pair) (second pair))))

(defun util/save-buffer-if-dirty ()
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer)))

(defun util/with-env-var (name value fn)
  "Temporarily sets an env var to the given value and excutes fn."
  (let ((original-value (getenv name)))
    (unwind-protect
        (progn
          (setenv name value)
          (funcall fn))
      (setenv name original-value))))

(defun util/without-confirmation (fn)
  (flet ((y-or-n-p (&rest args) t)) ; Skip the confirmation prompts.
    (funcall fn)))

(defun util/preserve-selected-window (f)
  "Runs the given function and then restores focus to the original window. Useful when you want to invoke
   a function (like showing documentation) but don't want to keep editing your current buffer."
  (lexical-let* ((f f)
                 (original-window (selected-window))
                 (result (funcall f)))
    (select-window original-window)
    result))

(defun util/preserve-line-and-column (f)
  "Runs the given function and restores the cursor to its former line and column. This is helpful when the
   text in the buffer moves (e.g. as a result of indentation commands). This is different from save-excursion
   which will restore the (point). This does not restore the cursor to the previous point."
  (lexical-let* ((previous-line (line-number-at-pos))
                 (previous-col (current-column))
                 (return-val (funcall f)))
    (goto-line previous-line)
    (move-to-column previous-col)
    return-val))

(defun util/thing-at-point-no-properties (thing)
  "Returns the text of the thing at point, but without properties. See `thing-at-point` for details."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (when bounds ; NOTE(philc): I'm not sure if bounds-of-thing-at-point ever returns nil.
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun util/delete-thing-at-point (thing)
  "Deletes the thing at point, leaving the cursor at the beginning of the thing's region. See
  `thing-at-point`."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (delete-region (car bounds) (cdr bounds))))
