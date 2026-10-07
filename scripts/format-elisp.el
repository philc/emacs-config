;;; format-elisp.el --- Reindent elisp files in batch mode. -*- lexical-binding: t; -*-

;; Usage: emacs -Q --batch -L <elisp-dir> -l format-elisp.el -f format-elisp-batch FILE...
;;
;; Reindents each file with `indent-region' and removes trailing whitespace, saving only the files
;; that changed. The indentation of macros depends on their `lisp-indent-function' property (usually
;; set via `declare'), which is only present once the macro's library is loaded. To make batch
;; indentation agree with interactive Emacs, each file's `require'd libraries are loaded first, any
;; top-level (put 'sym 'lisp-indent-function ...) forms in it are evaluated, and the (declare (indent
;; ...)) specs of the macros and functions it defines are applied.

(require 'package)
(setq package-user-dir (expand-file-name "~/.emacs.d/elpa"))
(package-initialize)

(setq-default indent-tabs-mode nil)
(setq make-backup-files nil)

(defun format-elisp--load-indent-specs ()
  "Load the libraries the current buffer requires, and apply its indent declarations."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "(require '\\([^ )]+\\)" nil t)
      (let ((feature (intern (match-string 1))))
        (condition-case err
            (require feature)
          (error (message "format-elisp: couldn't load %s: %S" feature err)))))
    (goto-char (point-min))
    (while (re-search-forward "^(put '[^ )]+ 'lisp-indent-function " nil t)
      (goto-char (match-beginning 0))
      (eval (read (current-buffer)) t))
    ;; Macros defined in this file declare their indentation via (declare (indent ...)), which only
    ;; takes effect once the definition is evaluated. Apply those specs without evaluating it.
    (goto-char (point-min))
    (while (re-search-forward "^(\\(?:cl-\\)?def\\(?:macro\\|un\\) " nil t)
      (goto-char (match-beginning 0))
      (let* ((form (read (current-buffer)))
             (declare-form (seq-find (lambda (x) (eq (car-safe x) 'declare)) (nthcdr 3 form)))
             (indent (assq 'indent (cdr declare-form))))
        (when indent
          (put (nth 1 form) 'lisp-indent-function (cadr indent)))))))

(defun format-elisp-file (file)
  "Reindent FILE and remove trailing whitespace. Return non-nil if FILE was changed."
  (with-current-buffer (find-file-noselect file)
    (format-elisp--load-indent-specs)
    (let ((inhibit-message t))
      (indent-region (point-min) (point-max)))
    (delete-trailing-whitespace)
    (prog1 (buffer-modified-p)
      (when (buffer-modified-p)
        (let ((inhibit-message t))
          (save-buffer)))
      (kill-buffer))))

(defun format-elisp-batch ()
  "Format the files given on the command line."
  (dolist (file command-line-args-left)
    (when (format-elisp-file file)
      (message "Formatted %s" file)))
  (setq command-line-args-left nil))

;;; format-elisp.el ends here
