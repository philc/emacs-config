;;; format-elisp.el --- Reindent elisp files in batch mode. -*- lexical-binding: t; -*-

;; Usage: emacs -Q --batch -L <elisp-dir> -l format-elisp.el -f format-elisp-batch FILE...
;;
;; Reindents each file with `indent-region' and removes trailing whitespace, saving only the files
;; that changed. The indentation of macros depends on their `lisp-indent-function' property (usually
;; set via `declare'), which is only present once the macro's library is loaded. To make batch
;; indentation agree with interactive Emacs, each file's `require'd libraries are loaded first, and
;; any top-level (put 'sym 'lisp-indent-function ...) forms in it are evaluated.

(require 'package)
(setq package-user-dir (expand-file-name "~/.emacs.d/elpa"))
(package-initialize)

(setq-default indent-tabs-mode nil)
(setq make-backup-files nil)
;; This must be set before markdown-lite-mode.el loads; it's normally set in init.el.
(defvar global-leader-prefix ";")

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
      (eval (read (current-buffer)) t))))

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
