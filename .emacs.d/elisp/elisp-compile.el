;;; -*- lexical-binding: t; -*-
;;
;; The files in elisp/ are byte-compiled with `make compile', because compiled code is much faster
;; than interpreted code. To keep the .elc files up to date, they're recompiled at startup if any
;; file has changed (e.g. edited outside of Emacs, or by a git checkout), and whenever a file in
;; elisp/ is saved. On save, the saved file's new .elc is also loaded, to replace any definitions
;; that were evaluated (interpreted) while editing it.
;;
;; init.el requires this, and calls `elisp-compile/compile-if-stale', before it loads anything else
;; in elisp/. So this file must only depend on built-in libraries.
;;
(provide 'elisp-compile)
(require 'cl-lib)
(require 'seq)

(defvar elisp-compile/repo-dir (file-name-parent-directory (file-truename user-emacs-directory))
  "The emacs-config repo, which has the Makefile.")

(defvar elisp-compile/elisp-dir (expand-file-name "elisp/" (file-truename user-emacs-directory)))

(defvar elisp-compile/process nil
  "The `make compile' process started after a save, while it's running.")

(defvar elisp-compile/rerun nil
  "Non-nil if a file was saved while `elisp-compile/process' was running, so it must run again.")

(defvar elisp-compile/files-to-reload nil
  "The saved files whose .elc files to load when `elisp-compile/process' finishes.")

(defun elisp-compile/source-files ()
  "The files in elisp/ which `make compile' compiles: all but the tests."
  (seq-remove (lambda (f) (string-suffix-p "-test.el" f))
              (directory-files elisp-compile/elisp-dir t "\\.el\\'")))

(defun elisp-compile/compile-if-stale ()
  "Synchronously recompile elisp/ if any file is newer than its .elc, or has none."
  (when (seq-some (lambda (f) (file-newer-than-file-p f (concat f "c")))
                  (elisp-compile/source-files))
    (message "Compiling elisp/...")
    (with-temp-buffer
      (unless (= 0 (call-process "make" nil t nil "-C" elisp-compile/repo-dir "compile"))
        (display-warning 'elisp-compile
                         (concat "make compile failed. Files which failed to compile are loaded "
                                 "as source.\n" (buffer-string)))))))

(defun elisp-compile/compile-and-reload-on-save ()
  "In an after-save-hook: when the saved file is in elisp/, recompile elisp/ in the background, and
   then load the saved file's new .elc, if the file is already loaded."
  (let ((file (and buffer-file-name (file-truename buffer-file-name))))
    (when (and file (member file (elisp-compile/source-files)))
      (cl-pushnew file elisp-compile/files-to-reload :test #'equal)
      ;; A compile which is already running may have read this file before it was saved, so compile
      ;; again once it's done.
      (if (process-live-p elisp-compile/process)
          (setq elisp-compile/rerun t)
        (elisp-compile/start)))))

(defun elisp-compile/start ()
  (setq elisp-compile/rerun nil)
  (let ((buf (get-buffer-create " *elisp-compile*")))
    (with-current-buffer buf (erase-buffer))
    (setq elisp-compile/process
          (make-process :name "elisp-compile"
                        :buffer buf
                        :command (list "make" "-C" elisp-compile/repo-dir "compile")
                        :sentinel #'elisp-compile/sentinel))))

(defun elisp-compile/sentinel (proc _event)
  (when (memq (process-status proc) '(exit signal))
    (if elisp-compile/rerun
        (elisp-compile/start)
      (unless (= 0 (process-exit-status proc))
        (display-warning 'elisp-compile
                         (concat "make compile failed. Files which failed to compile are loaded "
                                 "as source.\n"
                                 (with-current-buffer (process-buffer proc) (buffer-string)))))
      (dolist (file elisp-compile/files-to-reload)
        (let ((elc (concat file "c")))
          ;; A file's .elc is only installed if it compiled without warnings.
          (when (and (featurep (intern (file-name-base file)))
                     (file-newer-than-file-p elc file))
            (load elc nil t)
            (message "Compiled and reloaded %s" (file-name-nondirectory elc)))))
      (setq elisp-compile/files-to-reload nil))))
