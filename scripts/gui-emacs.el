;;; gui-emacs.el --- Run an experiment in a throwaway GUI Emacs. -*- lexical-binding: t; -*-

;; Loaded by gui-emacs.sh, which describes how to use it.

(add-to-list 'load-path (expand-file-name "../elisp/" (file-name-directory load-file-name)))

(defun gui-emacs--print (format-string &rest args)
  "Print a line to stderr, which gui-emacs.sh shows."
  (princ (concat "gui-emacs: " (apply #'format format-string args) "\n")
         #'external-debugging-output))

(defun gui-emacs--quit (code)
  "Quit with exit CODE, without running `kill-emacs-hook'."
  (let ((kill-emacs-hook nil))
    (kill-emacs code)))

(defun gui-emacs-run (file timeout)
  "Load FILE once the event loop has started, then quit. Quit after TIMEOUT seconds regardless."
  (run-with-timer timeout nil (lambda ()
                                (gui-emacs--print "timed out after %ss" timeout)
                                (gui-emacs--quit 2)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (run-with-timer
               0 nil
               (lambda ()
                 (condition-case err
                     (progn
                       (load file nil t)
                       (gui-emacs--print "done")
                       (gui-emacs--quit 0))
                   (error
                    (gui-emacs--print "error: %s" (error-message-string err))
                    (gui-emacs--quit 1))))))))
