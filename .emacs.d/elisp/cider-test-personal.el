;;
;; Functions for running clojure.test tests with Cider.
;;
;; This has two bindable functions of interest:
;; cider-test/run-tests-in-ns
;; cider-test/run-test-at-point
;;
(provide 'cider-test-personal)
(require 'cider)
(require 'cl)

(defun when-compile-successful (f)
  "Saves and loads the current buffer, and compiles it. Runs the given function if there are no compile
  errors."
  (save-buffer)
  (cider-load-current-buffer)
  (sleep-for 0.1) ; cider-load-current-buffer is asynchronous, so we must work around that.
  ;; If there was a compile error, halt. Otherwise we'll mask the compile error which has been printed to the
  ;; minibuffer.
  (when (not (cider-test/buffer-has-compile-errors?))
    (funcall f)))

(defun cider-test/defun-name-at-point ()
  "Returns the name of the function at point, and nil if it can't be found."
  ;; This should work with both defn and deftest.
  (let* ((form (cider-defun-at-point))
         (result (string-match "(def[^ ]* \\([^ ]*\\)" form)))
    (when result
      (match-string 1 form))))

(defun cider-test/buffer-has-compile-errors? ()
  "Returns true if the current buffer has been evaled previously and has a compile error."
  (interactive)
  ;; cider doesn't expose compile errors directly. `cider-highlight-compilation-errors` will set an overlay on
  ;; the buffer if there is a compile error. This fn checks for that overlay.
  (let* ((overlays (overlays-in (point-min) (point-max)))
         (cider-notes (remove-if-not (lambda (o) (overlay-get o 'cider-note-p)) overlays)))
    (> (length cider-notes) 0)))

;; TODO(philc): This command doesn't work; we must first load the tests before running them.
(defun cider-test/run-all-tests ()
  "Saves and evals the buffer, and then runs any clojure.test tests defined in the p;roject."
  (interactive)
  (let ((project-dir (nrepl-project-directory-for (nrepl-current-dir))))
    (print project-dir)
    ;; TODO(philc): Make this a recursive file search.
    (dolist (test-file (or argv (directory-files project-dir t "_test.clj$")))
      (print test-file)
      (cider-load-file-op test-file nil t)))
  (when-compile-successful
   (lambda () (cider-interactive-eval "(clojure.test/run-all-tests)"))))

(defun cider-test/run-tests-in-ns ()
  "Saves and evals the buffer, and then runs any clojure.test tests defined in the current namespace."
  (interactive)
  (save-buffer)
  (when-compile-successful
   (lambda () (cider-interactive-eval "(clojure.test/run-tests)"))))

(defun cider-test/run-test-at-point ()
  "Runs the clojure.test under the cursor by invoking the function defined by the test in the cider repl."
  (interactive)
  ;; Note that prior to running the test, we eval the test's definition in case we've edited it source since
  ;; our last eval. We use cider-load-current-buffer instead of cider-eval-defun-at-point for this because
  ;; load-current-buffer properly sets the file metadata for the function definition, so that test failure
  ;; output has the correct source file and line number of the failing test.
  (when-compile-successful
   (lambda ()
     (let ((fn-name (cider-test/defun-name-at-point)))
       ;; TODO(philc): It would be nice if we showed whether the test passed or failed in the minibuffer.
       ;; Currently we just show "nil", and one must look to the repl to see the test's output in the case of
       ;; failure.
       (when fn-name
         (cider-interactive-eval (concat "(" fn-name ")")))))))
