;;; -*- lexical-binding: t; -*-
;;
;; Unit tests for markdown-todo.el.
;;
;; Run these with `make test`.

(require 'ert)
(require 'cl-lib)
(require 'markdown-todo)

(defun mtodo-test/at-marker (text fn)
  "Inserts TEXT into a new markdown-lite-mode buffer, moves to the \"|\" marker in it, removing it,
   and calls FN. Returns the buffer's contents afterwards, with a \"|\" where point is."
  (with-temp-buffer
    (insert text)
    (markdown-lite-mode)
    (goto-char (point-min))
    (search-forward "|")
    (delete-char -1)
    (funcall fn)
    (insert "|")
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest mtodo-test/cycle-status ()
  (should (equal (mtodo-test/at-marker "* |a\n" #'mtodo/cycle-status) "* → |a\n"))
  (should (equal (mtodo-test/at-marker "* → a|\n" #'mtodo/cycle-status) "* ⏸ a|\n"))
  (should (equal (mtodo-test/at-marker "* ⏸ a|\n" #'mtodo/cycle-status) "* a|\n"))
  (should (equal (mtodo-test/at-marker "* ✓ a|\n" #'mtodo/cycle-status) "* → a|\n")))

(ert-deftest mtodo-test/capture ()
  "A captured item becomes its section's first item, and point stays on the same text."
  (should (equal (mtodo-test/at-marker "# A\n\n* |a1\n\n# B\n\n* b1\n"
                                       (lambda () (mtodo/capture "A" "new")))
                 "# A\n\n* new\n* |a1\n\n# B\n\n* b1\n"))
  ;; An empty section.
  (should (equal (mtodo-test/at-marker "|# A\n\n# B\n" (lambda () (mtodo/capture "A" "new")))
                 "|# A\n\n* new\n\n# B\n"))
  (should (equal (mtodo-test/at-marker "|# A" (lambda () (mtodo/capture "A" "new")))
                 "|# A\n\n* new\n"))
  (should-error (mtodo-test/at-marker "|# A\n" (lambda () (mtodo/capture "B" "new")))
                :type 'user-error))

(defun mtodo-test/archive (tasks archive)
  "Writes TASKS to a tasks.md and ARCHIVE (unless it's nil) to its tasks_archive.md, in a new
   directory, and archives the item at the \"|\" marker in TASKS. Returns the new contents of both
   files, as (TASKS . ARCHIVE), with today's date in the archive replaced by DATE."
  (let* ((dir (make-temp-file "mtodo-test" t))
         (file (expand-file-name "tasks.md" dir))
         (archive-file (expand-file-name "tasks_archive.md" dir)))
    (unwind-protect
        (progn
          (when archive
            (with-temp-file archive-file (insert archive)))
          (with-current-buffer (find-file-noselect file)
            (insert tasks)
            (markdown-lite-mode)
            (goto-char (point-min))
            (search-forward "|")
            (delete-char -1)
            (mtodo/archive)
            (cons (buffer-substring-no-properties (point-min) (point-max))
                  (with-temp-buffer
                    (insert-file-contents archive-file)
                    (replace-regexp-in-string (format-time-string "%Y-%m-%d") "DATE"
                                              (buffer-string))))))
      (dolist (f (list file archive-file))
        (when-let* ((b (get-file-buffer f)))
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b)))
      (delete-directory dir t))))

(ert-deftest mtodo-test/archive ()
  "An archived item and its children move to the end of their section in the archive, dedented to
   the top level, and dated."
  (let ((tasks "# A\n\n* a1\n  * |a2\n    * a3\n* a4\n\n# B\n\n* b1\n"))
    ;; Into an existing section.
    (should (equal (mtodo-test/archive tasks "# A\n\n* DATE ✓ old\n\n# B\n\n* DATE ✓ old\n")
                   (cons "# A\n\n* a1\n* a4\n\n# B\n\n* b1\n"
                         "# A\n\n* DATE ✓ old\n* DATE a2\n  * a3\n\n# B\n\n* DATE ✓ old\n")))
    ;; Into a new section, and a new archive file.
    (should (equal (cdr (mtodo-test/archive tasks "# B\n\n* DATE ✓ old\n"))
                   "# B\n\n* DATE ✓ old\n\n# A\n\n* DATE a2\n  * a3\n"))
    (should (equal (cdr (mtodo-test/archive tasks nil))
                   "# A\n\n* DATE a2\n  * a3\n")))
  ;; The last item, without a newline, and an empty section.
  (should (equal (mtodo-test/archive "# A\n\n* ✓ |a1" "# A\n")
                 (cons "# A\n\n" "# A\n\n* DATE ✓ a1\n"))))
