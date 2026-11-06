;;; -*- lexical-binding: t; -*-
;; Run these with `make test`.

(require 'ert)
(require 'cl-lib)
(require 'outline)
(require 's)
(require 'emacs-utils)

(defun emacs-utils-test/paste-with-fn (clipboard-text fn)
  "Puts CLIPBOARD-TEXT on the kill ring, calls FN in a new temp buffer to paste/transform it, and
   returns the buffer's contents afterwards."
  (kill-new clipboard-text)
  (with-temp-buffer
    (funcall fn)
    (buffer-string)))

(ert-deftest emacs-utils-test/clipboard-yank-and-remove-email-headers-strips-headers ()
  "Leading From/To/Date/Subject headers and the blank lines around them are stripped, leaving only
   the body."
  (let ((clipboard-text (concat "From: Alice <alice@example.com>\n"
                                "To:   bob@example.com\n"
                                "Date: Sunday, August 23 2026 at 12:57 PM PDT\n"
                                "\n"
                                "Subject: Some subject line\n"
                                "\n"
                                "This is the body.\n"
                                "It has two lines.")))
    (should (string= (emacs-utils-test/paste-with-fn
                      clipboard-text #'util/clipboard-yank-and-remove-email-headers)
                     "This is the body.\nIt has two lines."))))

(ert-deftest emacs-utils-test/clipboard-yank-and-remove-email-headers-no-headers ()
  "Text with no leading headers is pasted through unchanged."
  (let ((clipboard-text "Just a plain paste, no headers here."))
    (should (string= (emacs-utils-test/paste-with-fn
                      clipboard-text #'util/clipboard-yank-and-remove-email-headers)
                     clipboard-text))))

(ert-deftest emacs-utils-test/clipboard-yank-and-remove-query-string-strips-query-and-hash ()
  "Everything from the first '?' or '#' in a pasted URL is removed."
  (should (string= (emacs-utils-test/paste-with-fn
                    "https://example.com/path?utm_source=foo&x=1#section"
                    #'util/clipboard-yank-and-remove-query-string)
                   "https://example.com/path")))

;;
;; util/replace-text
;;

(defun emacs-utils-test/replace-text (beg end new-text)
  "Calls `util/replace-text', failing the test if it fell back to replacing the text wholesale,
   which it does only when its patching is broken."
  (cl-letf* ((orig-message (symbol-function 'message))
             ((symbol-function 'message)
              (lambda (fmt &rest args)
                (when (s-starts-with? "util/replace-text: patching failed" fmt)
                  (ert-fail "util/replace-text fell back to a wholesale replace"))
                (apply orig-message fmt args))))
    (util/replace-text beg end new-text)))

(defun emacs-utils-test/replace (old new)
  "Inserts OLD into a temp buffer, with point at its \"|\" marker, replaces the buffer's text with
   NEW using `util/replace-text', and returns the result with \"|\" marking where point ended up."
  (with-temp-buffer
    (insert old)
    (goto-char (point-min))
    (search-forward "|")
    (delete-char -1)
    (emacs-utils-test/replace-text (point-min) (point-max) new)
    (insert "|")
    (buffer-string)))

(ert-deftest emacs-utils-test/replace-text-keeps-point-in-reindented-line ()
  (should (string= (emacs-utils-test/replace "<div>\n<p>hello w|örld</p>\n</div>\n"
                                             "<div>\n  <p>hello wörld</p>\n</div>\n")
                   "<div>\n  <p>hello w|örld</p>\n</div>\n")))

(ert-deftest emacs-utils-test/replace-text-keeps-point-in-reformatted-line ()
  "Point stays on the same character when the formatter changes quotes and spacing around it."
  (should (string= (emacs-utils-test/replace "const x = {a: 1, b: 'tw|o'}\n"
                                             "const x = { a: 1, b: \"two\" };\n")
                   "const x = { a: 1, b: \"tw|o\" };\n")))

(ert-deftest emacs-utils-test/replace-text-keeps-point-below-inserted-line ()
  (should (string= (emacs-utils-test/replace "a\n|b\n" "a\nnew\nb\n")
                   "a\nnew\n|b\n")))

(ert-deftest emacs-utils-test/replace-text-handles-missing-final-newline ()
  (dolist (case '(("a\n|b" "a\nb\n")
                  ("a\n|b\n" "a\nb")
                  ("x|" "")
                  ("|" "a\nb")
                  ("a\nb|" "a\nc")))
    (should (string= (s-replace "|" "" (emacs-utils-test/replace (car case) (cadr case)))
                     (cadr case)))))

(ert-deftest emacs-utils-test/replace-text-leaves-unchanged-text-alone ()
  "Markers in unchanged lines don't move, and replacing text with itself doesn't modify the buffer."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (set-buffer-modified-p nil)
    (emacs-utils-test/replace-text (point-min) (point-max) (buffer-string))
    (should-not (buffer-modified-p))
    (let ((m (copy-marker 7))) ; Inside "two".
      (emacs-utils-test/replace-text (point-min) (point-max) "ONE\ntwo\nTHREE\n")
      (should (string= (buffer-string) "ONE\ntwo\nTHREE\n"))
      (should (= m 7)))))

(ert-deftest emacs-utils-test/replace-text-replaces-only-the-region ()
  (with-temp-buffer
    (insert "keep\nchange me\nkeep\n")
    (emacs-utils-test/replace-text 6 16 "changed\n")
    (should (string= (buffer-string) "keep\nchanged\nkeep\n"))))

(ert-deftest emacs-utils-test/replace-text-is-one-undo-step ()
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "<div><p>héllo</p>\n<span>x</span></div>\n")
    (undo-boundary)
    (let ((old (buffer-string)))
      (emacs-utils-test/replace-text (point-min) (point-max)
                         "<div>\n  <p>héllo</p>\n  <span>x</span>\n</div>\n")
      (undo-boundary)
      (primitive-undo 1 (cdr buffer-undo-list))
      (should (string= (buffer-string) old)))))

(ert-deftest emacs-utils-test/replace-text-keeps-outline-folds ()
  "Folded sections stay folded, including one whose heading line itself changes."
  (with-temp-buffer
    (insert "# A\nbody  one\n#  B\nbody two\n")
    (outline-mode)
    (setq-local outline-regexp "#+ ")
    (goto-char (point-min))
    (outline-hide-subtree)
    (search-forward "#  B")
    (outline-hide-subtree)
    (emacs-utils-test/replace-text (point-min) (point-max) "# A\nbody one\n# B\nbody 2\n")
    (should (string= (buffer-string) "# A\nbody one\n# B\nbody 2\n"))
    (dolist (s '("body one" "body 2"))
      (goto-char (point-min))
      (search-forward s)
      (should (invisible-p (match-beginning 0))))
    (dolist (s '("# A" "# B"))
      (goto-char (point-min))
      (search-forward s)
      (should-not (invisible-p (match-beginning 0))))))

(ert-deftest emacs-utils-test/replace-text-matches-random-edits ()
  "The result always equals the new text, for random line insertions, deletions and changes."
  (let ((state (cl-make-random-state 42)))
    (dotimes (_ 200)
      (let* ((lines (cl-loop repeat (cl-random 8 state)
                             collect (nth (cl-random 4 state) '("a" "  b" "é c" ""))))
             (old (concat (s-join "\n" lines) (if (= 0 (cl-random 2 state)) "\n" "")))
             (new-lines (cl-loop for l in lines
                                 for r = (cl-random 4 state)
                                 append (pcase r
                                          (0 nil)
                                          (1 (list (concat " " l)))
                                          (2 (list l "inserted"))
                                          (_ (list l)))))
             (new (concat (s-join "\n" new-lines) (if (= 0 (cl-random 2 state)) "\n" ""))))
        (with-temp-buffer
          (insert old)
          (goto-char (1+ (cl-random (1+ (length old)) state)))
          (emacs-utils-test/replace-text (point-min) (point-max) new)
          (should (string= (buffer-string) new)))))))

(ert-deftest emacs-utils-test/replace-text-is-fast-on-reformatted-non-ascii-text ()
  "Reindenting every line of a large buffer containing non-ASCII text is fast. A whole-buffer
   `replace-region-contents' takes 8s on this."
  (let* ((line "<p>Some text — with an em dash, and more words to make it longer.</p>\n")
         (old (s-repeat 2000 line))
         (new (s-repeat 2000 (concat "  " line)))
         (start (float-time)))
    (with-temp-buffer
      (insert old)
      (goto-char (/ (point-max) 2))
      (emacs-utils-test/replace-text (point-min) (point-max) new)
      (should (string= (buffer-string) new)))
    (should (< (- (float-time) start) 2))))

(ert-deftest emacs-utils-test/replace-text-keeps-mark-on-its-character ()
  (with-temp-buffer
    (insert "const x = {a: 1, b: 'two'}\n")
    (set-mark 24) ; Before the "o" of 'two'.
    (goto-char (point-min))
    (emacs-utils-test/replace-text (point-min) (point-max) "const x = { a: 1, b: \"two\" };\n")
    (should (string= (buffer-substring (mark t) (point-max)) "o\" };\n"))))
