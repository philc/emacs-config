;;; -*- lexical-binding: t; -*-
;; Run these with `make test`.

(require 'ert)
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
  (let ((util/replace-text-strict t))
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

(ert-deftest emacs-utils-test/replace-text-keeps-point-on-its-character ()
  (dolist (case '(;; Reindented line.
                  ("<div>\n<p>hello w|örld</p>\n</div>\n"
                   "<div>\n  <p>hello wörld</p>\n</div>\n"
                   "<div>\n  <p>hello w|örld</p>\n</div>\n")
                  ;; Quotes and spacing changed around point.
                  ("const x = {a: 1, b: 'tw|o'}\n"
                   "const x = { a: 1, b: \"two\" };\n"
                   "const x = { a: 1, b: \"tw|o\" };\n")
                  ;; A line inserted right where point is.
                  ("a\n|b\n" "a\nnew\nb\n" "a\nnew\n|b\n")))
    (should (string= (emacs-utils-test/replace (nth 0 case) (nth 1 case)) (nth 2 case)))))

(ert-deftest emacs-utils-test/replace-text-handles-missing-final-newline ()
  (dolist (case '(("a\n|b" "a\nb\n")
                  ("a\n|b\n" "a\nb")
                  ("x|" "")
                  ("|" "a\nb")
                  ("a\nb|" "a\nc")))
    (should (string= (s-replace "|" "" (emacs-utils-test/replace (car case) (cadr case)))
                     (cadr case)))))

(ert-deftest emacs-utils-test/replace-text-applies-hunks-anywhere ()
  "Each kind of hunk git's diff gives is applied correctly, at the start and end of the text, and
   several in one diff."
  (dolist (case '(("|a\nb\nc\n" "b\nc\n")                 ; Deletion at the start.
                  ("|a\nb\nc\n" "a\nb\n")                 ; Deletion at the end.
                  ("|b\n" "a\nb\n")                       ; Insertion at the start.
                  ("|a\n" "a\nb\n")                       ; Insertion at the end.
                  ("|a\nb\n" "")                          ; Everything deleted.
                  ("|\n\n" "\n  \n")                      ; Blank lines.
                  ;; Changes separated by one unchanged line, which git keeps as separate hunks.
                  ("|a\nb\nc\nd\ne\n" "A\nb\nC\nd\nE\n")
                  ;; A deletion and an insertion in one diff, which shift later line numbers.
                  ("|a\nb\nc\nd\n" "b\nc\nd\nx\n")
                  ("|a\nb\nc\n" "a\nx\ny\nb\n")
                  ;; Non-ASCII text before a change, where characters and bytes differ.
                  ("|é\nb\né\nd\n" "é\nB\né\nD\n")))
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
