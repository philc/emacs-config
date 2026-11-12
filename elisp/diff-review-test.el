;;; -*- lexical-binding: t; -*-
;;
;; Tests for diff-review.el. They drive a real magit revision buffer with keys, in a throwaway git
;; repo.
;;
;; Run these with `make test`.

(require 'ert)
(require 'cl-lib)
(require 'magit-config)
(require 'diff-review)

(evil-mode 1)

(defun dr-test/git (&rest args)
  (apply #'call-process "git" nil nil nil args))

(defun dr-test/write (file text)
  (with-temp-file file (insert text)))

(defun dr-test/delete-repo (dir)
  ;; `default-directory' is buffer-local, so it can't be trusted to still be the repo here: the
  ;; tests switch buffers. Delete only the directory made for the test.
  (let ((temp (file-name-as-directory (file-truename temporary-file-directory))))
    (unless (string-prefix-p temp (file-name-as-directory (file-truename dir)))
      (error "Refusing to delete %s, which isn't in %s" dir temp))
    (delete-directory dir t)))

(defmacro dr-test/with-repo (&rest body)
  "Run BODY in a repo whose HEAD commit edits a.txt and b.txt."
  (declare (indent 0))
  `(let* ((repo (file-name-as-directory (make-temp-file "dr-test" t)))
          (default-directory repo))
     (unwind-protect
         (progn
           (dr-test/git "init" "-q")
           (dr-test/git "config" "user.email" "t@t")
           (dr-test/git "config" "user.name" "t")
           (dr-test/write "a.txt" "one\ntwo\nthree\nfour\nfive\n")
           (dr-test/write "b.txt" "alpha\nbeta\n")
           (dr-test/git "add" ".")
           (dr-test/git "commit" "-qm" "initial")
           (dr-test/write "a.txt" "one\nTWO\nthree\nfour\nfive\nsix\n")
           (dr-test/write "b.txt" "alpha\nBETA\n")
           (dr-test/git "commit" "-qam" "change: edit a and b")
           ,@body)
       (when-let* ((buffer (magit-get-mode-buffer 'magit-revision-mode)))
         (kill-buffer buffer))
       (dr-test/delete-repo repo))))

(defun dr-test/show (rev &optional args)
  (magit-revision-setup-buffer rev args nil)
  (switch-to-buffer (magit-get-mode-buffer 'magit-revision-mode))
  (evil-normal-state))

(defun dr-test/keys (keys)
  (execute-kbd-macro (kbd keys)))

(defun dr-test/goto (regexp)
  (goto-char (point-min))
  (re-search-forward regexp)
  (beginning-of-line))

(defun dr-test/text ()
  "The buffer's text from the first file heading on, with each comment's text in [[ ]]."
  (let ((out "")
        (pos (save-excursion (dr-test/goto "^modified") (point))))
    (while (< pos (point-max))
      (let ((id (get-text-property pos 'dr-comment))
            (next (or (next-single-property-change pos 'dr-comment) (point-max))))
        (setq out (concat out (if id "[[" "") (buffer-substring-no-properties pos next)
                          (if id "]]" "")))
        (setq pos next)))
    out))

(defun dr-test/store ()
  "The saved comments, as (KIND FILE LINE COMMENT)."
  (mapcar (lambda (c) (let-alist c (list .kind .file .line .comment)))
          (dr--read-store)))

(defun dr-test/line ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(ert-deftest dr-test/line-comment ()
  (dr-test/with-repo
    (dr-test/show "HEAD")
    (dr-test/goto "^\\+TWO")
    (dr-test/keys "o first SPC line RET second <escape>")
    (should (equal (dr-test/store) '(("line" "a.txt" 2 "first line\nsecond"))))
    (let-alist (car (dr--read-store))
      (should (equal (list .side .code .subject) '("new" "TWO" "change: edit a and b")))
      (should (equal .commit (magit-rev-parse "HEAD"))))
    ;; The comment is inside the hunk, so the hunk's commands treat it as part of the hunk.
    (should (eq (oref (magit-current-section) type) 'hunk))
    ;; A comment on a removed line is about the parent's version of the file.
    (dr-test/goto "^-two")
    (dr-test/keys "o gone <escape>")
    (let-alist (cadr (dr--read-store))
      (should (equal (list .side .line .code) '("old" 2 "two"))))
    (should (string-prefix-p
             "modified   a.txt\n@@ -1,5 +1,6 @@\n one\n-two\n[[gone\n]]+TWO\n[[first line\nsecond\n]] three"
             (dr-test/text)))))

(ert-deftest dr-test/editing-in-a-comment ()
  (dr-test/with-repo
    (dr-test/show "HEAD")
    (dr-test/goto "^\\+TWO")
    (dr-test/keys "o first <escape>")
    ;; Inside a comment, o opens a new line of the comment, rather than a new comment.
    (dr-test/keys "o second <escape>")
    (should (equal (dr-test/store) '(("line" "a.txt" 2 "first\nsecond"))))
    (dr-test/keys "kdd")
    (should (equal (dr-test/store) '(("line" "a.txt" 2 "second"))))
    (dr-test/keys "u")
    (should (equal (dr-test/store) '(("line" "a.txt" 2 "first\nsecond"))))
    ;; o on the commented line opens a line at the top of its comment.
    (dr-test/goto "^\\+TWO")
    (dr-test/keys "o zeroth <escape>")
    (should (equal (dr-test/store) '(("line" "a.txt" 2 "zeroth\nfirst\nsecond"))))
    ;; Deleting every line deletes the comment.
    (dr-test/keys "3dd")
    (should (null (dr-test/store)))
    (should-not (dr--any-p))
    (should-not (file-exists-p (dr--store-file)))
    ;; ...and undo brings it back, even though point is now on a diff line.
    (dr-test/keys "u")
    (should (equal (dr-test/store) '(("line" "a.txt" 2 "zeroth\nfirst\nsecond"))))))

(ert-deftest dr-test/blank-comment-is-dropped ()
  (dr-test/with-repo
    (dr-test/show "HEAD")
    (dr-test/goto "^\\+TWO")
    (dr-test/keys "o <escape>")
    (should-not (dr--any-p))
    (should-not (file-exists-p (dr--store-file)))
    (should (equal (dr-test/line) "+TWO"))))

(ert-deftest dr-test/edits-stay-inside-the-comment ()
  (dr-test/with-repo
    (dr-test/show "HEAD")
    (dr-test/goto "^\\+TWO")
    (dr-test/keys "o note <escape>")
    (let ((text (dr-test/text)))
      ;; Joining the comment's last line with the diff line after it.
      (ignore-errors (dr-test/keys "J"))
      ;; Backspacing from the start of the comment into the diff line before it. The error stops
      ;; the keyboard macro before its escape.
      (ignore-errors (dr-test/keys "0i <backspace>"))
      (evil-normal-state)
      (should (equal (dr-test/text) text))
      ;; Deleting the comment's final newline puts one back, so the next diff line stays whole.
      (dr-test/keys "A <deletechar> <escape>")
      (should (equal (dr-test/text) text))
      ;; Editing the diff itself.
      (dr-test/goto "^ three")
      (ignore-errors (dr-test/keys "x"))
      (ignore-errors (dr-test/keys "dd"))
      (should (equal (dr-test/text) text)))
    ;; Pasting lines after the comment's last line adds them to the comment.
    (dr-test/goto "^note")
    (dr-test/keys "yyp")
    (should (equal (dr-test/store) '(("line" "a.txt" 2 "note\nnote"))))))

(ert-deftest dr-test/file-and-commit-comments ()
  (dr-test/with-repo
    (dr-test/show "HEAD")
    (dr-test/goto "^modified   b.txt")
    (dr-test/keys "o whole SPC file <escape>")
    (dr-test/goto "^change: edit")
    (dr-test/keys "o whole SPC commit <escape>")
    (should (equal (dr-test/store) '(("file" "b.txt" nil "whole file")
                                     ("commit" nil nil "whole commit"))))
    (should (string-match-p "modified   b.txt\n\\[\\[whole file\n\\]\\]@@" (dr-test/text)))
    (dr-test/goto "^change: edit")
    (forward-line)
    (should (equal (dr-test/line) "whole commit"))
    ;; Hunk headings have no line to comment on.
    (dr-test/goto "^@@")
    (should-error (dr/open-comment) :type 'user-error)))

;; With --stat, the diffstat's lines are file sections too, which mustn't be taken for the files.
(ert-deftest dr-test/diffstat ()
  (dr-test/with-repo
    (dr-test/show "HEAD" '("--stat"))
    (dr-test/goto "^\\+TWO")
    (dr-test/keys "o on SPC line <escape>")
    ;; `o' on a file's line in the diffstat comments on the file.
    (dr-test/goto "^b.txt *|")
    (dr-test/keys "o on SPC file <escape>")
    (magit-refresh)
    (should (string-match-p "\\+TWO\n\\[\\[on line\n\\]\\]" (dr-test/text)))
    (should (string-match-p "modified   b.txt\n\\[\\[on file\n\\]\\]@@" (dr-test/text)))))

(ert-deftest dr-test/refresh-keeps-comments-and-point ()
  (dr-test/with-repo
    (dr-test/show "HEAD")
    (dr-test/goto "^\\+TWO")
    (dr-test/keys "o some SPC note <escape>")
    (let ((text (dr-test/text)))
      (dr-test/goto "^some")
      (forward-char 5)
      (magit-refresh)
      (should (equal (dr-test/text) text))
      (should (equal (buffer-substring-no-properties (line-beginning-position) (point)) "some "))
      ;; Point on the diff line after a comment stays there, rather than moving into the comment.
      (dr-test/goto "^ three")
      (magit-refresh)
      (should (equal (dr-test/line) " three")))))

(ert-deftest dr-test/comment-on-last-line-of-hunk ()
  (dr-test/with-repo
    (dr-test/show "HEAD")
    (dr-test/goto "^\\+six")
    (dr-test/keys "o last <escape>")
    (let ((hunk (magit-current-section)))
      (should (eq (oref hunk type) 'hunk))
      (should (= (oref hunk end) (save-excursion (forward-line) (point)))))
    ;; Hunk navigation from inside the comment goes to the next file's hunk.
    (dr-test/keys "gn")
    (should (equal (oref (oref (magit-current-section) parent) value) "b.txt"))
    (dr-test/keys "gp")
    (should (looking-at "@@ -1,5"))
    ;; Folding a hunk folds its comments.
    (dr-test/keys "TAB")
    (dr-test/goto "^last")
    (should (invisible-p (point)))))

(ert-deftest dr-test/visit-file-skips-comments ()
  (dr-test/with-repo
    (dr-test/show "HEAD")
    (dr-test/goto "^-two")
    (dr-test/keys "o a <escape>")
    (dr-test/goto "^\\+TWO")
    (dr-test/keys "o b RET c <escape>")
    (dr-test/goto "^ four")
    (pcase-let ((`(,buffer ,pos) (magit-diff-visit-file--noselect)))
      (with-current-buffer buffer
        (should (= (line-number-at-pos pos) 4))))
    (should (equal (dr-test/line) " four"))
    (should (equal (dr-test/store) '(("line" "a.txt" 2 "a") ("line" "a.txt" 2 "b\nc"))))))

(ert-deftest dr-test/applying-a-hunk-with-comments-is-refused ()
  (dr-test/with-repo
    (dr-test/show "HEAD")
    (dr-test/goto "^\\+TWO")
    (dr-test/keys "o note <escape>")
    (dr-test/goto "^ three")
    (should-error (call-interactively #'magit-reverse) :type 'user-error)
    (should-error (call-interactively #'magit-apply) :type 'user-error)))

(ert-deftest dr-test/magit-painting-isnt-an-edit ()
  (dr-test/with-repo
    (dr-test/show "HEAD")
    (dr-test/goto "^\\+TWO")
    (dr-test/keys "o note <escape>")
    (setq buffer-undo-list nil)
    ;; Moving between hunks makes magit repaint them.
    (dr-test/keys "gn gp gn")
    (should (null buffer-undo-list))
    (dr-test/goto "^note")
    (should (eq (get-text-property (point) 'font-lock-face) 'dr/comment))))

(ert-deftest dr-test/comments-follow-rewritten-commits ()
  (dr-test/with-repo
    (dr-test/show "HEAD")
    (dr-test/goto "^\\+TWO")
    (dr-test/keys "o on SPC two <escape>")
    (dr-test/goto "^\\+six")
    (dr-test/keys "o on SPC six <escape>")
    ;; Rewording changes the hash, but not the patch-id.
    (dr-test/git "commit" "-q" "--amend" "-m" "reworded")
    (dr-test/show "HEAD")
    (dr-test/goto "^\\+TWO")
    (forward-line)
    (should (equal (dr-test/line) "on two"))
    ;; Changing the diff changes the patch-id, but the comments still match by the original
    ;; subject. The line "TWO" moves down, and the line "six" is gone.
    (dr-test/write "a.txt" "zero\none\nTWO\nthree\nfour\nfive\n")
    (dr-test/git "commit" "-q" "--amend" "-am" "change: edit a and b")
    (dr-test/show "HEAD")
    (dr-test/goto "^\\+TWO")
    (forward-line)
    (should (equal (dr-test/line) "on two"))
    ;; A comment whose line can't be found goes with its file's comments, marked as such.
    (dr-test/goto "^modified   a.txt")
    (forward-line)
    (should (equal (dr-test/line) "on six"))
    (should (equal (get-text-property (point) 'line-prefix) dr--orphan-bar))
    ;; Another commit doesn't show them.
    (dr-test/show "HEAD~1")
    (should-not (dr--any-p))))
