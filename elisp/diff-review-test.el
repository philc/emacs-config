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
    ;; Folding a hunk folds its comments.
    (dr-test/goto "^@@ -1,5")
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
    (my-magit/next-hunk)
    (my-magit/previous-hunk)
    (my-magit/next-hunk)
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

(defun dr-test/claude-replies (text &optional resolved)
  "Add TEXT to the first saved comment's thread as Claude's reply, as the /cr skill does."
  (let* ((store (dr--read-store))
         (comment (car store)))
    (setf (alist-get 'thread comment nil t)
          (append (alist-get 'thread comment) `(((author . "claude") (text . ,text)))))
    (setf (alist-get 'resolved comment nil t) resolved)
    (dr--write-store (cons comment (cdr store)))))

(defun dr-test/thread ()
  "The first saved comment's thread, as (AUTHOR TEXT)."
  (mapcar (lambda (e) (let-alist e (list .author .text)))
          (alist-get 'thread (car (dr--read-store)))))

(ert-deftest dr-test/threads ()
  (dr-test/with-repo
    (dr-test/show "HEAD")
    (dr-test/goto "^\\+TWO")
    (dr-test/keys "o note <escape>")
    (let ((sha (magit-rev-parse "--short" "HEAD")))
      (dr-test/claude-replies (format "Done in %s." sha))
      (magit-refresh)
      (should (string-match-p (format "\\+TWO\n\\[\\[note\n\\]\\]── Claude\nDone in %s.\n three" sha)
                              (dr-test/text)))
      ;; A hash in a reply is a link.
      (dr-test/goto "^Done in")
      (search-forward sha)
      (should (equal (get-text-property (1- (point)) 'dr-rev) sha))
      ;; RET on it shows that commit in another window and buffer, keeping the focus here.
      (let ((window (selected-window))
            (buffer (current-buffer)))
        (backward-char)
        (dr-test/keys "RET")
        (should (eq (selected-window) window))
        (should (eq (window-buffer window) buffer))
        (should (equal magit-buffer-revision-oid (magit-rev-parse "HEAD")))
        (let ((shown (window-buffer (next-window window))))
          (should-not (eq shown buffer))
          (should (equal (buffer-local-value 'magit-buffer-revision shown) sha)))))
    ;; Claude's replies can't be edited.
    (let ((text (dr-test/text)))
      (dr-test/goto "^Done in")
      (ignore-errors (dr-test/keys "x"))
      (should (equal (dr-test/text) text)))
    ;; o on the thread starts a reply, and o again adds a line to it.
    (dr-test/keys "o reply <escape>")
    (dr-test/goto "^Done in")
    (dr-test/keys "o more <escape>")
    (should (equal (cadr (dr-test/thread)) '("you" "reply\nmore")))
    ;; Saving keeps a reply Claude added since the buffer was refreshed.
    (dr-test/claude-replies "later")
    (dr-test/goto "^more")
    (dr-test/keys "A ! <escape>")
    (should (equal (cdr (dr-test/thread)) '(("you" "reply\nmore!") ("claude" "later"))))
    ;; A reply left blank is dropped, along with the line naming its author.
    (magit-refresh)
    (dr-test/goto "^later")
    (dr-test/keys "o <escape>")
    (should (= (length (dr-test/thread)) 3))
    (should (equal (dr-test/line) "later"))
    (should-not (string-match-p "later\n── You" (dr-test/text)))))

(ert-deftest dr-test/long-replies-wrap ()
  (let ((fill-column 30))
    (should (equal (dr--wrap "Short.\n- a list item which is long enough to wrap twice over\n```\nlong code line which isn't wrapped, though\n```")
                   (concat "Short.\n"
                           "- a list item which is long\n"
                           "  enough to wrap twice over\n"
                           "```\nlong code line which isn't wrapped, though\n```")))))

(ert-deftest dr-test/resolved-threads ()
  (dr-test/with-repo
    (dr-test/show "HEAD")
    (dr-test/goto "^\\+TWO")
    (dr-test/keys "o note <escape>")
    (dr-test/claude-replies "Fixed." t)
    (magit-refresh)
    ;; A resolved thread stays unfolded, with a checkmark before it.
    (should (string-match-p "\\[\\[note\n\\]\\]── Claude\nFixed.\n" (dr-test/text)))
    (dr-test/goto "^note")
    (should (seq-find (lambda (o) (equal (overlay-get o 'before-string) dr--resolved-mark))
                      (overlays-at (point))))
    ;; Replying reopens it.
    (dr-test/goto "^Fixed")
    (dr-test/keys "o why? <escape>")
    (should (equal (dr-test/thread) '(("claude" "Fixed.") ("you" "why?"))))
    (should-not (alist-get 'resolved (car (dr--read-store))))
    ;; R resolves it, and R again reopens it.
    (dr-test/goto "^Fixed")
    (dr-test/keys "R")
    (should (alist-get 'resolved (car (dr--read-store))))
    (should (equal (dr-test/line) "Fixed."))
    (dr-test/keys "R")
    (should-not (alist-get 'resolved (car (dr--read-store))))))

(ert-deftest dr-test/navigating-comments ()
  (dr-test/with-repo
    (dr-test/show "HEAD")
    ;; With no comments, gn and gp go between hunks.
    (dr-test/goto "^modified   a.txt")
    (dr-test/keys "gn gn")
    (should (equal (oref (oref (magit-current-section) parent) value) "b.txt"))
    (dr-test/goto "^\\+TWO")
    (dr-test/keys "o first <escape>")
    (dr-test/goto "^\\+BETA")
    (dr-test/keys "o second <escape>")
    ;; With comments, they go between comments, from inside a comment or not.
    (goto-char (point-min))
    (dr-test/keys "gn")
    (should (equal (dr-test/line) "first"))
    (dr-test/keys "gn")
    (should (equal (dr-test/line) "second"))
    (dr-test/keys "gp")
    (should (equal (dr-test/line) "first"))
    ;; Comments in folded sections are skipped.
    (dr-test/goto "^modified   a.txt")
    (magit-section-hide (magit-current-section))
    (goto-char (point-min))
    (dr-test/keys "gn")
    (should (equal (dr-test/line) "second"))))

(defun dr-test/comment-with (text)
  (seq-find (lambda (c) (string-prefix-p text (alist-get 'comment c))) (dr--read-store)))

(defun dr-test/pos (regexp)
  (save-excursion (dr-test/goto regexp) (point)))

(ert-deftest dr-test/review-buffer ()
  (dr-test/with-repo
    (dr-test/write "a.txt" "one\nTWO\nthree\nFOUR\nfive\nsix\n")
    (dr-test/git "commit" "-qam" "second")
    (dr-test/show "HEAD~1")
    (dr-test/goto "^\\+BETA")
    (dr-test/keys "o on SPC beta <escape>")
    (dr-test/goto "^\\+TWO")
    (dr-test/keys "o on SPC two <escape>")
    (dr-test/show "HEAD")
    (dr-test/goto "^\\+FOUR")
    (dr-test/keys "o on SPC four <escape>")
    (dr-test/goto "^second")
    (dr-test/keys "o whole <escape>")
    (dr/show-review)
    (switch-to-buffer (get-buffer (format "magit-review: %s" (file-name-nondirectory
                                                              (directory-file-name repo)))))
    (evil-normal-state)
    ;; In the order of the diffs: oldest commit first, then file, then line.
    (should (< (dr-test/pos " change: edit") (dr-test/pos "^on two") (dr-test/pos "^on beta")
               (dr-test/pos "second$") (dr-test/pos "^whole") (dr-test/pos "^on four")))
    ;; A line comment has the lines of the diff above it.
    (dr-test/goto "^on four")
    (forward-line -4)
    (should (equal (buffer-substring-no-properties (point) (line-end-position 4))
                   " TWO\n three\n-four\n+FOUR"))
    ;; Replying to a thread from the review buffer.
    (let* ((store (dr--read-store))
           (comment (dr-test/comment-with "on two")))
      (setf (alist-get 'thread comment) '(((author . "claude") (text . "Done."))))
      (dr--write-store (mapcar (lambda (c) (if (equal (alist-get 'id c) (alist-get 'id comment))
                                               comment c))
                               store)))
    (magit-refresh)
    (dr-test/goto "^Done\\.")
    (dr-test/keys "o thanks <escape>")
    (should (equal (alist-get 'text (cadr (alist-get 'thread (dr-test/comment-with "on two"))))
                   "thanks"))
    ;; A new comment on an excerpt's line is on that excerpt's commit.
    (dr-test/goto "^ three")
    (dr-test/keys "o new SPC one <escape>")
    (let-alist (dr-test/comment-with "new one")
      (should (equal (list .commit .file .line .code) (list (magit-rev-parse "HEAD") "a.txt" 3 "three"))))
    (magit-refresh)
    (should (dr-test/pos "^new one"))
    ;; RET opens the commit at the comment, in another window, keeping the focus here.
    (dr-test/goto "^on four")
    (forward-line -1)
    (dr-test/keys "RET")
    (should (eq major-mode 'dr/review-mode))
    (should (eq (window-buffer (selected-window)) (current-buffer)))
    (with-current-buffer (magit-get-mode-buffer 'magit-revision-mode)
      (should (equal magit-buffer-revision-oid (magit-rev-parse "HEAD")))
      (should (equal (dr-test/line) "+FOUR")))
    ;; X deletes a thread.
    (dr-test/goto "^Done\\.")
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (dr-test/keys "X"))
    (should-not (dr-test/comment-with "on two"))
    (should-not (save-excursion (goto-char (point-min)) (re-search-forward "^on two" nil t)))))
