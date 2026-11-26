;;; -*- lexical-binding: t; -*-
;;
;; Tests for diff-review.el. They drive a real magit revision buffer with keys, in a throwaway git
;; repo. Git's answers to magit's read-only questions are cached, for later tests which leave the
;; repo the same, and for later runs (see `dr-test/git-cache'), which makes the tests several times
;; as fast. `make test-fresh' asks git everything again.
;;
;; Run these with `make test`.

(require 'ert)
(require 'cl-lib)
(require 'magit-config)
(require 'diff-review)

(evil-mode 1)
;; As in my init.el.
(evil-set-undo-system 'undo-redo)

;; Leave out my global git config, so the tests don't depend on it. Its hooks also make each commit
;; take twice as long.
(setenv "GIT_CONFIG_GLOBAL" "/dev/null")
;; Give every commit the same date, so that the same changes to the repo make the same commits in
;; every test, and git's answers about them can be replayed.
(setenv "GIT_AUTHOR_DATE" "2026-01-01T00:00:00Z")
(setenv "GIT_COMMITTER_DATE" "2026-01-01T00:00:00Z")

(defvar dr-test/git-cache nil
  "A read-through cache of git's answers to the read-only commands magit runs, as (EXIT OUTPUT
STDERR), keyed by the directory, the command, and `dr-test/changes', with the repo's path in each
replaced by a placeholder. Every test starts with the same repo, so most of what magit asks git
while it shows a commit has the same answer in every test, and in every run. Running each costs
about 5ms, and showing a commit runs about 20. The cache is saved in `dr-test/git-cache-file' when
Emacs exits.")

(defconst dr-test/git-cache-file
  (expand-file-name "../var/dr-test-git-cache.eld"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defvar dr-test/git-cache-dirty nil
  "Whether `dr-test/git-cache' changed since it was read from its file.")

(defvar dr-test/git-cache-version nil)

(defun dr-test/git-cache-version ()
  "What the saved cache holds for: git's version, and the template, whose commits every test's
repo starts from."
  (with-memoization dr-test/git-cache-version
    (list (car (process-lines "git" "--version"))
          (let ((default-directory (dr-test/template)))
            (car (process-lines "git" "rev-parse" "HEAD"))))))

(defun dr-test/git-cache ()
  (or dr-test/git-cache
      (progn
        ;; Before the template is deleted when Emacs exits, since saving the cache needs it.
        (dr-test/git-cache-version)
        (setq dr-test/git-cache
              (or (ignore-errors
                    (with-temp-buffer
                      (insert-file-contents dr-test/git-cache-file)
                      (pcase-let ((`(,version ,cache) (read (current-buffer))))
                        (and (equal version (dr-test/git-cache-version)) cache))))
                  (make-hash-table :test #'equal))))))

(defun dr-test/save-git-cache ()
  (when dr-test/git-cache-dirty
    (let ((temp (make-temp-file (concat dr-test/git-cache-file ".")))
          ;; Unibyte output, such as from `git cat-file --batch', is printed as octal escapes, so it
          ;; reads back as the same bytes.
          (print-escape-nonascii t)
          (print-escape-multibyte t)
          (print-length nil)
          (print-level nil)
          (coding-system-for-write 'utf-8-emacs))
      (with-temp-file temp
        (prin1 (list (dr-test/git-cache-version) dr-test/git-cache) (current-buffer)))
      ;; Renaming the file into place, so that another run reading it at the same time never sees
      ;; part of it.
      (rename-file temp dr-test/git-cache-file t))))

(add-hook 'kill-emacs-hook #'dr-test/save-git-cache)

(defun dr-test/map-strings (fn object)
  (cond ((stringp object) (funcall fn object))
        ((consp object) (cons (dr-test/map-strings fn (car object))
                              (dr-test/map-strings fn (cdr object))))
        (t object)))

(defvar dr-test/repo-paths nil)

(defun dr-test/repo-paths ()
  "Placeholders for the repo's path in the saved cache, and the path, in the order to replace them.
The true name, which git prints, comes first, since it ends with the path:
/private/var/folders/... and /var/folders/..."
  (with-memoization dr-test/repo-paths
    (let ((repo (directory-file-name (dr-test/repo))))
      (list (cons "@REPO-TRUENAME@" (file-truename repo))
            (cons "@REPO@" repo)))))

(defun dr-test/portable (object)
  "OBJECT with the repo's path in its strings replaced by placeholders, which `dr-test/local'
replaces with the path of the repo in this run. Its strings' text properties, such as the magit
sections on a hash taken from a buffer, are left out, so that it can be saved."
  (dr-test/map-strings (lambda (s)
                         (setq s (substring-no-properties s))
                         (dolist (path (dr-test/repo-paths) s)
                           (setq s (string-replace (cdr path) (car path) s))))
                       object))

(defun dr-test/local (object)
  (dr-test/map-strings (lambda (s)
                         (dolist (path (dr-test/repo-paths) s)
                           (setq s (string-replace (car path) (cdr path) s))))
                       object))

(defun dr-test/recall (key ask)
  "Git's answer for KEY, from `dr-test/git-cache', or else from calling ASK, which is then cached."
  (let ((key (dr-test/portable key)))
    (dr-test/local
     (or (gethash key (dr-test/git-cache))
         (progn (setq dr-test/git-cache-dirty t)
                (puthash key (dr-test/portable (funcall ask)) (dr-test/git-cache)))))))

(defvar dr-test/changes nil
  "The changes the test made to the repo since it was copied from the template, newest first: the
git commands which might have changed it, and the files written, with their contents' hashes.
The same changes leave the repo the same, so git's answers in one test hold in another test which
made the same changes.")

(defun dr-test/repo-changed (change)
  (push change dr-test/changes))

(defconst dr-test/read-only-commands
  '("rev-parse" "cat-file" "log" "show" "symbolic-ref" "config" "for-each-ref" "merge-base"
    "rev-list" "diff" "diff-tree" "ls-files" "ls-tree" "status" "patch-id")
  "The git commands `dr-test/git-cache' caches. Any other command, which might change the repo, is
added to `dr-test/changes'.")

(defun dr-test/git-command (args)
  "The git command in ARGS, skipping the options before it, such as \"-c x=y\"."
  (let ((args (flatten-tree args)))
    (while (and args (string-prefix-p "-" (car args)))
      (when (equal (car args) "-c") (pop args))
      (pop args))
    (car args)))

(defun dr-test/cache-key (args &optional input)
  "The key in `dr-test/git-cache' of git's answer to ARGS, with INPUT, a string, as its input."
  (let* ((command (dr-test/git-command args))
         (layout-only (and (equal command "rev-parse")
                           (cl-every (lambda (a) (member a '("--show-toplevel" "--show-cdup" "--git-dir")))
                                     (cdr (member "rev-parse" (flatten-tree args)))))))
    (list default-directory args input
          ;; Where the repo's top level and git dir are doesn't change, and nor does a patch-id.
          (unless (or layout-only (equal command "patch-id"))
            dr-test/changes))))

(defun dr-test/replay-git (orig process &optional infile buffer display &rest args)
  "Replays git's answer to a read-only command run before on the same repo."
  (let ((stderr (and (consp buffer) (stringp (cadr buffer)) (cadr buffer))))
    (if (not (and (null infile)
                  (or (memq buffer '(nil t)) (and (consp buffer) (memq (car buffer) '(nil t))))
                  (member (dr-test/git-command args) dr-test/read-only-commands)))
        (progn (dr-test/repo-changed args)
               (apply orig process infile buffer display args))
      ;; Where the caller wants stdout and stderr, with the name of a file for stderr left out.
      (let ((destination (if stderr (list (car buffer) 'file) buffer)))
        (pcase-let ((`(,exit ,output ,error)
                     (dr-test/recall (cons destination (dr-test/cache-key args))
                                     (lambda ()
                                       (dr-test/ask-git
                                        (lambda (out) (apply orig process nil out nil args))
                                        destination)))))
          (when (or (eq buffer t) (eq (car-safe buffer) t))
            (insert output))
          (when (and stderr (not (equal error "")))
            (with-temp-file stderr (insert error)))
          exit)))))

(defun dr-test/ask-git (run destination)
  "Calls RUN with DESTINATION, where a process's output goes, as `call-process' takes it, except
that (t file) asks for stderr in a file. Returns (EXIT OUTPUT STDERR). Making a file for stderr
takes 0.1ms, and is done only when DESTINATION asks for one."
  (let ((stderr (and (eq (car-safe (cdr-safe destination)) 'file)
                     (make-temp-file "dr-test-stderr")))
        (multibyte enable-multibyte-characters))
    (unwind-protect
        (with-temp-buffer
          (set-buffer-multibyte multibyte)
          (let ((exit (funcall run (if stderr (list (car destination) stderr) destination))))
            (list exit
                  (buffer-string)
                  (if stderr
                      (with-temp-buffer (insert-file-contents stderr) (buffer-string))
                    ""))))
      (when stderr
        (delete-file stderr)))))

(defun dr-test/replay-git-with-input (orig start end program &optional delete buffer display
                                           &rest args)
  "Replays git's answer to a read-only command with input, such as \"git patch-id\"."
  (if (not (and (equal program (magit-git-executable))
                (or (eq buffer t) (equal buffer '(t nil)))
                (member (dr-test/git-command args) dr-test/read-only-commands)))
      (apply orig start end program delete buffer display args)
    (let* ((input (if (stringp start)
                      start
                    (buffer-substring-no-properties (or start (point-min)) (or end (point-max)))))
           (answer (dr-test/recall (cons buffer (dr-test/cache-key args input))
                                   (lambda ()
                                     (dr-test/ask-git
                                      (lambda (out) (apply orig input nil program nil out nil args))
                                      buffer)))))
      (when (and delete (not (stringp start)))
        (delete-region (or start (point-min)) (or end (point-max))))
      (insert (nth 1 answer))
      (car answer))))

(advice-add 'magit-process-file :around #'dr-test/replay-git)
(advice-add 'call-process-region :around #'dr-test/replay-git-with-input)

(defun dr-test/git (&rest args)
  "Runs git with ARGS, a command which changes only the files in .git, such as a commit. If an
earlier test or run made the same changes to the repo, this writes the files that git wrote then,
rather than running git, which takes 6-15ms."
  (dr-test/repo-changed args)
  (let* ((key (dr-test/portable (list 'git-files dr-test/changes)))
         (files (gethash key (dr-test/git-cache))))
    (if files
        (progn (dr-test/write-git-files files) 0)
      (let* ((before (dr-test/git-files))
             (exit (apply #'call-process "git" nil nil nil args)))
        (when (zerop exit)
          (setq dr-test/git-cache-dirty t)
          (puthash key (dr-test/written-git-files before) (dr-test/git-cache)))
        exit))))

(defun dr-test/git-files ()
  "Each file in the repo's .git, with what changes when git writes it: git writes a new file and
renames it into place, so its inode changes. Its modification time can change without its
contents, when git freshens a pack holding an object that it would have written."
  (mapcar (lambda (file)
            (let ((attributes (file-attributes file)))
              (list (file-relative-name file (dr-test/repo))
                    (file-attribute-inode-number attributes)
                    (file-attribute-size attributes))))
          (directory-files-recursively (expand-file-name ".git" (dr-test/repo)) "")))

(defun dr-test/written-git-files (before)
  "The files in .git which were written or deleted since BEFORE, from `dr-test/git-files', as
\(FILE . CONTENTS), with nil CONTENTS for a deleted file."
  (let ((after (dr-test/git-files)))
    (append
     (cl-loop for entry in after
              unless (member entry before)
              collect (cons (car entry)
                            (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally
                               (expand-file-name (car entry) (dr-test/repo)))
                              (buffer-string))))
     (cl-loop for entry in before
              unless (assoc (car entry) after)
              collect (list (car entry))))))

(defvar dr-test/writing-git-files nil)

(defun dr-test/write-git-files (files)
  (let ((dr-test/writing-git-files t)
        (coding-system-for-write 'no-conversion))
    (pcase-dolist (`(,file . ,contents) files)
      (let ((file (expand-file-name file (dr-test/repo))))
        ;; Git's object files are read-only.
        (delete-file file)
        (when contents
          (make-directory (file-name-directory file) t)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert contents)
            (write-region nil nil file nil 'silent)))))))

(defun dr-test/write (file text)
  (with-temp-file file (insert text)))

(defvar dr-test/repo nil
  "The directory of each test's repo. It's the same for every test in a run, so that git's answers
are too.")

(defun dr-test/repo ()
  (or dr-test/repo
      (let ((parent (make-temp-file "dr-test" t)))
        (add-hook 'kill-emacs-hook (lambda () (dr-test/delete-repo parent)))
        (setq dr-test/repo (file-name-as-directory (expand-file-name "repo" parent))))))

;; Writing a file in the repo, as the tests do by saving a buffer, changes git's answers. The review
;; file doesn't.
(advice-add 'write-region :after
            (lambda (_start _end file &rest _)
              (when (and (stringp file)
                         (not dr-test/writing-git-files)
                         (string-prefix-p (file-truename (dr-test/repo)) (file-truename file))
                         (not (equal (file-name-nondirectory file) "review.json")))
                (dr-test/repo-changed
                 (list (file-relative-name file (dr-test/repo))
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally file)
                         (md5 (current-buffer))))))))

(defun dr-test/delete-repo (dir)
  ;; `default-directory' is buffer-local, so it can't be trusted to still be the repo here: the
  ;; tests switch buffers. Delete only the directory made for the test.
  (let ((temp (file-name-as-directory (file-truename temporary-file-directory))))
    (unless (string-prefix-p temp (file-name-as-directory (file-truename dir)))
      (error "Refusing to delete %s, which isn't in %s" dir temp))
    (delete-directory dir t)))

(defvar dr-test/template nil
  "A repo whose HEAD commit edits a.txt and b.txt, made once and copied for each test, since making
it runs git several times.")

(defun dr-test/template ()
  (or dr-test/template
      (cl-flet ((git (&rest args) (apply #'call-process "git" nil nil nil args)))
        (let* ((repo (file-name-as-directory (make-temp-file "dr-test-template" t)))
               (default-directory repo))
          ;; Copying the repo for each test takes 0.3ms for each file, so it has as few as it can: no
          ;; sample hooks, or other files from git's template, or reflogs.
          (git "init" "-q" "--template=")
          (git "config" "user.email" "t@t")
          (git "config" "user.name" "t")
          (git "config" "core.logAllRefUpdates" "false")
          ;; No maintenance in the background after a commit, which would change .git while the tests
          ;; read it.
          (git "config" "maintenance.auto" "false")
          (git "config" "gc.auto" "0")
          (dr-test/write "a.txt" "one\ntwo\nthree\nfour\nfive\n")
          (dr-test/write "b.txt" "alpha\nbeta\n")
          (git "add" ".")
          (git "commit" "-qm" "initial")
          (dr-test/write "a.txt" "one\nTWO\nthree\nfour\nfive\nsix\n")
          (dr-test/write "b.txt" "alpha\nBETA\n")
          (git "commit" "-qam" "change: edit a and b")
          ;; One pack file, rather than a file and a directory for each object.
          (git "repack" "-adqn")
          (git "prune-packed")
          (delete-file ".git/COMMIT_EDITMSG")
          (add-hook 'kill-emacs-hook (lambda () (dr-test/delete-repo repo)))
          (setq dr-test/template repo)))))

(defmacro dr-test/with-repo (&rest body)
  "Run BODY in a repo whose HEAD commit edits a.txt and b.txt."
  (declare (indent 0))
  `(let* ((repo (dr-test/repo))
          (default-directory repo))
     ;; The repo starts as the template.
     (setq dr-test/changes nil)
     (unwind-protect
         (progn
           (copy-directory (dr-test/template) repo nil nil t)
           ,@body)
       ;; Find the buffer by its mode, since `magit-get-mode-buffer' runs git.
       (mapc #'kill-buffer (match-buffers '(major-mode . magit-revision-mode)))
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

(ert-deftest dr-test/commit-infos ()
  (dr-test/with-repo
    (dr-test/git "commit" "-q" "--allow-empty" "-m" "empty")
    (let* ((shas (magit-git-lines "rev-list" "HEAD"))
           (infos (dr--commit-infos shas)))
      (dolist (sha shas)
        (let ((patch-id (car (split-string
                              (shell-command-to-string
                               (format "git show %s | git patch-id --stable" sha))))))
          (should (equal (gethash sha infos)
                         (list :sha sha :subject (magit-rev-format "%s" sha)
                               :patch-id patch-id))))))))

(ert-deftest dr-test/fixups-of-one-commit-keep-their-own-comments ()
  (dr-test/with-repo
    (dr-test/write "a.txt" "one\nTWO\nthree\nfour\nfive\nsix\nseven\n")
    (dr-test/git "commit" "-qam" "fixup! change: edit a and b")
    (dr-test/write "b.txt" "alpha\nBETA\ngamma\n")
    (dr-test/git "commit" "-qam" "fixup! change: edit a and b")
    (dr-test/show "HEAD~1")
    (dr-test/goto "^\\+seven")
    (dr-test/keys "o on SPC seven <escape>")
    ;; The other fixup has the same subject, but the comment's commit is still on the branch.
    (dr-test/show "HEAD")
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
    ;; With a thread, they go to its last reply.
    (dr--write-store
     (mapcar (lambda (c)
               (when (equal (alist-get 'comment c) "first")
                 (setf (alist-get 'thread c) '(((author . "claude") (text . "Fixed."))
                                               ((author . "you") (text . "ok"))
                                               ((author . "claude") (text . "Fixed again.")))))
               c)
             (dr--read-store)))
    (magit-refresh)
    (goto-char (point-min))
    (dr-test/keys "gn")
    (should (equal (dr-test/line) "Fixed again."))
    (dr-test/keys "gn")
    (should (equal (dr-test/line) "second"))
    (dr-test/keys "gp")
    (should (equal (dr-test/line) "Fixed again."))
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
    ;; A line comment has the lines of the diff above it, and a blank line before and after its
    ;; thread.
    (dr-test/goto "^on four")
    (forward-line -5)
    (should (equal (buffer-substring-no-properties (point) (line-end-position 7))
                   " TWO\n three\n-four\n+FOUR\n\non four\n"))
    ;; gn goes to the comment, not the blank line before it.
    (goto-char (point-min))
    (dr-test/keys "gn")
    (should (equal (dr-test/line) "on two"))
    (dr-test/keys "gn")
    (should (equal (dr-test/line) "on beta"))
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
    (forward-line -2)
    (dr-test/keys "RET")
    (should (eq major-mode 'dr/review-mode))
    (should (eq (window-buffer (selected-window)) (current-buffer)))
    (with-current-buffer (magit-get-mode-buffer 'magit-revision-mode)
      (should (equal magit-buffer-revision-oid (magit-rev-parse "HEAD")))
      (should (equal (dr-test/line) "+FOUR")))
    ;; # deletes a thread.
    (dr-test/goto "^Done\\.")
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (dr-test/keys "#"))
    (should-not (dr-test/comment-with "on two"))
    (should-not (save-excursion (goto-char (point-min)) (re-search-forward "^on two" nil t)))))

;;
;; Comments on files.

(defmacro dr-test/with-file (file text &rest body)
  "Run BODY in the buffer of FILE, committed with TEXT, in a fresh repo."
  (declare (indent 2))
  `(dr-test/with-repo
     (dr-test/write ,file ,text)
     (dr-test/git "add" ,file)
     (dr-test/git "commit" "-qm" "add a file")
     (let ((buffer (find-file-noselect (expand-file-name ,file))))
       (unwind-protect
           (progn
             (switch-to-buffer buffer)
             (evil-normal-state)
             ,@body)
         (dolist (b (buffer-list))
           (when (and (buffer-file-name b)
                      (string-prefix-p (file-truename repo) (file-truename (buffer-file-name b))))
             (with-current-buffer b
               (set-buffer-modified-p nil))
             (kill-buffer b)))))))

(defun dr-test/buffer-text ()
  "The buffer's text, with the text of each comment and reply of mine in [[ ]]."
  (let ((out "")
        (pos (point-min)))
    (while (< pos (point-max))
      (let ((id (get-text-property pos 'dr-comment))
            (next (or (next-single-property-change pos 'dr-comment) (point-max))))
        (setq out (concat out (if id "[[" "") (buffer-substring-no-properties pos next)
                          (if id "]]" "")))
        (setq pos next)))
    out))

(defun dr-test/disk (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun dr-test/comment-on (regexp text)
  "Comment TEXT on the line matching REGEXP."
  (dr-test/goto regexp)
  (dr/comment)
  (dr-test/keys (concat text " <escape>")))

(ert-deftest dr-test/file-comment ()
  (dr-test/with-file "c.txt" "one\ntwo\nthree\n"
    (dr-test/comment-on "^two" "hi")
    (should (equal (dr-test/store) '(("text" "c.txt" 2 "hi"))))
    (should (equal (dr-test/buffer-text) "one\ntwo\n[[hi\n]]three\n"))
    ;; Which version of the file it's on.
    (let ((comment (car (dr--read-store))))
      (should (equal (alist-get 'head comment) (magit-rev-parse "HEAD")))
      (should-not (assq 'dirty comment)))
    (should-not (buffer-modified-p))
    ;; Editing the file and saving it leaves the comment out of the file.
    (dr-test/goto "^three")
    (dr-test/keys "A SPC 3 <escape>")
    (save-buffer)
    (should (equal (dr-test/disk "c.txt") "one\ntwo\nthree 3\n"))
    (should (equal (dr-test/buffer-text) "one\ntwo\n[[hi\n]]three 3\n"))
    (should-not (buffer-modified-p))
    ;; Now the file has an uncommitted edit.
    (dr-test/comment-on "^one" "hey")
    (should (alist-get 'dirty (seq-find (lambda (c) (equal (alist-get 'comment c) "hey"))
                                        (dr--read-store))))
    ;; The comment shows when the file is opened again.
    (kill-buffer)
    (switch-to-buffer (find-file-noselect (expand-file-name "c.txt" repo)))
    (should dr/file-mode)
    (should (equal (dr-test/buffer-text) "one\n[[hey\n]]two\n[[hi\n]]three 3\n"))))

(ert-deftest dr-test/undo-in-a-file-with-comments ()
  (dr-test/with-file "c.txt" "one\ntwo\nthree\nfour\n"
    (dr-test/goto "^four")
    (dr-test/keys "A SPC 4 <escape>")
    (dr-test/goto "^three")
    (dr-test/keys "A SPC 3 <escape>")
    ;; A comment above the edits, which isn't an edit of the file.
    (dr-test/comment-on "^one" "hi")
    (dr-test/goto "^two")
    (dr-test/keys "A SPC 2 <escape>")
    ;; Saving takes the comments out and puts them back, which mustn't throw off undo either.
    (save-buffer)
    (dr-test/keys "u")
    (should (equal (dr-test/buffer-text) "one\n[[hi\n]]two\nthree 3\nfour 4\n"))
    (dr-test/keys "u")
    (should (equal (dr-test/buffer-text) "one\n[[hi\n]]two\nthree\nfour 4\n"))
    (dr-test/keys "u")
    (should (equal (dr-test/buffer-text) "one\n[[hi\n]]two\nthree\nfour\n"))))

(ert-deftest dr-test/undo-history-adjusts-for-comments ()
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "aaa\nbbb\nccc\n")
    (undo-boundary)
    (goto-char 5)
    (delete-char 2)
    (undo-boundary)
    (goto-char (point-max))
    (insert "ddd\n")
    (undo-boundary)
    ;; Text inserted and then deleted without being recorded, before each change.
    (let ((pos 4))
      (dr--undo-insert pos 3)
      (let ((buffer-undo-list t))
        (goto-char pos)
        (insert "XYZ")))
    (should (equal (buffer-string) "aaaXYZ\nb\nccc\nddd\n"))
    ;; An edit of the inserted text, which is recorded.
    (goto-char 6)
    (insert "!")
    (undo-boundary)
    (should (equal (buffer-string) "aaaXY!Z\nb\nccc\nddd\n"))
    ;; Part of the inserted text deleted without being recorded.
    (dr--undo-delete 5 7)
    (let ((buffer-undo-list t))
      (delete-region 5 7))
    (should (equal (buffer-string) "aaaXZ\nb\nccc\nddd\n"))
    ;; The edit inside the deleted text is gone from the history.
    (undo-start)
    (undo-more 1)
    (should (equal (buffer-string) "aaaXZ\nb\nccc\n"))
    (undo-more 1)
    (should (equal (buffer-string) "aaaXZ\nbbb\nccc\n"))
    ;; The first insertion is split around the text inserted into it.
    (undo-more 1)
    (should (equal (buffer-string) "XZ"))))

(ert-deftest dr-test/editing-around-file-comments ()
  (dr-test/with-file "c.txt" "one\ntwo\nthree\n"
    (dr-test/comment-on "^two" "hi")
    ;; o on the line opens a line below its comment, and so does pasting a line with p.
    (dr-test/goto "^two")
    (dr-test/keys "o new <escape>")
    (should (equal (dr-test/buffer-text) "one\ntwo\n[[hi\n]]new\nthree\n"))
    (dr-test/goto "^one")
    (dr-test/keys "yy")
    (dr-test/goto "^two")
    (dr-test/keys "p")
    (should (equal (dr-test/buffer-text) "one\ntwo\n[[hi\n]]one\nnew\nthree\n"))
    ;; Joining the line with the comment, or deleting across the comment, is refused.
    (dr-test/goto "^two")
    (should-error (evil-join (point) (line-end-position)))
    (should-error (delete-region (point) (save-excursion (forward-line 3) (point))))
    (should (equal (dr-test/buffer-text) "one\ntwo\n[[hi\n]]one\nnew\nthree\n"))
    ;; Text typed in the comment is the comment's.
    (dr-test/goto "^hi")
    (dr-test/keys "A ! <escape>")
    (should (equal (dr-test/store) '(("text" "c.txt" 2 "hi!"))))
    ;; Deleting the line leaves the comment below the line above, where saving records it.
    (dr-test/goto "^two")
    (dr-test/keys "dd")
    (should (equal (dr-test/buffer-text) "one\n[[hi!\n]]one\nnew\nthree\n"))
    (save-buffer)
    (should (equal (dr-test/disk "c.txt") "one\none\nnew\nthree\n"))
    (should (equal (dr-test/store) '(("text" "c.txt" 1 "hi!"))))))

(ert-deftest dr-test/file-comments-follow-their-lines ()
  (dr-test/with-file "c.txt" "one\ntwo\nthree\n"
    (dr-test/comment-on "^two" "hi")
    ;; As when Claude edits the file.
    (dr-test/write "c.txt" "zero\none\ntwo\nthree\n")
    (revert-buffer t t t)
    (should (equal (dr-test/buffer-text) "zero\none\ntwo\n[[hi\n]]three\n"))
    ;; A comment whose line is gone goes at the top.
    (dr-test/write "c.txt" "zero\none\nthree\n")
    (revert-buffer t t t)
    (should (equal (dr-test/buffer-text) "[[hi\n]]zero\none\nthree\n"))))

(ert-deftest dr-test/comment-on-a-files-last-line ()
  (dr-test/with-file "c.txt" "one\ntwo"
    (dr-test/comment-on "^two" "hi")
    (should (equal (dr-test/buffer-text) "one\ntwo\n[[hi\n]]"))
    (dr-test/goto "^one")
    (dr-test/keys "A SPC 1 <escape>")
    (save-buffer)
    (should (equal (dr-test/disk "c.txt") "one 1\ntwo"))
    (should (equal (dr-test/buffer-text) "one 1\ntwo\n[[hi\n]]"))))

(ert-deftest dr-test/file-comments-arent-code ()
  (dr-test/with-file "c.el" "(a)\n(b \"x\")\n"
    (dr-test/comment-on "^(a)" "don't (\"")
    (dr-test/goto "^(b")
    (let ((state (syntax-ppss)))
      (should (= (nth 0 state) 0))
      (should-not (nth 3 state)))
    (font-lock-ensure)
    (dr-test/goto "^don't")
    (should-not (get-text-property (+ (point) 6) 'face))))

(ert-deftest dr-test/replies-in-a-file ()
  (dr-test/with-file "c.txt" "one\ntwo\n"
    ;; This deletes trailing whitespace before saving, as my init.el does.
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
    (dr-test/comment-on "^one" "hi")
    ;; Claude replies, which shows up without a refresh.
    (with-temp-buffer
      (dr-test/claude-replies "a1b2c3d: done.   "))
    (dr--check-stores)
    (should (equal (dr-test/buffer-text) "one\n[[hi\n]]── Claude\na1b2c3d: done.   \ntwo\n"))
    (dr-test/goto "^two")
    (dr-test/keys "A SPC <escape>")
    (save-buffer)
    (should (equal (dr-test/disk "c.txt") "one\ntwo\n"))
    ;; A reply of mine.
    (dr-test/goto "^a1b2")
    (dr/comment)
    (dr-test/keys "thanks <escape>")
    (should (equal (dr-test/thread) '(("claude" "a1b2c3d: done.   ") ("you" "thanks"))))
    (dr-test/goto "^thanks")
    (dr/toggle-resolved)
    (should (alist-get 'resolved (car (dr--read-store))))))

;; A hidden thread leaves only a mark on its line, which follows the line, and isn't in the
;; review buffer.
(ert-deftest dr-test/hidden-file-comment ()
  (dr-test/with-file "c.txt" "one\ntwo\nthree\n"
    (dr-test/comment-on "^two" "hi")
    (dr-test/goto "^hi")
    (dr/toggle-hidden)
    (should (equal (dr-test/buffer-text) "one\ntwo\nthree\n"))
    (should (looking-at-p "two"))
    (let ((comment (car (dr--read-store))))
      (should (alist-get 'hidden comment))
      (should (alist-get 'resolved comment)))
    (should dr/file-mode)
    ;; The mark follows its line through an edit and a save.
    (goto-char (point-min))
    (dr-test/keys "O zero <escape>")
    (save-buffer)
    (should (equal (dr-test/disk "c.txt") "zero\none\ntwo\nthree\n"))
    (should (equal (alist-get 'line (car (dr--read-store))) 3))
    (dr-test/goto "^two")
    (should (dr--hidden-on-line))
    (dr/show-review)
    (with-current-buffer (get-buffer (format "magit-review: %s" (file-name-nondirectory
                                                                 (directory-file-name repo))))
      (should-not (dr--any-p)))
    ;; Showing it again.
    (switch-to-buffer (get-file-buffer (expand-file-name "c.txt" repo)))
    (dr-test/goto "^two")
    (dr/toggle-hidden)
    (should (equal (dr-test/buffer-text) "zero\none\ntwo\n[[hi\n]]three\n"))
    (should-not (alist-get 'hidden (car (dr--read-store))))))

(ert-deftest dr-test/file-comments-in-the-review-buffer ()
  (dr-test/with-file "c.txt" "one\ntwo\nthree\n"
    (dr-test/comment-on "^two" "hi")
    ;; Not in the revision buffer of the commit which added the file.
    (dr-test/show "HEAD")
    (should-not (dr--any-p))
    (dr/show-review)
    (switch-to-buffer (get-buffer (format "magit-review: %s" (file-name-nondirectory
                                                              (directory-file-name repo)))))
    (should (string-search "Comments on files\nc.txt\none\ntwo\n\nhi\n\n"
                           (buffer-substring-no-properties (point-min) (point-max))))
    ;; RET shows the file at the comment.
    (dr-test/goto "^hi")
    (dr/review-visit)
    (with-current-buffer (get-file-buffer (expand-file-name "c.txt" repo))
      (should (equal (dr-test/line) "two")))
    (kill-buffer)))

(ert-deftest dr-test/formatting-a-file-with-comments ()
  (dr-test/with-file "c.txt" "one\ntwo  \nthree\n"
    (dr-test/comment-on "^two" "hi")
    ;; A formatter sees only the file's text, and the comment stays below its line, even though
    ;; the line's text changed.
    (util/replace-region-with-command-output "sed s/two/TWO/")
    (should (equal (dr-test/buffer-text) "one\nTWO  \n[[hi\n]]three\n"))
    (should (equal (alist-get 'code (car (dr--read-store))) "TWO  "))
    ;; So does deleting trailing whitespace before saving.
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
    (save-buffer)
    (should (equal (dr-test/buffer-text) "one\nTWO\n[[hi\n]]three\n"))
    (should (equal (alist-get 'code (car (dr--read-store))) "TWO"))))
