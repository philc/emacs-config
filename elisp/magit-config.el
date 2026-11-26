;;; -*- lexical-binding: t; -*-
;;
;; My configuration and customizations of magit mode, for interacting with Git.
;;
;; Note that magit's keybindings are notoriously hard to rebind consistently. It would be better if
;; there was a way to unset all of Magit's keybindings. For further discussion:
;; https://github.com/magit/magit/issues/1968
;;
;; Helpful references:
;; https://github.com/justbur/evil-magit/blob/master/evil-magit.el

(require 'emacs-utils)
(require 'dash)
(require 's)
(require 'view) ; for View-scroll-half-page-backward.

;; Syntax-highlight the code in diffs, using each file's major mode. Magit's diff faces color the
;; text of added and removed lines, which would hide the syntax colors, so leave their foregrounds
;; unset and mark lines by background color alone. This must be set before Magit is loaded, because
;; Magit reads it when defining its faces.
(defvar magit-diff-specify-hunk-foreground nil)

;; Magit requires these libraries, but uses them only when it's asked to: browse-url to open a link,
;; epa and epg to pick a GPG key to sign with, and dired to discard an untracked file. Skipping them
;; saves 85ms when Magit loads. Magit's with-editor requires shell, which can't be skipped: its
;; advice on `shell-command' reads `shell-mode-hook'. This doesn't use `util/require-without', since
;; this file uses Magit's macros and special variables (see `util/skip-libraries').
(defvar magit-config--skipped nil)
(setq magit-config--skipped (util/skip-libraries '(browse-url epa epg dired)))
(require 'magit)
(util/unskip-libraries magit-config--skipped)

;; Magit caches git's answers while it refreshes a buffer, but when it shows a buffer, it finds the
;; buffer and saves the repo's buffers before then, which ask for the repo's top level several times.
;; Caching from the start saves 10 of the 31 git calls to show a commit.
(defun magit-config--with-refresh-cache (fn &rest args)
  (let ((magit--refresh-cache (or magit--refresh-cache (list (cons 0 0)))))
    (apply fn args)))

(advice-add 'magit-setup-buffer-internal :around #'magit-config--with-refresh-cache)

;; Unlike Magit's other uses of the skipped libraries, this one doesn't load dired itself.
(advice-add 'magit-discard-files--delete :before
            (lambda (&rest _) (require 'dired))
            '((name . magit-config--require-dired)))
(require 'evil)
(require 'window-management)
(require 'diff-review)
(provide 'magit-config)

;; When committing, don't have Magit show the diff of what's changed. This feature is annoying
;; because it creates two buffers. I've already reviewed the staged changes prior to activating
;; commit mode and don't need to see the changes again.
(setq magit-commit-show-diff nil)

;; Don't link "#123"-style bug references in commit buffers. Turning the mode on asks vc which
;; backend owns the repo, which loads every vc backend the first time, and costs a little in every
;; commit buffer, for links I don't use.
(remove-hook 'magit-revision-mode-hook #'bug-reference-mode)

;; Magit's commit menu enables --verbose by default, which makes git embed the full staged diff
;; as a comment in the commit message buffer. That's slow to open for large diffs, so turn it off.
(with-eval-after-load 'magit-commit
  (oset (get 'magit-commit 'transient--prefix) default-value nil))

;; Disable Emacs' built-in VC package for git repositories. This prevents it from doing unnecessary
;; work when Magit is performing git operations. This was recommended by the Magit manual.
;; Empirically, I've noticed this greatly speeds up git rebasing with Magit.
(setq vc-handled-backends (delq 'Git vc-handled-backends))

;; Don't refresh the status buffer unless it's currently focused. This should improve performance.
(setq magit-refresh-status-buffer nil)

;; Don't warn when a diff has a very long line (e.g. a minified file) and Emacs switches the buffer
;; to its long-line optimizations, which disable section highlighting.
(setq magit-show-long-lines-warning nil)

;; Highlight only the heading of the hunk at point, not its body, which would otherwise be redrawn
;; in brighter diff colors and lose the theme's contrast.
(setq magit-diff-highlight-hunk-body nil)

;; Don't highlight the section at point in a revision buffer. The highlight's background stops short
;; of review comments' bars, which have their own background.
(defun magit-config--no-current-section-highlight ()
  (setq-local magit-section-highlight-current nil))

(add-hook 'magit-revision-mode-hook #'magit-config--no-current-section-highlight)

;; Narrow the log's author column from 18 characters to 12, so it cuts off fewer commit summaries.
;; Longer names are truncated with an ellipsis.
(let ((margin '(t age magit-log-margin-width t 12)))
  (setopt magit-log-margin margin
          magit-log-select-margin margin
          magit-cherry-margin margin))

;; Leave a blank column at the start of the right margin, so that a long commit summary doesn't run
;; into the author's name. Every Magit margin's width comes from `magit-log-margin-width', so widen
;; it by one column to make room.
(defun magit-config--pad-margin-string (args)
  "Prepend a space to the margin string in ARGS, the arguments to `magit-make-margin-overlay'."
  (if (car args)
      (cons (concat " " (car args)) (cdr args))
    args))

(advice-add 'magit-log-margin-width :filter-return #'1+)
(advice-add 'magit-make-margin-overlay :filter-args #'magit-config--pad-margin-string)

;; Syntax-highlight each diff hunk when it's first displayed. See
;; `magit-diff-specify-hunk-foreground' above.
;;
;; Magit's own `all' setting highlights every hunk before showing the buffer, and runs several git
;; processes per file to do so. As of 2026-11-24 this took 1.5s for a commit touching 24 files.
;; Instead, jit-lock calls `magit-config--fontify-hunks' as hunks come on screen, and one git
;; process loads the files which those hunks need.
;;
;; Emacs can't redraw while the diff highlighting runs, and so highlighting can block Emacs from
;; updating the display. It usually takes less than 50ms each time more of the diff comes on screen.
;; An alternative approach is to color the hunks in the background, i.e. from a timer when Emacs is
;; idle, stopping when there's input. That would reduce how long the display can block while
;; coloring, especially when holding down C-d, but diffs would appear uncolored at first, and
;; flicker when their colors arrive. The commit "magit: highlight diff hunks while idle, so
;; scrolling doesn't freeze" has an implementation of this.
(setq magit-diff-fontify-hunk nil)

(defvar magit-config--blob-buffers-limit 8
  "How many versions of files to keep loaded for highlighting each diff buffer's hunks.")

(defvar-local magit-config--blob-buffers nil
  "Alist of the versions of files loaded for highlighting this buffer's hunks, most recent first.
Each key is a list of a revision and a file, and each value is a buffer.")
;; Magit re-enables the mode of a buffer which it reuses, e.g. for another commit. The blob buffers
;; would never be killed if that cleared this.
(put 'magit-config--blob-buffers 'permanent-local t)

(defvar-local magit-config--hunk-git-cache nil
  "Git output cached while highlighting this buffer's hunks, in the form of `magit--refresh-cache'.
Kept until the buffer is next refreshed.")

(defvar-local magit-config--file-sides nil
  "Alist from each file section whose hunks were highlighted to its sides, so that highlighting more
of a file's hunks doesn't rerun git. `magit-config--hunk-git-cache' doesn't cover all of the git
commands which find the sides. Kept until the buffer is next refreshed.")

(defun magit-config--fontify-hunks (start end)
  "Syntax-highlight the diff hunks between START and END which aren't hidden. Run by jit-lock."
  (let ((hunks nil))
    (named-let collect ((section magit-root-section))
      (when (and section
                 (not (oref section hidden))
                 (< (oref section start) end)
                 (> (oref section end) start))
        (if (magit-section-match 'hunk section)
            (unless (or (oref section fontified) (magit--meta-hunk-p section))
              (push section hunks))
          (mapc #'collect (oref section children)))))
    (when hunks
      (with-demoted-errors "Error highlighting diff hunks: %S"
        (magit-config--fontify-hunk-sections hunks)))))

(defun magit-config--fontify-hunk-sections (hunks)
  "Syntax-highlight HUNKS, a list of hunk sections."
  ;; Magit caches git output only during a refresh. Without a cache, each hunk reruns the same git
  ;; commands.
  (let* ((magit--refresh-cache (with-memoization magit-config--hunk-git-cache (list (cons 0 0))))
         (sides-of-hunks (mapcar (lambda (hunk) (cons hunk (magit-config--hunk-sides hunk)))
                                 hunks)))
    (magit-config--load-blobs (seq-uniq (seq-mapcat #'cdr sides-of-hunks)))
    (pcase-dolist (`(,hunk . ,sides) sides-of-hunks)
      ;; Magit would run several git processes per file to find and load these.
      (cl-letf (((symbol-function 'magit-diff-visit--sides) (lambda () sides))
                ((symbol-function 'magit-find-file-noselect)
                 (lambda (rev file &rest _)
                   (cdr (assoc (list rev file) magit-config--blob-buffers)))))
        (with-demoted-errors "Error highlighting diff hunk: %S"
          (magit-diff--update-hunk-syntax hunk))))
    (magit-config--kill-blob-buffers magit-config--blob-buffers-limit)))

(defun magit-config--hunk-sides (hunk)
  "Return HUNK's old and new sides, each a list of a revision and a file.
The revision can also be \"{index}\" or \"{worktree}\". Magit also uses nil for the worktree."
  (let ((file-section (oref hunk parent)))
    (with-memoization (alist-get file-section magit-config--file-sides)
      (save-excursion
        (goto-char (oref hunk content))
        (if (derived-mode-p 'magit-revision-mode)
            ;; These are the revisions which `magit-diff-visit--sides' returns, but not abbreviated,
            ;; since that runs git. A review buffer (see diff-review.el) has no revision of its own,
            ;; but `magit-diff--dwim' returns the one its hunk is from.
            (let* ((rev (or magit-buffer-revision-oid (cdr (magit-diff--dwim))))
                   (new-file (oref file-section value))
                   (old-file (or (oref file-section source) new-file)))
              (list (list (concat rev "^") old-file)
                    (list rev new-file)))
          (magit-diff-visit--sides))))))

(defun magit-config--load-blobs (sides)
  "Load the files in SIDES into `magit-config--blob-buffers', except for those already loaded."
  (let* ((loaded-p (lambda (side)
                     (buffer-live-p (cdr (assoc side magit-config--blob-buffers)))))
         (worktree-p (lambda (side) (member (car side) '(nil "{worktree}"))))
         (missing (seq-remove loaded-p sides))
         (git-contents (magit-config--read-blobs (seq-remove worktree-p missing))))
    (dolist (side missing)
      (let* ((path (expand-file-name (cadr side)))
             (contents (or (cdr (assoc side git-contents))
                           (magit-config--read-file path))))
        (push (cons side (magit-config--make-blob-buffer path contents))
              magit-config--blob-buffers)))))

(defun magit-config--read-file (path)
  "Return the contents of the file at PATH as a unibyte string, or an empty string if it's missing."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (when (file-regular-p path)
      (insert-file-contents-literally path))
    (buffer-string)))

(defun magit-config--read-blobs (sides)
  "Return an alist from each of SIDES to its contents, a unibyte string, using one git process.
The contents are empty for a file which doesn't exist, e.g. the old side of an added file."
  (when sides
    (let ((specs (mapconcat (lambda (side)
                              (pcase-let ((`(,rev ,file) side))
                                (concat (if (equal rev "{index}") "" rev) ":" file "\n")))
                            sides))
          (coding-system-for-read 'binary)
          (coding-system-for-write 'utf-8))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (call-process-region specs nil magit-git-executable nil '(t nil) nil "cat-file" "--batch")
        (goto-char (point-min))
        (mapcar (lambda (side) (cons side (magit-config--next-blob))) sides)))))

(defun magit-config--next-blob ()
  "Return the contents of the object at point in the output of `git cat-file --batch', and skip it."
  (if (looking-at "[0-9a-f]+ \\([a-z]+\\) \\([0-9]+\\)\n")
      (let* ((type (match-string 1))
             (start (match-end 0))
             (end (+ start (string-to-number (match-string 2)))))
        (goto-char (1+ end))
        (if (equal type "blob")
            (buffer-substring-no-properties start end)
          ""))
    ;; Other responses are one line, e.g. "REV:FILE missing" or "OID submodule".
    (forward-line 1)
    ""))

(defun magit-config--make-blob-buffer (path contents)
  "Return a new buffer in the major mode of the file at PATH, with CONTENTS, a unibyte string."
  (when (string-match-p "\\.\\(css\\|scss\\|less\\|x?html?\\)\\'" path)
    (magit-config--load-css-mode))
  ;; There's no "*" in the buffer's name, since whitespace-mode isn't enabled in buffers with one.
  (with-current-buffer (generate-new-buffer (format " magit-config-blob %s" path))
    (insert (decode-coding-string contents 'undecided))
    (setq magit-buffer-file-name path)
    (let ((magit-find-blob-hook nil))
      (magit--blob-normal-mode))
    (current-buffer)))

(defun magit-config--load-css-mode ()
  "Load css-mode without eww, which takes over 100ms to load, since it loads shr and parts of Gnus.
css-mode requires eww only for `css-lookup-symbol', which calls autoloaded eww functions, so they
still load eww when they're called."
  (util/require-without 'css-mode '(eww)))

(defun magit-config--kill-blob-buffers (keep)
  "Kill this buffer's blob buffers, except for the KEEP most recently loaded."
  (dolist (entry (nthcdr keep magit-config--blob-buffers))
    (kill-buffer (cdr entry)))
  (setq magit-config--blob-buffers (seq-take magit-config--blob-buffers keep)))

(defun magit-config--reset-hunk-fontification ()
  "Forget the files and git output which were loaded for highlighting this buffer's hunks."
  (magit-config--kill-blob-buffers 0)
  (setq magit-config--hunk-git-cache nil)
  (setq magit-config--file-sides nil))

(defun magit-config--fontify-before-comment (_text _pos section)
  "Highlight the hunk SECTION before diff-review inserts a comment in it.
Magit can't highlight a hunk with a comment in it, since its text doesn't match its file's."
  (when (and (magit-section-match 'hunk section) (not (oref section fontified)))
    (with-demoted-errors "Error highlighting diff hunk: %S"
      (magit-config--fontify-hunk-sections (list section)))))

(defun magit-config--refontify-section (section)
  "Have jit-lock highlight the hunks in SECTION, which it skipped while SECTION was hidden."
  (when (and (derived-mode-p 'magit-mode) (oref section end))
    (jit-lock-refontify (oref section start) (oref section end))))

(defun magit-config--register-hunk-fontification ()
  (jit-lock-register #'magit-config--fontify-hunks)
  (add-hook 'kill-buffer-hook #'magit-config--reset-hunk-fontification nil t))

(add-hook 'magit-mode-hook #'magit-config--register-hunk-fontification)
(add-hook 'magit-refresh-buffer-hook #'magit-config--reset-hunk-fontification)
(advice-add 'magit-section-show :after #'magit-config--refontify-section)
(advice-add 'dr--insert :before #'magit-config--fontify-before-comment)

;; Have Magit open buffers in the current window, rather than a new split.
;; https://github.com/magit/magit/issues/2541
(setq magit-display-buffer-function (lambda (buffer)
                                      (display-buffer buffer '(display-buffer-same-window))))

;; Magit is causing empty lines in the git status diff view to be highlighted with
;; "whitespace-trailing", even though those lines do not have trailing whitespace, so it's
;; distracting. To fix, I've disabled the font face used to show such errors. Another approach is to
;; disable whitespace-mode (which is activated via `global-whitespace-mode`) in all magit-* buffers.
;; However, I was unable to get that to work, even by setting (whitespace-mode -1) in the hook for
;; every Magit mode I could find. There is some Magit mode used in the Magit diff view that is
;; enabling whitespace-mode, and I couldn't find it.
(defun magit-disable-whitespace-mode ()
  (setq-local whitespace-trailing nil))
(add-hook 'magit-mode-hook 'magit-disable-whitespace-mode)

;; Don't show the stashes in the status view. In my repos over time I accumulate a huge list of
;; stashes, and it clutters the UI to see them every time when reviewing diffs.
(remove-hook 'magit-status-sections-hook 'magit-insert-stashes)
(remove-hook 'magit-status-sections-hook 'magit-insert-status-headers) ; Remove boilerplate
(remove-hook 'magit-revision-sections-hook 'magit-insert-revision-tag) ; Don't show the commit ID.

;; This string formats the headers of revision mode, when reviewing diffs. It's taken from magit's
;; diff.el. I've edited it to omit the commiter name and the date, to allow more of the diff to show
;; and less diff metadata. I already know the commiter and the date because I view specific commits
;; from the git log view.
(setq magit-revision-headers-format "\
Author: %aN <%aE>
Date: %ad
")

;; This setting can speed up the diff view as suggested by
;; https://magit.vc/manual/magit/Performance.html
(setq magit-revision-insert-related-refs nil)

;; Magit runs git in a pty, so tools invoked by git hooks (e.g. deno, dprint) think they're writing
;; to a terminal and emit ANSI escape sequences: colors, plus cursor-movement and erase-line
;; sequences for progress indicators. Magit's process buffer interprets none of these as output
;; arrives, so they show up as garbage. Convert colors into faces and delete all other sequences.
;; `ansi-color-apply' keeps state between calls (sequences split across output chunks, and the
;; current color) in the buffer-local `ansi-color-context', so it must run in the process buffer
;; rather than whatever buffer happens to be current when output arrives.
(defun magit-apply-ansi-escapes (args)
  (let ((proc (car args))
        (string (cadr args)))
    (list proc (with-current-buffer (process-buffer proc)
                 (ansi-color-apply string)))))

(advice-add 'magit-process-filter :filter-args #'magit-apply-ansi-escapes)

;; The process buffer is shared by every git command in the repo, so if a command exits without
;; resetting its color (or mid-sequence), that state would leak into the next command's output.
(defun magit-reset-ansi-context (process _event)
  (when (and (memq (process-status process) '(exit signal))
             (buffer-live-p (process-buffer process)))
    (with-current-buffer (process-buffer process)
      (setq ansi-color-context nil))))

(advice-add 'magit-process-sentinel :after #'magit-reset-ansi-context)

(defun show-commit-and-preserve-window ()
  (interactive)
  (util/preserve-selected-window
   (lambda ()
     ;; I'm not sure why magit-show-commit needs to be called interactively, but just invoking it
     ;; directly gives an argument error.
     (call-interactively 'magit-show-commit)
     ;; After a delay, scroll the window opened by magit-show-commit to the top. While
     ;; magit-show-commit is supposed to scroll the window to the top, if the diff being shown is
     ;; large, that doesn't always work for some reason.
     ;; NOTE(philc): This is needed as of 2025-03-15.
     (let ((w (selected-window)))
       (run-with-timer 0.01 0
                       (lambda ()
                         (util/preserve-selected-window
                          (lambda ()
                            (select-window w)
                            (goto-char (point-min))))))))))

;; Magit mode feels twitchy because every key has a binding, and some are very destructive or
;; disorienting. I'm defining a whitelist of keys that I actually use, so this mode feels less
;; error-prone.
(evil-define-key 'normal magit-mode-map
  ";gg" 'magit-process
  (kbd "C-j") 'magit-section-forward
  (kbd "C-k") 'magit-section-backward)

;; This is a tricky binding -- depending on where your cursor is in the magit status view, you
;; may have the magit-file-section-map activated. evil-define-key doesn't work with this keymap.
;; magit-diff-section-keymap is the parent of magit-file-section and magit-hunk-section. See
;; magit-diff.el
(define-key magit-diff-section-map (kbd "C-k") 'magit-section-backward)
(define-key magit-diff-section-map (kbd "C-j") 'magit-section-forward)

;; Instead of removing the whole window, just close the buffer showing a commit's details and
;; surface the next buffer in the window.
;; "q" in this mode can't be bound with evil-define-key for some reason.
(util/define-keys magit-revision-mode-map "q" 'bury-buffer)

(evil-define-key 'normal magit-diff-mode-map
  (kbd "RET") 'my-magit/diff-visit-file
  "o" 'my-magit/diff-open
  "O" 'my-magit/diff-open-above
  ;; Without this, evil's TAB (evil-jump-forward) shadows magit's section toggle.
  (kbd "TAB") 'magit-section-toggle
  ;; From a line in a hunk, this goes to the file's heading; from a heading, to its parent's.
  "gu" 'magit-section-up
  "gn" 'my-magit/next-hunk-or-comment
  "gp" 'my-magit/previous-hunk-or-comment
  ;; Back and forward through the commits shown in this buffer, e.g. after following a link in a
  ;; review comment's thread.
  "gb" 'magit-go-backward
  "gf" 'magit-go-forward
  ;; Magit's own refresh key is g, which is evil's prefix key.
  "r" 'magit-refresh
  ;; Resolve or reopen the review comment thread at point, or delete it.
  "R" 'dr/toggle-resolved
  "#" 'dr/delete-thread)

;; In a file's buffer with review comments, gn and gp go to them.
(evil-define-minor-mode-key 'normal 'dr/file-mode
  "gn" 'dr/next-comment
  "gp" 'dr/previous-comment)

(defun my-magit/diff-open ()
  "In a commit's revision buffer, open a review comment (see diff-review.el). In other diffs,
visit the file."
  (interactive)
  (if (derived-mode-p 'magit-revision-mode)
      (dr/open-comment)
    (my-magit/diff-visit-file)))

(defun my-magit/diff-open-above ()
  "Like `my-magit/diff-open', except that in a revision buffer it does nothing, because a review
comment always goes below its line."
  (interactive)
  (unless (derived-mode-p 'magit-revision-mode)
    (my-magit/diff-visit-file)))

;; Inside a review comment, keys mean what they do when editing text, except for these section
;; navigation keys.
(evil-define-key 'normal dr/comment-map
  "gu" 'magit-section-up
  "gn" 'my-magit/next-hunk-or-comment
  "gp" 'my-magit/previous-hunk-or-comment
  (kbd "C-j") 'magit-section-forward
  (kbd "C-k") 'magit-section-backward)

(defun my-magit/hunk-stop-p (section)
  "Whether `my-magit/next-hunk' stops at SECTION: a hunk, a collapsed section, or a file with no
hunks (e.g. a binary or untracked file), which would otherwise be skipped over. It never stops at
the file lines of a revision buffer's diffstat."
  (and (not (magit-section-match [* diffstat] section))
       (or (magit-section-match 'hunk section)
           (oref section hidden)
           (and (magit-section-match 'file section)
                (not (oref section children))))))

(defun my-magit/move-to-hunk (move)
  "Call MOVE, `magit-section-forward' or `magit-section-backward', until reaching a section where
`my-magit/hunk-stop-p' holds. The intermediate moves skip `magit-section-movement-hook', so it
runs only once, on the final section."
  (let ((start (point))
        target)
    (condition-case nil
        (let ((magit-section-movement-hook nil))
          (while (not target)
            (funcall move)
            (when (my-magit/hunk-stop-p (magit-current-section))
              (setq target (magit-current-section)))))
      (user-error nil))
    (goto-char start)
    (if target
        (progn
          (magit-section-goto target)
          ;; A collapsed section shows only its heading.
          (util/scroll-into-view (if (oref target hidden)
                                     (line-end-position)
                                   (oref target end))))
      (message "No more hunks"))))

(defun my-magit/next-hunk ()
  "Go to the next hunk, skipping the headings of expanded sections, but stopping at collapsed ones."
  (interactive)
  (my-magit/move-to-hunk #'magit-section-forward))

(defun my-magit/previous-hunk ()
  "Go to the start of this hunk, or the previous one. See `my-magit/next-hunk'."
  (interactive)
  (my-magit/move-to-hunk #'magit-section-backward))

(defun my-magit/next-hunk-or-comment ()
  "In a revision buffer with review comments, go to the next comment. Otherwise, the next hunk."
  (interactive)
  (if (dr/any-comments-p)
      (dr/next-comment)
    (my-magit/next-hunk)))

(defun my-magit/previous-hunk-or-comment ()
  "Like `my-magit/next-hunk-or-comment', but backward."
  (interactive)
  (if (dr/any-comments-p)
      (dr/previous-comment)
    (my-magit/previous-hunk)))

(evil-define-key 'normal git-commit-mode-map
  ";wk" 'git-commit-abort
  (kbd "M-s") 'with-editor-finish
  "ZZ" 'with-editor-finish)

(evil-define-key 'insert git-commit-mode-map
  (kbd "M-s") 'with-editor-finish)

;; git-rebase loads when a rebase's todo list opens, and evil binds these then.
(evil-define-key 'normal git-rebase-mode-map
  ";gra" 'git-rebase-abort
  (kbd "S-C-k") 'git-rebase-move-line-up
  (kbd "S-C-j") 'git-rebase-move-line-down
  "e" 'git-rebase-edit
  "r" 'git-rebase-reword
  "p" 'git-rebase-pick
  "dd" 'git-rebase-kill-line
  "f" 'git-rebase-fixup
  "s" 'git-rebase-squash
  (kbd "M-s") 'with-editor-finish
  ";wk" 'with-editor-cancel
  "ZZ" 'with-editor-finish)

;; NOTE(philc): I'm setting the key bindings for these magit modes when their buffers load, because
;; for some reason, the evil bindings on these modes conflict (i.e. when a new mode loads, it
;; redefines the key for the other modes). This was needed the last time I checked, but may be
;; unnecessary in later versions of magit. But it does work in later versions of magit.
(add-hook 'magit-log-mode-hook 'init-magit-log-mode-keybindings)
(defun init-magit-log-mode-keybindings ()
  (util/define-keys
   evil-normal-state-local-map
   ";gca" 'magit-commit-amend
   ";gra" 'git-rebase-abort
   ";grc" 'git-rebase-continue
   ";gri" 'magit-rebase-interactive
   ";gpush" 'git-push
   ";gpull" 'git-pull
   ";re" 'ext-dev/reload-extension-in-browser
   ";rr" 'reload-active-browser-tab
   "o" 'show-commit-and-preserve-window
   "O" 'show-commit-and-preserve-window
   (kbd "RET") 'show-commit-and-preserve-window
   "yy" 'magit-copy-section-value ; Copies the commit ID of the commit under the cursor.
   "r" 'my-magit/magit-refresh-preserve-cursor
   "q" 'bury-buffer
   ;; I use C-d and C-u for scrolling the log view, and d and u for scrolling the diff view which
   ;; shows the diff of the currently-focused commit.
   "u" (lambda () (interactive) (scroll-magit-revision-buffer 'View-scroll-half-page-backward))
   "d" (lambda () (interactive) (scroll-magit-revision-buffer 'View-scroll-half-page-forward))))

(defun scroll-magit-revision-buffer (fn)
  "If the magit-revision buffer is showing in the current frame, scroll it using fn."
  (let* ((windows-in-visible-frames (-mapcat 'window-list (visible-frame-list)))
         (window (-find (lambda (w)
                          (eq (buffer-local-value 'major-mode (window-buffer w))
                              'magit-revision-mode))
                        windows-in-visible-frames)))
    (when window
      (with-selected-window window (funcall fn)))))

(defun init-magit-status-mode-keybindings ()
  ;; NOTE(philc): using `evil-define-key` for these keymaps does not work.
  (util/define-keys
   evil-normal-state-local-map
   ";grc" 'git-rebase-continue
   ";gca" 'magit-commit-amend
   ";gpush" 'git-push
   ";gpull" 'git-pull
   ";re" 'ext-dev/reload-extension-in-browser
   ";rr" 'reload-active-browser-tab
   "o" 'my-magit/diff-visit-file
   "O" 'my-magit/diff-visit-file
   (kbd "RET") 'my-magit/diff-visit-file
   "c" 'magit-commit
   "e" 'magit-show-level-4-all ; e for exapnd
   "d" 'magit-discard
   "s" 'magit-stage
   ;; Stage everything
   "S" (lambda () (interactive) (util/without-confirmation 'magit-stage-modified t))
   "u" 'magit-unstage
   "U" (lambda () (interactive) (util/without-confirmation 'magit-unstage-all))
   "-" 'magit-diff-smaller-hunks
   "+" 'magit-diff-larger-hunks
   "gu" 'magit-section-up
   "gU" 'magit-jump-to-unstaged
   "gn" 'my-magit/next-hunk
   "gp" 'my-magit/previous-hunk
   "zi" 'magit-stash-index
   "zp" 'magit-stash-pop
   "K" 'magit-section-backward
   (kbd "TAB") 'magit-section-toggle
   "q" 'bury-buffer
   "r" 'my-magit/magit-refresh-preserve-cursor))
(add-hook 'magit-status-mode-hook 'init-magit-status-mode-keybindings)

(defun my-magit/magit-refresh-preserve-cursor ()
  (interactive)
  (util/preserve-line-and-column 'magit-refresh))

;; Works like magit-diff-visible-file, except that it uses my custom function to select which window
;; to show the visited file in.
(defun my-magit/diff-visit-file ()
  "Visit the file at point. On a commit, e.g. in the status buffer's Unmerged section, show the
commit instead."
  (interactive)
  (cond
   ;; In the review buffer, the commit is more useful than the file.
   ((derived-mode-p 'dr/review-mode) (dr/review-visit))
   ((magit-section-match 'commit) (show-commit-and-preserve-window))
   ;; Like magit's own magit-diff-visit-file-other-window, which also uses this internal function.
   (t (magit-diff-visit-file--internal nil #'wm/show-buffer-rightward))))

(add-hook 'git-commit-mode-hook 'init-git-commit-mode)
(defun init-git-commit-mode ()
  ;; Wrap commit message bodies at 72 chars, the git convention, rather than the global 100.
  (setq-local fill-column 72)
  ;; Enter insert mode when the git commit window is shown. evil-set-initial-state doesn't work here
  ;; because the git commit window's major mode is text-mode, and not git-commit-mode, for some
  ;; reason.
  (evil-insert 0)
  ;; In some repos I have a git commit hook which prepopulates a commit message prefix for me. In
  ;; those cases, move to the end ;; of the line so I can begin typing after the prefix.
  (end-of-line))

(add-hook 'magit-commit-mode-hook 'init-magit-commit-mode-keybindings)
(defun init-magit-commit-mode-keybindings ()
  ;; I'm specifying these keys here because for some reason they get overridden by the yy shortcut
  ;; I've defined for magit-log-mode.
  (util/define-keys evil-normal-state-local-map "yy" 'evil-yank-line)
  (util/define-keys evil-visual-state-local-map "y" 'evil-yank))

(defun with-magit-output-buffer (f)
  "Displays the magit output buffer after invoking the given function"
  (util/preserve-selected-window
   (lambda ()
     (funcall f)
     (display-buffer (magit-process-buffer t)))))

(defun git-pull ()
  (interactive)
  (with-magit-output-buffer (lambda () (call-interactively 'magit-pull-from-upstream))))

(defun git-push ()
  (interactive)
  (with-magit-output-buffer (lambda () (call-interactively 'magit-push-current-to-upstream))))

(defun magit-status-and-focus-unstaged ()
  "Opens the magit-status view and focuses the cursor on the first unstaged file."
  (interactive)
  (magit-status-setup-buffer)
  ;; Look the sections up directly rather than calling magit-jump-to-untracked and
  ;; magit-jump-to-unstaged, which echo "Section ... wasn't found" when the section is absent.
  (let ((find (lambda (type)
                (magit-get-section (cons (cons type nil)
                                         (magit-section-ident magit-root-section))))))
    ;; Untracked files are collapsed by default. Show them, so I can resolve them. I tried to
    ;; instead customize magit-section-initial-visibility-alist to achieve this, but it had no
    ;; effect.
    (when-let* ((section (funcall find 'untracked)))
      (magit-section-show section))
    ;; Put the cursor on the unstaged section.
    (when-let* ((section (funcall find 'unstaged)))
      (goto-char (oref section start)))))

(defun show-git-status-in-left-column ()
  (interactive)
  ;; We want to show the git status for the current buffer's repo. We first get that repo before
  ;; switching to the first column's window, because it may contain a buffer from a different git
  ;; repo.
  (let ((dir (magit-toplevel)))
    (wm/switch-to-column 0)
    (magit-status-setup-buffer dir)
    (magit-status-and-focus-unstaged)))

;;
;; Restore Magit buffers when Emacs restarts (see desktop in init.el). Desktop only saves file
;; buffers unless a buffer says how to recreate it, so a window that showed a Magit buffer used to
;; come back showing an arbitrary buffer. For each mode below, save the buffer's repo and the
;; arguments of the function that sets the buffer up, and call that function again on restore.
;;

(defvar desktop-buffer-major-mode) ; Bound by desktop while it restores each buffer.

(defvar magit-config/desktop-setup-functions
  '((magit-status-mode . magit-status-setup-buffer)
    (magit-log-mode . magit-log-setup-buffer)
    (magit-revision-mode . magit-revision-setup-buffer)
    (magit-diff-mode . magit-diff-setup-buffer))
  "Magit modes whose buffers are restored by desktop, and the functions which recreate them.")

(defun magit-config/desktop-buffer-data (_desktop-dirname)
  "The state desktop saves for the current Magit buffer: its repo, its setup function's args, and
whether it's locked. Locking is Magit's own concept: a locked buffer keeps showing one commit (or
log, or diff), and Magit opens a new buffer for the next one, rather than reusing this buffer. See
`magit-toggle-buffer-lock'."
  (list default-directory
        (pcase major-mode
          ('magit-status-mode nil)
          ('magit-log-mode
           (list magit-buffer-log-revisions magit-buffer-log-args magit-buffer-log-files))
          ('magit-revision-mode
           (list magit-buffer-revision magit-buffer-diff-args magit-buffer-diff-files))
          ('magit-diff-mode
           (list magit-buffer-diff-range magit-buffer-diff-typearg magit-buffer-diff-args
                 magit-buffer-diff-files magit-buffer-diff-type)))
        magit-buffer-locked-p))

(defun magit-config/desktop-restore-buffer (_file-name _buffer-name data)
  "Recreate a Magit buffer from DATA, saved by `magit-config/desktop-buffer-data'."
  (seq-let (dir args locked) data
    (when (file-directory-p dir)
      (let ((default-directory dir)
            ;; Only create the buffer. Desktop puts it back in its windows.
            (magit-display-buffer-function #'ignore)
            (magit-display-buffer-noselect t)
            ;; Recreate a locked buffer locked (see above). Desktop renames the buffer to its saved
            ;; name, so an unlocked buffer would take a locked one's name, which has the commit in it,
            ;; e.g. "magit-revision(4bc7bf3): repo". Then Magit would reuse it for every commit.
            (magit-revision-use-dedicated-buffers locked)
            (args (if (memq desktop-buffer-major-mode '(magit-log-mode magit-diff-mode))
                      ;; These setup functions take whether to lock as their next argument.
                      (append args (list locked))
                    args)))
        (apply (alist-get desktop-buffer-major-mode magit-config/desktop-setup-functions)
               args)))))

(add-hook 'magit-mode-hook
          (lambda ()
            (when (assq major-mode magit-config/desktop-setup-functions)
              (setq-local desktop-save-buffer #'magit-config/desktop-buffer-data))))

(with-eval-after-load 'desktop
  (dolist (entry magit-config/desktop-setup-functions)
    (add-to-list 'desktop-buffer-mode-handlers
                 (cons (car entry) #'magit-config/desktop-restore-buffer))))
