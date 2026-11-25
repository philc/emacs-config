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

(require 'magit)
(require 'git-rebase) ; for git-rebase-mode-map.
(require 'evil)
(require 'window-management)
(require 'diff-review)
(provide 'magit-config)

;; When committing, don't have Magit show the diff of what's changed. This feature is annoying
;; because it creates two buffers. I've already reviewed the staged changes prior to activating
;; commit mode and don't need to see the changes again.
(setq magit-commit-show-diff nil)

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

;; Syntax-highlight the diff hunks which have been displayed, while Emacs is idle. See
;; `magit-diff-specify-hunk-foreground' above. Highlighting a hunk loads the old and new versions of
;; its file, which takes several git processes and a major mode setup per version. Magit's own `all'
;; setting does this for every hunk before showing the buffer. Doing it during redisplay instead
;; freezes scrolling: Emacs doesn't redraw while keys are waiting, so when C-d is held down, the
;; delays of successive hunks run together.
(setq magit-diff-fontify-hunk nil)

(defvar magit-config--fontify-delay 0.1
  "Seconds of idle time before highlighting hunks. Longer than the interval between key repeats.")

(defvar-local magit-config--pending-hunks nil
  "Hunk sections which have been displayed, but not yet highlighted.")

(defvar-local magit-config--fontify-timer nil)

(defvar-local magit-config--hunk-git-cache nil
  "Git output cached while highlighting this buffer's hunks, in the form of `magit--refresh-cache'.
Kept until the buffer is next refreshed, so highlighting more hunks of a file doesn't rerun git.")

(defvar-local magit-config--blobs nil
  "Hash table from a git object name, like \"REV:FILE\", to the file's contents.
The contents are a unibyte string, or `unavailable' if they couldn't be fetched.")

(defvar-local magit-config--blob-process nil
  "The git process which is fetching file contents, if any.")

(defvar magit-config--blob-buffers nil
  "Alist from a git object name to a buffer of its contents. Bound while highlighting.")

(defun magit-config--fontify-hunks (start end)
  "Queue the diff hunks between START and END to be highlighted. Run by jit-lock."
  (named-let collect ((section magit-root-section))
    (when (and section
               (< (oref section start) end)
               (> (oref section end) start))
      (if (magit-section-match 'hunk section)
          (unless (or (oref section fontified)
                      (magit--meta-hunk-p section)
                      (memq section magit-config--pending-hunks))
            (push section magit-config--pending-hunks))
        (mapc #'collect (oref section children)))))
  (when magit-config--pending-hunks
    (magit-config--schedule-fontify)))

(defun magit-config--schedule-fontify (&optional delay)
  "Highlight this buffer's pending hunks once Emacs is idle.
Check whether it's idle after DELAY seconds, by default `magit-config--fontify-delay'."
  (unless (memq magit-config--fontify-timer timer-list)
    ;; This isn't an idle timer because those count from when Emacs became idle, which may be long
    ;; ago. If a key arrived first, the timer would then wait for another idle period that long.
    (setq magit-config--fontify-timer
          (run-with-timer (or delay magit-config--fontify-delay) nil
                          #'magit-config--fontify-when-idle (current-buffer)))))

(defun magit-config--fontify-when-idle (buffer)
  "Highlight BUFFER's pending hunks if Emacs is idle. Otherwise, try again later."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq magit-config--fontify-timer nil)
      (let ((idle-time (current-idle-time)))
        (if (and idle-time (>= (float-time idle-time) magit-config--fontify-delay))
            (magit-config--fontify-pending)
          (magit-config--schedule-fontify))))))

(defun magit-config--hunk-visible-p (hunk)
  "Return non-nil if part of HUNK, in the current buffer, is showing in a window."
  (seq-some (lambda (window)
              (and (< (oref hunk start) (window-end window))
                   (> (oref hunk end) (window-start window))))
            (get-buffer-window-list nil nil 'visible)))

(defun magit-config--fontify-pending ()
  "Highlight this buffer's pending hunks, first fetching the versions of files they need."
  (let ((specs (magit-config--missing-blob-specs)))
    (cond
     ;; The process schedules this again when it exits.
     ((process-live-p magit-config--blob-process))
     (specs (magit-config--fetch-blobs specs))
     (t (magit-config--fontify-until-input)))))

(defun magit-config--fontify-until-input ()
  "Highlight this buffer's pending hunks, visible ones first, until there's input."
  ;; Magit caches git output only during a refresh. Without a cache, each hunk reruns the same git
  ;; commands.
  (let ((magit--refresh-cache (or magit-config--hunk-git-cache
                                  (setq magit-config--hunk-git-cache (list (cons 0 0)))))
        (magit-config--blob-buffers nil))
    (unwind-protect
        (while (and magit-config--pending-hunks (not (input-pending-p)))
          (let* ((hunk (or (seq-find #'magit-config--hunk-visible-p magit-config--pending-hunks)
                           (car magit-config--pending-hunks)))
                 (mode (magit-config--mode-to-load hunk)))
            (if mode
                (magit-config--load-mode mode)
              (setq magit-config--pending-hunks (delq hunk magit-config--pending-hunks))
              (with-demoted-errors "Error highlighting diff hunk: %S"
                (magit-config--fontify-hunk hunk)))))
      (mapc #'kill-buffer (mapcar #'cdr magit-config--blob-buffers))))
  (when magit-config--pending-hunks
    (magit-config--schedule-fontify)))

(defvar magit-config--load-attempted nil
  "Major modes which `magit-config--load-mode' has tried to load.")

(defun magit-config--mode-to-load (hunk)
  "Return the major mode for HUNK's file if it isn't loaded yet.
Loading a mode can take as long as highlighting several hunks, so it's a step of its own, which a
key press can come before or after."
  (when-let* ((file (oref (oref hunk parent) value))
              ((stringp file))
              (mode (assoc-default file auto-mode-alist #'string-match-p))
              ((symbolp mode))
              (mode (major-mode-remap mode))
              ((autoloadp (symbol-function mode)))
              ((not (memq mode magit-config--load-attempted))))
    (if (and (memq mode '(css-mode scss-mode less-css-mode mhtml-mode))
             (not (featurep 'css-mode))
             (not (memq 'css-mode magit-config--load-attempted)))
        'css-mode
      mode)))

(defun magit-config--load-mode (mode)
  "Load the library which defines MODE."
  (push mode magit-config--load-attempted)
  (with-demoted-errors "Error loading a major mode: %S"
    (if (eq mode 'css-mode)
        (magit-config--load-css-mode)
      (autoload-do-load (symbol-function mode) mode))))

(defun magit-config--load-css-mode ()
  "Load css-mode without eww, which takes over 100ms to load, since it loads shr and parts of Gnus.
css-mode requires eww only for `css-lookup-symbol', which calls autoloaded eww functions, so they
still load eww when they're called."
  (if (or (featurep 'css-mode) (featurep 'eww))
      (require 'css-mode)
    (provide 'eww)
    (unwind-protect
        (require 'css-mode)
      (setq features (delq 'eww features)))))

(defun magit-config--fontify-hunk (hunk)
  "Highlight HUNK, using the fetched versions of its file if there are any."
  (let ((sides (magit-config--hunk-sides hunk)))
    (if (and sides
             magit-config--blobs
             (seq-every-p (lambda (side) (stringp (gethash (car side) magit-config--blobs)))
                          sides))
        (let ((blobs magit-config--blobs))
          ;; Magit would run several git commands per file to find and load these.
          (cl-letf (((symbol-function 'magit-diff-visit--sides) (lambda () sides))
                    ((symbol-function 'magit-find-file-noselect)
                     (lambda (spec file &rest _)
                       (magit-config--blob-buffer spec file (gethash spec blobs)))))
            (magit-diff--update-hunk-syntax hunk)))
      (magit-diff--update-hunk-syntax hunk))))

;;; Fetching the versions of files in a commit's diff.

(defun magit-config--hunk-sides (hunk)
  "Return HUNK's old and new sides, each a list of a git object name and a file.
Return nil unless this buffer shows a commit. In other diffs, a side can be the index or worktree."
  (when-let* (((derived-mode-p 'magit-revision-mode))
              (rev (or magit-buffer-revision-oid magit-buffer-revision))
              (file-section (oref hunk parent))
              (new-file (oref file-section value))
              (old-file (or (oref file-section source) new-file)))
    (list (list (format "%s^:%s" rev old-file) old-file)
          (list (format "%s:%s" rev new-file) new-file))))

(defun magit-config--missing-blob-specs ()
  "Return the git object names which the pending hunks need and which haven't been fetched."
  (let ((specs (mapcan (lambda (hunk) (mapcar #'car (magit-config--hunk-sides hunk)))
                       magit-config--pending-hunks)))
    (seq-remove (lambda (spec)
                  (and magit-config--blobs (gethash spec magit-config--blobs)))
                (seq-uniq specs))))

(defun magit-config--fetch-blobs (specs)
  "Fetch the contents of SPECS with one git process, then highlight the pending hunks."
  (unless magit-config--blobs
    (setq magit-config--blobs (make-hash-table :test #'equal)))
  (let* ((diff-buffer (current-buffer))
         (blobs magit-config--blobs)
         (output (generate-new-buffer " *magit-config-blobs*"))
         (sentinel
          (lambda (process _event)
            (unless (process-live-p process)
              (magit-config--parse-blobs output specs blobs)
              (kill-buffer output)
              (when (buffer-live-p diff-buffer)
                (with-current-buffer diff-buffer
                  (when magit-config--pending-hunks
                    (magit-config--schedule-fontify 0))))))))
    (with-current-buffer output
      (set-buffer-multibyte nil))
    (setq magit-config--blob-process
          (make-process :name "magit-config-blobs" :buffer output :noquery t
                        :connection-type 'pipe :coding 'binary
                        :command (list magit-git-executable "cat-file" "--batch")
                        :sentinel sentinel))
    (process-send-string magit-config--blob-process
                         (encode-coding-string (concat (string-join specs "\n") "\n") 'utf-8))
    (process-send-eof magit-config--blob-process)))

(defun magit-config--parse-blobs (output specs blobs)
  "Store in BLOBS the contents of SPECS, from OUTPUT, a buffer of `git cat-file --batch' output."
  (with-current-buffer output
    (goto-char (point-min))
    (dolist (spec specs)
      (puthash spec
               (cond
                ((looking-at "[0-9a-f]+ \\([a-z]+\\) \\([0-9]+\\)\n")
                 (let* ((type (match-string 1))
                        (start (match-end 0))
                        (end (+ start (string-to-number (match-string 2)))))
                   (if (>= end (point-max))
                       'unavailable
                     (goto-char (1+ end))
                     (if (equal type "blob")
                         (buffer-substring-no-properties start end)
                       'unavailable))))
                ;; The commit added or deleted this file.
                ((looking-at ".* missing\n")
                 (goto-char (match-end 0))
                 "")
                ;; Other responses are one line too, e.g. "OID submodule".
                (t
                 (forward-line 1)
                 'unavailable))
               blobs))))

(defun magit-config--blob-buffer (spec file contents)
  "Return a buffer in FILE's major mode with CONTENTS, the file's contents at SPEC."
  (or (alist-get spec magit-config--blob-buffers nil nil #'equal)
      (let* ((file (expand-file-name file))
             (dir default-directory)
             ;; whitespace-mode isn't enabled in buffers named with a "*".
             (buffer (generate-new-buffer (format " magit-config-blob %s" spec))))
        (push (cons spec buffer) magit-config--blob-buffers)
        (with-current-buffer buffer
          (setq default-directory dir)
          (insert (decode-coding-string contents 'undecided))
          (setq magit-buffer-file-name file)
          (let ((magit-find-blob-hook nil))
            (magit--blob-normal-mode))
          buffer))))

(defun magit-config--register-hunk-fontification ()
  (jit-lock-register #'magit-config--fontify-hunks))

(defun magit-config--reset-hunk-fontification ()
  "Forget the hunks and git output from before this buffer was refreshed."
  (setq magit-config--pending-hunks nil)
  (setq magit-config--blobs nil)
  (setq magit-config--hunk-git-cache nil))

(add-hook 'magit-mode-hook #'magit-config--register-hunk-fontification)
(add-hook 'magit-refresh-buffer-hook #'magit-config--reset-hunk-fontification)

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
  "X" 'dr/delete-thread)

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
hunks (e.g. a binary or untracked file), which would otherwise be skipped over."
  (or (magit-section-match 'hunk section)
      (oref section hidden)
      (and (magit-section-match 'file section)
           (not (oref section children)))))

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
        (magit-section-goto target)
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
