;; My configuration and customizations of magit mode, for interacting with Git.
;;
;; Note that magit's keybindings are notoriously hard to rebind consistently. It would be better if there was
;; a way to unset all of Magit's keybindings. For further discussion:
;; https://github.com/magit/magit/issues/1968
;;
;; Helpful references:
;; https://github.com/justbur/evil-magit/blob/master/evil-magit.el

(require 'emacs-utils)
(require 'dash)
(require 's)
(require 'view) ; for View-scroll-half-page-backward.
(require 'magit)
(provide 'magit-config)

;; When committing, don't have Magit show the diff of what's changed. This feature is annoying because it
;; creates two buffers. I've already reviewed the staged changes prior to activating commit mode and don't
;; need to see the changes again.
(setq magit-commit-show-diff nil)

;; Disable Emacs' built-in VC package for git repositories. This prevents it from doing unnecessary work when
;; Magit is performing git operations. This was recommended by the Magit manual. Empirically, I've noticed
;; this greatly speeds up git rebasing with Magit.
(setq vc-handled-backends (delq 'Git vc-handled-backends))

;; Don't refresh the status buffer unless it's currently focused. This should improve performance.
(setq magit-refresh-status-buffer nil)

;; Have Magit open buffers in the current window, rather than a new split.
;; https://github.com/magit/magit/issues/2541
(setq magit-display-buffer-function (lambda (buffer)
                                      (display-buffer buffer '(display-buffer-same-window))))

;; Don't show the stashes in the status view. In my repos over time I accumulate a huge list of stashes,
;; and it clutters the UI to see them every time when reviewing diffs.
(remove-hook 'magit-status-sections-hook 'magit-insert-stashes)
(remove-hook 'magit-status-sections-hook 'magit-insert-status-headers) ; Remove boilerplate
(remove-hook 'magit-revision-sections-hook 'magit-insert-revision-tag) ; Don't show the commit ID.

;; This string formats the headers of revision mode, when reviewing diffs. It's taken from magit's diff.el and
;; has been edited to omit commiter name and date.
(setq magit-revision-headers-format "\
Author: %aN <%aE>
Date: %ad
")

;; Don't use a unicode ellipsis character when truncating author names in the git log view. It screws up
;; the line height with my current font (Inconsolata).
(setq magit-ellipsis (get-byte 0 "."))

;; This setting can speed up the diff view as suggested by https://magit.vc/manual/magit/Performance.html
(setq magit-revision-insert-related-refs nil)

(defun show-commit-and-preserve-window ()
  (interactive)
  ;; NOTE(philc): I'm not sure why magit-show-commit needs to be called interactively, but just invoking it
  ;; directly gives an argument error.
  (util/preserve-selected-window (lambda () (call-interactively 'magit-show-commit))))

;; Magit mode feels twitchy because every key has a binding, and some are very destructive or disorienting.
;; I'm defining a whitelist of keys that I actually use, so this mode feels less error-prone.
(evil-define-key 'normal magit-mode-map
  ";gg" 'magit-process
  "J" 'magit-section-forward
  "K" 'magit-section-backward)

;; This is a tricky binding -- depending on where your cursor is in the magit status view, you may have
;; the magit-file-section-map activated. evil-define-key doesn't work with this keymap.
(define-key magit-file-section-map "K" 'magit-section-backward) ; "K" was bound to magit-file-untrack.

;; "q" in this mode can't be bound with evil-define-key for some reason.
(util/define-keys magit-revision-mode-map "q" 'bury-buffer)

(evil-define-key 'normal magit-diff-mode-map
  (kbd "RET") 'magit-diff-visit-file
  "o" 'magit-diff-visit-file)

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

;; NOTE(philc): I'm setting the key bindings for these magit modes when their buffers load, because for
;; some reason, the evil bindings on these modes conflict (i.e. when a new mode loads, it redefines the key
;; for the other modes). This was needed the last time I checked, but may be unnecessary in later versions of
;; magit. But it does work in later versions of magit.
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
   "o" 'show-commit-and-preserve-window
   (kbd "RET") 'show-commit-and-preserve-window
   "yy" 'magit-copy-section-value ; Copies the commit ID of the commit under the cursor.
   "r" 'magit-refresh
   "q" 'bury-buffer
   ;; I use C-d and C-u for scrolling the log view, and d and u for scrolling the diff view which shows the
   ;; diff of the currently-focused commit.
   "u" (lambda () (interactive) (scroll-magit-revision-buffer 'View-scroll-half-page-backward))
   "d" (lambda () (interactive) (scroll-magit-revision-buffer 'View-scroll-half-page-forward))))

(defun scroll-magit-revision-buffer (fn)
  "If the magit-revision buffer is showing in the current frame, scroll it using fn."
  (let* ((windows-in-visible-frames (-mapcat 'window-list (visible-frame-list)))
         (window (-find (lambda (w)
                          (->> w window-buffer buffer-name (s-starts-with? "*magit-revision:")))
                        windows-in-visible-frames)))
    (when window
      (with-selected-window window (funcall fn)))))

(add-hook 'magit-status-mode-hook 'init-magit-status-mode-keybindings)
(defun init-magit-status-mode-keybindings ()
  ;; NOTE(philc): using `evil-define-key` for these keymaps does not work.
  (util/define-keys
   evil-normal-state-local-map
   ";grc" 'git-rebase-continue
   ";gca" 'magit-commit-amend
   ";gpush" 'git-push
   ";gpull" 'git-pull
   "o" 'magit-diff-visit-file
   (kbd "RET") 'magit-diff-visit-file
   "c" 'magit-commit
   ;; I have a git precommit hook which does style checks. Sometimes I want to disable it when committing.
   "C" (lambda() (interactive) (util/with-env-var "SKIP_GIT_STYLE_CHECK" "true" 'magit-commit))
   "e" 'magit-show-level-4-all ; e for exapnd
   "d" 'magit-discard
   "s" 'magit-stage
   "S" (lambda () (interactive) (util/without-confirmation 'magit-stage-all))
   "d" 'magit-discard
   "u" 'magit-unstage-item
   "U" (lambda () (interactive (util/without-confirmation 'magit-unstage-all)))
   "-" 'magit-diff-smaller-hunks
   "+" 'magit-diff-larger-hunks
   "gu" 'magit-jump-to-unstaged
   "K" 'magit-section-backward
   (kbd "TAB") 'magit-section-toggle
   "q" 'bury-buffer
   "r" 'magit-refresh))

(add-hook 'git-commit-mode-hook 'init-git-commit-mode)
(defun init-git-commit-mode ()
  ;; Enter insert mode when the git commit window is shown. evil-set-initial-state doesn't work here because
  ;; the git commit window's major mode is text-mode, and not git-commit-mode, for some reason.
  (evil-insert 0)
  ;; In some repos I have a git commit hook which prepopulates a commit message prefix for me. In those cases,
  ;; move to the end ;; of the line so I can begin typing after the prefix.
  (end-of-line))

(add-hook 'magit-commit-mode-hook 'init-magit-commit-mode-keybindings)
(defun init-magit-commit-mode-keybindings ()
  ;; I'm specifying these keys here because for some reason they get overridden by the yy shortcut I've
  ;; defined for magit-log-mode.
  (util/define-keys evil-normal-state-local-map "yy" 'evil-yank-line)
  (util/define-keys evil-visual-state-local-map "y" 'evil-yank))

(defun with-magit-output-buffer (f)
  "Displays the magit output buffer after invoking the given function"
  (lexical-let ((f f))
    (util/preserve-selected-window
     (lambda ()
       (funcall f)
       (display-buffer (magit-process-buffer t))))))

(defun git-pull ()
  (interactive)
  (with-magit-output-buffer (lambda () (call-interactively 'magit-pull-from-upstream))))

(defun git-push ()
  (interactive)
  (with-magit-output-buffer (lambda () (call-interactively 'magit-push-current-to-upstream))))

(defun magit-status-and-focus-unstaged ()
  "Opens the magit-status view and focuses the cursor on the first unstaged file."
  (interactive)
  (call-interactively 'magit-status)
  (magit-jump-to-unstaged))
