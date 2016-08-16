;;; My configuration and customizations of magit mode, for interacting with Git.

(require 'emacs-utils)
(provide 'magit-mode-personal)

(with-eval-after-load "magit"
  ;; Magit mode feels twitchy because every key has a binding, and some are very destructive or
  ;; disorienting. I'm defining a whitelist of keys that I actually use, so this mode feels less
  ;; erorr-prone.
  (evil-define-key 'normal magit-mode-map
    ";gg" 'magit-process
    "n" 'magit-goto-next-section
    "p" 'magit-goto-previous-section
    "L" 'magit-key-mode-popup-logging
    "o" '(lambda () (interactive) (util/preserve-selected-window 'magit-visit-item))
    (kbd "RET") '(lambda () (interactive) (util/preserve-selected-window 'magit-visit-item)))

  (evil-define-key 'normal git-commit-mode-map
    ";wk" 'git-commit-abort
    (kbd "M-s") 'git-commit-commit
    "ZZ" 'git-commit-commit)

  (evil-define-key 'insert git-commit-mode-map
    (kbd "M-s") 'git-commit-commit)

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
    (kbd "M-s") 'git-rebase-server-edit
    "ZZ" 'git-rebase-server-edit)

  (evil-leader/set-key-for-mode 'git-commit-mode
    "c" 'git-commit-commit)

  ;; Customize the order of the sections which are shown in the status view. You can find the full list in
  ;; the magit source code.
  (setq magit-status-sections-hook
        '(magit-insert-status-local-line
          magit-insert-status-head-line
          magit-insert-status-merge-line
          magit-insert-status-rebase-lines
          magit-insert-empty-line
          ;; magit-rewrite-diff-pending
          magit-insert-pending-commits
          magit-insert-unstaged-changes
          magit-insert-staged-changes
          magit-insert-untracked-files
          magit-insert-stashes
          magit-insert-unpulled-commits
          magit-insert-unpushed-commits))

  ;; Don't use a unicode ellipsis character when truncating author names in the git log view. It screws up
  ;; the line height with my current font (Inconsolata).
  (setq magit-ellipsis (get-byte 0 ".")))

(evil-set-initial-state 'magit-mode 'normal)
(evil-set-initial-state 'magit-status-mode 'normal)
(evil-set-initial-state 'magit-log-mode 'normal)
(evil-set-initial-state 'magit-commit-mode 'normal)
(evil-set-initial-state 'magit-diff-mode 'normal)
(evil-set-initial-state 'git-commit-mode 'insert)

;; Have Magit open in the current window, not a new split.
(setq magit-status-buffer-switch-function 'switch-to-buffer)

;; Disable the `highlight` face that Magit uses to highlight diffs. It's unreadable with my color scheme.
;; An unreadable highlight face is a common issue on the Magit tracker.
(defun disable-magit-highlight-in-buffer () (face-remap-add-relative 'magit-item-highlight '()))
(add-hook 'magit-status-mode-hook 'disable-magit-highlight-in-buffer)
(add-hook 'magit-commit-mode-hook 'disable-magit-highlight-in-buffer)
(add-hook 'magit-diff-mode-hook 'disable-magit-highlight-in-buffer)

;; NOTE(philc): I'm setting the key bindings for these magit modes when their buffers load, because for
;; some reason, the evil bindings on these modes conflict (i.e. when a new mode loads, it redefines the key
;; for the other modes.
(add-hook 'magit-log-mode-hook 'init-magit-log-mode-keybindings)
(defun init-magit-log-mode-keybindings ()
  (util/define-keys
   evil-normal-state-local-map
   ";gca" 'magit-commit-amend
   ";gra" 'git-rebase-abort
   ";grc" 'git-rebase-continue
   ";gri" 'magit-interactive-rebase
   ";gpush" 'git-push
   ";gpull" 'git-pull
   "yy" 'magit-copy-full-commit-id ; Copies the commit ID of the commit under the cursor.
   "r" 'magit-refresh
   (kbd "SPC") 'magit-goto-next-section
   ;; I use C-d and C-u for scrolling the log view, and d and u for scrolling the diff view showing the
   ;; diff for the focused commit.
   "u" (lambda () (interactive) (magit-show-item-or-scroll 'View-scroll-half-page-backward))
   "d" (lambda () (interactive) (magit-show-item-or-scroll 'View-scroll-half-page-forward))))

(add-hook 'magit-status-mode-hook 'init-magit-status-mode-keybindings)
(defun init-magit-status-mode-keybindings ()
  ;; NOTE(philc): using `evil-define-key` for these keymaps stopped working upon upgrading to Emacs 24.4.
  (util/define-keys
   evil-normal-state-local-map
   ";grc" 'git-rebase-continue
   ";gca" 'magit-commit-amend
   ";gpush" 'git-push
   ";gpull" 'git-pull
   "c" 'magit-commit
   ;; I have a git precommit hook which does style checks. Sometimes I want to disable it when committing.
   "C" (lambda() (interactive) (util/with-env-var "SKIP_GIT_STYLE_CHECK" "true" 'magit-commit))
   "e" 'magit-show-level-4-all ; e for exapnd
   "d" 'magit-discard-item
   "s" 'magit-stage-item
   "S" (lambda () (interactive) (util/without-confirmation 'magit-stage-all))
   "d" 'magit-discard-item
   "u" 'magit-unstage-item
   "U" (lambda () (interactive (util/without-confirmation 'magit-unstage-all)))
   (kbd "SPC") 'magit-goto-previous-section
   "-" 'magit-diff-smaller-hunks
   "+" 'magit-diff-larger-hunks
   "gu" 'magit-jump-to-unstaged
   (kbd "TAB") 'magit-toggle-section
   "r" 'magit-refresh))

(add-hook 'git-commit-mode-hook 'init-git-commit-mode)
(defun init-git-commit-mode ()
  ;; Sometimes I have a git hook which prepopulates a commit message prefix for me. Move to the end of the
  ;; line so I can begin typing after the prefix.
  (end-of-line))

(add-hook 'magit-commit-mode-hook 'init-magit-commit-mode-keybindings)
(defun init-magit-commit-mode-keybindings ()
  ;; I'm specifying these keys here, becuase for some reason they get overridden by the yy shortcut I've
  ;; defined for magit-log-mode.
  (util/define-keys evil-normal-state-local-map
    "yy" 'evil-yank-line)
  (util/define-keys evil-visual-state-local-map
    "y" 'evil-yank))

;; Cache the buffer which was showing before we invoked magit.  In some cases magit doesn't properly restore
;; the buffer when you type "q", so we forcefully do it here ourselves.
(setq previous-buffer-under-magit nil)

(defadvice magit-mode-display-buffer (before cache-buffer-behind-magit activate)
  (when (not (string/starts-with (buffer-name) "*magit"))
    (setq previous-buffer-under-magit (current-buffer))))

(defadvice magit-mode-quit-window (after restore-buffer-behind-magit activate)
  (when previous-buffer-under-magit
    (switch-to-buffer previous-buffer-under-magit)
    (setq previous-buffer-under-magit nil)))

(defun with-magit-output-buffer (f)
  "Displays the magit output buffer before invoking the given function"
  (lexical-let ((f f))
    (util/preserve-selected-window
     (lambda ()
       (funcall f)
       (magit-process)))))

(defun git-pull ()
  (interactive)
  (with-magit-output-buffer 'magit-pull))

(defun git-push ()
  (interactive)
  (with-magit-output-buffer 'magit-push))

(defun git-full-commit-id (commit-id)
  "Asks magit for the git directory of the current buffer and translates the abbreviated git commit ID to a
  full one."
  (let ((git-dir (magit-git-dir)))
    (-> (concat "cd " git-dir "; git log --oneline --no-abbrev-commit -n1 " commit-id)
        shell-command-to-string
        split-string
        first)))

(defun magit-copy-full-commit-id ()
  (interactive)
  (magit-copy-item-as-kill) ; Copies the commit ID of the commit under the cursor.
  (-> (substring-no-properties (car kill-ring))
      git-full-commit-id
      (kill-new t))) ; Replace the existing entry in the kill ring

(defun magit-status-and-focus-unstaged ()
  "Opens the magit-status view and focuses the cursor on the first unstaged file."
  (interactive)
  (call-interactively 'magit-status)
  (magit-jump-to-unstaged)
  (magit-goto-next-section))
