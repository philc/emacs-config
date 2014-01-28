;;
;; Package management
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(ace-jump-mode ; Jump to any text on screen in a few keystrokes. Like Vim's EasyMotion.
                      autopair ; Insert matching delimiters, e.g. insert closing braces.
                      cider ; repl for Clojure code evaluation.
                      clojure-mode
                      clojure-test-mode
                      coffee-mode ; For syntax highlighting coffeescript.
                      dired-details+ ; Hides all of the unnecessary file details in dired mode.
                      diminish ; For hiding and shortening minor modes in the modeline
                      evil
                      evil-leader
                      evil-nerd-commenter
                      flx-ido ; Fuzzy matching for ido, which improves the UX of Projectile.
                      go-mode ; For editing Go files.
                      less-css-mode ; Syntax highlighting for LESS CSS files.
                      ido-ubiquitous ; Make ido completions work everywhere.
                      ido-vertical-mode ; Show ido results vertically.
                      magit
                      markdown-mode
                      midje-mode ; For editing clojure tests
                      multi-term ; Display many termianls inside emacs, not just one.
                      org ; For outlining. This is bundled with Emacs, but I'm using the latest version.
                      powerline ; Improve the appearance & density of the Emacs status bar.
                      projectile ; Find file in project (ala CTRL-P).
                      rainbow-delimiters ; Highlight parentheses in rainbow colors.
                      ruby-electric ; Insert matching delimiters; unindent end blocks after you type them.
                      scss-mode
                      yaml-mode
                      yasnippet
                      zoom-frm))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;
;; General
;;
(require 'cl)
(add-to-list 'load-path "~/.emacs.d/")
(require 'lisp-helpers-personal)

;; Anecdotally, this reduces the amount of display flicker on some Emacs startup.
(setq redisplay-dont-pause t)

;; Turn off graphical toolbars.
(if (display-graphic-p) (menu-bar-mode 1) (menu-bar-mode -1))
(when (and (fboundp 'tool-bar-mode) tool-bar-mode) (tool-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) scroll-bar-mode) (scroll-bar-mode -1))

(setq initial-scratch-message "") ; When opening a new buffer, don't show the scratch message.

;; Use the same PATH variable as your shell does. From http://clojure-doc.org/articles/tutorials/emacs.html
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(global-auto-revert-mode 1) ; Reload an open file from disk if it is changed outside of Emacs.

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)
(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'meta)

;; Require typing only "y" or"n" instead of the full "yes" to confirm destructive actions.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Turn off backups and autosaves so we don't have ~ and # files strewn about the working directory. I've
;; tried storing backups in my home directory as suggested by http://stackoverflow.com/q/151945/46237, but
;; still I see the occasional backup file.
(setq make-backup-files nil)
(setq auto-save-default nil)

(setq vc-follow-symlinks t) ; Don't ask confirmation to follow symlinks to edit files.

;; NOTE(philc): Disabling savehist-mode in an attempt to isolate a periodic Emacs segfault.
;; (savehist-mode t) ; Save your minibuffer history across Emacs sessions. UX win!

(setq text-scale-mode-step 1.1) ;; When changing font size, change in small increments.

;; Include path information in duplicate buffer names (e.g. a/foo.txt b/foo.txt)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Start scrolling the window when the cursor reaches its edge.
;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq
  scroll-margin 7
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1
  ; Make touchpad scrolling on OSX less jerky
  mouse-wheel-scroll-amount '(0.01))

;; The preference file for Emac's "Customize" system. `M-x customize` to access it.
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file t)

;; Colorscheme
(load-theme 'tangotango t) ; A reasonable color scheme which lives in my .emacs.d.
; A font face size of 160 can show 110 chars before wrapping on a 1920x1200 resolution.
(set-face-attribute 'default nil :height 160)

;; Whitespace & line wrapping.
(global-whitespace-mode t)
(eval-after-load 'whitespace
  '(progn
     (setq whitespace-line-column 110) ; When text flows past 110 chars, highlight it.
     ; whitespace-mode by default highlights all whitespace. Show only tabs and trailing spaces.
     (setq whitespace-style '(face trailing lines-tail))))
;; NOTE(philc): Flip the following two settings for editing snippets
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (setq-default mode-require-final-newline nil)
(setq-default tab-width 2)
(setq-default evil-shift-width 2)
; Some modes have their own tab-width variables.
(setq-default css-indent-offset 2)

(setq-default fill-column 110) ; When wrapping with the Emacs fill commands, wrap at 110 chars.
(auto-fill-mode t) ; When typing across the fill-column, hard-wrap the line as you type.
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; Some modes, like markdown, turn off autofill. Force it!
;; Visually wrap long lines on word boundaries. By default, Emacs will wrap mid-word. Note that Evil doesn't
;; have good support for moving between visual lines versus logical lines. Here's the start of a solution:
;; https://lists.ourproject.org/pipermail/implementations-list/2011-December/001430.html
(global-visual-line-mode t)

;; Highlight the line the cursor is on. This is mostly to make it easier to tell which split is active.
(global-hl-line-mode)

;; Don't use tabs by default. Modes that really need tabs should enable indent-tabs-mode explicitly.
;; Makefile-mode already does that, for example. If indent-tabs-mode is off, untabify before saving.
(setq-default indent-tabs-mode nil)
(add-hook 'write-file-hooks
          (lambda ()
            (if (not indent-tabs-mode)
                (untabify (point-min) (point-max)))
            nil))

;; Enable the common Bash text-editing shortcuts in the minibuffer.
(define-key minibuffer-local-map (kbd "C-k") 'kill-line)
(define-key minibuffer-local-map (kbd "C-e") 'end-of-line)
(define-key minibuffer-local-map (kbd "C-u") 'backward-kill-line)
(define-key minibuffer-local-map (kbd "C-d") 'delete-char)
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-h") 'backward-delete-char)

;; RecentF mode is the Emacs minor mode used when opening files via C-x C-f.
(require 'recentf)
(define-key recentf-mode-map (kbd "C-w") 'backward-kill-word)
(define-key recentf-mode-map (kbd "C-h") 'backward-delete-char)

;; Settings for window splits.
(setq split-height-threshold 40)
(setq split-width-threshold 200)
(setq split-window-preferred-function 'split-window-sensibly-reverse)
;; Ensure these open in the selected window, not a new popup.
(setq same-window-buffer-names '("*magit-log*"))

;; I manage my windows in a 4x4 grid. I want ephemeral or status-based buffers to always show in the
;; lower-right or right window, in that order of preference.
(setq special-display-buffer-names '("*Help*" "*compilation*" "COMMIT_EDITMSG" "*Messages*"
                                     "*magit-process*" "*magit-commit*" "*Compile-Log*" "*Gofmt Errors*"))
(setq special-display-regexps '("*cider.*"))
(setq special-display-function 'show-ephemeral-buffer-in-a-sensible-window)

;; A list of "special" (ephemeral) buffer names which should be focused after they are shown. Used by
;; show-ephemeral-buffer-in-a-sensible-window
(setq special-display-auto-focused-buffers '())

;; References, for context:
;; http://snarfed.org/emacs_special-display-function_prefer-other-visible-frame
;; http://stackoverflow.com/questions/1002091/how-to-force-emacs-not-to-display-buffer-in-a-specific-window
;; The implementation of this function is based on `special-display-popup-frame` in window.el.
(defun show-ephemeral-buffer-in-a-sensible-window (buffer &optional buffer-data)
  "Given a buffer, shows the window in a right-side split."
  (let* ((original-window (selected-window))
         (create-new-window (one-window-p))
         (window (if create-new-window
                     (split-window-sensibly-reverse)
                   (save-excursion (switch-to-lower-right) (selected-window)))))
    (display-buffer-record-window (if create-new-window 'window 'reuse) window buffer)
    (set-window-buffer window buffer)
    (when create-new-window (set-window-prev-buffers window nil))
    (select-window original-window)
    (when (member (buffer-name buffer) special-display-auto-focused-buffers)
      (select-window window))
    window))

(defun dismiss-ephemeral-windows ()
  "Dismisses any visible windows in the current frame identifiedy by `special-display-buffer-names` and
   `special-display-regexps`. I use this to quickly dismiss help windows, compile output, etc."
  (interactive)
  (save-excursion
    (let ((original-window (selected-window)))
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (when (special-display-p (buffer-name buffer))
            (quit-window nil window))))
      (select-window original-window))))

(defun preserve-selected-window (f)
  "Runs the given function and then restores focus to the original window. Useful when you want to invoke
   a function (like showing documentation) but don't want to keep editing your current buffer."
  (lexical-let ((f f))
    (let ((original-window (selected-window)))
      (funcall f)
      (select-window original-window))))

;; The poorly-named winner mode saves the history of your window splits, so you can undo and redo changes to
;; your window configuration.
(winner-mode t)

;;
;; Evil mode -- Vim keybindings for Emacs.
;;
(setq evil-want-C-u-scroll t)
(require 'evil-leader) ; Provide configuration functions for assigning actions to a Vim leader key.
(require 'evil)
(require 'evil-nerd-commenter)
(global-evil-leader-mode t)
(evil-mode t)
;; Note that there is a bug where Evil-leader isn't properly bound to the initial buffers Emacs opens
;; with. We work around this by killing them. See https://github.com/cofi/evil-leader/issues/10.
(kill-buffer "*Messages*")

;; When opening new lines, indent according to the previous line.
(setq evil-auto-indent t)

;; Unbind "q" so it doesn't record macros. I activate this mistakenly all the time and wreak havoc.
(define-key evil-normal-state-map (kbd "q") nil)
(define-key evil-normal-state-map (kbd "M-s") 'save-buffer)
(define-key evil-insert-state-map (kbd "M-s") 'save-buffer)


;; Move up and down through long, wrapped lines one visual line at a time.
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "K") 'info-lookup-symbol)
;; I use this shortcut for manually splitting lines. Note that it does not put you in insert mode.
(define-key evil-normal-state-map (kbd "RET") 'newline-and-indent)

;; By default, Emacs will not indent when you hit enter/return within a comment.
(define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)

;; When jumping back and forth between marks, recenter the screen on the cursor.
(define-key evil-normal-state-map (kbd "C-o")
  (lambda () (interactive) (evil-jump-backward) (recenter-no-redraw)))
(define-key evil-normal-state-map (kbd "C-i")
  (lambda () (interactive) (evil-jump-forward) (recenter-no-redraw)))


; Some help keybindings which conflict with nothing else, so you can pull up help in any context.
(global-set-key (kbd "C-A-M-h") 'help) ; Here we clobber C-h, which accesses Emacs's help.
(global-set-key (kbd "C-A-M-b") 'describe-bindings)

;; gq is normally bound to evil-fill-and-move, but when I reflow a paragraph, I like the cursor to remain
;; where it was.
(define-key evil-normal-state-map "gq" 'evil-fill)
(define-key evil-normal-state-map "-" 'evil-indent-without-move)

(evil-leader/set-key
  "h" 'help
  "b" 'ido-switch-buffer
  "t" 'projectile-find-file
  "q" 'evil-fill-inside-paragraph ; Shortcut for Vim's gqip
  "i" 'evil-indent-inside-paragraph ; Shortcut to Vim's =ip
  "a" 'projectile-ack
  "d" 'projectile-dired
  "D" (lambda () (interactive) (-> (buffer-file-name) file-name-directory dired))
  "vt" 'multi-term
  ;; "v" is a mnemonic prefix for "view X".
  "gs" 'magit-status
  "gl" 'magit-log
  "vu" 'mu4e
  "vp" 'open-root-of-project-in-dired
  "vn" 'open-markdown-file-from-notes-folder
  "vo" (lambda () (interactive) (find-file "~/Dropbox/tasks.org"))
  "ve" (lambda () (interactive) (find-file "~/.emacs")))

(setq evil-leader/leader ";")

;; Ensure we evil-leader works in non-editing modes like magit. This is referenced from evil-leader's README.
(setq evil-leader/no-prefix-mode-rx '("magit-.*-mode"))

(defun prompt-to-open-info-page ()
  "Prompts you for the name of an info page to view. It's the same as calling info with a prefix argument
   ala C-u C-h i using the regular Emacs key bindings."
  (interactive)
  (setq current-prefix-arg '(4)) ; C-u
  (call-interactively 'info))

(defun evil-fill-inside-paragraph ()
  "Fills (reflows/linewraps) the current paragraph. Equivalent to gqap in vim."
  (interactive)
  (let ((region (if (use-region-p)
                  (list (region-beginning) (region-end))
                  (evil-inner-paragraph))))
    (evil-fill (first region) (second region))))

(defun evil-indent-inside-paragraph ()
  "Fills (reflows/linewraps) the current paragraph. Equivalent to gqap in vim."
  (interactive)
  (let ((region (if (use-region-p)
                  (list (region-beginning) (region-end))
                  (evil-inner-paragraph))))
    (evil-indent-without-move (first region) (second region))))

(defun evil-shift-paragraph-left (beg end)
  "Shifts a paragraph left."
  (interactive "r")
  (let ((region (evil-inner-paragraph)))
    (save-excursion
      (evil-shift-left (first region) (second region)))))

(defun evil-shift-paragraph-right (beg end)
  "Shifts a paragraph right."
  (interactive "r")
  (let ((region (evil-inner-paragraph)))
    (save-excursion
      (evil-shift-right (first region) (second region)))))

(defun eval-surrounding-sexp (levels)
  (interactive "p")
  (save-excursion
    (up-list (abs levels))
    (eval-last-sexp nil)))

(defun backward-kill-line (arg)
  "Delete backward (Ctrl-u) as in Bash."
  (interactive "p")
  (kill-line (- 1 arg)))

(evil-define-operator evil-indent-without-move (beg end)
  "Indent text."
  :move-point nil
  :type line
  (save-excursion
    (evil-indent beg end)))

;; Enable Emacs/Bash insert-mode keybindings.
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-u") 'backward-kill-line)
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
(global-set-key (kbd "C-h") 'backward-delete-char) ; Here we clobber C-h, which accesses Emacs's help.

;; Commenting via NERD commentor.
(define-key evil-normal-state-map "," 'evilnc-comment-operator)
(define-key evil-visual-state-map "," 'evilnc-comment-operator)

;; Window manipulation, switching, & management.
;; Evil's window map is the set of keys which control window functions. All of its keys are prefixed with
;; <C-w>.
(evil-leader/set-key "wn" 'create-window-in-next-logical-spot)
(evil-leader/set-key "wv" 'split-window-horizontally-and-focus)
(evil-leader/set-key "ws" 'split-window-vertically-and-focus)
(evil-leader/set-key "wk" (lambda () (interactive) (kill-buffer (current-buffer))))
(evil-leader/set-key "wm" 'toggle-window-maximize)
(evil-leader/set-key "wr" 'evil-window-rotate-downwards)
(evil-leader/set-key "wR" 'evil-window-rotate-upwards)
;; Undo the last change you made to your window configuration.
(evil-leader/set-key "wb" 'winner-undo)
(evil-leader/set-key "SPC" 'dismiss-ephemeral-windows)

(defun toggle-window-maximize ()
  (interactive)
  (if (= 1 (length (window-list)))
      ;; winner-undo undoes the last change you made to the state of your widnows.
      ;; This isn't an exact inverse of "delete-other-windows", but it works OK for me in practice.
      (winner-undo)
      (delete-other-windows)))

(defun split-window-vertically-and-focus ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun split-window-horizontally-and-focus ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun switch-to-upper-left () (interactive) (select-window (frame-first-window)))
(defun switch-to-lower-left () (interactive) (switch-to-upper-left) (ignore-errors (windmove-down)))
(defun switch-to-upper-right () (interactive) (switch-to-upper-left) (ignore-errors (windmove-right 1)))
(defun switch-to-lower-right () (interactive) (switch-to-upper-right) (ignore-errors (windmove-down)))

(defun switch-to-nth-window (n)
  (lexical-let ((window (frame-first-window)))
    (dotimes (i n nil)
      (when window
        (setq window (next-window window))))
    (when window
      (select-window window))))

(defun create-window-in-next-logical-spot ()
  "Creates a window in the next slot in my standard 2x2 configuration. So for instance, if I have only 1
   window open, it will split the screen into two vertical windows."
  (interactive)
  (let ((window-count (length (window-list)))
        (buffer (current-buffer)))
    (case window-count
      (1 (split-window-horizontally-and-focus))
      (2 (progn
           (switch-to-nth-window 1)
           (split-window-vertically-and-focus)))
      (3 (progn
           (switch-to-nth-window 0)
           (split-window-vertically-and-focus))))
    ;; Ensure that no matter where the window is created, it's has the same buffer as the window prior to
    ;; creating the new one. Otherwise, the new window could have some random buffer in it, making it
    ;; difficult to use commands like open-in-project, for instance.
    (set-window-buffer (selected-window) buffer)))

(defadvice windmove-do-window-select (after windowmove-change-to-normal-mode)
  "Ensure we reset to Evil's normal mode when switching windows."
  (evil-change-to-initial-state))
(ad-activate 'windmove-do-window-select)

(defun split-window-sensibly-reverse (&optional window)
  "Identical to the built-in function split-window-sensibly, but prefers horizontal splits over vertical."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
       ;; Split window horizontally.
       (with-selected-window window
         (split-window-right)))
  (and (window-splittable-p window)
       ;; Split window vertically.(column-marker-1 80)
       (with-selected-window window
         (split-window-below)))
  (and (eq window (frame-root-window (window-frame window)))
       (not (window-minibuffer-p window))
       ;; If WINDOW is the only window on its frame and is not the
       ;; minibuffer window, try to split it vertically disregarding
       ;; the value of `split-height-threshold'.
       (let ((split-height-threshold 0))
         (when (window-splittable-p window)
     (with-selected-window window
       (split-window-below))))))))

;; Taken from http://www.emacswiki.org/emacs/misc-cmds.el.
(defun kill-buffer-and-its-windows (buffer)
  "Kill BUFFER and delete its windows. Default is `current-buffer'. BUFFER may be either a buffer or its name"
  (interactive (list (read-buffer "Kill buffer: " (current-buffer) 'existing)))
  (setq buffer (get-buffer buffer))
  (if (buffer-live-p buffer)
      (let ((windows (get-buffer-window-list buffer nil t)))
        (when (kill-buffer buffer)
          (dolist (window windows)
            (when (window-live-p window)
              ;; Ignore error, in particular,;; "Attempt to delete the sole visible or iconified frame".
              (condition-case nil (delete-window window) (error nil))))))
    (when (interactive-p)
      (error "Cannot kill buffer.  Not a live buffer: `%s'" buffer))))

;; Save buffers whenever they lose focus.
;; This obviates the need to hit the Save key thousands of times a day. Inspired by http://goo.gl/2z0g5O.
(defun save-buffer-if-dirty ()
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer)))

(defadvice switch-to-buffer (before save-buffer-now activate) (save-buffer-if-dirty))
(defadvice other-window (before other-window-now activate) (save-buffer-if-dirty))
(defadvice windmove-up (before other-window-now activate) (save-buffer-if-dirty))
(defadvice windmove-down (before other-window-now activate) (save-buffer-if-dirty))
(defadvice windmove-left (before other-window-now activate) (save-buffer-if-dirty))
(defadvice windmove-right (before other-window-now activate) (save-buffer-if-dirty))
;; This hasn't been a problem yet, but advising "select-window" may cause problems. For instance, it's called
;; every time a character is typed in isearch mode.
(defadvice select-window (before select-window activate) (save-buffer-if-dirty))

;; Make it so Esc means quit, no matter the context.
;; http://stackoverflow.com/a/10166400/46237
;; Note that when Emacs becomes unresponsive (e.g. because I accidentally grepped my home directory), I might
;; still need to hold C-g (the Emacs esc/cancel key) to bring it back.
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is active, just deactivate it;
   then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;;
;; Ace jump - for quickly jumping to a precise character in view. Similar to Vim's EasyMotion.
;;
(require 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-word-mode)
;; Note that Evil mode's ace-jump integration is supposed to add this motion keybinding automatically for you,
;; but it doesn't work. So I've defined it here explicitly.
(define-key evil-motion-state-map (kbd "SPC") 'evil-ace-jump-word-mode)

;;
;; Incremental search (isearch)
;;
;; Make highlighting during incremental search feel snappier.
(setq case-fold-search t) ; Make searches case insensitive.
(setq lazy-highlight-initial-delay 0)
(setq lazy-highlight-max-at-a-time nil)
;; Hitting esacpe aborts the search, restoring your cursor to the original position, as it does in Vim.
(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
;; Make C-h act the same as backspace.
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
;; Make M-v paste the clipboard's text into the search ring.
(define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)
(define-key isearch-mode-map (kbd "C-w") 'isearch-del-word)

(defun trim-last-word-of-string (string)
  "Removes the last word from the given string. Word separators are -, _ and spaces. This is designed to
  perform the same function as kill-word, but on a string argument."
  (lexical-let ((i 0))
    (while (and (< i (length string))
                (string-match "[-_ ]+" string i))
      (setq i (second (match-data))))
    (if (= i 0)
      ""
      (substring string 0 (dec i)))))

(defun isearch-del-word (&optional arg)
  "Delete word from end of search string and search again. If search string is empty, just beep.
  This function definition is based on isearch-del-char, from isearch.el."
  (interactive "p")
  (if (= 0 (length isearch-string))
    (ding)
    (setq isearch-string (trim-last-word-of-string isearch-string)
          isearch-message (mapconcat 'isearch-text-char-description
                                     isearch-string "")))
  ;; Use the isearch-other-end as new starting point to be able
  ;; to find the remaining part of the search string again.
  (when isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

;; When pressing enter to confirm a search, or jumping to the next result, scroll the result to the center of
;; the window. This solves the UX problem of the result appearing at the bottom of the screen, with little
;; context.
(defadvice evil-search-next (after isearch-recenter activate)
  (recenter-no-redraw))

(defadvice evil-search-previous (after isearch-recenter activate)
  (recenter-no-redraw))

(defadvice isearch-exit (before isearch-recenter activate)
  (recenter-no-redraw))

;; Taken from https://groups.google.com/forum/#!topic/gnu.emacs.help/vASrP0P-tXM
(defun recenter-no-redraw (&optional arg)
  "Centers the viewport around the cursor."
  (interactive "P")
  (let ((recenter-redisplay nil))
    (recenter arg)))

;;
;; Mac OS X keybindings minor mode.
;; Make it so the OSX keybindings you're used to always work.
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;;
(defvar osx-keys-minor-mode-map (make-keymap) "osx-keys-minor-mode-keymap")
(define-key osx-keys-minor-mode-map (kbd "M-`") 'other-frame)
(define-key osx-keys-minor-mode-map (kbd "M-~")
  '(lambda () (interactive) (other-frame -1)))
(define-key osx-keys-minor-mode-map (kbd "M-w") 'vimlike-quit)
(define-key osx-keys-minor-mode-map (kbd "M-q") 'save-buffers-kill-terminal)
(define-key osx-keys-minor-mode-map (kbd "M-n") 'new-frame)
(define-key osx-keys-minor-mode-map (kbd "M-a") 'mark-whole-buffer)
(define-key osx-keys-minor-mode-map (kbd "M-h") 'ns-do-hide-emacs)
(define-key osx-keys-minor-mode-map (kbd "M-v") 'clipboard-yank)
(define-key osx-keys-minor-mode-map (kbd "M-c") 'clipboard-kill-ring-save)
(define-key osx-keys-minor-mode-map (kbd "M-m") 'iconify-or-deiconify-frame)
(define-key osx-keys-minor-mode-map (kbd "M-W") 'evil-quit) ; Close all tabs in the current frame..
(require 'zoom-frm)
(define-key osx-keys-minor-mode-map (kbd "M--") 'zoom-out)
(define-key osx-keys-minor-mode-map (kbd "M-=") 'zoom-in)
(define-key osx-keys-minor-mode-map (kbd "M-0") 'zoom-frm-unzoom)

;; These aren't specifically replicating OSX shortcuts, but they manipulate the window, so I want them to take
;; precedence over everything else.
(define-key osx-keys-minor-mode-map (kbd "M-C-n") 'other-window)
;; Note that I have Ctrl-Space mapped to Alt, which makes these shortcuts easy to hit.
(define-key osx-keys-minor-mode-map (kbd "A-e") 'switch-to-upper-left)
(define-key osx-keys-minor-mode-map (kbd "A-d") 'switch-to-lower-left)
(define-key osx-keys-minor-mode-map (kbd "A-r") 'switch-to-upper-right)
(define-key osx-keys-minor-mode-map (kbd "A-f") 'switch-to-lower-right)
(define-key osx-keys-minor-mode-map (kbd "M-1") (lambda () (interactive) (switch-to-nth-window 0)))
(define-key osx-keys-minor-mode-map (kbd "M-2") (lambda () (interactive) (switch-to-nth-window 1)))
(define-key osx-keys-minor-mode-map (kbd "M-3") (lambda () (interactive) (switch-to-nth-window 2)))
(define-key osx-keys-minor-mode-map (kbd "M-4") (lambda () (interactive) (switch-to-nth-window 3)))

(define-minor-mode osx-keys-minor-mode
  "A minor-mode for emulating osx keyboard shortcuts."
  t " osx" osx-keys-minor-mode-map)

(osx-keys-minor-mode t)

(defadvice load (after give-osx-keybindings-priority)
  "Try to ensure that osx keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'osx-keys-minor-mode))
      (let ((osx-keys (assq 'osx-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'osx-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist osx-keys))))
(ad-activate 'load)

; Closes the current elscreen, or if there's only one screen, use the ":q" Evil
; command. This simulates the ":q" behavior of Vim when used with tabs.
; http://zuttobenkyou.wordpress.com/2012/06/15/emacs-vimlike-tabwindow-navigation/
(defun vimlike-quit ()
  "Vimlike ':q' behavior: close current window if there are split windows;
   otherwise, close current tab (elscreen)."
  (interactive)
  (let ((one-elscreen (elscreen-one-screen-p))
        (one-window (one-window-p)))
    (cond
     ; if current tab has split windows in it, close the current live window
     ((not one-window)
      (delete-window) ; delete the current window
      (balance-windows) ; balance remaining windows
      nil)
     ; if there are multiple elscreens (tabs), close the current elscreen
     ((not one-elscreen)
      (elscreen-kill)
      nil)
     ; if there is only one elscreen, just try to quit (calling elscreen-kill
     ; will not work, because elscreen-kill fails if there is only one
     ; elscreen)
     (one-elscreen
      (evil-quit)
      nil))))

;;
;; Filename completions (CTRL-P / CMD+T)
;;
(ido-mode t)
(ido-ubiquitous-mode t)
(ido-vertical-mode t)
(eval-after-load 'ido
  '(progn
     (setq ido-enable-flex-matching t)
     (setq ido-use-virtual-buffers t)
     (setq ido-everywhere t)))

;;
;; Dired mode - using the Emacs file browser.
;;
(require 'dired-details+)
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

(put 'dired-find-alternate-file 'disabled nil) ; By default, the dired-find-alternative-file fn is disabled.

;; "go to dired, then call split-window-vertically, then go to another dired dir. Now, when you press C to
;; copy, the other dir in the split pane will be default destination. Same for R (rename; move)."
(setq dired-dwim-target t)

;; Use the same buffer for going into and up directories.
(evil-define-key 'normal dired-mode-map (kbd "gu") (lambda () (interactive) (find-alternate-file "..")))
(evil-define-key 'normal dired-mode-map "H" (lambda () (interactive) (find-alternate-file "..")))
(evil-define-key 'normal dired-mode-map (kbd "<return>")
  'dired-find-alternate-file) ; was dired-advertised-find-file

;; dired overrides my global "other window" shorcut.
(evil-define-key 'normal dired-mode-map (kbd "M-C-n") 'other-window)
(evil-define-key 'normal dired-mode-map ";" nil) ; Ensure my evil-leader key works unhindered.
(evil-define-key 'normal dired-mode-map (kbd "M-C-n") 'other-window)
(evil-define-key 'normal dired-mode-map "cd" 'dired-create-directory)
(evil-define-key 'normal dired-mode-map "cf" 'dired-create-file)
(evil-define-key 'normal dired-mode-map "x" 'dired-mark)
(evil-define-key 'normal dired-mode-map "v" 'dired-details-toggle)
;; The "e" prefix is for execute.
(evil-define-key 'normal dired-mode-map "ed" 'dired-do-flagged-delete)
(evil-define-key 'normal dired-mode-map "em" 'dired-do-rename)

;; Taken from http://stackoverflow.com/a/18885461/46237.
(defun dired-create-file (file)
  "Create a file called FILE, and recursively create any parent directories.
  If FILE already exists, signal an error."
  (interactive
   (list (read-file-name "Create file: " (dired-current-directory))))
  (let* ((expanded (expand-file-name file))
         (try expanded)
         (dir (directory-file-name (file-name-directory expanded)))
         new)
    (if (file-exists-p expanded)
        (error "Cannot create file %s: file exists" expanded))
    ;; Find the topmost nonexistent parent dir (variable `new')
    (while (and try (not (file-exists-p try)) (not (equal new try)))
      (setq new try
            try (directory-file-name (file-name-directory try))))
    (when (not (file-exists-p dir))
      (make-directory dir t))
    (write-region "" nil expanded t)
    (when new
      (dired-add-file new)
      (dired-move-to-filename))))

;;
;; Terminal (multi-term mode)
;; This setup is a work-in-progress. I'm still exploring whether this is a viable terminal replacement. It's
;; very klunky out of the box.
;;
(require 'multi-term)
(setq multi-term-buffer-name "term"
      multi-term-program "/bin/zsh"
      multi-term-program-switches "--login")
(defun init-term-settings ()
  ;; Normally we maintain a margin of N lines between the cursor and the edge of the window, but in term mode,
  ;; the cursor should always be at the bottom of the window.
  (setq-local scroll-margin 0)
  (setq-local scroll-conservatively 0)
  (setq-local scroll-step 1))

;; The default Evil state for terminals should be ready to accept commands.
(evil-set-initial-state 'term-mode 'emacs)

;; (setq scroll-margin 1)
;; (setq scroll-conservatively 0)

(add-hook 'term-mode-hook 'init-term-settings)
(add-hook 'term-mode-hook (lambda() (setq-local yas-dont-activate t)))

;; (evil-define-key 'normal term-raw-map "i" 'evil-emacs-state)
;; (evil-define-key 'normal term-raw-map (kbd "RET")
;;   (lambda ()
;;     (interactive)
;;     (term-send-input)))

;; What you want to do here is add mode-specific advice. Override evil-insert-state and in it,
;; call evil-eamcs-state if not evil-emacs-state-p
(evil-define-key 'normal term-raw-map "i"
  (lambda ()
    (interactive)
    (evil-emacs-state)))
(evil-define-key 'normal term-raw-map "a"
  (lambda ()
    (interactive)
    (evil-emacs-state)))
(evil-define-key 'emacs term-raw-map (kbd "C-y")
  (lambda ()
    (interactive)
    (term-send-raw-string "\e")))

(evil-define-key 'emacs term-raw-map (kbd "C-p") '(lambda () (interactive) (term-send-raw-string "\C-p")))
(evil-define-key 'emacs term-raw-map (kbd "C-n") '(lambda () (interactive) (term-send-raw-string "\C-n")))

;; Define the blue used by the emacs term to be readable for dark backgrounds.
(defface term-color-blue
  '((t :foreground "#1F93F3" :background "#1F93F3"))
  "Face used to render blue color code."
  :group 'term)

;;
;; Org mode, for TODOs and note taking.
;;
(require 'org-mode-personal)

;;
;; Projectile (find file from the root of the current project).
;;
(projectile-global-mode)

;;
;; elscreen (tabs on the window).
;;
(elscreen-start)
(global-set-key (kbd "<A-M-left>") 'elscreen-previous) ; KeyRemap4Macbook translates M-J to these keys.
(global-set-key (kbd "<A-M-right>") 'elscreen-next) ; KeyRemap4Macbook translates M-J to these keys.

(define-key evil-normal-state-map (kbd "M-t") 'open-current-buffer-in-new-tab)
(define-key evil-insert-state-map (kbd "M-t") 'open-current-buffer-in-new-tab)

(defun open-current-buffer-in-new-tab ()
  (interactive)
  ;; Exit out of insert mode when opening a new tab.
  (evil-change-to-initial-state)
  ;; I'm using elscreen-clone here instead of elscreen-create so that the new tab has the current directory set
  ;; properly, so projectile can be used immediately.
  (elscreen-clone)
  (delete-other-windows))

;;
;; Markdown
;;
(defun markdown-insert-list-item-below ()
  "Inserts a new list item under the current one. markdown-insert-list-item inserts above, by default."
  (interactive)
  (end-of-line)
  (call-interactively 'markdown-insert-list-item)
  (evil-append nil))

(defun insert-markdown-header (header-line-text)
  "With the cursor focused on the header's text, insert a setext header line below that text.
   header-line-text: either '===' or '---'"
  (end-of-line)
  (insert (concat "\n" header-line-text))
  (markdown-complete)
  ;; markdown-complete inserts a newline after the header. Remove it and move the cursor to a logical place.
  (next-line)
  (next-line)
  (delete-backward-char 1)
  (next-line)
  (beginning-of-line))

(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

(eval-after-load 'markdown-mode
  '(progn
     (evil-leader/set-key-for-mode 'markdown-mode
        "r" 'markdown-cleanup-list-numbers)
     (evil-define-key 'normal markdown-mode-map
       ;; Autocomplete setext headers by typing "==" or "--" on the header's line in normal mode.
       (kbd "==") '(lambda () (interactive) (insert-markdown-header "=="))
       (kbd "--") '(lambda () (interactive) (insert-markdown-header "--"))
       (kbd "C-S-L") 'evil-shift-paragraph-right
       (kbd "C-S-H") 'evil-shift-paragraph-left)
     ;; Note that while in insert mode, using "evil-shift-paragraph-right" while the cursor is at the end of a
     ;; line will shift the paragraph incorrectly. That's why we jump to normal mode first, as a workaround.
     (evil-define-key 'insert markdown-mode-map
       (kbd "C-S-H") '(lambda ()
                        (interactive)
                        (evil-change-to-initial-state)
                        (call-interactively 'evil-shift-paragraph-left)
                        (evil-append nil))
       (kbd "C-S-L") '(lambda ()
                        (interactive)
                        (evil-change-to-initial-state)
                        (call-interactively 'evil-shift-paragraph-right)
                        (evil-append nil)))
     (mapc (lambda (state)
             (evil-define-key state markdown-mode-map
               (kbd "C-S-K") 'markdown-move-up
               (kbd "C-S-J") 'markdown-move-down
               ;; M-return creates a new todo item and enters insert mode.
               (kbd "<C-return>") 'markdown-insert-list-item-below))
           '(normal insert))))

;;
;; Auto save every few seconds.
;;
;; http://www.litchie.net/programs/real-auto-save.html
;; NOTE(philc): I'm turning this off for now because the truncation of whitespace throws me off while
;; typing and messed up markdown syntax highlighting & indentation.
;; (add-to-list 'load-path "~/.emacs.d/plugins/real-auto-save")
;; (require 'real-auto-save)
;; (add-hook 'text-mode-hook 'turn-on-real-auto-save)
;; (setq real-auto-save-interval 10) ;; in seconds

;;
;; Snippets - yassnippet
;;
;; Ignore the default snippets that come with yasnippet. I only need my own, and don't want any conflicts.
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-keymap (kbd "ESC") 'yas-abort-snippet)
;; By default, you can't delete selected text using backspace when tabbing through a snippet.
;; Filed as a bug here: https://github.com/capitaomorte/yasnippet/issues/408
(define-key yas-keymap (kbd "C-h") 'yas-skip-and-clear-or-delete-backward-char)
;; (define-key yas-keymap (kbd "backspace") 'yas-skip-and-clear-or-delete-backward-char)

;; This function is based on yas-skip-and-clear-or-delete-char from yassnippet.el.
(defun yas-skip-and-clear-or-delete-backward-char (&optional field)
  "Clears unmodified field if at field start, skips to next tab. Otherwise deletes backward."
  (interactive)
  (let ((field (or field
                   (and yas--active-field-overlay
                        (overlay-buffer yas--active-field-overlay)
                        (overlay-get yas--active-field-overlay 'yas--field)))))
    (cond ((and field
                (not (yas--field-modified-p field))
                (eq (point) (marker-position (yas--field-start field))))
           (yas--skip-and-clear field)
           (yas-next-field 1))
          (t
           (call-interactively 'delete-backward-char)))))

;;
;; Fill column indicator. Show a vertical bar at the fill column (for me, that's 110 chars).
;;
;; This is pretty broken for me because it does not handle text scaling due to this bug:
;; https://github.com/alpaker/Fill-Column-Indicator/issues/15
;; Even more importantly, activating this mode also disables line wrapping, which I don't want.
;;
;; (add-to-list 'load-path "~/.emacs.d/plugins/fill-column-indicator")
;; (require 'fill-column-indicator)

;;
;; Spell checking
;; http://www.emacswiki.org/emacs/SpeckMode
;; FlySpell is the default choice for spellchecking, but I found it slow, even using every flyspell perf
;; improvement I could find. Speck doesn't slow down your typing.
;;
;; You may need to install aspell (e.g. `brew install aspell`).

(add-to-list 'load-path "~/.emacs.d/plugins/speck-mode")

(require 'speck)
;; This apparently needs to be a fully-qualified path.
(setq speck-personal-dictionary-file "/Users/reformist/.personal_dict.txt")
(setq speck-engine (quote Aspell))
(add-hook 'text-mode-hook 'speck-mode)
 ;; Triggers a spell-correction menu. I use this to add words to my dictionary (hit "i").
(define-key evil-normal-state-map (kbd "zg") 'speck-popup-menu-at-point)

;;
;; Diminish - hide or shorten the names of minor modes in your modeline.
;; To see which minor modes you have loaded and what their modeline strings are: (message minor-mode-alist)
(require 'diminish)
(diminish 'visual-line-mode "")
(diminish 'global-whitespace-mode "")
(diminish 'global-visual-line-mode "")
(diminish 'auto-fill-function "")
(diminish 'projectile-mode " p")
(diminish 'yas-minor-mode "yas")
(diminish 'osx-keys-minor-mode "")
(diminish 'undo-tree-mode "")

;;
;; Powerline: improve the appearance & density of the Emacs status bar (mode line).
;;
(require 'powerline)

(defface powerline-white-face
  '((t (:background "#e0e0e0" :foreground "black" :inherit mode-line)))
    "Face for powerline")
(defface powerline-black-face
  '((t (:background "#191919" :inherit mode-line)))
  "Face for powerline")

(defun powerline-projectile-project-name (&optional face padding)
  "Returns a string describing the projectile project for the current buffer. Takes the same arguments as
   powerline-raw."
  (powerline-raw (concat "(" (projectile-project-name) ")") face padding))

(defun powerline-personal-theme ()
  "My customized powerline, copied and slightly modified from the default theme."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" 'powerline-black-face 'l)
                                     (powerline-buffer-id 'powerline-black-face 'l)
                                     (powerline-raw " " 'powerline-black-face)
                                     (powerline-projectile-project-name 'powerline-black-face 'l)
                                     (powerline-raw " " 'powerline-black-face)
                                     (funcall separator-left mode-line face1)
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     ;; "Version control" - show the modeline of any active VC mode.
                                     (powerline-vc face1 'r)
                                     (powerline-raw "%4l" face1 'l) ; Current line number
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r) ; Current column number
                                     (powerline-raw " " face1)
                                     ;; A visual scrollbar shown inside 1x1 char
                                     (powerline-hud 'powerline-white-face face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)))))))

(powerline-personal-theme)

;;
;; CSS
;;
(add-hook 'css-mode-hook (lambda ()
                           (autopair-mode 1) ; Auto-insert matching delimiters.
                           ;; Properly unindent a closing brace after you type it and hit enter.
                           (eletric-indent-mode)))

;;
;; Coffeescript
;;
(setq coffee-tab-width 2)
(evil-leader/set-key-for-mode 'coffee-mode
  "c" nil ; Establishes "c" as a "prefix key". I found this trick here: http://www.emacswiki.org/emacs/Evil
  "cf" (lambda ()
         (interactive)
         (save-buffer)
         (coffee-compile-file))
  ;; The mnemonic for this is "compile & preview".
  "cp" 'coffee-compile-buffer)

;; Make return and open-line indent the cursor properly.
(evil-define-key 'insert coffee-mode-map (kbd "RET") 'coffee-newline-and-indent)
(evil-define-key 'normal coffee-mode-map "o" '(lambda ()
                                                (interactive)
                                                (end-of-line)
                                                (evil-append nil)
                                                (coffee-newline-and-indent)))

;;
;; Ruby
;;
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

(eval-after-load 'ruby-mode
  '(progn
     ;; Ruby mode defines this as "next block". I define it globally as "next window".
     (define-key ruby-mode-map (kbd "C-M-n") nil)))

;; Insert matching delimiters; unindent end blocks after you type them.
(add-hook 'ruby-mode-hook (lambda () (ruby-electric)))

;;
;; Emacs Lisp (elisp)
;;
(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)))
(evil-define-key 'normal emacs-lisp-mode-map
  "K"'(lambda ()
        (interactive)
        ;; Run `describe-function` and show its output in a help
        ;; window. Inspired from help-fns.el.
        (with-help-window "*Help*"
          (describe-function (intern (current-word))))))

(defun current-sexp ()
  "Returns the text content of the sexp list around the cursor."
  (let ((position (bounds-of-thing-at-point 'list)))
    (buffer-substring-no-properties (car position) (cdr position))))

(defun elisp-eval-current-sexp ()
  (interactive)
  (print (eval (read (current-sexp)))))

(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "eb" (lambda() (interactive) (save-buffer) (eval-buffer))
  "es" 'elisp-eval-current-sexp
  "ex" 'eval-defun
  "ee" 'view-echo-area-messages)

;; Indentation rules.
(put '-> 'lisp-indent-function nil)
(put '->> 'lisp-indent-function nil)

;;
;; Clojure
;;
;; Docs:
;; https://github.com/clojure-emacs/cider
;; http://clojure-doc.org/articles/tutorials/emacs.html

;; Count hyphens, etc. as word characters in lisps
(add-hook 'clojure-mode-hook (lambda () (modify-syntax-entry ?- "w" clojure-mode-syntax-table)))
(add-hook 'clojure-mode-hook (lambda ()
                               (setq indent-line-function 'lisp-indent-line-single-semicolon-fix)
                               ;; Comment lines using only one semi-colon instead of two.
                               (setq comment-add 0)))

(evil-define-key 'normal clojure-mode-map "K"
  (lambda () (interactive) (preserve-selected-window (lambda () (call-interactively 'cider-doc)))))

(evil-define-key 'normal clojure-mode-map "gf" 'cider-jump)

;; Hide the uninteresting nrepl-connection and nrepl-server buffers from the buffer list.
(setq nrepl-hide-special-buffers t)

;; Prevent the auto-display of the REPL buffer in a separate window after connection is established.
(setq cider-repl-pop-to-buffer-on-connect nil)

;; TODO(philc): Try (cider-toggle-pretty-printing)

;; Don't ask confirmation for closing any open nrepl connections when exiting Emacs.
;; http://stackoverflow.com/q/2706527/46237
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(evil-define-operator evil-cider-eval (beg end)
  "Evaluate the text region moved over by an evil motion."
  (cider-eval-region beg end))

;; Eval a paragraph. This is different from eval-surrounding-sexp in that it will eval multiple adjacent
;; s-expressions which are not separated by a new line. It's equivalent to wrapping the expressions in a do.
(defun cider-eval-paragraph (beg end)
  (interactive "r")
  (let ((region (evil-a-paragraph)))
    (evil-cider-eval (first region) (second region))))

(defun cider-show-cider-buffer ()
  (interactive)
  "Shows the nrepl buffer, but does not focus it."
  (command-execute 'cider-switch-to-repl-buffer)
  (command-execute 'cider-switch-to-last-clojure-buffer))

(defun cider-clear-buffer-inside-cider-buffer ()
  (interactive)
  (command-execute 'cider-switch-to-repl-buffer)
  (cider-clear-buffer)
  (command-execute 'cider-switch-to-last-clojure-buffer))

;; Disable the prompt we get when killing a buffer with a process.
(setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(defun cider-restart-nrepl ()
  "Restarts or starts afresh the nrepl."
  (interactive)
  (let ((repl-buffer (nrepl-connection-for-buffer (current-buffer))))
    (flet ((y-or-n-p (&rest args) t)) ; Skip the confirmation prompts.
      (when (not (stringp repl-buffer))
        (nrepl-close repl-buffer))
      (cider-jack-in nil))))

(defun cider-make-connection-buffer-the-current-connection (connection-buffer)
  (cons connection-buffer (delq connection-buffer nrepl-connection-list)))

(defun with-nrepl-connection-of-current-buffer (f)
  (let ((result (nrepl-connection-for-buffer (current-buffer))))
    (if (stringp result)
        (message result)
      (progn
        (cider-make-connection-buffer-the-current-connection result)
        (funcall f)))))

;; Based on `cider-switch-to-relevant-repl-buffer` in cider.el.
(defun nrepl-connection-for-buffer (buffer)
  "Returns either the corresponding nrepl buffer for the given buffer, or a string error message."
  (if (not (cider-connected-p))
      "No active nREPL connection."
    (let ((project-directory (nrepl-project-directory-for (nrepl-current-dir))))
      (if project-directory
          (let ((buf (car (-filter
                           (lambda (conn)
                             (let ((conn-proj-dir (with-current-buffer (get-buffer conn)
                                                    nrepl-project-dir)))
                               (when conn-proj-dir
                                 (equal (file-truename project-directory)
                                        (file-truename conn-proj-dir)))))
                           nrepl-connection-list))))
            (if buf
                (get-buffer buf)
              "No relevant nREPL connection found."))
        "No project directory found."))))

(defun cider-eval-current-sexp ()
  "Eval the sexp the current is currently in. In Emacs' syntax table, this is called a list of expressions."
  (interactive)
  (cider-interactive-eval (current-sexp)))

(evil-leader/set-key-for-mode 'clojure-mode
  "eap" (lambda () (interactive) (with-nrepl-connection-of-current-buffer 'cider-eval-paragraph))
  "ee" (lambda () (interactive) (with-nrepl-connection-of-current-buffer 'cider-show-cider-buffer))
  "ek" (lambda () (interactive) (with-nrepl-connection-of-current-buffer 'cider-find-and-clear-repl-buffer))
  ; Note that I actually use cider-load-file here, not cider-eval-buffer, because it gives useful line numbers
  ; on exceptions.
  "eb" (lambda ()
         (interactive)
         (save-buffer)
         (with-nrepl-connection-of-current-buffer 'cider-load-current-buffer))
  ; cider-restart-nrepl is more handy than cider-jack-in, because it doesn't leave existing repls running.
  "en" 'cider-restart-nrepl
  "es" 'cider-eval-current-sexp
  "ex" (lambda () (interactive) (with-nrepl-connection-of-current-buffer 'cider-eval-expression-at-point))
  "er" (lambda () (interactive) (with-nrepl-connection-of-current-buffer 'cider-eval-region)))

;; Highlight parentheses in rainbow colors.
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; NOTE(philc): rainbow delimiters mode seems to be broken in the latest cider. Perhaps upgrade
;; rainbow-delimieters.
;; (add-hook 'cider-mode-hook 'rainbow-delimiters-mode)

;; Clojure indentation rules
(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (send-off 1) (cli 1) (go-loop 1)                                  ; Core
     (ANY 2) (GET 2) (POST 2) (PUT 2) (PATCH 2) (DELETE 2) (context 2) ; Compojure
     (select 1) (insert 1) (update 1) (where 1) (set-fields 1)         ; Korma
     (values 1) (delete 1) (upsert 1) (subselect 1)
     (clone-for 1)                                                     ; Enlive
     (up 1) (down 1) (alter 1) (table 1) (create 1)                    ; Lobos
     (checker 1)                                                         ; Midje
     (with-eligible-values 1) (when-eligible 1) (check 4)              ; Personal
     (url-of-form 1)                                                   ; Personal
     ))

(defun lisp-indent-line-single-semicolon-fix (&optional whole-exp)
  "Identical to the built-in function lisp-indent-line,
but doesn't treat single semicolons as right-hand-side comments."
  (interactive "P")
  (let ((indent (calculate-lisp-indent)) shift-amt end
        (pos (- (point-max) (point)))
        (beg (progn (beginning-of-line) (point))))
    (skip-chars-forward " \t")
    (if (or (null indent) (looking-at "\\s<\\s<\\s<"))
        ;; Don't alter indentation of a ;;; comment line
        ;; or a line that starts in a string.
        ;; FIXME: inconsistency: comment-indent moves ;;; to column 0.
        (goto-char (- (point-max) pos))
      (if (listp indent) (setq indent (car indent)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
          nil
        (delete-region beg (point))
        (indent-to indent)))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))))

;;
;; HTML mode
;;
(add-to-list 'auto-mode-alist '("\\.erb$" . html-mode))

;;
;; SCSS mode, for editing SCSS files.
;;
(setq scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;;
;; mu4e - email & gmail in Emacs.
;;
;; References:
;; * the mu4e manual
;; * https://groups.google.com/forum/#!topic/mu-discuss/qJ2zvyLPBX0
(add-to-list 'load-path "/usr/local/Cellar/mu/0.9.9.5/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(setq mu4e-mu-binary "/usr/local/Cellar/mu/0.9.9.5/bin/mu")
(setq mu4e-maildir "~/.Maildir")
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")
(setq mu4e-refile-folder  "/[Gmail].All Mail")

;; Folder shortcuts for the "jump-to-maildir" command.
(setq mu4e-maildir-shortcuts
    '(("/INBOX"               . ?i)
      ("/[Gmail].Sent Mail"   . ?s)
      ("/1action"             . ?1)
      ("/2hold"               . ?2)
      ("/[Gmail].All Mail"    . ?a)))

;; Use offline imap when fetching and reindexing mail.
(setq mu4e-get-mail-command "offlineimap")

(setq user-mail-address "phil.crosby@gmail.com"
      user-full-name  "Phil Crosby")

;; Don't save messages to Sent Messages, Gmail/IMAP takes care of this.
(setq mu4e-sent-messages-behavior 'delete)

;; Don't keep message buffers around.
(setq message-kill-buffer-on-exit t)

;; Use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

(setq mu4e-attachment-dir "~/Downloads")

;; Attempt to show images when viewing messages
(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)

;; https://groups.google.com/forum/#!searchin/mu-discuss/html/mu-discuss/7WwtyrCBeDg/nr0vK9fT7BEJ
(setq mu4e-html2text-command "w3m -dump -cols 110 -T text/html")
;; (setq mu4e-html2text-command "html2text | grep -v '&nbsp_place_holder;'")

(add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; TODO(philc): what does this do?
(setq mu4e-compose-dont-reply-to-self t)

;; How tall to make the headers view when viewing headers+mail as a split.
(setq mu4e-headers-visible-lines 22)

;; Trim down the types of columns we show, to leave more room for the sender & subject.
(setq mu4e-headers-fields '((:human-date . 12)
                            ;; (:flags . 6)
                            (:from-or-to . 22)
                            (:subject . 74)))

(eval-after-load 'mu4e
  '(progn
     (evil-make-overriding-map mu4e-main-mode-map 'normal t)
     (evil-define-key 'normal mu4e-main-mode-map
       "q" 'vimlike-quit
       "j" nil ; originally "jump to maildir".
       "gl" 'mu4e~headers-jump-to-maildir)

     (evil-make-overriding-map mu4e-headers-mode-map 'normal t)
     (evil-define-key 'normal mu4e-headers-mode-map
       "j" 'evil-next-line
       "k" 'evil-previous-line
       "n" 'mu4e-headers-next
       "p" 'mu4e-headers-prev
       "#" 'mu4e-headers-mark-for-trash
       "y" 'mu4e-headers-mark-for-refile
       "/" 'mu4e-headers-search-edit
       "z" 'mu4e-headers-mark-for-unmark
       "x" 'mu4e-headers-mark-for-something
       "gl" 'mu4e~headers-jump-to-maildir
       ;; consider calling this with t, for "no confirmation".
       "e" 'mu4e-mark-execute-all
       "q" 'vimlike-quit
       (kbd "RET") 'mu4e-headers-view-message
       "ESC" nil
       ;; TODO(philc): How can I reply-all without confirmation?
       ;; TODO(philc): mu4e-headers-toggle-full-search - show all results or just up until the cap.
       "r" 'mu4e-compose-reply
       ;; TODO(philc): mu4e-view-action opens URL
       "f" 'mu4e-compose-forward
       (kbd "M-r") 'mu4e-update-mail-and-index
       "c" 'mu4e-compose-new)

     (evil-make-overriding-map mu4e-view-mode-map 'normal t)
     (evil-define-key 'normal mu4e-view-mode-map
       "j" 'evil-next-line
       "k" 'evil-previous-line
       "n" 'mu4e-view-headers-next
       "p" 'mu4e-view-headers-prev
       "#" 'mu4e-view-mark-for-trash
       "y" 'mu4e-view-mark-for-refile
       "/" 'mu4e-view-search-edit
       "x" 'mu4e-view-mark-for-something
       "z" 'mu4e-view-mark-for-unmark
       "q" 'vimlike-quit
       ;; This prompts you for which link (starting with 1) you want to visit.
       ;; Note that you can just move your cursor to a URL and hit M-ret to open it.
       "go" 'mu4e-view-go-to-url
       (kbd "<enter>") 'mu4e-view-go-to-url
       "gl" (lambda ()
              (interactive)
              (switch-to-buffer-other-window "*mu4e-headers*")
              (call-interactively 'mu4e~headers-jump-to-maildir))
       ;; consider calling this with t, for "no confirmation".
       "e" 'mu4e-view-marked-execute
       (kbd "RET") 'mu4e-headers-view-message
       "ESC" nil
       ;; How to get reply-all without confirmation?
       "r" 'mu4e-compose-reply
       "f" 'mu4e-compose-forward
       (kbd "M-r") '(lambda () (interactive) (mu4e-update-mail-and-index t))
       "c" 'mu4e-compose-new)


     (evil-make-overriding-map mu4e-compose-mode-map 'normal t)
     (evil-define-key 'normal mu4e-compose-mode-map
       "c" nil)
     (evil-leader/set-key-for-mode 'mu4e-compose-mode
       "s" 'message-send-and-exit)))

;; Settings for sending mail.
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-stream-type 'ssl)
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 465)

;;
;; YAML mode, for editing YAML files
;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;
;; Go mode, for writing Go code
;;
(defun go-save-and-compile-fn (command-name)
  "Returns a function for the purpose of binding to a key which saves the current buffer and then
   runs the given command in the root of the go project."
  (lexical-let ((command-name command-name))
    #'(lambda ()
        (interactive)
        (save-buffer)
        (message command-name)
        (compile (concat "cd " (projectile-project-root) " && " command-name)))))

(evil-leader/set-key-for-mode 'go-mode
  ;; "r" is a nemsapce for run-related commands.
  "rr" (go-save-and-compile-fn "make run")
  "rb" (go-save-and-compile-fn "make synthetic-benchmark")
  "rt" (go-save-and-compile-fn "make test")
  "rw" (go-save-and-compile-fn "make run-web")
  ;; "c" is a namespace for compile-related commands.
  "cn" 'next-error
  "cp" 'previous-error
  "cw" (go-save-and-compile-fn "make web")
  "cb" (go-save-and-compile-fn "make benchmark")
  "cc" (go-save-and-compile-fn "make build"))

(defun gofmt-before-save-ignoring-errors ()
  "Don't pop up syntax errors in a new window when running gofmt-before-save."
  (interactive)
  (flet ((gofmt--process-errors (&rest args) t)) ; Don't show any syntax error output
    (gofmt-before-save)))

(defun init-go-buffer-settings ()
  ;; I have Emacs configured to save when switching buffers, so popping up errors when I switch buffers is
  ;; really jarring.
  (add-hook 'before-save-hook 'gofmt-before-save-ignoring-errors)
  ;; Make it so comments are line-wrapped properly when filling. It's an oversight that this is missing from
  ;; go-mode.
  (setq-local fill-prefix "// "))

(add-hook 'go-mode-hook 'init-go-buffer-settings)

(defun go-package-of-current-buffer ()
  "Returns the go package name defined in the current buffer. Returns nil if no package has been defined."
  (let ((file-contents (buffer-string)))
    (let ((match-exists (string-match "^package \\(.+\\)\w*" file-contents)))
      (when match-exists
        (buffer-substring-no-properties (+ 1 (match-beginning 1))
                                        (+ 1 (match-end 1)))))))

;;
;; Magit - for staging hunks and making commits to git
;;
(eval-after-load 'magit
  '(progn
     ;; Magit mode feels twitchy because every key has a binding, and some are very destructive or
     ;; disorienting. I'm defining a whitelist of keys that I actually use, so this mode feels less
     ;; erorr-prone.
     (evil-define-key 'normal magit-mode-map
       ";gg" 'magit-display-process
       "n" 'magit-goto-next-section
       "p" 'magit-goto-previous-section
       "L" 'magit-key-mode-popup-logging
       "yy" 'magit-copy-item-as-kill ; Copies the commit ID of the commit under the cursor.
       (kbd "RET") 'magit-visit-item
       ;; These scroll the diff window. Normally these are mapped to space and shift-space in magit.
       ;; TODO(philc): Uncomment these once the latest magit lands in melpa.
       ;; (define-key magit-mode-map (kbd "C-d") '(lambda () (interactive)
       ;;                                           (magit-show-item-or-scroll 'View-scroll-half-page-forward)))
       ;; (define-key magit-mode-map (kbd "C-u") '(lambda () (interactive)
       ;;                                           (magit-show-item-or-scroll 'View-scroll-half-page-backward)))
       ;; (define-key magit-mode-map (kbd "C-d") 'magit-show-item-or-scroll-up)
       ;; (define-key magit-mode-map (kbd "C-u") 'magit-show-item-or-scroll-down)

       ;; Kill the ephemeral diff popup which appears when you type space.
       (kbd "S-SPC") (lambda () (interactive) (kill-buffer-and-its-windows "*magit-commit*")))

     (evil-define-key 'normal magit-log-mode-map
       ";gca" 'magit-commit-amend
       ";gri" 'magit-interactive-rebase
       ";gri" 'magit-interactive-rebase
       ;; I use C-d and C-u for scrolling the log view, and d and u for scrolling the diff view showing the
       ;; diff for the focused commit. TODO(philc): Change this to scorll half page down/up
       ;; "u" 'magit-show-item-or-scroll-up
       ;; "d" 'magit-show-item-or-scroll-down
       )

     (evil-define-key 'normal magit-status-mode-map
       "c" 'magit-commit
       "e" 'magit-show-level-4-all ; e for exapnd
       "d" 'magit-discard-item
       "s" 'magit-stage-item
       "S" 'magit-stage-all
       "d" 'magit-discard-item
       "u" 'magit-unstage-item
       "U" 'magit-unstage-all
       "-" 'magit-diff-smaller-hunks
       "+" 'magit-diff-larger-hunks
       "gu" 'magit-jump-to-unstaged
       ;; ;; NOTE(philc): I'm not sure why I need to define shortcuts for j and k explicitly.
       ;; "j" 'evil-next-line
       ;; "k" 'evil-previous-line
       (kbd "TAB") 'magit-toggle-section
       "r" 'magit-refresh)

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
             magit-insert-pending-changes
             magit-insert-pending-commits
             magit-insert-unstaged-changes
             magit-insert-staged-changes
             magit-insert-untracked-files
             magit-insert-stashes
             magit-insert-unpulled-commits
             magit-insert-unpushed-commits))))

(evil-set-initial-state 'magit-mode 'normal)
(evil-set-initial-state 'magit-status-mode 'normal)
(evil-set-initial-state 'magit-log-mode 'normal)
(evil-set-initial-state 'magit-commit-mode 'normal)
(evil-set-initial-state 'git-commit-mode 'insert)

;; Have Magit open in the current window, not a new split.
(setq magit-status-buffer-switch-function 'switch-to-buffer)

(defun with-magit-output-buffer (f)
  (lexical-let ((f f))
    (preserve-selected-window
     (lambda ()
       (magit-display-process)
       (funcall f)))))

(defun git-pull ()
  (interactive)
  (with-magit-output-buffer 'magit-pull))

(defun git-push ()
  (interactive)
  (with-magit-output-buffer 'magit-push))

;; Disable the `highlight` face that Magit uses to highlight diffs. It's unreadable with my color scheme.
(defun disable-magit-highlight-in-buffer () (face-remap-add-relative 'magit-item-highlight '()))
(add-hook 'magit-status-mode-hook 'disable-magit-highlight-in-buffer)

;;
;; Project navigation (my own functions on top of dired-mode and projectile)
;;
(setq project-folders '("~/p" "~/liftoff"))
(setq notes-directories '("~/personal/notes" "~/Desktop"))
(setq notes-file-extensions '(".md" ".sql" ".txt"))

;; This is set to 600 by default. It shouldn't be the case, but for some reason, the filter-files-in-directory
;; function hits this limit.
(setq max-lisp-eval-depth 1200)

(defun filter-files-in-directory (directory filter-fn include-subdirectories)
  "Filters the files in the given directory and subdirectories using filter-fn. Excludes .git subdirectories."
  (->> (directory-files directory t)
       (remove-if (lambda (path)
                    (or (string/ends-with path ".")
                        (string/ends-with path "..")
                        (string/ends-with path ".git"))))
       (mapcar (lambda (file)
                 (if (and include-subdirectories (file-directory-p file))
                     (filter-files-in-directory file filter-fn include-subdirectories)
                   file)))
       flatten
       (remove-if-not filter-fn)))

(defun open-markdown-file-from-notes-folder ()
  "Prompts for the name of a .md notes file to open."
  (interactive)
  (let* ((file-matches-pattern? (lambda (file)
                                  (some (lambda (ext) (string/ends-with file ext)) notes-file-extensions)))
         (file-list (->> notes-directories
                         (mapcar (lambda (directory)
                                   (filter-files-in-directory directory file-matches-pattern? t)))
                         flatten)))
    (let ((file-to-open (ido-completing-read "Notes file: " (mapcar 'file-name-nondirectory file-list))))
      (->> file-list
           (remove-if-not (lambda (file) (string/ends-with file (concat "/" file-to-open))))
           first
           find-file))))

(defun open-root-of-project-in-dired ()
  "Prompts for the name of a project which exists in your common project folders and opens a dired window in
   the root of the project folder. This is a fast way to open a new project and be able to run
   projectile-file-file.
   Once a project is chosen, the current elscreen-tab is set to be the name of that project."
  (interactive)
  (let ((all-project-folders (->> project-folders
                                  (mapcar (lambda (file)
                                            (filter-files-in-directory file 'file-directory-p nil)))
                                  flatten)))
    (let ((project-to-open (ido-completing-read "Project folder: "
                                                (mapcar 'file-name-nondirectory all-project-folders)
                                                nil t)))
      (->> all-project-folders
           (remove-if-not (lambda (project) (string/ends-with project (concat "/" project-to-open))))
           first
           ((lambda (project)
              (dired project)
              ;; If we invoke this inside of a split, don't set the tab's title.
              (when (= 1 (length (window-list)))
                (elscreen-screen-nickname (file-name-nondirectory project)))))))))
