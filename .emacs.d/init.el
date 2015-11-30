;;
;; I try to keep this file well-documented so new and veteran users can easily understand the parts
;; of my setup they may want to lift.
;;
;; While the function definitions which implement major pieces of functionality are kept in separate files, I
;; try to centralize the configuration for everything here in this file.
;;

;; Colorscheme
(load-theme 'tangotango t) ; A reasonable color scheme which lives in my .emacs.d.
;; Make the dividing line between window splits less bright. It's #EAEAEA in tangotango.
(set-face-attribute 'vertical-border nil :foreground "#888888")

;;
;; Package management
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))

;; Cider from MELPA has been too unstable for me. Only use versions from MELPA Stable.
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(ace-jump-mode ; Jump to any text on screen in a few keystrokes. Like Vim's EasyMotion.
                      ac-nrepl
                      ag ; Silver searcher integration for Emacs
                      autopair ; Insert matching delimiters, e.g. insert closing braces.
                      auto-complete
                      cider ; repl for Clojure code evaluation.
                      clojure-mode
                      coffee-mode ; For syntax highlighting coffeescript.
                      dash
                      dash-functional
                      dired-details+ ; Hides all of the unnecessary file details in dired mode.
                      diminish ; For hiding and shortening minor modes in the modeline
                      escreen
                      evil
                      evil-leader
                      evil-nerd-commenter
                      flx-ido ; Fuzzy matching for ido, which improves the UX of Projectile.
                      go-mode ; For editing Go files.
                      hiwin ; For highlighting the active pane/window in Emacs.
                      less-css-mode ; Syntax highlighting for LESS CSS files.
                      ido-ubiquitous ; Make ido completions work everywhere.
                      ido-vertical-mode ; Show ido results vertically.
                      magit
                      midje-mode ; For editing clojure tests
                      ;; multi-term ; Display many termianls inside emacs, not just one.
                      org ; For outlining. This is bundled with Emacs, but I'm using the latest version.
                      outline-magic ; Extensions to ouline mode, which I use heavily in markdown mode.
                      powerline ; Improve the appearance & density of the Emacs status bar.
                      projectile ; Find file in project (ala Vim's CTRL-P or Textmate's Cmd-T)
                      rainbow-delimiters ; Highlight parentheses in rainbow colors.
                      ruby-electric ; Insert matching delimiters; unindent end blocks after you type them.
                      scss-mode
                      smartparens
                      smex
                      wcheck-mode ; Spell checking
                      yaml-mode
                      yasnippet
                      zoom-frm))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;
;; General settings
;;
(require 'cl)
(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'lisp-helpers-personal)
(require 'emacs-utils-personal)

;; Based on my anecdotal observations, this reduces the amount of display flicker during Emacs startup.
(setq redisplay-dont-pause t)

;; Turn off graphical toolbars.
(if (display-graphic-p) (menu-bar-mode 1) (menu-bar-mode -1))
(when (and (fboundp 'tool-bar-mode) tool-bar-mode) (tool-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) scroll-bar-mode) (scroll-bar-mode -1))

(setq initial-scratch-message "") ; When opening a new buffer, don't show the scratch message.

;; Make it so that the scratch buffer uses markdown. By default it uses Emacs Lisp mode.
(setq initial-major-mode 'markdown-lite-mode)

;; Use the same PATH variable as your shell does. From http://clojure-doc.org/articles/tutorials/emacs.html
;; NOTE(philc): If you use OSX and you use zsh, OSX may be configured to use bash. Either hard code
(defun set-exec-path-from-shell-PATH ()
  (let* ((shell "zsh") ;; NOTE(philc): Change to your desired shell. You could also use the $SHELL env var.
         (path-from-shell (shell-command-to-string (concat shell " -i -c 'echo $PATH'"))))
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

;; Increase the maximum stack depth (the default is 1000).
;; This triggers for some routes I've written (like open-markdown-file-from-notes-folder) which are iterative.
(setq max-specpdl-size 2000)

;; Turn off backups and autosaves so we don't have ~ and # files strewn about the working directory. I've
;; tried storing backups in my home directory as suggested by http://stackoverflow.com/q/151945/46237, but
;; still I see the occasional backup file.
(setq make-backup-files nil)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-default nil)

;; Disable Emacs' write-lock, which creates temporary .#files when saving. This crashes coffeescript --watch.
;; https://github.com/jashkenas/coffeescript/issues/985
(setq create-lockfiles nil)

(setq vc-follow-symlinks t) ; Don't ask confirmation to follow symlinks to edit files.

;; NOTE(philc): Disabling savehist-mode in an attempt to isolate a periodic Emacs segfault.
;; (savehist-mode t) ; Save your minibuffer history across Emacs sessions. UX win!

(setq text-scale-mode-step 1.1) ;; When changing font size, change in small increments.

;; Include path information in duplicate buffer names (e.g. a/foo.txt b/foo.txt)
(setq uniquify-buffer-name-style 'forward)

;; Start scrolling the window when the cursor reaches its edge.
;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq scroll-margin 7
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      ;; Make touchpad scrolling on OSX less jerky
      mouse-wheel-scroll-amount '(0.01))

;; The preference file for Emac's "Customize" system. `M-x customize` to access it.
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file t)

;; Whitespace & line wrapping.
(global-whitespace-mode t)
(with-eval-after-load "whitespace"
  (setq whitespace-line-column 110) ; When text flows past 110 chars, highlight it.
  ;; whitespace-mode by default highlights all whitespace. Show only tabs and trailing spaces.
  (setq whitespace-style '(face trailing lines-tail)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default tab-width 2)
(setq-default evil-shift-width 2)
;; Some modes have their own tab-width variables which need to be overridden.
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
(util/define-keys minibuffer-local-map
                  (kbd "C-k") 'kill-line
                  (kbd "C-e") 'end-of-line
                  (kbd "C-u") 'backward-kill-line
                  (kbd "C-d") 'delete-char
                  (kbd "C-w") 'backward-kill-word
                  (kbd "C-h") 'backward-delete-char)

;; Disable the prompt we get when killing a buffer with a process. This affects clojure mode in particular,
;; when we want to restart the nrepl process.
(setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; Use smex to show the M-x command prompt. It has better completion support than the default M<x.
(require 'smex)
(global-set-key (kbd "M-x") 'smex)

;; RecentF mode is the Emacs minor mode used when opening files via C-x C-f.
(require 'recentf)
(define-key recentf-mode-map (kbd "C-w") 'backward-kill-word)
(define-key recentf-mode-map (kbd "C-h") 'backward-delete-char)

;; The poorly-named winner mode saves the history of your window splits, so you can undo and redo changes to
;; your window configuration.
(winner-mode t)

;;
;; Emacs general autocompletion
;; One nice feature of Emac's autocomplete mode is that it auto-populates likely completions as you type.
;; However, in doing so, it causes my cursors to flicker in other splits while I'm typing. This makes the mode
;; unusable.
(setq ac-auto-start nil)
;; (add-hook 'prog-mode-hook 'auto-complete-mode)
;; (with-eval-after-load "auto-complete"
;;      (define-key ac-complete-mode-map "\C-n" 'ac-next)
;;      (define-key ac-complete-mode-map "\C-p" 'ac-previous)
;;      (setq ac-auto-start nil)
;;      (ac-set-trigger-key "TAB")
;;      (ac-linum-workaround))

(defun create-scratch-buffer nil
   "Create a scratch buffer. Helpful if you save your scratch buffer as a file, or accidentally kill it."
   (interactive)
   (switch-to-buffer (get-buffer-create "*scratch*"))
   (lisp-interaction-mode))

;; Save buffers whenever they lose focus.
;; This obviates the need to hit the Save key thousands of times a day. Inspired by http://goo.gl/2z0g5O.
(add-hook 'focus-out-hook 'util/save-buffer-if-dirty) ; This hook is only available in Emacs 24.4+.
(add-hook 'focus-out-hook '(lambda () (evil-normal-state))) ; This hook is only available in Emacs 24.4+.

(defadvice windmove-up (before other-window-now activate) (util/save-buffer-if-dirty))
(defadvice windmove-down (before other-window-now activate) (util/save-buffer-if-dirty))
(defadvice windmove-left (before other-window-now activate) (util/save-buffer-if-dirty))
(defadvice windmove-right (before other-window-now activate) (util/save-buffer-if-dirty))

; This is fired whenever the buffer list is updated, which is a reasonably robust way to detect that the
; window config has changed and the current buffer should be saved.
(add-hook 'buffer-list-update-hook 'util/save-buffer-if-dirty)

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

;; This splits the current line at the cursor position.
(define-key evil-normal-state-map (kbd "RET") 'newline-and-indent)
;; This creates a newline below the current line, like open, but does not enter insert mode.
(define-key evil-normal-state-map (kbd "s") (lambda () (interactive) (evil-insert-newline-below)))

;; By default, Emacs will not indent when you hit enter/return within a comment.
(define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)

;; When jumping back and forth between marks, recenter the screen on the cursor.
(define-key evil-normal-state-map (kbd "C-o")
  (lambda () (interactive) (evil-jump-backward) (recenter-no-redraw)))
(define-key evil-normal-state-map (kbd "C-i")
  (lambda () (interactive) (evil-jump-forward) (recenter-no-redraw)))

; Some help keybindings which conflict with nothing else, so you can pull up help in any context.
(global-set-key (kbd "C-A-M-h") 'help)
(global-set-key (kbd "C-A-M-b") 'describe-bindings)

;; gq is normally bound to evil-fill-and-move, but when I reflow a paragraph, I like the cursor to remain
;; where it was.
(define-key evil-normal-state-map "gq" 'evil-fill)
(define-key evil-normal-state-map "-" 'evil-indent-without-move)

;; Evil uses the current file's mode's definition of a paragraph, which is often surprising. For instance, in
;; Markdown mode, a single item in a bulleted list consistutes a paragraph. Instead, I've defined a paragraph
;; to be hunks of text separated by newlines. That's typically what I would expect of a paragraph. You can
;; still use Evil's paragraph definition using the text object "P" instead of "p".
(evil-define-text-object evil-paragraph-from-newlines (count &optional beg end type)
  "Select a paragraph separated by newlines."
  :type line
  ;; These two vars are set by the current programming mode. Set them to their default text mode values
  ;; temporarily while we select the paragraph. The implementation of evil-select-an-object will invokes
  ;; `forward-paragraph`, which uses these variables.
  (let ((paragraph-start "\f\\|[     ]*$")
        (paragraph-separate "[  ]*$"))
    (evil-select-an-object 'evil-paragraph beg end type count)))

(define-key evil-outer-text-objects-map "p" 'evil-paragraph-from-newlines)
(define-key evil-outer-text-objects-map "P" 'evil-a-paragraph)

(defun count-chars-region (beg end)
  "Prints the character count of the selected region."
  (interactive "r")
  (-> (buffer-substring beg end) length number-to-string message))

(evil-leader/set-key
  "h" 'help
  "b" 'ido-switch-buffer
  "f" 'projectile-find-file
  "p" 'escreen-tab-switcher
  "SPC" 'evil-fill-inside-paragraph ; Shortcut for Vim's gqip
  "i" 'evil-indent-inside-paragraph ; Shortcut to Vim's =ip
  "d" 'projectile-dired
  "D" (lambda () (interactive) (-> (buffer-file-name) file-name-directory dired))
  "gs" (lambda() (interactive)
          (util/save-buffer-if-dirty)
          (magit-status-and-focus-unstaged))
  "gl" 'magit-log
  "o" 'xah-open-file-at-cursor
  "wc" 'count-chars-region
  ;; "v" is a mnemonic prefix for "view X".
  ;; "vv" will be a natural choice as a mode-specific shortcut for previewing the current file.
  "vu" 'notmuch-go-to-inbox
  "vp" 'navigate-to-project
  "vn" 'open-markdown-file-from-notes-folder
  "vo" (lambda () (interactive) (find-file "~/Dropbox/tasks.org"))
  "ve" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(setq evil-leader/leader ";")

;; Ensure we evil-leader works in non-editing modes like magit. This is referenced from evil-leader's README.
(setq evil-leader/no-prefix-mode-rx '("magit-.*-mode"))

(defun evil-fill-inside-paragraph ()
  "Fills (reflows/linewraps) the current paragraph. Equivalent to gqap in vim."
  (interactive)
  ;; When we line wrap (fill) the paragraph, the default behavior is to put your cursor at the end of the
  ;; newly-wrapped paragraph. Better would be to keep your cursor where it was when you triggered this
  ;; command, so you don't need ot navigate back there if you want to continue typing.
  ;; This tries to estimate where your cursor should be after lines get wrapped.
  (lexical-let* ((estimated-col (mod (current-column) fill-column))
                 (estimated-line (+ (line-number-at-pos)
                                    (/ (current-column) fill-column))))
    (util/preserve-line-and-column
     (lambda ()
       (let ((region (if (use-region-p)
                         (list (region-beginning) (region-end))
                       (evil-inner-paragraph))))
         (evil-fill (first region) (second region)))))
    (goto-line estimated-line)
    (move-to-column estimated-col)))

(defun evil-indent-inside-paragraph ()
  "Fills (reflows/linewraps) the current paragraph. Equivalent to gqap in vim."
  (interactive)
  ;; Even though evil-indent-without-move preserves the cursor position, we must also do it here.
  (util/preserve-line-and-column
   (lambda ()
     (let ((region (if (use-region-p)
                       (list (region-beginning) (region-end))
                     (evil-inner-paragraph))))
       (evil-indent-without-move (first region) (second region))))))

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
  (util/preserve-line-and-column (lambda () (evil-indent beg end))))

;; Enable the typical Bash/readline keybindings when in insert mode.
(util/define-keys evil-insert-state-map
                  (kbd "C-k") 'kill-line
                  (kbd "C-e") 'end-of-line
                  (kbd "C-u") 'backward-kill-line
                  (kbd "C-d") 'delete-char)
(global-set-key (kbd "C-h") 'backward-delete-char) ; Here we clobber C-h, which accesses Emacs's help.

;; Commenting via NERD commentor.
(define-key evil-normal-state-map "," 'evilnc-comment-operator)
(define-key evil-visual-state-map "," 'evilnc-comment-operator)

;;
;; Window manipulation, switching, & management.
;;
(require 'window-management-personal)

;; Don't use the native OSX full screen support, because it uses OSX Spaces which don't play well with
;; CMD-tabbing to applications which are behind Emacs. Invoke fullscreen with `toggle-frame-fullscreen`.
(setq ns-use-native-fullscreen nil)

;; Window-management keybindings. "w" is the namespace I use.
(evil-leader/set-key "wn" 'create-new-column)
(evil-leader/set-key "wv" 'split-window-horizontally-and-focus)
(evil-leader/set-key "wh" 'split-window-vertically-and-focus)
;; (evil-leader/set-key "ws A-e" 'swap-window-with-upper-left)
;; (evil-leader/set-key "ws A-d" 'swap-window-with-lower-left)
;; (evil-leader/set-key "ws A-r" 'swap-window-with-upper-right)
;; (evil-leader/set-key "ws A-f" 'swap-window-with-lower-right)
;; (evil-leader/set-key "ws A-e" 'swap-window-with-upper-right)
(evil-leader/set-key "wk" (lambda () (interactive) (kill-buffer (current-buffer))))
(evil-leader/set-key "wm" 'toggle-window-maximize)
(evil-leader/set-key "wr" 'evil-window-rotate-downwards)
(evil-leader/set-key "wR" 'evil-window-rotate-upwards)
;; winner-undo will undo the last change you made to your window configuration.
(evil-leader/set-key "wb" 'winner-undo)
(evil-leader/set-key "we" 'toggle-maximize-lower-right-window)
(evil-leader/set-key "q" 'dismiss-ephemeral-windows)
(evil-leader/set-key "wf" 'toggle-frame-fullscreen)

;; Make it so Esc means quit, no matter the context.
;; http://stackoverflow.com/a/10166400/46237
;; Note that when Emacs becomes unresponsive (e.g. because I accidentally grepped my home directory), I might
;; still need to hold C-g (the Emacs esc/cancel key) to bring it back.
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is active, just deactivate it;
   then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
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
;; Make C-h act the same as backspace, as it does in readline.
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
;; Make it so the OSX keybindings you're used to always work in every mode in Emacs.
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;;
(require 'zoom-frm) ; For zooming (increasing font size of all windows) using M-- and M-+
(defvar osx-keys-minor-mode-map (make-keymap) "osx-keys-minor-mode-keymap")
(util/define-keys osx-keys-minor-mode-map
                  (kbd "M-`") 'other-frame
                  (kbd "M-~")
                  '(lambda () (interactive) (other-frame -1))
                  (kbd "M-w") 'vimlike-quit
                  (kbd "M-q") 'save-buffers-kill-terminal
                  (kbd "M-n") 'new-frame
                  (kbd "M-a") 'mark-whole-buffer
                  (kbd "M-h") 'ns-do-hide-emacs
                  (kbd "M-v") 'clipboard-yank
                  (kbd "M-c") 'clipboard-kill-ring-save
                  (kbd "M-m") 'iconify-or-deiconify-frame
                  (kbd "M-W") 'evil-quit ; Close all tabs in the current frame..
                  (kbd "M--") 'zoom-out
                  (kbd "M-=") 'zoom-in
                  (kbd "M-0") 'zoom-frm-unzoom
                  ;; These aren't specifically replicating OSX shortcuts, but they manipulate the window, so I
                  ;; want them to take precedence over everything else.
                  (kbd "M-C-n") 'other-window
                  ;; Note that I have Ctrl-Space mapped to Alt, which makes these shortcuts easy to hit.
                  (kbd "A-f") (lambda () (interactive) (ignore-errors (windmove-right)))
                  (kbd "A-d") (lambda () (interactive) (ignore-errors (windmove-down)))
                  (kbd "A-s") (lambda () (interactive) (ignore-errors (windmove-left)))
                  (kbd "A-e") (lambda () (interactive) (ignore-errors (windmove-up)))
                  (kbd "A-F") 'swap-window-right
                  (kbd "A-D") 'swap-window-down
                  (kbd "A-S") 'swap-window-left
                  (kbd "A-E") 'swap-window-up
                  (kbd "M-i") 'escreen-set-tab-alias
                  (kbd "M-t") 'open-current-buffer-in-new-tab)

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

(defun open-folder-in-finder ()
  "Opens the folder of the current file in OSX's Finder."
  (interactive)
  (call-process-region nil nil "/usr/bin/open" nil nil nil "."))

;; Closes the current escreen, or if there's only one screen, use the ":q" Evil
;; command. This simulates the ":q" behavior of Vim when used with tabs.
;; http://zuttobenkyou.wordpress.com/2012/06/15/emacs-vimlike-tabwindow-navigation/
(defun vimlike-quit ()
  "Vimlike ':q' behavior: close current window if there are split windows;
   otherwise, close current tab (elscreen)."
  (interactive)
  (let ((one-escreen (= 1 (length (escreen-get-active-screen-numbers))))
        (one-window (one-window-p)))
    (cond
     ; if current tab has split windows in it, close the current live window
     ((not one-window)
      (delete-window) ; delete the current window
      (balance-windows) ; balance remaining windows
      nil)
     ; if there are multiple escreens (tabs), close the current escreen
     ((not one-escreen)
      (escreen-kill-screen)
      nil)
     ; if there is only one elscreen, just try to quit (calling elscreen-kill
     ; will not work, because elscreen-kill fails if there is only one
     ; elscreen)
     (one-escreen
      (evil-quit)
      nil))))

;;
;; Filename completions (i.e. CTRL-P or CMD-T in other editors)
;;
(ido-mode t)
(ido-ubiquitous-mode t)
(ido-vertical-mode t)
(with-eval-after-load "ido"
  (setq ido-enable-flex-matching t)
  (setq ido-use-virtual-buffers t)
  (setq ido-everywhere t)
  ;; Kill (unload) the highlighted buffer in the matches list.
  (define-key ido-buffer-completion-map (kbd "M-d") 'ido-kill-buffer-at-head))

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
(evil-define-key 'normal dired-mode-map "o" 'dired-find-alternate-file)

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
;; Emacs Lisp (elisp)
;;
(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)))
(evil-define-key 'normal emacs-lisp-mode-map
  "gf" 'find-function-at-point
  (kbd "C-S-H") 'shift-sexp-backward
  (kbd "C-S-L") 'shift-sexp-forward
  "K" (lambda ()
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
  (message "%s" (eval (read (current-sexp)))))

(defun view-echo-area-messages-and-scroll ()
  "Opens the echo area messages buffer and scrolls to the bottom of it. That's where the latest messages are."
  (interactive)
  (view-echo-area-messages)
  (util/preserve-selected-window
   (lambda ()
     (select-window (get-buffer-window "*Messages*"))
     (goto-char (point-max)))))

(defun erase-messages-buffer ()
  (interactive)
  (util/preserve-selected-window
   (lambda ()
     (select-window (get-buffer-window "*Messages*"))
     ;; The *Messages* buffer is readonly.
     (read-only-mode -1)
     (erase-buffer)
     (read-only-mode 1))))

(evil-leader/set-key-for-mode 'emacs-lisp-mode
  ; Note that I'm saving the buffer before each eval because otherwise, the buffer gets saved after the eval,
  ; (due to save-when-switching-windows setup) and the output from the buffer save overwrites the eval results
  ; in the minibuffer.
  "eb" (lambda() (interactive) (util/save-buffer-if-dirty) (eval-buffer))
  "es" (lambda () (interactive) (util/save-buffer-if-dirty) (elisp-eval-current-sexp))
  "ex" (lambda () (interactive) (util/save-buffer-if-dirty) (call-interactively 'eval-defun))
  "ek" 'erase-messages-buffer
  "ee" 'view-echo-area-messages-and-scroll)

;; Indentation rules.
(put '-> 'lisp-indent-function nil)
(put '->> 'lisp-indent-function nil)

;;
;; Org mode, for TODOs and note taking.
;;
(require 'org-mode-personal)

;;
;; Projectile (find file from the root of the current project).
;;
(projectile-global-mode)
;; NOTE(philc): Using this cache is annoying because it gets stale if files appear on disk after a git pull.
;; However, in my large repos, without it, projectile-find-file takes about 1s to open, which is an
;; unacceptable delay.
(setq projectile-enable-caching t)

(defun restart-projectile-find-file-hook ()
  (remove-hook 'post-command-hook 'restart-projectile-find-file-hook)
  (let ((query previous-projectile-input))
    (makunbound 'previous-projectile-input)
    (projectile-find-file-with-initial-value query)))

(defun projectile-find-file-with-initial-value (initial-val)
  "Useful to call after reloading the project cache while the find file dialog still open."
  (interactive)
  (let ((file (projectile-completing-read "Find file: "
                                            (projectile-current-project-files) initial-val)))
      (find-file (expand-file-name file (projectile-project-root)))
      (run-hooks 'projectile-find-file-hook)))

;; Bind "M-r" when the find-files minibuffer is open to refreshing Projectile's cache. This is a common need
;; when you open a find files dialog and realize a newly added file is not there, due to a stale cache.
;; NOTE(philc): Ideally we would bind this key to ido-file-completion-map, but minibuffer-local-map has M-r
;; bound already. Also, minor note: for some reason, typing this keybinding recursively fails with "Error in
;; post-command hook..."
(define-key minibuffer-local-map (kbd "M-r")
  (lambda ()
    (interactive)
    (setq previous-projectile-input (minibuffer-contents))
    (projectile-invalidate-cache nil)
    ;; Reference for running code after `minibuffer-keyboard-quit`: http://stackoverflow.com/q/21000540/46237
    (add-hook 'post-command-hook 'restart-projectile-find-file-hook)
    (minibuffer-keyboard-quit)))

;;
;; escreen (tabs)
;;
;; I use one tab per "workspace" (i.e. open project). All of my tab-related config is geared towards that use
;; case. I was previously using elscreen, but it has two major bugs: tab bars would get rendered on
;; random windows, and Emacs's redraws would begin flashing if you changed monitors or font size.
(require 'escreen)
(escreen-install)

;; KeyRemap4Macbook translates M-j and M-k to these keys.
(global-set-key (kbd "<A-M-left>") 'escreen-goto-prev-screen)
(global-set-key (kbd "<A-M-right>") 'escreen-goto-next-screen)

;; I alias/nickname each of my tabs (escreen's numbered screens).
(setq escreen-number->alias (make-hash-table))

(defun escreen-set-tab-alias (alias)
  "Give the current tab an alias. This alias is shown by escreen-tab-switcher."
  (interactive "sTab alias: ")
  (when (> (length alias) 0)
    (puthash (escreen-get-current-screen-number) alias escreen-number->alias)))

(defun escreen-tab-switcher ()
  "Shows a menu in the minibuffer of tab names and numbers. Type the tab number to switch to it."
  (interactive)
  (lexical-let* ((get-display-name (lambda (i)
                                     (let ((template (if (= i (escreen-get-current-screen-number))
                                                         "*%d.%s*"
                                                       "%d.%s")))
                                       (->> (or (gethash i escreen-number->alias) "unnamed")
                                            (format template (+ i 1))))))
                 (tab-names (-map get-display-name (escreen-get-active-screen-numbers))))
    (message (string/join tab-names "  "))
    (lexical-let* ((input (string (read-char)))
                   (is-digit (and (string= (number-to-string (string-to-number input)) input))))
      (when is-digit
        (escreen-goto-screen (- (string-to-number input) 1))))))

(defun open-current-buffer-in-new-tab ()
  (interactive)
  ;; Exit out of insert mode when opening a new tab.
  (evil-change-to-initial-state)
  ;; I'm using the current buffer in the new tab so that the current directory is set as it was previously,
  ;; which lets me begin using projectile immediately.
  (let ((buffer (current-buffer)))
    (escreen-create-screen)
    (set-window-buffer (get-buffer-window) buffer)))

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
;; Spell checking
;; https://github.com/tlikonen/wcheck-mode
;;
;; * FlySpell is the default choice for spellchecking, but I found it slow, even using every flyspell perf
;;   improvement I could find. Speck doesn't slow down your typing.
;; * I suspect speck mode is the culrprit of periodic emacs crashes. It's also poorly documented and doesn't
;;   support binding "add to personal dictionary" as a keybinding.
;; * wcheck-mode is hard to configure because of its genericism, but at least it's documented and performs
;;   well once configured.
;;
;; You may need to install aspell and enchant (e.g. `brew install aspell enchant`).

(require 'wcheck-mode)
(setq-default wcheck-language "English")
(setq-default wcheck-language-data
              '(("English"
                 (program . "/usr/local/bin/aspell")
                 (args "list") ; -l: list only the mispellings.
                 (face . hi-yellow)
                 (connection . pty)
                 ;; Note that I don't use this functionality of providing suggested spelling corrects, and
                 ;; this config is untested. I just like to highlight mispelled words.
                 (action-program . "/usr/local/bin/aspell")
                 (action-args "-a") ; -a: lists alternatives.
                 (action-parser . wcheck-parser-ispell-suggestions))))

(add-hook 'text-mode-hook 'wcheck-mode)

(define-key evil-normal-state-map (kbd "zg") 'wcheck-add-to-dictionary)

(defvar custom-dictionary-file "~/.aspell.en.pws")

(defun wcheck-add-to-dictionary ()
  "Adds the word under the cursor to your personal dictionary. Also re-spellchecks the buffer to clear any
   stale highlights."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (if (not (and custom-dictionary-file (file-writable-p custom-dictionary-file)))
        (message "Couldn't locate your custom dictionary file '%s'" custom-dictionary-file)
      (progn
        (with-temp-buffer
          (insert word) (newline)
          (append-to-file (point-min) (point-max) custom-dictionary-file))
        (message "Added word \"%s\" to %s" word custom-dictionary-file)
        ; This is a hack to toggle the mode on and then off, to rescane the buffer and remove the mispelt
        ; marker for the word that was just added to the dict.
        (wcheck-mode)
        (wcheck-mode)))))

;;
;; Diminish - hide or shorten the names of minor modes in your modeline.
;; To see which minor modes you have loaded and what their modeline strings are: (message minor-mode-alist)
(require 'diminish)
(diminish 'visual-line-mode "")
(diminish 'global-whitespace-mode "")
;; (diminish 'global-visual-line-mode "")
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

;; TODO(philc): Can this be deleted? It may be the source of my crashing issues with powerline.
(defun powerline-projectile-project-name (&optional face padding)
  "Returns a string describing the projectile project for the current buffer. Takes the same arguments as
   powerline-raw."
  (powerline-raw (concat "(" (projectile-project-name) ")") face padding))

(defun powerline-personal-theme ()
  "My customized powerline, copied and slightly modified from the default theme in powerline.el."
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
                                     ;; (powerline-projectile-project-name 'powerline-black-face 'l)
                                     (powerline-raw " " 'powerline-black-face)
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
                                     (powerline-raw " " face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)))))))

(powerline-personal-theme)

;;
;; Markdown
;;
(require 'markdown-mode-lite)

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
  ;; This compiles the file and jumps to the first error, if there is one.
  "rr" 'reload-active-chrome-tab
  "cc" (lambda ()
         (interactive)
         (save-buffer)
         (coffee-compile-without-side-effect))
  ;; The mnemonic for this is "compile & preview". It shows the javascript output in a new buffer.
  "cp" 'coffee-compile-buffer)

(defun coffee-compile-without-side-effect ()
  ;; coffee-compile-file annoyingly creates a file on disk.
  (let* ((js-file (concat (file-name-sans-extension (buffer-file-name)) ".js"))
         (js-file-existed (file-exists-p js-file)))
    (coffee-compile-file)
    (when (and (not js-file-existed) (file-exists-p js-file))
      (delete-file js-file))))

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

(with-eval-after-load "ruby-mode"
  ;; Ruby mode defines this as "next block". I define it globally as "next window".
  (define-key ruby-mode-map (kbd "C-M-n") nil))

;;
;; Rainbow-delimiters: highlight parentheses in rainbow colors.
;;
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;
;; Smartparens utility functions. Used by emacs lisp and clojure.
;;
(require 'smartparens)

(defun shift-sexp-backward ()
  (interactive)
  (let* ((next (save-excursion (sp-forward-sexp)))
         (prev (save-excursion (goto-char (sp-get next :beg-prf)) (sp-backward-sexp))))
    (sp--transpose-objects prev next))
  ;; Focus the cursor correctly.
  (sp-backward-sexp)
  (sp-backward-sexp))

(defun shift-sexp-forward ()
  (interactive)
  (sp-forward-sexp)
  (let* ((next (save-excursion (sp-forward-sexp)))
         (prev (save-excursion (goto-char (sp-get next :beg-prf)) (sp-backward-sexp))))
    (sp--transpose-objects prev next))
  ;; Focus the cursor correctly.
  (sp-backward-sexp))

;;
;; Clojure
;;
(require 'clojure-mode-personal)
(require 'cider-test-personal)

(evil-leader/set-key-for-mode 'clojure-mode
  ; t is a mnemonic for "test"
  "ta" (lambda ()
         (interactive)
         (with-nrepl-connection-of-current-buffer 'cider-test/run-all-tests))
  "tt" (lambda ()
         (interactive)
         (with-nrepl-connection-of-current-buffer 'cider-test/run-test-at-point))
  "tf" (lambda ()
         (interactive)
         (with-nrepl-connection-of-current-buffer 'cider-test/run-tests-in-ns)))

;;
;; HTML mode
;;
;; html-beautify is used for indentation in these commands. It's here:
;; https://github.com/beautify-web/js-beautify
;; To install: cd ~; npm install js-beautify; add ~/node_modules/.bin to your PATH.
(add-to-list 'auto-mode-alist '("\\.erb$" . html-mode))

(defun preview-html ()
  "Pipes the buffer's contents into a script which opens the HTML in a browser."
  (interactive)
  (call-process-region (point-min) (point-max) "/bin/bash" nil nil nil "-c" "browser"))

(defun indent-html-buffer ()
  "Pipe the current buffer into `html-beautify`, and replace the current buffer's contents."
  (interactive)
  ;; I don't know why, but save-excursion does not maintain the cursor position.
  (let ((p (point))
        (scroll-y (window-start)))
    (call-process-region (point-min) (point-max) "html-beautify" t (buffer-name) t
                         "-f" "-" ; Use STDIN as the input file.
                         "--indent-size" "2"
                         "--wrap-line-length" "110")
    (set-window-start (selected-window) scroll-y)
    (goto-char p)))

(evil-leader/set-key-for-mode 'html-mode
  "i" 'indent-html-buffer
  "rr" 'reload-active-chrome-tab
  "vv" 'preview-html)

(defun reload-active-chrome-tab ()
  "Reloads the current tag in Chrome. This works on OSX only, using Applescript."
  (interactive)
  (util/call-process-with-exit-status "osascript"
                                      "tell app \"Google Chrome\" to reload active tab of window 1"))

;;
;; CSS, LESS mode
;;
(defun indent-css-buffer ()
  "Pipe the current buffer into `css-beautify`, and replace the current buffer's contents."
  (interactive)
  ;; I don't know why, but save-excursion does not maintain the cursor position.
  ;; (save-excursion
  (let ((p (point))
        (scroll-y (window-start)))
    (call-process-region (point-min) (point-max) "css-beautify" t (buffer-name) t
                         "--file" "-" ; STDIN
                         "--indent-size" "2"
                         "--wrap-line-length" "110")
    (set-window-start (selected-window) scroll-y)
    (goto-char p)))

;; `brace-block` is a text object which can be operated on by `thing-at-point`. (thing-at-point 'brace-block)
;; will return all text between and including the set of curly braces surrounding the cursor.
(put 'brace-block 'beginning-op (lambda () (re-search-backward "{")))
(put 'brace-block 'end-op (lambda () (re-search-forward "}")))

(defun toggle-fold-css-block ()
  "Toggles whether a CSS rule is one line or multiple lines."
  (interactive)
  (let* ((text (util/thing-at-point-no-properties 'brace-block))
         (lines (split-string text "\n"))
         (should-expand (= (length lines) 1))
         (new-text
          (if should-expand
              (let ((contents (-> text
                                  (substring 1 -1) ; Remove the enclosing braces.
                                  (split-string "; *")
                                  ((lambda (x) (-map 'chomp x)))
                                  (string/join ";\n"))))
                (concat "{\n" contents "}"))
            ;; else, collapse the CSS block into a single line.
            (-> (-map 'chomp lines)
                (string/join " ")))))
    (util/delete-thing-at-point 'brace-block)
    (insert new-text)
    (when should-expand
      ;; When expanding the CSS to multiple lines, we didn't preserve line indentation, so here we just
      ;; re-indent the paragraph around the cursor. This is a workaround.
      (evil-indent-inside-paragraph))))

(evil-define-key 'normal css-mode-map
  (kbd "A-C-f") 'toggle-fold-css-block)

(evil-leader/set-key-for-mode 'css-mode
  "rr" 'reload-active-chrome-tab)

(evil-leader/set-key-for-mode 'less-css-mode
  "rr" 'reload-active-chrome-tab)

;;
;; SCSS mode, for editing SCSS files.
;;
(setq scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;;
;; mu4e - email & gmail in Emacs.
;; NOTE(philc): This mu4e just doesn't work well; I don't actively use it.
;;
;; (require 'mu4e-mode-personal)

;;
;; notmuch - email & gmail in Emacs.
;; This is an alternative to mu4e. WIP.
;;
(require 'notmuch-ext)

;; Don't keep message buffers around.
(setq message-kill-buffer-on-exit t)

(setq notmuch-poll-script "~/scripts/mail/fetch_email")

;; Settings for composing and sending emails.
;; http://chrisdone.com/posts/emacs-mail
;; Credentials are stored in ~/.authoinfo. See here for the format:
;; http://emacswiki.org/emacs/GnusAuthinfo
;; Note that you need to use a site-specific password if you have Google's two-factor auth enabled.
(setq user-full-name "Phil Crosby")
;; (setq user-mail-address "phil.crosby@gmail.com")
(setq user-mail-address "phil@liftoff.io")
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-stream-type 'ssl)
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 465)
;; TODO(philc): This can be nil. I have a folder here so that draft buffers are backed by disk. Is that
;; necessary?
(setq notmuch-fcc-dirs ".sentmail")

;; Sort messages newest first.
(set 'notmuch-search-oldest-first nil) ; The default is t.

;; The command used for converting plaintext markdown emails into HTML emails.
(setq notmuch-ext/markdown-to-html-command "markdown_page.rb")

(evil-set-initial-state 'notmuch-search-mode 'normal)
(evil-set-initial-state 'notmuch-tree-mode 'normal)
(evil-set-initial-state 'notmuch-hello-mode 'normal)
(evil-set-initial-state 'notmuch-show-mode 'normal)

(evil-define-key 'normal notmuch-search-mode-map
  "o" 'notmuch-search-show-thread-in-other-window
  "q" 'vimlike-quit
  "c" 'notmuch-mua-new-mail
  ;; "t" (fn () (interactive) (print (notmuch-search-get-result) ))
  "t" (fn () (interactive) (print (notmuch-search-find-thread-id t)))
  "T" (fn () (interactive) (notmuch-ext/reply))
  ;; I'm using uppercase Y here to archive, so that I can still copy text from the thread view if I want. Will
  ;; I ever do that, or should I switch this to being lowercase y?
  "Y" 'archive-message
  "D" 'delete-thread
  "r" 'notmuch-reply-to-newest-in-thread
  "R" 'notmuch-reply-all-to-newest-in-thread
  ;; I'm using these custom-scroll functions for page-up and page-down because the built-in ones in Emacs
  ;; switch to the buffer when you try to scroll up past the beginning of the window.
  "u" (fn () (interactive)
        (within-message-view (fn ()
                               (condition-case nil (scroll-down)
                                 (beginning-of-buffer (goto-char (point-min)))))))
  "d" (fn () (interactive)
        (within-message-view (fn ()
                               (condition-case nil (scroll-up)
                                 (end-of-buffer (goto-char (point-max))))))))

(evil-leader/set-key-for-mode 'notmuch-search-mode
  "gli" (fn () (interactive) (notmuch-go-to-inbox))
  "gls" (fn () (interactive) (notmuch-go-to-sent))
  "gl1" (fn () (interactive) (notmuch-go-to-label-1action))
  "t" (fn () (interactive) (print (notmuch-search-find-thread-id)))
  "r" 'notmuch-refresh-this-buffer
  "R" 'notmuch-poll-and-refresh-this-buffer
  "1" (fn () (interactive) (move-thread "1action")))

(evil-define-key 'normal notmuch-show-mode-map
  "r" 'notmuch-reply-to-newest-in-thread
  "R" 'notmuch-reply-all-to-newest-in-thread)

(evil-leader/set-key-for-mode 'notmuch-hello-mode
  "gi" (fn () (interactive) (notmuch-search "tag:inbox")))

(evil-leader/set-key-for-mode 'message-mode ; The compose window.
  "rr" 'notmuch-ext/view-message-in-browser
  "x" (fn () (interactive) (util/without-confirmation 'message-kill-buffer))
  "S" 'message-send-and-exit
  "s" 'notmuch-ext/convert-to-markdown-and-send)

;; Initialize the Emacs message mode for sending emails for use with notmuch-mode.
(defun init-message-mode-settings ()
  ;; Use markdown-mode for composing emails.
  (markdown-lite-mode)
  ;; TODO(philc): Not sure why this is needed, but without it, syntax highlighting isn't updated.
  (markdown-reload-extensions)
  (setq tab-width 2) ; This is set to 4 by message-mode.
  ;; TODO(philc): This mutates all of the evil keybindings in markdown mode. I haven't been able to figure out
  ;; how to limit these changes to the current buffer.
  (util/define-keys
   evil-normal-state-local-map
   ";rr" 'notmuch-ext/view-message-in-browser
   ";x" (fn () (interactive) (util/without-confirmation 'message-kill-buffer))
   ";S" 'message-send-and-exit ; For when you want to send a plaintext message, not markdown.
   ";s" 'notmuch-ext/convert-to-markdown-and-send))

(add-hook 'message-mode-hook 'init-message-mode-settings)

;;
;; YAML mode, for editing YAML files
;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;
;; Go mode, for writing Golang code
;;
(with-eval-after-load "go-mode"
  (evil-define-key 'normal go-mode-map
    "gf" 'godef-jump
    "K" 'godef-describe))

(defun go-save-and-compile-fn (command)
  "Returns a function for the purpose of binding to a key which saves the current buffer and then
   runs the given command in the root of the go project."
  (lexical-let ((command command))
    #'(lambda ()
        (interactive)
        (go-save-and-compile command))))

;; Note that this function uses (projectile-project-root) to determine the directory to run `go` commands,
;; which requires that the go project have a .projectile file in it or that it be at the root of a git repo.
(defun go-save-and-compile (command)
  "Saves the current buffer before invoking the given command."
  (lexical-let ((has-makefile (file-exists-p (concat (projectile-project-root) "Makefile"))))
    (save-buffer)
    (message command)
    (util/without-confirmation
     (lambda () (compile (concat "cd " (projectile-project-root) " && " command))))))

(evil-leader/set-key-for-mode 'go-mode
  ;; "r" is a namespace for run-related commands.
  "rr" (go-save-and-compile-fn "make run")
  "rb" (go-save-and-compile-fn "make synthetic-benchmark")
  "rt" (go-save-and-compile-fn "make test")
  "rw" (go-save-and-compile-fn "make run-web")
  ;; "c" is a namespace for compile-related commands.
  "cn" 'next-error
  "cp" 'previous-error
  "cw" (go-save-and-compile-fn "make web")
  "cb" (go-save-and-compile-fn "make benchmark")
  "cc" (go-save-and-compile-fn "go build")

  "ai" 'go-import-add)

;; goimports formats your code and also adds or removes imports as needed.
;; goimports needs to be on your path. See https://godoc.org/code.google.com/p/go.tools/cmd/goimports
(setq gofmt-command "goimports")

(setq gofmt-in-progress nil)

(defun gofmt-before-save-ignoring-errors ()
  "Don't pop up syntax errors in a new window when running gofmt-before-save."
  (interactive)
  ;; Note that `gofmt-before-save` triggers this save-hook for some reason, so we lock on gmt-in-progress to
  ;; to protect from infinite recurision.
  (when (not gofmt-in-progress)
    (setq gofmt-in-progress 't)
    (flet ((gofmt--process-errors (&rest args) t)) ; Don't show any syntax error output
      (gofmt-before-save))
    (setq gofmt-in-progress nil)))

(defun init-go-buffer-settings ()
  ;; I have Emacs configured to save when switching buffers, so popping up errors when I switch buffers is
  ;; really jarring.
  (add-hook 'before-save-hook 'gofmt-before-save-ignoring-errors nil t)
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
;; Magit - for staging hunks and making commits to git from within Emacs.
;;
(require 'magit-mode-personal)

;;
;; Project navigation (my own functions on top of dired-mode and projectile)
;;
(setq project-folders '("~/p" "~/src/liftoff"))
(setq notes-directories '("~/personal/notes" "~/Desktop" "~/Dropbox/scratch"))
(setq notes-file-extensions '(".md" ".sql" ".txt"))

;; This is set to 600 by default. It shouldn't be the case, but for some reason, the filter-files-in-directory
;; function hits this limit.
(setq max-lisp-eval-depth 1200)

(defun filter-files-in-directory (directory filter-fn include-subdirectories)
  "Filters the files in the given directory and subdirectories using filter-fn. Excludes .git subdirectories."
  (->> (directory-files directory t)
       (--remove (or (string/ends-with it ".")
                     (string/ends-with it "..")
                     (string/ends-with it ".git")))
       (--map (if (and include-subdirectories (file-directory-p it))
                  (filter-files-in-directory it filter-fn include-subdirectories)
                it))
       flatten
       (-filter filter-fn)))

(defun open-markdown-file-from-notes-folder ()
  "Prompts for the name of a .md notes file to open."
  (interactive)
  (let* ((file-matches-pattern? (lambda (file)
                                  (some (lambda (ext) (string/ends-with file ext)) notes-file-extensions)))
         (file-list (->> notes-directories
                         (--map (filter-files-in-directory it file-matches-pattern? t))
                         flatten)))
    (let ((file-to-open (ido-completing-read "Notes file: " (mapcar 'file-name-nondirectory file-list))))
      (->> file-list
           (--filter (string/ends-with it (concat "/" file-to-open)))
           first
           find-file))))

(defun open-root-of-project (project-path)
  "Opens the project at path. If it's a clojure project, find the project's 'main' file and open that.
   Otherwise, used dired to open the file in `path`."
  ;; NOTE(philc): This function opens the "main" files in the project types that I typically work on.
  ;; Customzie this to meet your needs if you want this functionality.
  (let* ((project-name (file-name-nondirectory project-path))
         (is-clojure (file-exists-p (concat project-path "/project.clj")))
         (main-file (when is-clojure
                      (->> ["core.clj" "handler.clj"]
                           (--map (concat project-path "/src/" project-name "/" it))
                           (remove-if-not 'file-exists-p)
                           first))))
    (if main-file
        (set-window-buffer (selected-window) (find-file main-file))
      (dired project-path))))

(defun navigate-to-project ()
  "Prompts for the name of a project which exists in your common project folders and opens a dired window in
   the root of the project folder. This is a fast way to open a new project and be able to run
   projectile-file-file.
   Once a project is chosen, the current elscreen-tab is set to be the name of that project."
  (interactive)
  (let* ((all-project-folders (->> project-folders
                                   (--map (filter-files-in-directory it 'file-directory-p nil))
                                   flatten))
         (project-to-open (ido-completing-read "Project folder: "
                                               (-map 'file-name-nondirectory all-project-folders)
                                               nil t))
         (project (->> all-project-folders
                       (--filter (string/ends-with it (concat "/" project-to-open)))
                       first)))
    (open-root-of-project project)
    ;; If we invoke this inside of a split, don't set the tab's title.
    (when (= 1 (length (window-list)))
      (escreen-set-tab-alias (file-name-nondirectory project)))))

;;
;; JSON
;;
(defun json-format ()
  "Pipe the current buffer into `jq .`, and replace the current buffer's contents."
  (interactive)
  (save-excursion
    (call-process-region (point-min) (point-max) "jq" t (buffer-name) t ".")))

;; TODO(philc): bind json-format to "leader-i"

;;
;; Java
;;
(add-hook 'java-mode-hook (lambda () (setq c-basic-offset 2)))

;; TODO(philc): It would be nice to parameterize this further and combine it with go-save-and-compile-fn.
(defun java-save-and-compile-fn (command-name)
  "Returns a function for the purpose of binding to a key which saves the current buffer and then
   runs the given command in the root of the go project."
  (lexical-let ((command-name command-name))
    #'(lambda ()
        (interactive)
        (save-buffer)
        (message command-name)
        (util/without-confirmation
         (lambda ()
           (compile (concat "cd " (locate-dominating-file (buffer-file-name) "build.xml")
                            " && " command-name)))))))

(evil-leader/set-key-for-mode 'java-mode
  ;; ant -find searches up the directory tree and finds the closest build file.
  "cc" (java-save-and-compile-fn "ant debug -silent")
  "cn" 'next-error
  "cp" 'previous-error)

;;
;; Javascript
;;
(setq js-indent-level 2)

;;
;; Ag (silver searcher)
;;
;; Note that ag mode configures itself to start in Evil's "motion" state.
;; compile-goto-error is what the "Return" key does.
(evil-define-key 'motion ag-mode-map "o" 'compile-goto-error)

;;
;; Terminal (multi-term mode)
;;
;; (require 'multi-term-personal) ; Currently disabled; this doesn't work well.

;;
;; Misc
;;

;; I just invoke this by name using M-x.
(defun prompt-to-open-info-page ()
  "Prompts you for the name of an info page to view. It's the same as calling info with a prefix argument
   ala C-u C-h i using the regular Emacs key bindings."
  (interactive)
  (setq current-prefix-arg '(4)) ; Sets the prefix to C-u
  (call-interactively 'info))

(add-to-list 'auto-mode-alist '("\\.mustache$" . mustache-mode))

(defun open-in-chrome (url)
  (call-process "open" nil nil nil url "-a" "Google Chrome"))

;; Taken from http://ergoemacs.org/emacs/emacs_open_file_path_fast.html.
(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
   If there is text selection, uses the text selection for path.
   If the path starts with “http://”, open the URL in browser.
   Input path can be {relative, full path, URL}.
   Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
   If path does not have a file extention, automatically try with “.el” for elisp files.
   This command is similar to `find-file-at-point' but without prompting for confirmation.
   URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'"
  (interactive)
  (let ((ξpath (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let (p0 p1 p2)
                   (setq p0 (point))
                   ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                   (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
                   (setq p1 (point))
                   (goto-char p0)
                   (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
                   (setq p2 (point))
                   (goto-char p0)
                   (buffer-substring-no-properties p1 p2)))))
    (if (string-match-p "\\`https?://" ξpath)
        (open-in-chrome ξpath)
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" ξpath)
            (progn
              (let (
                    (ξfpath (match-string 1 ξpath))
                    (ξline-num (string-to-number (match-string 2 ξpath))))
                (if (file-exists-p ξfpath)
                    (progn
                      (find-file ξfpath)
                      (goto-char 1)
                      (forward-line (1- ξline-num)))
                  (progn
                    (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξfpath))
                      (find-file ξfpath))))))
          (progn
            (if (file-exists-p ξpath)
                (find-file ξpath)
              (if (file-exists-p (concat ξpath ".el"))
                  (find-file (concat ξpath ".el"))
                (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξpath))
                  (find-file ξpath ))))))))))
