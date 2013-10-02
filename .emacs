;;
;; Package management
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(autopair ; Insert matching delimiters, e.g. insert closing braces.
                      clojure-mode
                      clojure-test-mode
                      coffee-mode ; For syntax highlighting coffeescript.
                      evil
                      evil-leader
                      evil-nerd-commenter
                      flx ; Fuzzy matching for ido, which improves the UX of Projectile.
                      less-css-mode ; Syntax highlighting for LESS CSS files.
                      ido-ubiquitous ; Make ido completions work everywhere.
                      ido-vertical-mode ; Show ido results vertically.
                      markdown-mode
                      midje-mode
                      nrepl
                      powerline ; Improve the appearance & density of the Emacs status bar.
                      projectile ; Find file in project (ala CTRL-P).
                      rainbow-delimiters ; Highlight parentheses in rainbow colors.
                      ruby-electric ; Insert matching delimiters; unindent end blocks after you type them.
                      yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;
;; General
;;
(require 'cl)

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

(global-auto-revert-mode t) ; Reload an open file from disk if it is changed outside of Emacs.

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)
(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'meta)

;; Put backup and autosave files in your home directory, out of the way.
;; http://www.emacswiki.org/emacs/AutoSave
(setq backup-directory-alist `((".*" . "~/.backups")))
(setq auto-save-file-name-transforms `((".*" "~/.backups" t)))

(setq vc-follow-symlinks t) ; Don't ask confirmation to follow symlinks to edit files.

(savehist-mode t) ; Save your minibuffer history across Emacs sessions. UX win!

(setq text-scale-mode-step 1.1) ;; When changing font size, change in small increments.

;; Include path information in duplicate buffer names (e.g. a/foo.txt b/foo.txt)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Start scrolling the window when the cursor reaches its edge.
;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq
  scroll-margin 5
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

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
     (setq whitespace-style '(face trailing tabs tab-mark lines-tail))))
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

;; Creating window splits.
(setq split-height-threshold 40)
(setq split-width-threshold 200)
(setq split-window-preferred-function 'split-window-sensibly-reverse)

;;
;; Evil mode -- Vim keybindings for Emacs.
;;
(setq evil-want-C-u-scroll t)
(require 'evil)
(require 'evil-leader) ; Provide configuration functions for assigning actions to a Vim leader key.
(require 'evil-nerd-commenter)
(evil-mode t)
(global-evil-leader-mode)

;; When opening new lines, indent according to the previous line.
(setq evil-auto-indent t)

;; Unbind "q" so it doesn't record macros. I activate this mistakenly all the time and wreak havoc.
(define-key evil-normal-state-map (kbd "q") nil)

;; Move up and down through long, wrapped lines one visual line at a time.
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "K") 'info-lookup-symbol)

;; By default, Emacs will not indent when you hit enter/return within a comment.
(define-key evil-insert-state-map (kbd "RET") 'comment-indent-new-line)

(evil-leader/set-key
  "h" 'help
  "b" 'ido-switch-buffer
  "t" 'projectile-find-file
  "q" 'evil-fill-around-paragraph
  "a" 'projectile-ack
  "d" 'projectile-dired
  ; "v" is a mnemonic prefix for "view X".
  "vg" 'mu4e
  "vo" (lambda () (interactive) (find-file "~/Dropbox/tasks.org"))
  "ve" (lambda () (interactive) (find-file "~/.emacs")))

(eval-after-load 'evil
  '(progn (setq evil-leader/leader ";")))
     ;; Unbind these keys in evil so they can instead be used for code navigation.
     ;; TODO(philc): Will I need these?
     ; (define-key evil-normal-state-map (kbd "M-,") nil)
     ; (define-key evil-normal-state-map (kbd "M-.") nil)))

(defun evil-fill-around-paragraph (beg end)
  "Fills (reflows/linewraps) the current paragraph. Equivalent to gqap in view."
  (interactive "r")
  (let ((region (evil-a-paragraph)))
    (evil-fill-and-move (first region) (second region))))

(defun eval-surrounding-sexp (levels)
  (interactive "p")
  (save-excursion
    (up-list (abs levels))
    (eval-last-sexp nil)))

(defun backward-kill-line (arg)
  "Delete backward (Ctrl-u) as in Bash."
  (interactive "p")
  (kill-line (- 1 arg)))

;; Enable Emacs/Bash insert-mode keybindings.
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-u") 'backward-kill-line)
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
(global-set-key (kbd "C-h") 'backward-delete-char) ; Here we clobber C-h, which accesses Emacs's help.

;; Window switching
;; Evil's window map is the set of keys which control window functions. All of its keys must be prefixed
;; by <C-w>.
(define-key evil-window-map (kbd "x") 'delete-window)

;; Commenting via NERD commentor.
(define-key evil-normal-state-map "," 'evilnc-comment-operator)
(define-key evil-visual-state-map "," 'evilnc-comment-operator)

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
(define-key osx-keys-minor-mode-map (kbd "M-s") 'save-buffer)
(define-key osx-keys-minor-mode-map (kbd "M-a") 'mark-whole-buffer)
(define-key osx-keys-minor-mode-map (kbd "M-v") 'clipboard-yank)
(define-key osx-keys-minor-mode-map (kbd "M-c") 'clipboard-kill-ring-save)
(define-key osx-keys-minor-mode-map (kbd "M-m") 'iconify-or-deiconify-frame)
(define-key osx-keys-minor-mode-map (kbd "M-W") 'evil-quit) ; Close all tabs in the current frame..
(define-key osx-keys-minor-mode-map (kbd "M--") 'text-scale-decrease)
(define-key osx-keys-minor-mode-map (kbd "M-=") 'text-scale-increase)
(define-key osx-keys-minor-mode-map (kbd "M-0") (lambda () (interactive) (text-scale-increase 0)))

;; These aren't specifically replicating OSX shortcuts, but they manipulate the window, so I want them to take
;; precedence over everything else.
;; Moving between Emacs windows (splits).
(define-key osx-keys-minor-mode-map (kbd "M-C-n") 'other-window)
(define-key osx-keys-minor-mode-map (kbd "M-1") 'switch-to-window-1)
(define-key osx-keys-minor-mode-map (kbd "M-2") 'switch-to-window-2)
(define-key osx-keys-minor-mode-map (kbd "M-3") 'switch-to-window-3)
(define-key osx-keys-minor-mode-map (kbd "M-4") 'switch-to-window-4)

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

;; These switch-to-window functions jump to a numbered window on-screen. They assume my convential window
;; layout, which is either a single window on the left and 2 splits on the right, or 4 splits in a 2x2 grid.
(defun switch-to-window-1 ()
  (interactive)
  (condition-case nil (windmove-up) (error nil))
  (condition-case nil (windmove-left) (error nil)))

(defun switch-to-window-2 ()
  (interactive)
  (call-interactively 'switch-to-window-1)
  (condition-case nil (windmove-down) (error (windmove-right 1))))

(defun switch-to-window-3 ()
  (interactive)
  (call-interactively 'switch-to-window-1)
  (condition-case nil
      (progn
        (windmove-down)
        (windmove-up)
        (windmove-right 1))
    (error (progn (windmove-right 1)
                  (windmove-down)))))

(defun switch-to-window-4 ()
  (interactive)
  (call-interactively 'switch-to-window-1)
  (windmove-right)
  (windmove-down))

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
;; Org mode, for TODOs and note taking.
;;
(require 'org)
; NOTE(philc): I've modified this evil-org-mode file to provide my desired shortcuts.
(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
(require 'evil-org)
(eval-after-load 'org
  '(progn
     ; This enables "clean mode", such that sublists use whitespace for indentation (ala markdown) instead of
     ; many stars.
     (setq org-startup-indented t)))

;; Moves the current heading (and all of its children) into the matching parent note in the archive file.
;; I think this is the most sensible way to archive TODOs in org mode files.
;; http://orgmode.org/worg/org-hacks.html
(defadvice org-archive-subtree (around my-org-archive-subtree activate)
  (let ((org-archive-location
         (if (save-excursion (org-back-to-heading)
                             (> (org-outline-level) 1))
             (concat (car (split-string org-archive-location "::"))
                     "::* "
                     (car (org-get-outline-path)))
           org-archive-location)))
    ad-do-it))

(defun org-show-todo-and-done-tree ()
  "Shows only subtrees which are TODOs or DONE items. Similar to org-show-todo-tree, but it matches DONE items
   as well."
  (interactive)
  ;; Note that these tags are case insensitive.
  (org-occur "\\(TODO\\|DONE\\|INPROGRESS\\|WAITING\\)")
  ;; org-occur highlights every TODO and DONE string match in the doc, which is distracting. Remove it.
  (org-remove-occur-highlights))

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
;; I'm using elscreen-clone here instead of elscreen-create so that the new tab has the current directory set
;; properly, so projectile can be used immediately.
(define-key evil-normal-state-map (kbd "M-t") 'elscreen-clone)
(define-key evil-insert-state-map (kbd "M-t") '(lambda ()
                                                 ;; Exit out of insert mode when opening a new tab.
                                                 (interactive)
                                                 (evil-change-to-initial-state)
                                                 (elscreen-clone)))

;; Make it so M-1 selects the first tab, etc.
(dolist (i (number-sequence 1 9))
  (lexical-let ((tab-index (- i 1)))
    (global-set-key (kbd (concat "M-" (number-to-string i)))
                    (lambda () (interactive) (elscreen-goto tab-index)))))
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
    (evil-define-key 'insert markdown-mode-map
      (kbd "<C-return>") 'markdown-insert-list-item-below)
    (evil-define-key 'normal markdown-mode-map
      ; Autocomplete setext headers by typing "==" or "--" on the header's line in normal mode.
      (kbd "==") '(lambda () (interactive) (insert-markdown-header "=="))
      (kbd "--") '(lambda () (interactive) (insert-markdown-header "--"))
      (kbd "<C-return>") 'markdown-insert-list-item-below)))

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
;; Snippets
;;
;; Ignore the default snippets that come with yasnippet. I only need my own, and don't want any conflicts.
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(require 'yasnippet)
(yas-global-mode 1)

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
;; http://www.emacswiki.org/emacs/FlySpell.
;;
;; You may need to install aspell (e.g. `brew install aspell`) to use it via Flyspell.
(setq ispell-program-name "/usr/local/bin/aspell" ispell-extra-args '("--sug-mode=ultra"))

;; Highlight all mispellings when a file is opened. Flyspell only spellchecks words you modify.
;; I would like to enable this for programming modes (by running (flyspell-prog-mode) in a prog-mode-hook,
;; but it visibly slows down how quickly my can move through lines.
(add-hook 'text-mode-hook (lambda ()
                            (flyspell-mode)
                            (flyspell-buffer)))

 ;; Triggers a spell-correction menu. I use this to add words to my dictionary (hit "i").
(define-key evil-normal-state-map (kbd "zg") 'ispell-word)

;; Disable showing messages for performance reasons, as suggested by http://www.emacswiki.org/emacs/FlySpell.
(setq flyspell-issue-message-flag nil)
(setq ispell-personal-dictionary "~/.personal_dict.txt")

;;
;; Powerline: improve the appearance & density of the Emacs status bar.
;;
(require 'powerline)
(powerline-default-theme)

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

;; Insert matching delimiters; unindent end blocks after you type them.
(add-hook 'ruby-mode (lambda () (ruby-electric)))

;;
;; Emacs Lisp (elisp)
;;
(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))
(evil-define-key 'normal emacs-lisp-mode-map "K" '(lambda ()
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
  "eb" 'eval-buffer
  "es" 'elisp-eval-current-sexp
  "ex" 'eval-defun)

;;
;; Clojure
;;
;; Docs:
;; https://github.com/clojure-emacs/nrepl.el
;; http://clojure-doc.org/articles/tutorials/emacs.html

;; Count hyphens, etc. as word characters in lisps
(add-hook 'clojure-mode-hook (lambda () (modify-syntax-entry ?- "w")))
(add-hook 'clojure-mode-hook (lambda ()
                               (setq indent-line-function 'lisp-indent-line-single-semicolon-fix)
                               ;; Comment lines using only one semi-colon instead of two.
                               (setq comment-add 0)))

(evil-define-key 'normal clojure-mode-map "K" 'nrepl-doc)
(evil-define-key 'normal clojure-mode-map "gf" 'nrepl-jump)

;; Hide the uninteresting nrepl-connection and nrepl-server buffers from the buffer list.
(setq nrepl-hide-special-buffers t)

;; Don't ask confirmation for closing any open nrepl connections when exiting Emacs.
;; http://stackoverflow.com/q/2706527/46237
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(evil-define-operator evil-nrepl-eval (beg end)
  "Evaluate the text region moved over by an evil motion."
  (nrepl-eval-region beg end))

;; Eval a paragraph. This is different from eval-surrounding-sexp in that it will eval multiple adjacent
;; s-expressions which are not separated by a new line. It's equivalent to wrapping the expressions in a do.
(defun nrepl-eval-paragraph (beg end)
  (interactive "r")
  (let ((region (evil-a-paragraph)))
    (evil-nrepl-eval (first region) (second region))))

(defun nrepl-show-nrepl-buffer ()
  (interactive)
  "Shows the nrepl buffer, but does not focus it."
  (command-execute 'nrepl-switch-to-repl-buffer)
  (command-execute 'nrepl-switch-to-last-clojure-buffer))

(defun nrepl-clear-buffer-inside-nrepl-buffer ()
  (interactive)
  (command-execute 'nrepl-switch-to-repl-buffer)
  (nrepl-clear-buffer)
  (command-execute 'nrepl-switch-to-last-clojure-buffer))

;; Disable prompt on killing buffer with a process
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(defun nrepl-restart ()
  "Restarts or starts afresh the nrepl."
  (interactive)
  (nrepl-quit)
  (nrepl-jack-in nil))

;; Enable eldoc integration in buffers
(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)

(defun nrepl-eval-current-sexp ()
  "Eval the sexp the current is currently in. In Emacs' syntax table, this is called a list of expressions."
  (interactive)
  (nrepl-interactive-eval (current-sexp)))

;; The all-important nREPL eval shortcuts.
(evil-leader/set-key-for-mode 'clojure-mode
  "eap" 'nrepl-eval-paragraph
  "eb" 'nrepl-load-current-buffer
  "ee" 'nrepl-show-nrepl-buffer
  "ek" 'nrepl-clear-buffer-inside-nrepl-buffer
  ; nrepl-restart is more handy than nrepl-jack-in, because it doesn't leave existing repls running.
  "en" 'nrepl-restart
  "es" 'nrepl-eval-current-sexp
  "ex" 'nrepl-eval-expression-at-point
  "er" 'nrepl-eval-region)

;; Highlight parentheses in rainbow colors.
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

;; Clojure indentation rules
(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (send-off 1)                                              ; Core
     (GET 2) (POST 2) (PUT 2) (PATCH 2) (DELETE 2) (context 2) ; Compojure
     (where 1) (set-fields 1) (values 1) (delete 1) (upsert 1)    ; Korma cont
     (clone-for 1)                                             ; Enlive
     (up 1) (down 1) (alter 1) (table 1)                       ; Lobos
     (with-eligible-values 1) (when-eligible 1)                ; Personal
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
       "n" 'mu4e-view-headers-next
       "p" 'mu4e-view-headers-prev
       "#" 'mu4e-view-mark-for-trash
       "y" 'mu4e-view-mark-for-refile
       "/" 'mu4e-view-search-edit
       "x" 'mu4e-view-mark-for-something
       "z" 'mu4e-view-mark-for-unmark
       "q" 'vimlike-quit
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
       (kbd "M-r") 'mu4e-update-mail-and-index
       "c" 'mu4e-compose-new)


     (evil-make-overriding-map mu4e-compose-mode-map 'normal t)
     (evil-define-key 'normal mu4e-compose-mode-map
       "c" nil)))

(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-stream-type 'ssl)
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 465)
