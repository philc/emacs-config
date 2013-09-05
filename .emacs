;;
;; Package management
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(clojure-mode
                      clojure-test-mode
                      evil
                      evil-leader
                      evil-nerd-commenter
                      flx ; Fuzzy matching for ido, which improves the UX of Projectile.
                      ido-ubiquitous ; Make ido completions work everywhere.
                      ido-vertical-mode ; Show ido results vertically.
                      markdown-mode
                      midje-mode
                      powerline ; Improve the appearance & density of the Emacs status bar.
                      projectile ; Find file in project (ala CTRL-P).
                      yasnippet
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;
;; General
;;
;; Turn off graphical toolbars.
(require 'cl)
(if (display-graphic-p) (menu-bar-mode 1) (menu-bar-mode -1))
(when (and (fboundp 'tool-bar-mode) tool-bar-mode) (tool-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) scroll-bar-mode) (scroll-bar-mode -1))

(setq initial-scratch-message "") ; When opening a new buffer, don't show the scratch message.

(global-auto-revert-mode t) ; Reload an open file from disk if it is changed outside of Emacs.

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)
(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'meta)
; Put backup files in your home directory, out of the way.
(setq backup-directory-alist `(("." . "~/.backups")))
(setq vc-follow-symlinks t) ; Don't ask confirmation to follow symlinks to edit files.

(savehist-mode t) ; Save your minibuffer history across Emacs sessions. UX win!

(setq text-scale-mode-step 1.1) ;; When changing font size, change in small increments.

;; Start scrolling the window when the cursor reaches its edge.
;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t
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

(setq-default fill-column 110) ; When wrapping with the Emacs fill commands, wrap at 110 chars.
(auto-fill-mode t) ; When typing across the fill-column, hard-wrap the line as you type.
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; Some modes, like markdown, turn off autofill. Force it!
; Visually wrap long lines on word boundaries. By default, Emacs will wrap mid-word. Note that Evil doesn't
; have good support for moving between visual lines versus logical lines. Here's the start of a solution:
;; https://lists.ourproject.org/pipermail/implementations-list/2011-December/001430.html
(global-visual-line-mode t)

;; Don't use tabs by default. Modes that really need tabs should enable indent-tabs-mode explicitly.
;; Makefile-mode already does that, for example. If indent-tabs-mode is off, untabify before saving.
(setq-default indent-tabs-mode nil)
(add-hook 'write-file-hooks
          (lambda ()
            (if (not indent-tabs-mode)
                (untabify (point-min) (point-max)))
            nil))

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

;; By default, Emacs will not indent when you hit enter/return within a comment.
(define-key evil-insert-state-map (kbd "RET") 'comment-indent-new-line)

(evil-leader/set-key
  "h" 'help
  "b" 'ido-switch-buffer
  "t" 'projectile-find-file
  "q" 'fill-paragraph
  "a" 'projectile-ack
  "d" 'projectile-dired
  ; Shift-J is usually join line in Vim. I use Shift-J and K for tab switching.
  "j" (lambda () (interactive) (join-line t))
  "ee" 'open-emacs-config
  "eb" 'eval-buffer
  "es" 'eval-last-sexp
  "ex" 'eval-surrounding-sexp
  ; "v" is a mnemonic prefix for "view X".
  "vo" (lambda () (interactive) (find-file "~/Dropbox/tasks.org"))
  "ve" (lambda () (interactive) (find-file "~/.emacs")))

(eval-after-load 'evil
  '(progn (setq evil-leader/leader ";")))
     ;; Unbind these keys in evil so they can instead be used for code navigation.
     ;; TODO(philc): Will I need these?
     ; (define-key evil-normal-state-map (kbd "M-,") nil)
     ; (define-key evil-normal-state-map (kbd "M-.") nil)))

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

;; Moving between Emacs windows (splits).
;; If you want to have directional keys for switching windows, bind them to windmove-down, windmove-left, etc.
(global-set-key (kbd "M-C-n") 'other-window)

;; Commenting via NERD commentor.
(define-key evil-normal-state-map "," 'evilnc-comment-operator)
(define-key evil-visual-state-map "," 'evilnc-comment-operator)

;; Make it so Esc means quit, no matter the context.
;; http://stackoverflow.com/a/10166400/46237
;; Note that when Emacs becomes unresponsive (e.g. because I accidentally grepped my home directory), I might
;; still need to hold C-g (the Emacs ESC key) to bring it back.
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
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;;
;; OS X keybindings minor mode.
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
(define-key osx-keys-minor-mode-map (kbd "M-v") 'clipboard-yank)
(define-key osx-keys-minor-mode-map (kbd "M-c") 'clipboard-kill-ring-save)
(define-key osx-keys-minor-mode-map (kbd "M--") 'text-scale-decrease)
(define-key osx-keys-minor-mode-map (kbd "M-=") 'text-scale-increase)
(define-key osx-keys-minor-mode-map (kbd "M-0") (lambda () (interactive) (text-scale-increase 0)))

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
  (org-occur "\\(TODO\\|DONE\\)")
  ; org-occur highlights every TODO and DONE string match in the doc, which is distracting. Remove it.
  (org-remove-occur-highlights))

;;
;; Projectile (find file from the root of the current project).
;;
(projectile-global-mode)

;;
;; elscreen (tabs on the window).
;;
(elscreen-start)
(define-key evil-normal-state-map (kbd "<A-M-left>") 'elscreen-previous) ; Spark translates M-J to these keys.
(define-key evil-normal-state-map (kbd "<A-M-right>") 'elscreen-next)
(define-key evil-normal-state-map (kbd "M-t") 'elscreen-create)

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
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;; Disable showing messages for performance reasons, as suggested by http://www.emacswiki.org/emacs/FlySpell.
(setq flyspell-issue-message-flag nil)

;;
;; Powerline: improve the appearance & density of the Emacs status bar.
;;
(require 'powerline)
(powerline-default-theme)

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

;;
;; Clojure
;;
;; Count hyphens, etc. as word characters in lisps
(add-hook 'clojure-mode-hook (lambda () (modify-syntax-entry ?- "w")))
(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))
