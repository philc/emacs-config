;;; melange-theme.el --- A warm, lower-contrast dark theme.  -*- lexical-binding: t; -*-

;; Colors from Melange (https://github.com/savq/melange-nvim), which matches the Melange Dark theme
;; in Ghostty.

;;; Code:

(deftheme melange
  "A warm, lower-contrast dark theme, based on the Melange palette.")

;; NOTE(philc): After modifying this file, eval (load-theme 'melange t) to see live changes.

(let (;; Grays, from darkest to lightest.
      (bg "#292522")
      (bg-float "#34302c") ; Slightly lighter than bg, for the mode line and minibuffer.
      (bg-sel "#403a36")
      ;; The selection. It's much lighter than the current line, so that it's visible over it.
      (bg-region "#5e4229")
      (ui "#867462") ; Borders, line numbers, and other UI that should recede.
      (comment "#c1a78e")
      (fg "#f4ebe3")
      ;; Muted colors, the terminal's normal ANSI colors.
      (red "#bd8183")
      (orange "#e49b5d")
      (green "#78997a")
      (cyan "#7b9695")
      (blue "#7f91b2")
      (magenta "#b380b0")
      ;; Brighter colors, the terminal's bright ANSI colors.
      (red-bright "#d47766")
      (yellow-bright "#ebc06d")
      (green-bright "#85b695")
      (cyan-bright "#89b3b6")
      (blue-bright "#a3a9ce")
      (magenta-bright "#cf9bc2")
      ;; Diff backgrounds. Lines are faint, and the changed words within them a little stronger.
      (added "#2a3328")
      (added-highlight "#2f392d")
      (added-word "#3c4c38")
      (removed "#3b2a28")
      (removed-highlight "#42302d")
      (removed-word "#553633"))
  (custom-theme-set-faces
   'melange
   ;; Source Code Pro gets me 3 more lines on screen than Monaco.
   `(default
     ((t
       (:family
        "Source Code Pro"
        :foundry "unknown"
        :width normal
        :height 140 ; 14 pt
        :weight normal
        :slant normal
        :foreground ,fg
        :background ,bg))))
   `(cursor ((t (:background ,yellow-bright))))
   `(fixed-pitch ((t (:inherit (default)))))
   `(variable-pitch ((t (:family "Sans Serif"))))
   `(highlight ((t (:background ,bg-float))))
   ;; The current line. It's a little lighter than `highlight`, so it's easier to find.
   `(hl-line ((t (:background "#3b3631"))))
   `(region ((t (:background ,bg-region))))
   `(secondary-selection ((t (:background ,bg-sel))))
   `(shadow ((t (:foreground ,ui))))
   `(vertical-border ((t (:foreground ,ui))))
   `(fringe ((t (:background ,bg))))
   `(line-number ((t (:foreground ,ui))))
   `(line-number-current-line ((t (:foreground ,comment))))
   `(whitespace-trailing ((t (:background ,removed-word))))
   `(minibuffer-prompt ((t (:foreground ,blue-bright :bold t))))
   `(link ((t (:foreground ,blue-bright :underline t))))
   `(link-visited ((t (:foreground ,magenta-bright :underline t))))
   `(button ((t (:inherit (link)))))
   `(error ((t (:foreground ,red-bright))))
   `(warning ((t (:foreground ,orange))))
   `(success ((t (:foreground ,green-bright))))
   `(tooltip ((t (:background ,bg-float :foreground ,fg))))

   ;; Syntax. Variables and plain identifiers stay the default color, so that fewer things are
   ;; colored and each color means something.
   `(font-lock-builtin-face ((t (:foreground ,cyan-bright))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
   `(font-lock-doc-face ((t (:foreground ,comment))))
   `(font-lock-constant-face ((t (:foreground ,magenta-bright))))
   `(font-lock-function-name-face ((t (:foreground ,yellow-bright))))
   `(font-lock-keyword-face ((t (:foreground ,orange))))
   `(font-lock-negation-char-face ((t nil)))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-string-face ((t (:foreground ,blue-bright))))
   `(font-lock-type-face ((t (:foreground ,cyan-bright))))
   `(font-lock-variable-name-face ((t (:foreground ,fg))))
   `(font-lock-warning-face ((t (:foreground ,orange))))

   ;; Mode line and tab bar.
   `(mode-line ((t (:box (:line-width 1 :color ,bg-sel) :background ,bg-sel :foreground ,fg))))
   `(mode-line-inactive ((t (:box (:line-width 1 :color ,bg-float) :background ,bg-float
                                  :foreground ,ui))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(header-line ((t (:background ,bg-float :foreground ,fg))))
   `(tab-bar ((t (:inherit mode-line :foreground ,ui :background ,bg))))
   `(tab-bar-tab ((t (:inherit tab-bar :foreground ,fg :background ,bg-sel))))
   `(tab-bar-tab-inactive ((t (:inherit tab-bar :foreground ,comment :background ,bg-float))))

   ;; Search and matching.
   `(isearch ((t (:foreground ,bg :background ,orange))))
   `(isearch-fail ((t (:background ,removed-word))))
   `(lazy-highlight ((t (:foreground ,bg :background ,comment))))
   `(match ((t (:foreground ,bg :background ,comment))))
   `(query-replace ((t (:inherit (isearch)))))
   `(show-paren-match ((t (:background ,ui :foreground ,fg))))
   `(show-paren-mismatch ((t (:background ,removed-word :foreground ,fg))))

   ;; markdown-lite-mode's list items. Only top-level items are colored, since they're the sections
   ;; of a document. Deeper items are plain, with a muted "*" marker which stands apart from their
   ;; text.
   `(mlm-list-item-1 ((t (:foreground ,yellow-bright))))
   `(mlm-list-item-2 ((t (:foreground ,fg))))
   `(mlm-list-item-3 ((t (:foreground ,fg))))
   `(mlm-list-item-4 ((t (:foreground ,fg))))
   `(mlm-list-item-5 ((t (:foreground ,fg))))
   `(mlm-list-item-6 ((t (:foreground ,fg))))
   `(mlm-list-marker-1 ((t (:foreground ,yellow-bright))))
   `(mlm-list-marker-2 ((t (:foreground ,blue-bright))))
   `(mlm-list-marker-3 ((t (:foreground ,blue-bright))))
   `(mlm-list-marker-4 ((t (:foreground ,blue-bright))))
   `(mlm-list-marker-5 ((t (:foreground ,blue-bright))))
   `(mlm-list-marker-6 ((t (:foreground ,blue-bright))))
   ;; Todo list items: a done item and the in progress and waiting markers recede, the markers more.
   `(mlm-todo-done ((t (:foreground ,comment))))
   `(mtodo-status-marker ((t (:foreground ,ui))))

   ;; Diffs, in Magit and diff-mode. Lines get a faint background so that syntax colors stay
   ;; readable, and changed words get a stronger one.
   `(magit-diff-added ((t (:extend t :background ,added))))
   `(magit-diff-added-highlight ((t (:extend t :background ,added-highlight))))
   `(magit-diff-removed ((t (:extend t :background ,removed))))
   `(magit-diff-removed-highlight ((t (:extend t :background ,removed-highlight))))
   `(magit-diff-context ((t (:foreground ,comment))))
   `(magit-diff-context-highlight ((t (:extend t :background ,bg-float :foreground ,fg))))
   `(magit-diff-hunk-heading ((t (:extend t :background ,bg-float :foreground ,ui))))
   `(magit-diff-hunk-heading-highlight ((t (:extend t :background ,bg-sel :foreground ,fg))))
   `(magit-diff-file-heading ((t (:weight bold))))
   ;; A commit's summary line. Magit draws it like a hunk heading, which this theme dims, but it's the
   ;; most important line of the commit message.
   `(magit-diff-revision-summary ((t (:extend t :background ,bg-float :foreground ,fg :weight bold))))
   `(magit-diff-revision-summary-highlight
     ((t (:extend t :background ,bg-sel :foreground ,fg :weight bold))))
   `(magit-section-highlight ((t (:extend t :background ,bg-float))))
   `(diff-added ((t (:extend t :background ,added))))
   `(diff-removed ((t (:extend t :background ,removed))))
   `(diff-refine-added ((t (:background ,added-word))))
   `(diff-refine-removed ((t (:background ,removed-word))))
   `(diff-indicator-added ((t (:foreground ,green-bright))))
   `(diff-indicator-removed ((t (:foreground ,red-bright))))
   `(diff-context ((t (:foreground ,comment))))
   `(diff-header ((t (:background ,bg-float))))
   `(diff-file-header ((t (:foreground ,fg :bold t))))
   `(diff-hunk-header ((t (:foreground ,ui))))
   `(ediff-current-diff-A ((t (:background ,removed))))
   `(ediff-current-diff-B ((t (:background ,added))))
   `(ediff-current-diff-C ((t (:background ,bg-sel))))
   `(ediff-current-diff-Ancestor ((t (:background ,bg-sel))))
   `(ediff-fine-diff-A ((t (:background ,removed-word))))
   `(ediff-fine-diff-B ((t (:background ,added-word))))
   `(ediff-fine-diff-C ((t (:background ,bg-float))))
   `(ediff-fine-diff-Ancestor ((t (:background ,bg-float))))
   `(ediff-even-diff-A ((t (:background ,bg-float))))
   `(ediff-even-diff-B ((t (:background ,bg-float))))
   `(ediff-even-diff-C ((t (:background ,bg-float))))
   `(ediff-even-diff-Ancestor ((t (:background ,bg-float))))
   `(ediff-odd-diff-A ((t (:background ,bg-float))))
   `(ediff-odd-diff-B ((t (:background ,bg-float))))
   `(ediff-odd-diff-C ((t (:background ,bg-float))))
   `(ediff-odd-diff-Ancestor ((t (:background ,bg-float))))))

;; Make the minibuffer's background a little lighter than other buffers', as tangotango does.
;;
;; The minibuffer area uses the default face's background color. We don't want to change the color
;; of other buffers, so we set it as a local face.
(defun melange-minibuffer-setup ()
  (setq-local face-remapping-alist '((default (:background "#34302c")))))

(defun melange-minibuffer-exit ()
  (kill-local-variable 'face-remapping-alist))

(add-hook 'minibuffer-setup-hook 'melange-minibuffer-setup)
(add-hook 'minibuffer-exit-hook 'melange-minibuffer-exit)

(provide-theme 'melange)
