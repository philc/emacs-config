;;; melange-theme.el --- A warm, lower-contrast dark theme.  -*- lexical-binding: t; -*-

;; Colors from Melange (https://github.com/savq/melange-nvim), which matches the Melange Dark theme
;; in Ghostty. The faces are in melange-faces.el, shared with melange-light-theme.el.

;;; Code:

(deftheme melange
  "A warm, lower-contrast dark theme, based on the Melange palette.")

;; NOTE(philc): After modifying this file or melange-faces.el, eval (load-theme 'melange t) to see
;; live changes.

;; Load rather than require, so that reloading the theme picks up edits to melange-faces.el.
(load (expand-file-name "melange-faces" (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(melange-faces-set
 'melange
 '(;; Grays, from darkest to lightest.
   (bg . "#292522")
   (bg-float . "#34302c") ; Slightly lighter than bg, for the mode line and minibuffer.
   (bg-line . "#3b3631") ; The current line.
   (bg-sel . "#403a36")
   (bg-tab . "#403a36") ; The current tab.
   ;; The selection. It's much lighter than the current line, so that it's visible over it.
   (bg-region . "#5e4229")
   (ui . "#867462") ; Borders, line numbers, and other UI that should recede.
   (comment . "#c1a78e")
   (fg . "#f4ebe3")
   ;; Muted colors, the terminal's normal ANSI colors.
   (red . "#bd8183")
   (orange . "#e49b5d")
   (green . "#78997a")
   (cyan . "#7b9695")
   (blue . "#7f91b2")
   (magenta . "#b380b0")
   ;; Brighter colors, the terminal's bright ANSI colors.
   (red-bright . "#d47766")
   (yellow-bright . "#ebc06d")
   (green-bright . "#85b695")
   (cyan-bright . "#89b3b6")
   (blue-bright . "#a3a9ce")
   (magenta-bright . "#cf9bc2")
   ;; Diff backgrounds. Lines are faint, and the changed words within them a little stronger.
   (added . "#2a3328")
   (added-highlight . "#2f392d")
   (added-word . "#3c4c38")
   (removed . "#3b2a28")
   (removed-highlight . "#42302d")
   (removed-word . "#553633")))

(provide-theme 'melange)
