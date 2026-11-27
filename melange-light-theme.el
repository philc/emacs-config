;;; melange-light-theme.el --- A warm, lower-contrast light theme.  -*- lexical-binding: t; -*-

;; Colors from Melange's light palette (https://github.com/savq/melange-nvim), which matches the
;; Melange Light theme in Ghostty. The faces are in melange-faces.el, shared with melange-theme.el.

;;; Code:

(deftheme melange-light
  "A warm, lower-contrast light theme, based on the Melange palette.")

;; Load rather than require, so that reloading the theme picks up edits to melange-faces.el.
(load (expand-file-name "melange-faces" (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(melange-faces-set
 'melange-light
 '(;; Grays, from lightest to darkest.
   (bg . "#f1f1f1")
   (bg-float . "#e9e1db") ; Slightly darker than bg, for the mode line and minibuffer.
   (bg-line . "#e4dcd5") ; The current line.
   (bg-sel . "#d9d3ce")
   ;; The selection. It's a warm tan, much darker than the current line, so that it's visible over
   ;; it.
   (bg-region . "#e3c9a8")
   (ui . "#a98a78") ; Borders, line numbers, and other UI that should recede.
   (comment . "#7d6658")
   (fg . "#54433a")
   ;; Muted colors, the terminal's normal ANSI colors.
   (red . "#c77b8b")
   (orange . "#bc5c00")
   (green . "#6e9b72")
   (cyan . "#739797")
   (blue . "#7892bd")
   (magenta . "#be79bb")
   ;; Stronger colors, the terminal's bright ANSI colors. On a light background, these are the
   ;; darker ones.
   (red-bright . "#bf0021")
   (yellow-bright . "#a06d00")
   (green-bright . "#3a684a")
   (cyan-bright . "#3d6568")
   (blue-bright . "#465aa4")
   (magenta-bright . "#904180")
   ;; Diff backgrounds. Lines are faint, and the changed words within them a little stronger.
   (added . "#e4ede0")
   (added-highlight . "#dbe7d6")
   (added-word . "#c4dabc")
   (removed . "#f4e2df")
   (removed-highlight . "#efd8d4")
   (removed-word . "#e6bdb8")))

(provide-theme 'melange-light)
