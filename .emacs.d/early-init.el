;;
;; Load graphical settings, so we have less UI flicker as Emacs starts up.
;;

;; This allows Emacs to occupy the full screen width and height, so that nothing below it (e.g. the
;; desktop) is visible. The default behavior in Emacs is to allow only resizing by whole
;; character-columns and rows.
(setq frame-resize-pixelwise t)

;; Set the titlebar to be transparent and dark, although just after, it gets hidden altogether.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;; Remove the titlebar on MacOS, so that Emacs occupies the entire screen.
(add-to-list 'default-frame-alist '(undecorated . t))

;; Turn off graphical toolbars.
(if (display-graphic-p) (menu-bar-mode 1) (menu-bar-mode -1))
(when (and (fboundp 'tool-bar-mode) tool-bar-mode) (tool-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) scroll-bar-mode) (scroll-bar-mode -1))

;;
;; Color scheme
;;
(defun reload-theme ()
  (interactive)
  ;; A reasonable color scheme which lives in my .emacs.d.
  (load-theme 'tangotango t))

(reload-theme)

;; Make the dividing line between window splits less bright. It's #EAEAEA in tangotango.
(set-face-attribute 'vertical-border nil :foreground "#888888")

;; Remove some of Emacs UI
(setq initial-scratch-message "") ; When opening a new buffer, don't show the scratch message.
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)
