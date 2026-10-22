;;; -*- lexical-binding: t; -*-
;;
;; Load graphical settings, so we have less UI flicker as Emacs starts up.
;;

;; The files in elisp/ are byte-compiled, and init.el recompiles them at startup and on save, so their
;; .elc files should never be stale. This is a safety net: if a .el is ever newer than its .elc,
;; load the .el.
(setq load-prefer-newer t)

;; Packages are installed and activated by straight.el (see packages.el), not package.el.
(setq package-enable-at-startup nil)

;; Keep the native-compilation cache in var/ with the other runtime state (see no-littering in
;; init.el). This has to happen in early-init.el, before anything is natively compiled.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory))))

;; Don't pop up the *Warnings* buffer when a package is natively compiled in the background. The
;; warnings are almost always harmless ones in third-party code (e.g. browse-at-remote calling
;; dired functions without requiring dired). They're still logged to *Warnings*.
(setq native-comp-async-report-warnings-errors 'silent)

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
