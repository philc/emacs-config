;;; package-load-path.el --- Make packages loadable in a batch Emacs. -*- lexical-binding: t; -*-

;; Usage: emacs -Q --batch -l scripts/package-load-path.el ...
;;
;; Adds each package which straight.el built (see packages.el) to `load-path', so they can be
;; `require'd, and loads their autoloads, as init.el does. This is for scripts which run Emacs with
;; -Q, which skips init.el. Unlike loading packages.el, it doesn't install anything or write any of
;; straight.el's state, so it's safe to run in many Emacs processes at once.

;; Natively-compiled code (e.g. the trampolines made when a package advises a primitive) goes into
;; user-emacs-directory/eln-cache/, which is this repo, unless redirected to var/eln-cache/, as
;; early-init.el does. -Q skips early-init.el.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name "../var/eln-cache/"
                     (file-name-directory (or load-file-name buffer-file-name)))))

(let ((build-dir (expand-file-name "../var/straight/build/"
                                   (file-name-directory (or load-file-name buffer-file-name)))))
  (unless (file-directory-p build-dir)
    (error "%s doesn't exist. Run `make sync-packages' to install the packages" build-dir))
  (dolist (dir (directory-files build-dir t "\\`[^.]"))
    (when (file-directory-p dir)
      (push dir load-path)))
  (dolist (dir (directory-files build-dir t "\\`[^.]"))
    (let ((autoloads (expand-file-name (concat (file-name-nondirectory dir) "-autoloads.el")
                                       dir)))
      (when (file-exists-p autoloads)
        (load autoloads nil t)))))
