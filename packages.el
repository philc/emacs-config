;;; -*- lexical-binding: t; -*-
;;
;; Installs and activates packages, using straight.el. init.el loads this, and so do the Makefile's
;; package targets, which run it in a batch Emacs.
;;
;; straight.el installs each package by cloning its git repo into var/straight/repos/, and builds it
;; (copies or symlinks its files, generates autoloads, and byte-compiles it) into
;; var/straight/build/. The commit of each repo is recorded in package-lock.el, which is checked in.
;; When a repo is cloned, e.g. on a new machine, its commit from package-lock.el is checked out, so
;; every machine runs the same version of each package.
;;
;; - To upgrade packages, run `make upgrade-packages`. It checks out each package's latest release
;;   (its newest version tag), and records the new commits in package-lock.el. See
;;   package-upgrade.el.
;; - After pulling a change to package-lock.el, run `make sync-packages` to check out its commits.
;; - To see each package's version, run `make package-versions`.
;; - To add a package, add it to `my-packages', restart Emacs (or run `make sync-packages`), then
;;   run `make upgrade-packages` to move it to its latest release and record it in package-lock.el.
;;

(defvar my-packages-dir (file-name-directory (or load-file-name buffer-file-name))
  "This repo, which contains packages.el and package-lock.el.")

;; straight.el natively compiles packages when it builds them. early-init.el puts the
;; native-compilation cache in var/eln-cache/, but a batch Emacs (e.g. the Makefile's package
;; targets) skips early-init.el, and would put it in this repo's eln-cache/.
(when (and noninteractive (fboundp 'startup-redirect-eln-cache))
  (startup-redirect-eln-cache (expand-file-name "var/eln-cache/" my-packages-dir)))

;; These must be set before straight.el is loaded.
(defvar straight-base-dir)
(defvar straight-profiles)
(defvar straight-check-for-modifications)
(defvar straight-built-in-pseudo-packages)
(setq straight-base-dir (expand-file-name "var/" my-packages-dir))
(setq straight-profiles `((nil . ,(expand-file-name "package-lock.el" my-packages-dir))))
;; By default, straight.el searches every repo for edited files at startup, to rebuild their
;; packages. Packages aren't edited here, and the package-upgrade.el commands rebuild the packages
;; they change.
(setq straight-check-for-modifications nil)
;; Don't install these as packages, even when a package depends on them. Emacs's built-in versions
;; are recent enough. A cloned copy would shadow the built-in one, and its version wouldn't match
;; the rest of Emacs's built-in libraries.
(setq straight-built-in-pseudo-packages
      '(emacs nadvice python image-mode cl-lib seq transient org project xref eldoc jsonrpc flymake
              let-alist map use-package bind-key))

;; The bootstrap code from straight.el's README. It installs straight.el on first run.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defvar my-packages '(avy ; Jump to any text on screen in a few keystrokes.
                      ag ; Silver searcher integration for Emacs
                      better-jumper ; A configurable jump implementation
                      browse-at-remote ; Jump to the Github page for a given line in a file.
                      clojure-mode ; For editing Clojure files.
                      consult
                      dash ; Dash provides modern functions for working with lists in Emacs Lisp.
                      diminish ; For hiding and shortening minor modes in the modeline
                      evil ; Evil mode implements Vim's modal bindings and text object manipulation.
                      evil-nerd-commenter
                      general ; Functions for defining keybindings and leader keys. Complements Evil
                      go-mode ; For editing Go files.
                      inf-clojure ; Clojure REPL mode
                      jinx ; Spell checker
                      magit ; A mode for committing to git repositories and viewing Git history.
                      no-littering ; Keeps packages' state files in var/ and etc/
                      powerline ; Improve the appearance & density of the Emacs status bar.
                      projectile ; Find file in project (ala Vim's CTRL-P or Textmate's Cmd-T)
                      rainbow-delimiters ; Highlight parentheses in rainbow colors.
                      relint ; Lints regexps. Used by `make check`.
                      s ; A strings library.
                      scss-mode ; For editing SCSS files.
                      smartparens ; For editing expressions in parentheses.
                      swift-mode ; Swift syntax highlighting
                      tempel ;; Insert snippets
                      vertico ; Nicely show menu completions
                      yaml-mode ; For editing YAML files
                      yascroll ; For rendering scroll bars in the right fringe
                      ))

(defvar my-packages-at-head '(ag
                              browse-at-remote
                              diminish
                              evil
                              go-mode
                              goto-chg ; A dependency of evil.
                              powerline
                              scss-mode
                              smartparens
                              yaml-mode
                              yascroll)
  "Packages which `make upgrade-packages' moves to the newest commit on their default branch, rather
   than to their newest version tag. These are packages whose last release is years older than their
   latest code. (Packages which have no version tags, e.g. general, are always moved to the newest
   commit.)")

;; Use these packages' upstream repos, rather than their default recipes' GNU ELPA mirrors. compat's
;; mirror has no version tags, but its upstream repo tags releases. relint's and xr's mirrors have
;; both a NEWS and a news file, which are the same file on macOS's case-insensitive file system, so
;; the checkout always looks modified, and straight.el refuses to change its commit.
(straight-register-package '(compat :type git :host github :repo "emacs-compat/compat"))
(straight-register-package '(relint :type git :host github :repo "mattiase/relint"))
(straight-register-package '(xr :type git :host github :repo "mattiase/xr"))

(dolist (p my-packages)
  (straight-use-package p))
