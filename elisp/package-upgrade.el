;;; -*- lexical-binding: t; -*-
;;
;; Commands for upgrading the packages installed by straight.el (see packages.el), and keeping them
;; in sync with package-lock.el. The Makefile's package targets run these in a batch Emacs.
;;
;; straight.el's own upgrade commands (e.g. `straight-pull-all') move each package to the newest
;; commit on its default branch. `package-upgrade/upgrade-all' instead moves each package to its
;; newest version tag, i.e. its latest release, which is what MELPA Stable serves. This seems more
;; sane to me. Packages in `my-packages-at-head', and packages without any version tags, are moved
;; to the newest commit on their default branch.
;;
;; Each repo is a git repo whose checked-out branch straight.el resets to the commit it's meant to
;; be at, so `git describe --tags' in a repo gives its package's version.
;;
(provide 'package-upgrade)
(require 'cl-lib)
(require 'straight)

(defvar my-packages-at-head) ; Defined in packages.el.

(defun package-upgrade/git (repo &rest args)
  "Run git with ARGS in REPO's directory, and return its output, trimmed. Signals an error if git
   fails."
  (let ((default-directory (straight--repos-dir repo)))
    (with-temp-buffer
      (let ((status (apply #'call-process "git" nil t nil args)))
        (unless (eq status 0)
          (error "git %s failed in %s: %s" (string-join args " ") repo (buffer-string)))
        (string-trim (buffer-string))))))

(defun package-upgrade/repos ()
  "Returns an alist of (repo . recipe) for each installed repo. A repo can contain more than one
   package (e.g. magit and magit-section); the recipe is one of them."
  (let ((repos nil))
    (maphash (lambda (_package recipe)
               (let ((repo (plist-get recipe :local-repo)))
                 (when (and repo
                            (not (assoc repo repos))
                            (straight--repository-is-available-p recipe))
                   (push (cons repo recipe) repos))))
             straight--recipe-cache)
    (sort repos (lambda (a b) (string< (car a) (car b))))))

(defun package-upgrade/at-head-p (repo)
  "Non-nil if any package in REPO is in `my-packages-at-head'."
  (let ((at-head nil))
    (maphash (lambda (package recipe)
               (when (and (equal (plist-get recipe :local-repo) repo)
                          (memq (intern package) my-packages-at-head))
                 (setq at-head t)))
             straight--recipe-cache)
    at-head))

(defun package-upgrade/ancestor-p (repo commit descendant)
  "Non-nil if COMMIT is DESCENDANT or one of its ancestors, in REPO."
  (let ((default-directory (straight--repos-dir repo)))
    (eq 0 (call-process "git" nil nil nil "merge-base" "--is-ancestor" commit descendant))))

(defun package-upgrade/commit (repo)
  (package-upgrade/git repo "rev-parse" "HEAD"))

(defun package-upgrade/version (repo)
  "REPO's version: the tag of its checked-out commit, or the newest tag before it plus the number of
   commits since, e.g. v4.7.1-47-g1a2b3c4. Falls back to the commit's short hash."
  (package-upgrade/git repo "describe" "--tags" "--always"))

(defun package-upgrade/newest-version-tag (repo)
  "The tag of REPO's newest release, e.g. v1.2.0 or 1.2, or nil if it has no version tags. Tags are
   compared as version numbers, ignoring any v prefix."
  (let ((newest nil)
        (newest-version nil))
    (dolist (tag (split-string (package-upgrade/git repo "tag" "--list") "\n" t))
      (when (string-match "\\`v?\\([0-9]+\\(?:\\.[0-9]+\\)*\\)\\'" tag)
        (let ((version (version-to-list (match-string 1 tag))))
          (when (or (null newest-version) (version-list-< newest-version version))
            (setq newest tag
                  newest-version version)))))
    newest))

(defun package-upgrade/default-branch-commit (repo recipe)
  "The newest fetched commit on the remote branch which REPO's recipe tracks."
  (let* ((remote straight-vc-git-default-remote-name)
         (branch (or (plist-get recipe :branch)
                     (straight-vc-git--default-remote-branch remote repo))))
    (package-upgrade/git repo "rev-parse" (format "%s/%s^{commit}" remote branch))))

(defun package-upgrade/rebuild (repos)
  "Rebuild each package whose code is in one of REPOS."
  (maphash (lambda (package recipe)
             (when (member (plist-get recipe :local-repo) repos)
               (straight-rebuild-package package)))
           straight--recipe-cache))

(defun package-upgrade/print-table (rows)
  "Print ROWS, a list of lists of strings, as aligned columns, to stdout."
  (let ((widths (apply #'cl-mapcar (lambda (&rest cells) (apply #'max (mapcar #'length cells)))
                       rows)))
    (dolist (row rows)
      (princ (string-trim-right
              (mapconcat #'identity
                         (cl-mapcar (lambda (cell width) (string-pad cell width)) row widths)
                         "  ")))
      (terpri))))

(defun package-upgrade/upgrade-all ()
  "Move each package to its newest release, or the newest commit on its default branch if it's
   in `my-packages-at-head' or has no version tags. Rebuilds the packages which changed, records
   the new commits in package-lock.el, and prints each package's old and new version."
  (interactive)
  (let ((rows nil)
        (changed nil))
    (pcase-dolist (`(,repo . ,recipe) (package-upgrade/repos))
      (message "Fetching %s..." repo)
      (package-upgrade/git repo "fetch" "--quiet" "--tags" straight-vc-git-default-remote-name)
      (let* ((old-version (package-upgrade/version repo))
             (old-commit (package-upgrade/commit repo))
             (at-head (package-upgrade/at-head-p repo))
             (tag (unless at-head (package-upgrade/newest-version-tag repo)))
             (commit (if tag
                         (package-upgrade/git repo "rev-parse" (concat tag "^{commit}"))
                       (package-upgrade/default-branch-commit repo recipe)))
             (change (cond ((equal commit old-commit) "")
                           ((package-upgrade/ancestor-p repo old-commit commit) "upgraded")
                           (t "downgraded"))))
        (unless (equal commit old-commit)
          (straight-vc-check-out-commit recipe commit)
          (push repo changed))
        (push (list repo old-version (package-upgrade/version repo)
                    (cond (tag "release") (at-head "head") (t "head (no tags)"))
                    change)
              rows)))
    (package-upgrade/rebuild changed)
    (straight-freeze-versions t)
    (package-upgrade/print-table (cons '("repo" "old" "new" "tracks" "") (nreverse rows)))
    (princ (format (concat "\n%d repos changed. The new versions are recorded in package-lock.el. "
                           "Restart Emacs to load them.\n")
                   (length changed)))))

(defun package-upgrade/sync ()
  "Check out the commits recorded in package-lock.el, e.g. after pulling a change to it, and rebuild
   the packages which changed."
  (interactive)
  (let ((before (mapcar (lambda (r) (cons (car r) (package-upgrade/commit (car r))))
                        (package-upgrade/repos))))
    (straight-thaw-versions)
    (let ((changed (cl-loop for (repo . commit) in before
                            unless (equal commit (package-upgrade/commit repo))
                            collect repo)))
      (package-upgrade/rebuild changed)
      (princ (format "%d repos changed%s\n" (length changed)
                     (if changed (concat ": " (string-join changed ", ")) ""))))))

(defun package-upgrade/print-versions ()
  "Print the version of each installed repo."
  (interactive)
  (package-upgrade/print-table
   (mapcar (lambda (r) (list (car r) (package-upgrade/version (car r))))
           (package-upgrade/repos))))
