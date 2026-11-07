;;; -*- lexical-binding: t; -*-
;;
;; Opening files from my notes folders.
;;
(provide 'project-nav)
(require 'cl-lib)
(require 'dash)
(require 's)
(require 'evil-ext)

;; Customize this to include the paths of folders containing notes.
(defvar project-nav/notes-directories '())

(defvar project-nav/notes-file-extensions '(".md" ".sql" ".txt"))

(defun project-nav/notes-files (directory)
  "The notes files in DIRECTORY and its subdirectories, except in .git directories, following
   symlinks."
  (when (file-directory-p directory)
    (directory-files-recursively
     directory
     (concat (regexp-opt project-nav/notes-file-extensions) "\\'")
     nil
     (lambda (dir) (not (string= (file-name-nondirectory dir) ".git")))
     t)))

(defun project-nav/open-file-from-notes-folder ()
  "Prompts for the name of a notes file to open."
  (interactive)
  (let ((file-list (->> project-nav/notes-directories
                        (mapcan #'project-nav/notes-files)
                        project-nav/sort-by-file-mtime)))
    (let ((file-to-open (completing-read "Notes file: "
                                         (mapcar 'file-name-nondirectory file-list))))
      (->> file-list
           (--filter (s-ends-with? (concat "/" file-to-open) it))
           cl-first
           find-file))
    ;; When that file is shown, ensure it's in normal mode. If the file is open in another window in
    ;; insert mode, then it will remain in insert mode in this current window.
    (evil-ext/switch-to-normal-state)))

(defun project-nav/sort-by-file-mtime (paths)
  "Returns the paths, sorted by mtime descending."
  ;; NOTE(philc): Maybe it would be better to tap into recentf's database of files, so I can sort
  ;; these by when I last accessed them.
  ;;
  ;; It requires many more syscalls to get file attributes per file, rather than in batch for a full
  ;; directory. So, if this is ever slow, we can switch to getting the file mtimes when we scan
  ;; directories, using directory-files-and-attributes.
  (let* ((paths-and-mtimes (-map (lambda (path)
                                   (list path
                                         (-> path
                                             file-attributes
                                             file-attribute-modification-time)))
                                 paths))
         (sorted (sort paths-and-mtimes
                       (lambda (a b)
                         (time-less-p (cl-second b) (cl-second a)))))
         (sorted-names (-map 'cl-first sorted)))
    sorted-names))
