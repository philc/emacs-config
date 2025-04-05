;;
;; Project navigation functions for jumping to different projects/workspaces.
;;
(provide 'project-nav)
(require 's)

;; Customize this to include the paths of folders containing projects, and notes.
(setq project-nav/project-folders '())
(setq project-nav/notes-directories '())

;; This is set to 600 by default. It shouldn't be the case, but for some reason, the filter-files-in-directory
;; function hits this limit.
(setq max-lisp-eval-depth 9999)
;; This is set to 1300 by default. Emacs randomly exceeds this limit sometimes. Restarting Emacs resolves it
;; temporarily.
(setq max-specpdl-size 13000)

(defun project-nav/filter-files-in-directory (directory filter-fn include-subdirectories)
  "Filters the files in the given directory and subdirectories using filter-fn. Excludes .git subdirectories."
  (when (file-exists-p directory)
    (->> (directory-files directory t)
         (--remove (or (s-ends-with? "." it)
                       (s-ends-with? ".." it)
                       (s-ends-with? ".git" it)))
         (--map (if (and include-subdirectories (file-directory-p it))
                    (project-nav/filter-files-in-directory it filter-fn include-subdirectories)
                  it))
         flatten
         (-filter filter-fn))))

(setq project-nav/notes-file-extensions '(".md" ".sql" ".txt"))

(defun project-nav/open-file-from-notes-folder ()
  "Prompts for the name of a notes file to open."
  (interactive)
  (let* ((file-matches-pattern? (lambda (file)
                                  (some (lambda (ext) (s-ends-with? ext file))
                                        project-nav/notes-file-extensions)))
         (file-list (->> project-nav/notes-directories
                         (--map (project-nav/filter-files-in-directory it file-matches-pattern? t))
                         flatten project-nav/sort-by-file-mtime)))
    (let ((file-to-open (completing-read "Notes file: "
                                         (mapcar 'file-name-nondirectory file-list))))
      (->> file-list
           (--filter (s-ends-with? (concat "/" file-to-open) it))
           cl-first
           find-file))
    ;; When that file is shown, ensure it's in normal mode. If the file is open in another window in insert
    ;; mode, then it will remain in insert mode in this current window.
    (switch-to-evil-normal-state)))

(defun project-nav/sort-by-file-mtime (paths)
  "Returns the paths, sorted by mtime descending."
  ;; NOTE(philc): Maybe it would be better to tap into recentf's database of files, so I can sort these by
  ;; when I last accessed them.
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

(defun project-nav/open-root-of-project (project-path)
  "Opens the project at path. If it's a clojure project, find the project's 'main' file and open that.
   Otherwise, used dired to open the file in `path`."
  ;; NOTE(philc): This function opens the "main" files in the project types that I typically work on.
  ;; Customzie this to meet your needs if you want this functionality.
  (let* ((project-name (file-name-nondirectory project-path))
         (is-clojure (file-exists-p (concat project-path "/project.clj")))
         (main-files (-> (if is-clojure
                             (->> ["core.clj" "handler.clj"]
                                  (--map (concat project-path "/src/" project-name "/" it)))
                           [])
                         (append (list (concat project-path "/" "README.md")))))
         (main-file (->> main-files
                         (remove-if-not 'file-exists-p)
                         cl-first)))
    (if main-file
        (set-window-buffer (selected-window) (find-file main-file))
      (dired project-path))))

(defun project-nav/navigate-to-project ()
  "Prompts for the name of a project which exists in your common project folders and opens a dired window in
   the root of the project folder. This is a fast way to open a new project and be able to run
   projectile-find-file.
   Once a project is chosen, the current elscreen-tab is set to be the name of that project."
  (interactive)
  (let* ((all-project-folders (->> project-nav/project-folders
                                   (--map (project-nav/filter-files-in-directory it 'file-directory-p nil))
                                   flatten))
         (project-to-open (completing-read "Project folder: "
                                           (-map 'file-name-nondirectory all-project-folders)
                                           nil t))
         (project (->> all-project-folders
                       (--filter (s-ends-with? (concat "/" project-to-open) it))
                       cl-first)))
    (project-nav/open-root-of-project project)
    ;; If we invoke this inside of a split, don't set the tab's title.
    (when (= 1 (length (window-list)))
      (tab-bar-rename-tab (file-name-nondirectory project)))))
