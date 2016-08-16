;;
;; Project navigation functions for opening project folders in dired-mode.
;;
(provide 'project-nav)
(require 'lisp-utils) ; TODO(philc): Update this to use the s.el strings library.

;; Customize this to include the paths of folders containing projects, and notes.
(setq project-nav/project-folders '())
(setq project-nav/notes-directories '())

;; This is set to 600 by default. It shouldn't be the case, but for some reason, the filter-files-in-directory
;; function hits this limit.
(setq max-lisp-eval-depth 1200)

(defun project-nav/filter-files-in-directory (directory filter-fn include-subdirectories)
  "Filters the files in the given directory and subdirectories using filter-fn. Excludes .git subdirectories."
  (->> (directory-files directory t)
       (--remove (or (string/ends-with it ".")
                     (string/ends-with it "..")
                     (string/ends-with it ".git")))
       (--map (if (and include-subdirectories (file-directory-p it))
                  (project-nav/filter-files-in-directory it filter-fn include-subdirectories)
                it))
       flatten
       (-filter filter-fn)))

(setq project-nav/notes-file-extensions '(".md" ".sql" ".txt"))

(defun project-nav/open-file-from-notes-folder ()
  "Prompts for the name of a notes file to open."
  (interactive)
  (let* ((file-matches-pattern? (lambda (file)
                                  (some (lambda (ext) (string/ends-with file ext))
                                        project-nav/notes-file-extensions)))
         (file-list (->> project-nav/notes-directories
                         (--map (project-nav/filter-files-in-directory it file-matches-pattern? t))
                         flatten)))
    (let ((file-to-open (ido-completing-read "Notes file: " (mapcar 'file-name-nondirectory file-list))))
      (->> file-list
           (--filter (string/ends-with it (concat "/" file-to-open)))
           first
           find-file))))

(defun project-nav/open-root-of-project (project-path)
  "Opens the project at path. If it's a clojure project, find the project's 'main' file and open that.
   Otherwise, used dired to open the file in `path`."
  ;; NOTE(philc): This function opens the "main" files in the project types that I typically work on.
  ;; Customzie this to meet your needs if you want this functionality.
  (let* ((project-name (file-name-nondirectory project-path))
         (is-clojure (file-exists-p (concat project-path "/project.clj")))
         (main-file (when is-clojure
                      (->> ["core.clj" "handler.clj"]
                           (--map (concat project-path "/src/" project-name "/" it))
                           (remove-if-not 'file-exists-p)
                           first))))
    (if main-file
        (set-window-buffer (selected-window) (find-file main-file))
      (dired project-path))))

(defun project-nav/navigate-to-project ()
  "Prompts for the name of a project which exists in your common project folders and opens a dired window in
   the root of the project folder. This is a fast way to open a new project and be able to run
   projectile-file-file.
   Once a project is chosen, the current elscreen-tab is set to be the name of that project."
  (interactive)
  (let* ((all-project-folders (->> project-nav/project-folders
                                   (--map (project-nav/filter-files-in-directory it 'file-directory-p nil))
                                   flatten))
         (project-to-open (ido-completing-read "Project folder: "
                                               (-map 'file-name-nondirectory all-project-folders)
                                               nil t))
         (project (->> all-project-folders
                       (--filter (string/ends-with it (concat "/" project-to-open)))
                       first)))
    (project-nav/open-root-of-project project)
    ;; If we invoke this inside of a split, don't set the tab's title.
    (when (= 1 (length (window-list)))
      (escreen-set-tab-alias (file-name-nondirectory project)))))
