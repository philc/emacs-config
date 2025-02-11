;; Functions for working with Javascript source and evaluating it in a REPL.
(provide 'javascript-repl)
(require 'repl) ; For connecting to and evaluating code in a Deno REPL.

(setq js/program-command "deno")
(setq js/program-arguments '())

(defun js/get-repl-buffer ()
  ; TODO(philc): Make this fail if a REPL doesn't exist.
  (get-buffer repl/buffer-name))

(defun js/show-repl ()
  (interactive)
  (util/show-and-scroll-repl-window (js/get-repl-buffer)))

(defun js/restart-repl ()
  (interactive)
  (util/preserve-selected-window
   (lambda ()
     (when (repl/is-running?)
       (repl/send-command "close()")
       ;; We could wait for the process to clean up, but restart it immediately so we can begin
       ;; starting the next repl.
       ;; (sit-for 0.5)
       (-?> (ignore-errors (repl/get-process))
            delete-process)
       (-?> (get-buffer repl/buffer-name)
            kill-buffer))
     (js/start-or-switch-to-repl))))

(defun js/ensure-repl-is-running ()
  (interactive)
  (when (not (repl/is-running?))
    (util/preserve-selected-window 'js/start-or-switch-to-repl)))

(defun js/start-or-switch-to-repl ()
  "Start a new repl or switch to existing repl."
  (interactive)
  (setenv "NODE_NO_READLINE" "1")
  ;; I'm setting NO_COLOR here because when "undefined" is the return value of a command, it
  ;; pollutes the color used in the terminal from then on. I'm not sure why. This obviates the
  ;; issue, but it would be nice to have color.
  (setenv "NO_COLOR" "1")
  (let* (;; Launch the REPL process in the directory of this project's root, rather than starting
         ;; the REPL from the directory of the current buffer's file. Here I'm using projectfile to
         ;; determine what is the project's root.
         (the-default-directory
          ;; (locate-dominating-file (buffer-file-name) ".git")
          (if (projectile-project-p)
              (projectile-project-root)
            "./"))
         (repl-buffer (get-buffer-create repl/buffer-name)))
    (repl/start js/program-command js/program-arguments the-default-directory)
    (pop-to-buffer (get-buffer-create repl/buffer-name))
    ;; (js-comint-mode)
    ))

(setq js/load-file-counter 1)

(defun js/load-file ()
  (interactive)
  (util/save-buffer-if-dirty)
  (js/ensure-repl-is-running)
  (let* (;; Since Deno treats files required with relative paths as separate from files required via
         ;; absolute paths, and doesn't deduplicate them, we first resolve any symlinks in the
         ;; file's path, and then make the path relative to the project's root, if we're loading a
         ;; file in a Projectile project.
         (file (file-truename (buffer-file-name)))
         (file (if (projectile-project-p)
                   (->> file
                        (s-chop-prefix (projectile-project-root))
                        (concat "./"))
                 file))
         ;; Note that unfortunately, if the file being imported has an unhandled rejected promise, it
         ;; will not be propagated by Deno until this is fixed:
         ;; https://github.com/denoland/deno/issues/8858
         ;;
         ;; For Deno, append a random query string to the path so that Deno loads the latest version
         ;; of the file. Add a bar after the query string so it doesn't visually look like a line
         ;; number in backtraces.
         (cmd-str "import * as main from \"%s?v=%s|\"")
         ;; For Node.js, delete any previous versions of this file in the require cache.
         ;; (cmd-str "if (true) { let f = \"%s\"; delete require.cache[require.resolve(f)]; require(f) }\n")
         )
    ;; For Deno
    ;; https://stackoverflow.com/questions/61903993/how-to-delete-runtime-import-cache-in-deno
    (-> (format cmd-str file js/load-file-counter)
        js/eval-str))
  (setq js/load-file-counter (inc js/load-file-counter))
  (util/scroll-to-buffer-end (js/get-repl-buffer)))

(defun js/extract-shoulda-js-import-path-from-buffer ()
  "Returns the URL of the shoulda.js file being imported by the source file in the current buffer,
   or nil if there is no such import."
  (save-excursion
    (let* ((result nil)
           (regexp "^import .+ from \"\\([^\"]+\\)/\\(shoulda\\(?:.js\\)?\\)\";"))
      (goto-char (point-min))
      (while (and (not result)
                  (re-search-forward regexp nil t))
        (setq result (concat (match-string 1) "/" (match-string 2))))
      result)))

(defun js/run-file-as-shoulda-test ()
  (interactive)
  (js/ensure-repl-is-running)
  ;; This whole approach is flawed. It's tricky to determine how "shoulda" can be imported here by
  ;; this helper such that it's not imported twice by Deno as different modules. shoulda may be
  ;; imported from the current file via a URL, via an import map, or a symlinked file, none of which
  ;; this Emacs function can easily distinguish between.
  (util/save-buffer-if-dirty)
  (let ((shoulda-import-path (js/extract-shoulda-js-import-path-from-buffer)))
    ;; If shoulda is imported directly by the file, then use that same path to import it into
    ;; an anonymous module and run shoulda.reset() and shoulda.test().
    ;; If shoulda is not imported directly by this file, assume it has been added to globalThis
    ;; by a testing helper (as is the case in Vimium's tests).
    ;; We run shoulda.reset() to clear any previous tests that have been defined.
    (if shoulda-import-path
        (let* ((file-dir (-> (buffer-file-name) file-truename file-name-directory))
               (shoulda-is-url (or (s-starts-with? "https://" shoulda-import-path)
                                   ;; Support JSR imports.
                                   (s-starts-with? "@" shoulda-import-path)))
               (shoulda-js-path (if shoulda-is-url
                                    shoulda-import-path
                                  (expand-file-name shoulda-import-path file-dir))))
          (js/eval-str (format "import * as shoulda from \"%s\"; shoulda.reset()" shoulda-js-path)))
      (js/eval-str "if (globalThis.shoulda) shoulda.reset()"))
    (js/load-file)
    (js/eval-str "await shoulda.run(); undefined")
    (util/scroll-to-buffer-end (js/get-repl-buffer))))

(defun js/eval-str (str)
  (repl/send-command str))

(defun js/clear ()
  (interactive)
  (repl/clear))
