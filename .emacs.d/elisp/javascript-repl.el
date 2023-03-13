;; Functions for working with Javascript source and evaluating it in a REPL.
(provide 'javascript-repl)
(require 'js-comint) ; For connecting to and evaluating code in a Node or Deno REPL.

;; Note that comint-mode supports command being a (HOST . SERVICE) pair, for TCP connections.
(setq js-comint-program-command "deno")
;; Some of my projects require the unstable flag, so ensure the REPL is started with that.
(setq js-comint-program-arguments '("--unstable"))

(defun js/get-repl-buffer ()
  ; TODO(philc): Make this fail if a REPL doesn't exist.
  (get-buffer (js-comint-get-buffer-name)))

(defun js/show-repl ()
  (interactive)
  (util/show-and-scroll-repl-window (js/get-repl-buffer)))

(defun js/restart-repl ()
  ;; Based on js-comint-reset-repl.
  (interactive)
  (util/preserve-selected-window
   (lambda ()
     (when (js-comint-get-process)
       (process-send-string (js-comint-get-process) "close()\n")
       ;; We could wait for the process to clean up, but restart it immediately so we can begin
       ;; starting the next repl.
       ;; (sit-for 0.5)
       (-?> (ignore-errors (js-comint-get-process)) delete-process)
       (-?> (get-buffer (js-comint-get-buffer-name)) kill-buffer))
     (js-comint-start-or-switch-to-repl))))

(defun js/ensure-repl-is-running ()
  (interactive)
  (when (not (js-comint-get-process))
    (util/preserve-selected-window 'js-comint-start-or-switch-to-repl)))

;; (defun js-comint-reset-repl ()
;;   "Kill existing REPL process if possible.
;; Create a new Javascript REPL process.
;; The environment variable `NODE_PATH' is setup by `js-comint-module-paths'
;; before the process starts."
;;   (interactive)
;;   (when (js-comint-get-process)
;;     (process-send-string (js-comint-get-process) ".exit\n")
;;     ;; wait the process to be killed
;;     (sit-for 1))
;;   (js-comint-start-or-switch-to-repl))

;;;###autoload
;; js-comint mode assumes we're using node.js and appends "-e" to the command line. Here, I'm modifying this
;; to use Deno's "eval" instead.
(defun js-comint-start-or-switch-to-repl ()
  "Start a new repl or switch to existing repl."
  (interactive)
  (setenv "NODE_NO_READLINE" "1")
  ;; I'm setting NO_COLOR here because when "undefined" is the return value of a command, it pollutes the
  ;; color used in the terminal from then on. I'm not sure why. This obviates the issue, but it would be nice
  ;; to have color.
  (setenv "NO_COLOR" "1")
  (js-comint-setup-module-paths)
  (let* ((repl-mode (or (getenv "NODE_REPL_MODE") "magic"))
         (js-comint-code (format js-comint-code-format
                                 (window-width) js-comint-prompt repl-mode))
         (js-comint-code "")
         ;; Launch the REPL process in the directory of this project's root, rather than starting the REPL
         ;; from the directory of the current buffer's file.
         ;; Here I'm using projectfile to determine what is the project's root.
         (the-default-directory
          ;; (locate-dominating-file (buffer-file-name) ".git")
          (if (projectile-project-p)
              (projectile-project-root)
            "./")))
    ;; Set the buffer-local variable default-directory in the js-comint buffer. This is the directory the Deno
    ;; process will get run from. This variable gets set by make-comint, but the directory is not overridden
    ;; on subsequent invocations, i.e. when the REPL is restarted.
    (with-current-buffer (get-buffer-create (js-comint-get-buffer-name))
      (setq default-directory the-default-directory))
    (pop-to-buffer
     (apply 'make-comint js-comint-buffer js-comint-program-command nil
            js-comint-program-arguments))
    (js-comint-mode)))

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

(defun js/run-file-as-shoulda-test ()
  (interactive)
  (js/ensure-repl-is-running)
  ;; This whole approach doesn'really work; I need a different one. It is tricky to determine how
  ;; "shoulda" can be imported such that it's not imported twice by Deno as different modules.
  ;; shoulda may be imported from the current file via a URL, via an import map, or a symlinked
  ;; file, none of which this Emacs function can easily distinguish between.
  ;;
  ;; Also, projectile-current-project-files can have a stale cache and provide the wrong result
  ;; here!
  (let ((shoulda-js-path (-?>> (projectile-current-project-files)
                               (-find (lambda (path) (s-ends-with? "shoulda.js" path))))))
    (if (not shoulda-js-path)
        (message "Could not find shoulda.js in current project.")
      ;; Here we call shoulda.reset to clear any previous tests which have been run. We're importing
      ;; shoulda because it shoulda may not yet be defined if the file hasn't imported it yet.
      (js/eval-str
       (format "import * as shoulda from \"./%s\"; shoulda.reset()" shoulda-js-path))
      ;; (js/eval-str "import * as shoulda from \"./resources/vendor/shoulda.js\"; shoulda.reset()")
      (js/load-file)
      (js/eval-str "shoulda.run(); null")
      (util/scroll-to-buffer-end (js/get-repl-buffer)))))

(defun js/eval-str (str)
  ;; NOTE(philc): I would like to add an option to optionally ignore the output, so I can send the
  ;; REPL setup commands and not have it pollute the REPL output with a newline, and "undefined".
  ;; `js-comint-drop-regexp` may be helpful if I decide to do this.
  (js-comint-send-string str))
