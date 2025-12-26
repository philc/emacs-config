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
  (when (repl/is-running?)
    (repl/send-command "close()")
    (let ((repl-buffer (get-buffer repl/buffer-name)))
      ;; We could wait for the process to clean up, but restart it immediately so we can begin
      ;; starting the next repl.
      ;; (sit-for 0.5)
      (-?> (ignore-errors (repl/get-process))
           delete-process)
      ;; Erase what was previously in the buffer, so we get a new, blank REPL buffer.
      (with-current-buffer repl-buffer
        (erase-buffer))
      ;; Don't kill the existing buffer: my window management functions will open a new buffer where
      ;; the existing one is (if it's showing) and can't do this if the existing buffer is killed
      ;; before restarting the REPL.
      ;; (-?> (get-buffer repl/buffer-name)
      ;;      kill-buffer)
      ))
  (js/start-or-switch-to-repl))

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
  (let* ((repl-buffer (get-buffer-create repl/buffer-name)))
    ;; Launch the REPL process in the project's directory, rather than starting it from the
    ;; directory of the current buffer's file.
    (repl/start js/program-command js/program-arguments (js/project-root))
    ;; Using with-current-buffer here prevents display-buffer from changing the current buffer.
    ;; Code invoking js/start-or-switch-to-repl expects that the buffer doesn't change.
    (util/preserve-selected-frame
     (lambda ()
       (with-current-buffer (current-buffer)
         (display-buffer (get-buffer-create repl/buffer-name)))))))

(setq js/load-file-counter 1)

(defun js/load-current-file ()
  (interactive)
  (setq js/last-run-command (list (current-buffer) 'js/load-current-file))
  (js/load-file (buffer-file-name)))

(defun js/project-root ()
  "A project directory can be the directory with a deno.json, or a .git directory, or a
  .projectfile file. Use whichever is the most specific (has the longer path)."
  (let* ((deno-json (-?> (locate-dominating-file (buffer-file-name) "deno.json")
                         expand-file-name
                         file-truename))
         (project-root
          (if (projectile-project-p)
              (projectile-project-root)
            (expand-file-name "./"))))
    (if (> (length deno-json)
           (length project-root))
        deno-json
      project-root)))

(defun js/load-file (file-name)
  (js/ensure-repl-is-running)
  (let* (;; Since Deno treats files required with relative paths as separate from files required via
         ;; absolute paths, and doesn't deduplicate them, we first resolve any symlinks in the
         ;; file's path, and then make the path relative to the project's root, if we're loading a
         ;; file in a Projectile project.
         (file (file-truename file-name))
         (file (->> file
                    (s-chop-prefix (js/project-root))
                    (concat "./")))
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
        (setq result (concat (match-string-no-properties 1)
                             "/"
                             (match-string-no-properties 2))))
      result)))

;; The JS action that was last run. It's a tuple of (buffer, function-name).
(setq js/last-run-command nil)

;; The JS action that has been saved (via js/save-last-run-command) for invoking again in the
;; future. It's a tuple of (buffer, function-name).
(setq js/saved-run-command nil)

(defun js/save-last-run-command ()
  "Save the last high-level run command for future invocation."
  (interactive)
  (when js/last-run-command
    (let ((fn-name (nth 1 js/last-run-command))
          (buf (cl-first js/last-run-command)))
      (message (format "Saving command: %s on %s" fn-name (buffer-name buf)))
      (setq js/saved-run-command js/last-run-command))))

(defun js/run-saved-command ()
  "Execute the last saved runnable command, if any."
  (interactive)
  (evil-normal-state)
  (if js/saved-run-command
      (let ((fn-name (nth 1 js/saved-run-command))
            (buf (cl-first js/saved-run-command)))
        (with-current-buffer buf
          (message (format "Running %s on %s" fn-name (buffer-name buf)))
          (funcall fn-name)))
    (message "No runnable command has yet been saved.")))

;; TODO(philc): I would like to instead make import reloading work well in Deno. I haven't looked
;; into it yet, so this is a workaround.
(defun js/restart-and-run-file-as-shoulda-test ()
  (interactive)
  (setq js/last-run-command (list (current-buffer) 'js/restart-and-run-file-as-shoulda-test))
  (js/restart-repl)
  (js/run-shoulda-test))

(defun js/run-file-as-shoulda-test ()
  (interactive)
  (setq js/last-run-command (list (current-buffer) 'js/run-file-as-shoulda-test))
  (js/run-shoulda-test))

(defun js/run-shoulda-test ()
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
                                   (s-starts-with? "@" shoulda-import-path)
                                   (s-starts-with? "jsr:" shoulda-import-path)))
               (shoulda-js-path (if shoulda-is-url
                                    shoulda-import-path
                                  (expand-file-name shoulda-import-path file-dir))))
          (js/eval-str (format "import * as shoulda from \"%s\"; shoulda.reset()" shoulda-js-path)))
      (js/eval-str "if (globalThis.shoulda) shoulda.reset()"))
    (js/load-file (buffer-file-name))
    (js/eval-str "await shoulda.run(); undefined")
    (util/scroll-to-buffer-end (js/get-repl-buffer))))

(defun js/eval-str (str)
  (util/raise-repl-frame (js/get-repl-buffer))
  (repl/send-command str))

(defun js/clear ()
  (interactive)
  (util/raise-repl-frame (js/get-repl-buffer))
  (repl/clear))
