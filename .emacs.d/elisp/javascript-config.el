;; Functions for working with Javascript source and evaluating it in a REPL.
(provide 'javascript-config)
(require 'js-comint) ; For connecting to and evaluating code in a Node REPL.

(setq js-comint-program-command "deno")
(setq js-comint-program-arguments nil)

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
       ;; wait the process to be killed
       (sit-for 1))
     (js-comint-start-or-switch-to-repl))))

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
  (js-comint-setup-module-paths)
  (let* ((repl-mode (or (getenv "NODE_REPL_MODE") "magic"))
         (js-comint-code (format js-comint-code-format
                                 (window-width) js-comint-prompt repl-mode))
         (js-comint-code "")
         ;; Launch the REPL process in the directory of this project's root (containing a .git file),
         ;; rather than starting the rePL from the directory of the current buffer's file.
         (default-directory (or (locate-dominating-file (buffer-file-name) ".git")
                                default-directory)))
    (pop-to-buffer
     (apply 'make-comint js-comint-buffer js-comint-program-command nil nil))
    (js-comint-mode)))

(setq js/load-file-counter 1)

(defun js/load-file ()
  (interactive)
  (util/save-buffer-if-dirty)
  (let ((file (expand-file-name (buffer-file-name)))
        ;; For Deno, append a random query string to the path so that Deno loads the latest version of the
        ;; file.
        (cmd-str "import * as main from \"file://%s?q=%s\"")
        ;; For Node.js, delete any previous versions of this file in the require cache.
        ;; (cmd-str "if (true) { let f = \"%s\"; delete require.cache[require.resolve(f)]; require(f) }\n")
        )
    ;; For Deno
    ;; https://stackoverflow.com/questions/61903993/how-to-delete-runtime-import-cache-in-deno
    (-> (format cmd-str file js/load-file-counter)
        js/eval-str))
  (setq js/load-file-counter (inc js/load-file-counter))
  (util/scroll-to-buffer-end (js/get-repl-buffer )))

(defun js/eval-str (str)
  (js-comint-send-string str))
