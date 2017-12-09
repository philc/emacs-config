;; Functions for working with Javascript source and evaluating it in a REPL.
(provide 'javascript-config)
(require 'js-comint) ; For connecting to and evaluating code in Node REPLs.

(defun js/get-repl-buffer ()
  (get-buffer (js-comint-get-buffer-name)))

(defun js/show-repl ()
  (interactive)
  (util/show-and-scroll-repl-window (js/get-repl-buffer)))

(defun js/restart-repl ()
  (interactive)
  (util/preserve-selected-window 'js-comint-reset-repl))

(defun js/load-file ()
  (interactive)
  (util/save-buffer-if-dirty)
  (let ((file (expand-file-name (buffer-file-name))))
    (-> "if (true) { let f = \"%s\"; delete require.cache[require.resolve(f)]; require(f) }\n"
        (format file)
        js/eval-str))
  (util/scroll-to-buffer-end (js/get-repl-buffer )))

(defun js/eval-str (str)
  (comint-send-string (js-comint-get-process) str))
