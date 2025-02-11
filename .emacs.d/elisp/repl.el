;; This provides functions for interacting with an external REPL: starting and restarting the REPL
;; program, and sending and receiving text.

;; It's similar to Emacs' comint mode. But unlike comint mode, this does not provide an interactive
;; readline-style REPL UI within Emacs. Instead, lines and paragraphs from any buffer can be sent to
;; the REPL process, and the output is shown in the *REPL* buffer. As a result, this implementation
;; is vastly simpler and easier to debug than comint mode. To illustrate, see the complexity of
;; comint-send-input, comint-output-filter, and their associated functions.

(define-derived-mode repl-mode fundamental-mode "REPL")

(defvar repl/buffer-name "*REPL*"
  "The name of the buffer that's associated with the REPL process")

(defun repl/start (command arguments directory)
  "- directory: the directory the command will be run from."
  (let* (; If the `directory` arg is nil, then use the current buffer's directory.
         (target-directory (or directory default-directory))
         (process
          (let ((default-directory target-directory))
            (apply 'start-process
                   repl/buffer-name
                   repl/buffer-name
                   command
                   (or arguments '())
                   ))))
    ;; scroll-margin is a user preference which always keeps N lines of text in view as the user's
    ;; cursor reaches the edge of the window. Here we set it to 0 for this REPL buffer, so that we
    ;; don't unnecessarily always have blank lines at the bottom of the buffer. This is reasonable
    ;; to do on buffers which are always showing the bottom of the file, as is the case here.
    (with-current-buffer (get-buffer repl/buffer-name)
      (repl-mode)
      (setq-local scroll-margin 0))
    (set-process-filter process 'repl/handle-process-output)))

(defun repl/get-process ()
  (get-buffer-process (get-buffer repl/buffer-name)))

(defun repl/is-running? ()
  (let ((proc (repl/get-process)))
    (if (and proc
             (memq (process-status proc) '(open run stop)))
        t
      nil)))

(defun repl/send-command (str)
  (when (not (repl/is-running?))
    (error "REPL process is not running."))
  ;; Append a newline to the str so the REPL stops waiting for input.
  (process-send-string (repl/get-process)
                       (concat str "\n")))

(defun repl/filter-noisy-output (str)
  (let (;; TODO(philc): Move this filtering logic into javascript-repl.el; it's specific to Deno.
        ;; re-undefined cleans up two cases of noise:
        ;; * When sending multi-line output to the subprocess, each line of output is followed by
        ;;   "undefined" for some reason.
        ;; * When evaluating expressions with no return value (like console.log(...) statements),
        ;;   "undefined" is printed, which is not useful. The Deno REPL does this by design, to be
        ;;   consistent with Node's REPL. https://github.com/denoland/deno/issues/12896
        (re-undefined "^\\(> \\)?undefined\n")
        ;; We replace the naked prompt ("> ") with "-----", so it's easier to see the difference
        ;; between sections of output. It would be best if this was done only before output, rather
        ;; than after the output. To do that, we'd need to insert a message to the *REPL* buffer
        ;; before sending input to the REPL process.
        (re-naked-prompt "^> $"))
    (->> str
         (replace-regexp-in-string re-naked-prompt "-----\n")
         (replace-regexp-in-string re-undefined ""))))

(defun repl/append-to-process-buffer (str)
  "Appends `str` to the end of the REPL process buffer, and scrolls all windows showing this buffer
   to the bottom."
  (let* ((buffer (process-buffer process))
         (windows (get-buffer-window-list buffer nil t)))
    ;; Insert the string into the process's buffer.
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (insert str)
          (goto-char (point-max))))
      ;; TODO(philc): Here, we scroll every window showing the REPL to the bottom. If the cursor is
      ;; not at the bottom of the REPL (because the user moved it, or has some text selected), it
      ;; would be nice not to oblitherate the user's scroll position.
      ;;
      ;; To accomplish this, before inserting the process's output, we need to check whether every
      ;; window that's showing the REPL buffer has its cursor at the end of the buffer, and only
      ;; scroll the ones that do. We also need to do this on the buffer itself, in case no window is
      ;; currently showing the REPL buffer.
      (let ()
        (dolist (window windows)
          (with-selected-window window
            (goto-char (point-max))))))))

(defun repl/handle-process-output (process str)
  ;; NOTE(philc): If we allow the REPL program to output ANSI color escape sequences, we could
  ;; interpret those sequences using `ansi-color-apply`. But this is a bit delicate to do as the
  ;; output is spread across chunks. I looked into getting this working with color output from Deno
  ;; for an hour, and had several problems along the way.
  (repl/append-to-process-buffer (repl/filter-noisy-output str)))

(defun repl/clear ()
  (interactive)
  (let ((buffer (get-buffer repl/buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (erase-buffer)))))

(provide 'repl)
