;;; -*- lexical-binding: t; -*-
;;
;; Utility functions for programming Emacs.
;;
(provide 'emacs-utils)
(require 'lisp-utils)
(require 'browse-url)

(defmacro define-leader-keys (keymaps &rest keybindings)
  "A shorthand for defining leader keys using the `general` keybinding package.
   `global-leader-key` must be defined as a top-level variable."
  `(general-define-key :prefix global-leader-prefix :states '(normal visual) :keymaps ,keymaps ,@keybindings))

(defun util/call-process-with-exit-status (program stdin &rest args)
  "Runs a command and returns a list containing the status code and output string.
   E.g.: (call-process-with-exit-status 'ls' '-h' '-l' '-a') ;; => (0 '-r-w-r-- 1 ...')"
  (with-temp-buffer
    (when stdin
      (insert stdin))
    (list (apply 'call-process-region (point-min) (point-max) program t (current-buffer) nil args)
          (buffer-string))))

(defun util/call-process-and-check (program stdin &rest args)
  "Calls the given program and raises an error if the exist status is non-zero. The error's message
   is the program's output."
  (let* ((result (apply 'util/call-process-with-exit-status program stdin args))
                 (exit-code (cl-first result))
                 (output (cl-second result)))
    (if (= exit-code 0)
        output
      (error "%s" output))))

(defun util/line-indentation-level (line)
  "The number of space characters prefixing a line."
  (string-match "\\([ ]*\\)" line)
  (length (match-string 1 line)))

(defun util/replace-current-line (new-line)
  "Replaces the current line with the new one."
  (save-excursion
   (delete-region (line-beginning-position) (line-end-position))
   (insert new-line)))

(defun util/replace-buffer-text (new-text)
  "Replaces the entire buffer with the new text."
  (save-excursion
    ;; We're using replace-buffer-contents because it very helpfully preserves the scroll positions
    ;; of all windows displaying this buffer.
    ;; https://emacs.stackexchange.com/a/47889/2278
    (let ((tmp-buf (generate-new-buffer " *temp*")))
      (with-current-buffer tmp-buf (insert new-text))
      (replace-buffer-contents tmp-buf)
      (kill-buffer tmp-buf))))

(defun util/replace-region (new-text)
  "Replaces the entire buffer with the new text."
  (save-excursion
   (delete-region (region-beginning) (region-end))
   (insert new-text)))

(defun util/get-line (offset)
  "Returns the text (without string properties) of the line offset by `offset` from the current.
   This has the same behavior as `forward-line`, so if offset is larger than the buffer, the
   buffer's last line of text will be returned."
  (save-excursion
    (forward-line offset)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun util/get-current-line ()
  "Returns the text (without string properties) of the current line."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun util/define-keys (keymap &rest key-and-fn-pairs)
  "Like define-key, but takes a variable number of arguments -- two per key binding pair."
  (dolist (pair (partition key-and-fn-pairs 2))
    (define-key keymap (cl-first pair) (cl-second pair))))

(defun util/save-buffer-if-dirty ()
  (when (and buffer-file-name (buffer-modified-p))
    (util/save-buffer-silently)))

(defun util/save-buffer-silently ()
  "Saves the buffer while suppressing output to the *Messages* buffer. This avoids cluttering up the
   Messages buffer with lots of 'save' output."
  (interactive)
  ;; NOTE(philc): Saving files generates messages in the *Messages* buffer of the form "Wrote file
  ;; xyz". However, note that if there's an issue saving a file, it's possible the error will be
  ;; masked/hidden because of this output supression.
  (let* ((inhibit-message t) ; Don't show messages in the echo area.
         (message-log-max nil)) ; Don't show in the Messages buffer. http://stackoverflow.com/q/10164929
    (save-buffer)))

(defun util/with-env-var (name value fn)
  "Temporarily sets an env var to the given value and excutes fn."
  (let ((original-value (getenv name)))
    (unwind-protect
        (progn
          (setenv name value)
          (funcall fn))
      (setenv name original-value))))

(defun util/without-confirmation (fn &rest args)
  "Applies the given fn but skips any confirmation prompts invoked via yes-or-no-p."
  ;; Taken from https://www.emacswiki.org/emacs/YesOrNoP.
  (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
            ((symbol-function 'yes-or-no-p) #'always-yes))
    (apply fn args)))

(defun util/get-frame->selected-window ()
  "Returns a list of pairs of (frame selected-window)"
  (let* ((original-frame (window-frame))
         (result (->> (visible-frame-list)
                      (-map (lambda (f)
                              (select-frame f t)
                              (list f (selected-window)))))))
    (select-frame original-frame t)
    result))

(defun util/preserve-selected-window (f)
  "Runs the given function and then restores focus to the original window. Useful when you want to
   invoke a function (like showing documentation) but desire to keep your current window focused."
  ;; Note that we must preserve the selected window of every frame, because the function being
  ;; executed may change the focused frame, even if the current frame is in focus.
  (let* ((original-frame (selected-frame))
                 (frames->windows (util/get-frame->selected-window))
                 (result (funcall f)))
    (-each frames->windows (lambda (x)
                             (select-frame (cl-first x) t)
                             (select-window (cl-second x) t)))
    (select-frame-set-input-focus original-frame t)
    result))

(defun util/preserve-line-and-column (f)
  "Runs the given function and restores the cursor to its former line and column. This is helpful
   when the text in the buffer moves (e.g. as a result of indentation commands). This is different
   from save-excursion which will restore the (point). This does not restore the cursor to the
   previous point."
  (let* ((former-line (line-number-at-pos))
                 (former-col (current-column))
                 (return-val (funcall f)))
    (goto-line former-line)
    (move-to-column former-col)
    return-val))

(defun util/thing-at-point-no-properties (thing)
  "Returns the text of the thing at point, but without properties. See `thing-at-point` for
details."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (when bounds ; NOTE(philc): I'm not sure if bounds-of-thing-at-point ever returns nil.
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun util/delete-thing-at-point (thing)
  "Deletes the thing at point, leaving the cursor at the beginning of the thing's region. See
  `thing-at-point`."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (delete-region (car bounds) (cdr bounds))))

;; Copied from http://ergoemacs.org/emacs/emacs_open_file_path_fast.html.
(defun util/open-file-at-cursor ()
  "Open the file path under cursor.
   If there is text selection, uses the text selection for path.
   If the path starts with “http://”, open the URL in browser.
   Input path can be {relative, full path, URL}.
   Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
   If path does not have a file extention, automatically try with “.el” for elisp files.
   This command is similar to `find-file-at-point' but without prompting for confirmation.
   URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'"
  (interactive)
  (let ((ξpath (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let (p0 p1 p2)
                   (setq p0 (point))
                   ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                   (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
                   (setq p1 (point))
                   (goto-char p0)
                   (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
                   (setq p2 (point))
                   (goto-char p0)
                   (buffer-substring-no-properties p1 p2))))
        (open-file-fn (lambda (path)
                        (let ((b (find-file-noselect path)))
                          (wm/show-buffer-rightward b)))))
    (if (string-match-p "\\`https?://" ξpath)
        (browse-url-default-browser ξpath)
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" ξpath)
            (progn
              (let (
                    (ξfpath (match-string 1 ξpath))
                    (ξline-num (string-to-number (match-string 2 ξpath))))
                (if (file-exists-p ξfpath)
                    (progn
                      (funcall open-file-fn ξfpath)
                      (goto-char 1)
                      (forward-line (1- ξline-num)))
                  (progn
                    (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξfpath))
                      (funcall open-file-fn ξfpath))))))
          (progn
            (if (file-exists-p ξpath)
                (funcall open-file-fn ξfpath)
              (if (file-exists-p (concat ξpath ".el"))
                  (funcall open-file-fn (conat ξfpath ".el"))
                (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξpath))
                  (funcall open-file-fn ξpath ))))))))))

(defmacro util/with-patch-function (fun-name fun-args fun-body &rest body)
  "Temporarily override the definition of FUN-NAME whilst BODY is executed.
   Assumes FUNCTION is already defined. See http://emacs.stackexchange.com/a/3452/304."
  `(cl-letf (((symbol-function ,fun-name)
              (lambda ,fun-args ,fun-body)))
     ,@body))

;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun util/read-file-as-string (path)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun util/show-and-scroll-repl-window (buffer)
  "Shows the REPL in a popup window and scrolls to its end, but does not switch to the window."
  (interactive)
  (util/preserve-selected-window
   (lambda ()
     (pop-to-buffer buffer nil t)
     (util/scroll-to-buffer-end buffer))))

(defun util/scroll-to-buffer-end (buffer)
  (let ((w (get-buffer-window buffer t)))
    (when w
      (with-selected-window w
        (View-scroll-to-buffer-end)))))

(defun util/select-region (start end)
  (set-mark start)
  (goto-char end)
  (activate-mark))

;; Calling this interactively can be very helpful in determining why a key isn't getting bound
;; by evil. Evil may be getting overriden by a keybinding on a text property.
(defun util/keymaps-at-point ()
  (interactive)
  "List keymaps that are active at the current point."
  (let ((keymaps (list)))
    (if (get-char-property (point) 'keymap)
        (push (get-char-property (point) 'keymap) keymaps))
    (if (get-char-property (point) 'local-map)
        (push (get-char-property (point) 'local-map) keymaps))
    (dolist (overlay (overlays-at (point)))
      (if (overlay-get overlay 'keymap)
          (push (overlay-get overlay 'keymap) keymaps))
      (if (overlay-get overlay 'local-map)
          (push (overlay-get overlay 'local-map) keymaps)))
    (progn (print "keymaps") (prin1 keymaps t))
    keymaps))

;; I use this occasionally to garbage collect unused buffers, so that desktop.el doesn't restore
;; them on Emacs startup.
(defun util/kill-non-visible-buffers ()
  "Kill all buffers that are not currently displayed in any window."
  (interactive)
  (let* ((exclusions '("*Messages*" "*scratch*"))
         (visible-buffers (util/visible-buffers-in-all-tabs))
         (to-delete (->> (buffer-list)
                         (-remove (lambda (b)
                                    (or (get-buffer-window b 'visible)
                                        (-contains? visible-buffers b)
                                        (-contains? exclusions (buffer-name b))))))))
    (dolist (b to-delete)
      (printall "Killing" (buffer-name b))
      (kill-buffer b))))

(defun util/visible-buffers-in-all-tabs ()
  "Returns all visible buffers in all tabs."
  (interactive)
  (let* ((original-wc (current-window-configuration))
         (visible-buffers '()))
    ;; Iterate over all tabs
    (dolist (tab (tab-bar-tabs))
      (let* ((tab-buffers '())
             (wc (alist-get 'wc tab)))
        ;; NOTE(philc): I don't really understand this code, but from printing values, it seems `wc`
        ;; is nil for whichever is the current tab in tab-bar-mode.
        (when wc
          (set-window-configuration wc)
          (dolist (win (window-list))
            (let ((buffer (window-buffer win)))
              (unless (memq buffer tab-buffers)
                (push buffer tab-buffers))))
          ;; Combine buffers from all tabs, avoiding duplicates
          (dolist (buf tab-buffers)
            (unless (memq buf visible-buffers)
              (push buf visible-buffers))))))
    (set-window-configuration original-wc)
    visible-buffers))
