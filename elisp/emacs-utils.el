;;; -*- lexical-binding: t; -*-
;;
;; Utility functions for programming Emacs.
;;
(provide 'emacs-utils)
(require 'lisp-utils)
(require 'browse-url)

;; This file is loaded early in init.el, before these, so it declares the functions it uses from them
;; rather than requiring them.
(declare-function evil-define-key* "evil-core")
(declare-function evil-visual-select "evil-states")
(declare-function View-scroll-to-buffer-end "view")
(declare-function wm/show-buffer-rightward "window-management")

(defvar global-leader-prefix ";"
  "The key sequence which prefixes the keybindings defined by `define-leader-keys`.")

(defun define-leader-keys (keymap &rest bindings)
  "Binds each key in BINDINGS, prefixed with `global-leader-prefix', in KEYMAP in Evil's normal and
   visual states. KEYMAP is a keymap's symbol (e.g. \\='org-mode-map), a list of them, or \\='global
   for Evil's global keymaps. If Evil isn't loaded yet, or the keymap isn't defined yet, the keys
   are bound once they are, as `evil-define-key' does.

   BINDINGS alternates keys, in `kbd' syntax, and their definitions. To give a prefix key a label in
   which-key's popup, bind it to (cons \"label\" (make-sparse-keymap)) before the keys it prefixes."
  (declare (indent 1))
  (if (consp keymap)
      (dolist (k keymap)
        (apply #'define-leader-keys k bindings))
    (let ((args (cl-loop for (key def) on bindings by #'cddr
                         append (list (kbd (concat global-leader-prefix " " key)) def))))
      (cl-flet ((bind ()
                  (apply #'evil-define-key* '(normal visual)
                         (if (eq keymap 'global) keymap (symbol-value keymap))
                         args)))
        (with-eval-after-load 'evil
          (if (or (eq keymap 'global) (boundp keymap))
              (bind)
            (letrec ((bind-when-defined (lambda (&rest _)
                                          (when (boundp keymap)
                                            (remove-hook 'after-load-functions bind-when-defined)
                                            (bind)))))
              (add-hook 'after-load-functions bind-when-defined))))))))

(defun util/call-process-with-exit-status (program stdin &rest args)
  "Runs PROGRAM with ARGS, passing STDIN as its input if non-nil. Returns a list containing the exit
   status and the output string."
  ;; E.g. (util/call-process-with-exit-status "ls" nil "-h" "-l") ;; => (0 "-rw-r--r-- 1 ...")
  (with-temp-buffer
    (when stdin
      (insert stdin))
    (list (apply 'call-process-region (point-min) (point-max) program t (current-buffer) nil args)
          (buffer-string))))

(defun util/call-process-and-check (program stdin &rest args)
  "Calls the given program and raises an error if the exit status is non-zero. The error's message
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

(defun util/diff-hunks (old new)
  "Compares strings OLD and NEW line by line. Returns the changed blocks as a list of
   (OLD-START-LINE OLD-LINE-COUNT NEW-TEXT), last block first. This uses git's diff, which takes
   0.08s on a 2MB file where macOS's diff takes 27s."
  (let ((old-file (make-temp-file "util-diff-old"))
        (new-file (make-temp-file "util-diff-new"))
        (coding-system-for-write 'utf-8-unix)
        (coding-system-for-read 'utf-8-unix)
        (default-directory temporary-file-directory)
        hunks)
    (unwind-protect
        (with-temp-buffer
          (write-region old nil old-file nil 'silent)
          (write-region new nil new-file nil 'silent)
          (let ((status (call-process "git" nil t nil "diff"
                                      ;; Compare two files, rather than files in a repo.
                                      "--no-index"
                                      ;; Plain diff output, unaffected by the user's git config:
                                      ;; no color codes, no external diff tool (diff.external),
                                      ;; and no textconv filters (diff.<driver>.textconv).
                                      "--no-color" "--no-ext-diff" "--no-textconv"
                                      ;; Diff the text even if git would deem it binary, e.g. if
                                      ;; it contains a NUL byte.
                                      "--text"
                                      ;; No context lines, so each hunk is only changed lines.
                                      "-U0"
                                      old-file new-file)))
            (unless (memq status '(0 1))
              (error "git diff failed: %s" (buffer-string))))
          (goto-char (point-min))
          ;; Hunk bodies' lines start with -, + or \, so only hunk headers match ^@@.
          (while (re-search-forward
                  "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+[0-9]+\\(?:,\\([0-9]+\\)\\)? @@.*\n"
                  nil t)
            (let* ((old-line (string-to-number (match-string 1)))
                   (old-count (if (match-string 2) (string-to-number (match-string 2)) 1))
                   (new-count (if (match-string 3) (string-to-number (match-string 3)) 1))
                   (new-lines nil))
              (forward-line old-count)
              (when (looking-at-p "\\\\")
                (forward-line 1)) ; "\ No newline at end of file"
              (dotimes (_ new-count)
                (let ((line (buffer-substring-no-properties (1+ (point)) (line-end-position))))
                  (forward-line 1)
                  (if (looking-at-p "\\\\")
                      (forward-line 1)
                    (setq line (concat line "\n")))
                  (push line new-lines)))
              ;; For a pure insertion, git gives the line *after which* to insert.
              (push (list (if (= old-count 0) (1+ old-line) old-line)
                          old-count
                          (apply #'concat (nreverse new-lines)))
                    hunks))))
      (delete-file old-file)
      (delete-file new-file))
    hunks))

(defun util/map-offsets (old new offsets)
  "Returns where each character offset in OFFSETS, into string OLD, lands in string NEW, using a
   character-level diff. An offset at the end of OLD maps to the end of NEW. The diff runs in
   unibyte buffers, which avoids the slow position conversions of multibyte ones."
  (if (string-empty-p old)
      (mapcar (lambda (_) (length new)) offsets)
    (let ((src (generate-new-buffer " *util-map-offsets*" t)))
      (unwind-protect
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert (encode-coding-string old 'utf-8-unix))
            (let ((markers (mapcar (lambda (o)
                                     (copy-marker (1+ (string-bytes (substring old 0 o)))
                                                  (= o (length old))))
                                   offsets)))
              (with-current-buffer src
                (set-buffer-multibyte nil)
                (insert (encode-coding-string new 'utf-8-unix)))
              ;; The diff is fast for the few lines a formatter usually changes, but when one span
              ;; is huge, like a minified file being reformatted, it can take over a minute (84s on
              ;; a 1.9MB file). Past the time limit, place the offsets proportionally.
              (if (replace-region-contents (point-min) (point-max) src 0.5)
                  (mapcar (lambda (m)
                            (min (length new)
                                 (length (decode-coding-string (buffer-substring (point-min) m)
                                                               'utf-8-unix))))
                          markers)
                (mapcar (lambda (o) (round (* o (/ (float (length new)) (length old)))))
                        offsets))))
        (kill-buffer src)))))

(defun util/common-prefix-length (a b)
  (let ((r (compare-strings a nil nil b nil nil)))
    (if (eq r t) (min (length a) (length b)) (1- (abs r)))))

;; Why not `replace-region-contents' on the whole text: it diffs character by character, and in a
;; buffer with any non-ASCII text, each comparison converts character positions to byte positions
;; by scanning the buffer. Reformatting a 67KB HTML file with a few em dashes took 33s that way,
;; versus 0.08s for the same file in ASCII. Instead, diff line by line, edit only the changed
;; blocks, and do the character-level work only to place point, in a small unibyte buffer.
(defun util/replace-text (beg end new-text)
  "Replaces the text between BEG and END with NEW-TEXT, editing only the parts that differ. Point,
   markers and overlays outside those parts stay put. Inside a changed part, point, the mark, the
   points of other windows showing the buffer, and the ends of folds (overlays with an `invisible'
   property, as outline and hideshow use) are placed on the same characters in the new text. The
   selected window keeps point on the same screen row. The whole replacement is one change for
   undo and change hooks."
  (let ((old (buffer-substring-no-properties beg end)))
    (unless (string= old new-text)
      (let* ((hunks (util/diff-hunks old new-text))
             (pt (copy-marker (point)))
             (win (and (eq (window-buffer) (current-buffer)) (selected-window)))
             (row (and win (-some-> (posn-at-point (point) win) posn-col-row cdr)))
             (other-windows (->> (get-buffer-window-list nil nil t)
                                 (remq win)
                                 (mapcar (lambda (w) (cons w (copy-marker (window-point w)))))))
             (markers (append (list pt)
                              (and (mark t) (list (mark-marker)))
                              (mapcar #'cdr other-windows))))
        (combine-change-calls beg end
          (save-restriction
            (narrow-to-region beg end)
            (goto-char (point-min))
            (let ((line 1))
              (dolist (h hunks)
                (forward-line (- (nth 0 h) line))
                (setq line (nth 0 h))
                (let* ((bs (point))
                       (be (progn (forward-line (nth 1 h)) (point)))
                       (old-block (buffer-substring-no-properties bs be))
                       (new-block (nth 2 h))
                       ;; Trim the block to the span that actually changed.
                       (p (util/common-prefix-length old-block new-block))
                       (s (min (util/common-prefix-length (reverse old-block) (reverse new-block))
                               (- (min (length old-block) (length new-block)) p)))
                       (eb (+ bs p))
                       (ee (- be s))
                       (old-mid (substring old-block p (- (length old-block) s)))
                       (new-mid (substring new-block p (- (length new-block) s)))
                       (delta (- (length new-mid) (length old-mid)))
                       (inside-p (lambda (pos) (and (< eb pos) (<= pos ee))))
                       ;; Deleting the span would collapse these to its start, so map them into
                       ;; the new text first. A position at the span's start stays there anyway.
                       (moved-markers (cl-loop for m in markers
                                               when (funcall inside-p m)
                                               collect (cons m (marker-position m))))
                       (folds (cl-remove-if-not
                               (lambda (ov)
                                 (and (overlay-get ov 'invisible)
                                      (or (funcall inside-p (overlay-start ov))
                                          (funcall inside-p (overlay-end ov)))))
                               (overlays-in eb (min (1+ ee) (point-max)))))
                       (fold-ends (mapcar (lambda (ov) (list ov (overlay-start ov) (overlay-end ov)))
                                          folds))
                       (positions (delete-dups
                                   (append (mapcar #'cdr moved-markers)
                                           (cl-loop for (_ s e) in fold-ends
                                                    append (cl-remove-if-not inside-p (list s e))))))
                       (mapped (and positions
                                    (cl-mapcar #'cons positions
                                               (util/map-offsets old-mid new-mid
                                                                 (mapcar (lambda (pos) (- pos eb))
                                                                         positions)))))
                       (new-pos (lambda (pos)
                                  (cond ((<= pos eb) pos)
                                        ((> pos ee) (+ pos delta))
                                        (t (+ eb (cdr (assq pos mapped))))))))
                  (goto-char eb)
                  (delete-region eb ee)
                  (insert new-mid)
                  ;; `new-pos' takes positions from before the edit, as recorded above.
                  (pcase-dolist (`(,m . ,pos) moved-markers)
                    (set-marker m (funcall new-pos pos)))
                  ;; `move-overlay' also restores a fold that the deletion evaporated.
                  (cl-loop for (ov s e) in fold-ends
                           do (move-overlay ov (funcall new-pos s) (funcall new-pos e)
                                            (current-buffer)))
                  ;; Point at a pure insertion goes after the inserted text, e.g. after a line
                  ;; inserted above point's line, or indentation added before point.
                  (when (and (= eb ee) (= pt eb))
                    (set-marker pt (+ eb (length new-mid))))
                  (goto-char bs))))
            (unless (string= (buffer-substring-no-properties (point-min) (point-max)) new-text)
              ;; Shouldn't happen, but never leave the buffer with the wrong text.
              (message "util/replace-text: patching failed; replaced the text wholesale")
              (delete-region (point-min) (point-max))
              (insert new-text))))
        (goto-char pt)
        (when row
          (recenter row))
        (pcase-dolist (`(,w . ,m) other-windows)
          (set-window-point w m)
          (set-marker m nil))
        (set-marker pt nil)))))

(defun util/get-line (&optional offset)
  "Returns the text (without string properties) of the line offset by `offset` from the current.
   This has the same behavior as `forward-line`, so if offset is larger than the buffer, the
   buffer's last line of text will be returned."
  (save-excursion
    (when offset
      (forward-line offset))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun util/get-current-line ()
  "Returns the text (without string properties) of the current line."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun util/define-keys (keymap &rest key-and-fn-pairs)
  "Like define-key, but takes a variable number of arguments -- two per key binding pair."
  (dolist (pair (partition key-and-fn-pairs 2))
    (define-key keymap (cl-first pair) (cl-second pair))))

(defun util/revert-buffers-changed-on-disk ()
  "Revert every unmodified buffer whose file was changed on disk, e.g. by an external script. This
   is immediate, whereas `global-auto-revert-mode` reverts buffers after a delay."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name
                 (not (buffer-modified-p))
                 (file-exists-p buffer-file-name)
                 (not (verify-visited-file-modtime buf)))
        (revert-buffer t t t)))))

(defun util/save-buffer-if-dirty ()
  (when (and buffer-file-name (buffer-modified-p))
    (util/save-buffer-silently)))

(defun util/save-buffer-silently ()
  "Saves the buffer while suppressing output to the *Messages* buffer. This avoids cluttering up the
   Messages buffer with lots of save output."
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
  (cl-letf (((symbol-function 'y-or-n-p) #'always)
            ((symbol-function 'yes-or-no-p) #'always))
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

(defun util/preserve-selected-frame (f)
  "Runs the given function and raises (refocuses) the existing frame afterwards, in case the
   selected frame was changed."
  (let ((frame (selected-frame)))
    (funcall f)
    (raise-frame frame)))

;; TODO(philc): I think I can switch this out for the built-in save-selected-window.
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

;; The built-in `goto-line' is interactive-only: it widens the buffer and pushes a mark, which is
;; undesirable when called from Elisp.
(defun util/goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun util/preserve-line-and-column (f)
  "Runs the given function and restores the cursor to its former line and column. This is helpful
   when the text in the buffer moves (e.g. as a result of indentation commands). This is different
   from save-excursion which will restore the (point). This does not restore the cursor to the
   previous point."
  (let* ((former-line (line-number-at-pos))
         (former-col (current-column))
         (return-val (funcall f)))
    (util/goto-line former-line)
    (move-to-column former-col)
    return-val))

(defun util/preserve-scroll-position (f)
  (let ((point-pos (point))
        (start-pos (window-start)))
    (unwind-protect
        (funcall f)
      (goto-char point-pos)
      (set-window-start (selected-window) start-pos))))

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
                   (skip-chars-backward "^ \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。`")
                   (setq p1 (point))
                   (goto-char p0)
                   (skip-chars-forward "^ \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。")
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
                (funcall open-file-fn ξpath)
              (if (file-exists-p (concat ξpath ".el"))
                  (funcall open-file-fn (concat ξpath ".el"))
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
  (save-selected-window
    (pop-to-buffer buffer nil t)
    (util/scroll-to-buffer-end buffer)))

(defun util/raise-repl-frame (buffer)
  "If the REPL buffer is visible in a window, in the case where that window is in a different frame
   than the current one, raise that window so we can see the REPL output."
  (let ((f (selected-frame)))
    ;; We must save the selected window, or when we return focus to the current frame after raising
    ;; the REPL's frame, the first window of the current frame will be selected.
    (save-selected-window
      (-?> buffer
           ;; Search all visible frames.
           (get-buffer-window "visible")
           window-frame
           raise-frame)
      ;; Restore focus back to the frame we were previously on.
      (raise-frame f))))

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
  "List keymaps that are active at the current point."
  (interactive)
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

(defun util/replace-string-in-buffer (from to)
  "Replace string in entire buffer using selected text or prompt. If region is active, use its
   content as replacement target string."
  (interactive
   (let ((from (if (use-region-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (read-string "Replace string: "))))
     (list from (read-string (format "Replace '%s' with: " from)))))
  (deactivate-mark)
  ;; This mirrors `replace-string`'s case handling, which we can't call non-interactively: the search
  ;; is case-insensitive if FROM has no uppercase letters, and the replacement then matches the case
  ;; of the text it replaces.
  (let ((case-fold-search (and case-fold-search search-upper-case
                               (isearch-no-upper-case-p from nil)))
        (count 0))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward from nil t)
        (replace-match to (not (and case-fold-search case-replace)) t)
        (setq count (1+ count))))
    (message "Replaced %d occurrence%s" count (if (= count 1) "" "s"))))

(defun util/time (fn)
  "Executes fn, prints how long it took, and returns its value."
  (let ((start (current-time)))
    (prog1
        (funcall fn)
      (let ((duration (float-time (time-since start))))
        (message "Duration: %.1f ms" (* 1000 duration))))))

(defun util/save-and-call (fn)
  "Returns an interactive command that saves the current buffer and then calls `fn`."
  (lambda ()
    (interactive)
    (util/save-buffer-if-dirty)
    (funcall fn)))

;; I use this when writing make files. The key combination to insert tabs is hard to remember. I
;; prefer a named commmand.
(defun util/insert-tab ()
  "Inserts a tab character."
  (interactive)
  (insert "\t"))

(defun util/clipboard-yank-and-remove-query-string ()
  "Assumes the clipboard contents are a URL, and strips everything after a query string and hash
   mark before inserting it. This exists because I often paste URLs into my notes and I don't want
   the extra clutter due to the URL's trackers, campaign IDs, etc."
  (interactive)
  (let ((clipboard nil))
    (with-temp-buffer
      (yank)
      (setq clipboard (buffer-string)))
    ;; NOTE(philc): We could check if this is a URL and avoid mangling the clipboard if it's not.
    ;; For now, assume it's a URL.
    (let ((trimmed-url (->> clipboard
                            (s-split "[?]")
                            cl-first
                            (s-split "#")
                            cl-first)))
      (insert trimmed-url))))

(defun util/clipboard-yank-and-remove-email-headers ()
  "Pastes the clipboard's contents at point, stripping any leading email
   headers (From, To, Date, Subject, etc.), e.g. when copying an email from
   Superhuman and wanting only the body."
  (interactive)
  ;; Expected clipboard shape, e.g. as copied from Superhuman:
  ;; From: Alice <alice@example.com>
  ;; To:   bob@example.com
  ;; Date: Sunday, August 23 2026 at 12:57 PM PDT
  ;;
  ;; Subject: Some subject line
  ;;
  ;; This is the actual email body.
  (let ((header-re "^[A-Za-z][A-Za-z0-9-]*:[ \t]")
        (lines (split-string (current-kill 0) "\n")))
    (while (and lines
                (or (string-match-p "^[ \t]*$" (car lines)) ; A blank (or whitespace-only) line.
                    (string-match-p header-re (car lines)))) ; A "Header-Name: value" line.
      (setq lines (cdr lines)))
    (->> lines
         (s-join "\n")
         string-trim
         insert)))

(defun util/replace-region-with-command-output (command-string)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-substring-no-properties (point-min) (point-max)))))
    (condition-case err
        ;; This will throw an error if the command exits with an error status.
        (let ((out (util/call-process-and-check "/bin/bash" input "-c" command-string)))
          (if (region-active-p)
              (util/replace-text (region-beginning) (region-end) out)
            (util/replace-text (point-min) (point-max) out)))
      (error
       (message "%s failed: %s"
                (cl-first (s-split " " command-string))
                (error-message-string err))))))

(defun util/run-deno-fmt (ext)
  (setenv "NO_COLOR" "1")
  (util/replace-region-with-command-output (format "deno fmt --ext %s -" ext)))

(defun util/log-word-under-cursor (&optional omit-the-value)
  "Inserts a logging statement for the word under the cursor, or the current selection.
   * omit-the-value: only log the string under the cursor, but don't log its value as a variable.
     This is useful when you want to print some tracing statements."
  (interactive)
  (let* ((word
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'word t)))
         (word (->> word
                    s-trim
                    (s-replace "\n" "")
                    (s-chop-suffix ";")))
         (format-str
          (if omit-the-value
              (pcase major-mode
                ('js-mode "console.log(\"%s\");")
                ('go-mode "fmt.Println(\"%s\");")
                ('rust-mode "println!(\"%s\");")
                ('emacs-lisp-mode "(printall \"%s\"))"))
            (pcase major-mode
              ('js-mode "console.log(\"%s:\", %s);")
              ('go-mode "fmt.Println(\"%s:\", %s);")
              ('rust-mode "println!(\"%s: {:?}\", %s);")
              ('emacs-lisp-mode "(printall \"%s\" %s)"))))
         (statement
          (format format-str
                  ;; Escape any quotes.
                  (s-replace "\"" "\\\"" word)
                  word)))
    (end-of-line)
    (insert "\n")
    (indent-according-to-mode)
    (insert statement)))

(defun util/log-word-under-cursor-without-value ()
  (interactive)
  (util/log-word-under-cursor t))

;; Taken from https://groups.google.com/forum/#!topic/gnu.emacs.help/vASrP0P-tXM
(defun util/recenter-no-redraw (&optional arg)
  "Centers the viewport around the cursor."
  (interactive "P")
  (let ((recenter-redisplay nil))
    (recenter arg)))
