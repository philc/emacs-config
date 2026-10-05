;;; -*- lexical-binding: t; -*-
;;
;; Commands for editing JavaScript source: formatting, linting, navigating to definitions, and
;; renaming variables. The heavy lifting is done by the Deno scripts in scripts/.
;;
(provide 'js-editing)
(require 'lisp-utils)
(require 'emacs-utils)
(require 'evil)
(require 'projectile)

(defun js/format-buffer ()
  "Format and replace the current buffer's contents using `deno fmt`."
  (interactive)
  (let ((ext (or (-> (buffer-file-name) (file-name-extension))
                 ;; The file might not have an extension. Assume javascript.
                 "js")))
    (util/run-deno-fmt ext)))

(defun js/lint-file ()
  (interactive)
  (save-buffer)
  (compile (format "deno lint %s" (buffer-file-name))))

(defun js/lint-project ()
  (interactive)
  (save-buffer)
  (compile (format "deno lint %s" (projectile-project-root))))

(defun js/goto-def-in-file ()
  (interactive)
  (let* ((bin (expand-file-name "scripts/list_symbols.js" user-emacs-directory))
         (lines (->> (util/call-process-and-check bin
                                                  nil
                                                  (buffer-file-name)
                                                  (projectile-project-root))
                     s-trim
                     (s-split "\n")))
         (symbols (mapcar (lambda (s)
                            (cl-second (s-split " " s)))
                          lines))
         (selected (completing-read "fn: " symbols nil t))
         (index (-elem-index selected symbols))
         (line-col (->> lines
                        (nth index)
                        (s-split " ")
                        cl-first
                        (s-split ":")))
         (line (-> line-col cl-first string-to-number))
         (col (-> line-col cl-second string-to-number)))
    (util/goto-line line)
    (move-to-column col)))

(defun js/goto-def ()
  (interactive)
  (let* ((bin (expand-file-name "scripts/goto_def.js" user-emacs-directory))
         (filename-arg (format "%s:%s:%s"
                               (buffer-file-name)
                               (line-number-at-pos)
                               (evil-column-of-last-char)))
         (result (util/call-process-with-exit-status bin nil filename-arg))
         (exit-code (cl-first result))
         (lines (-?>> result
                      cl-second
                      s-trim
                      (s-split "\n"))))
    (if (= exit-code 1)
        (message (s-join "\n" lines))
      (let*
          ;; TODO(philc): If there are multiple matches, handle that.
          ((result (cl-first lines))
           (components (s-split ":" result))
           (path (nth 0 components))
           (line (->> components (nth 1) string-to-number))
           (col (->> components (nth 2) string-to-number)))
        ;; Before moving the cursor, exit visual mode if there is a selection.
        (evil-normal-state)
        (when (not (string= path (buffer-file-name)))
          (find-file path))
        (util/goto-line line)
        ;; move-to-column uses zero-based column numbers.
        (move-to-column col)
        (evil-scroll-line-to-center nil)))))

(defun js/nearest-symbol-position (name)
  "Return the start of the occurrence of the symbol `name` which is nearest to the cursor."
  (let* ((re (concat "\\_<" (regexp-quote name) "\\_>"))
         (before (save-excursion (when (re-search-backward re nil t) (point))))
         (after (save-excursion (when (re-search-forward re nil t) (match-beginning 0)))))
    (if (and before after)
        (if (< (- (point) before) (- after (point)))
            before
          after)
      (or before after))))

(defun js/rename-symbol ()
  "Rename a JS variable everywhere it's in scope: within its block or function, its file, or
   across the project's files. If text is selected, that's the variable to rename. Otherwise,
   prompt for the variable's name, defaulting to the symbol under the cursor. See
   scripts/rename.js."
  (interactive)
  (let* ((script (expand-file-name "scripts/rename.js" user-emacs-directory))
         ;; Outside of a project, use the directory of the current buffer's file.
         (project-root (expand-file-name (or (projectile-project-root) default-directory)))
         ;; The selection, as (beginning . end). Evil's visual range includes the character under
         ;; the cursor, unlike Emacs's region.
         (selection (cond ((evil-visual-state-p)
                           (let ((range (evil-visual-range)))
                             (cons (evil-range-beginning range) (evil-range-end range))))
                          ((use-region-p)
                           (cons (region-beginning) (region-end)))))
         (symbol-bounds (bounds-of-thing-at-point 'symbol))
         (old-name (if selection
                       (buffer-substring-no-properties (car selection) (cdr selection))
                     (read-string "Rename: " (thing-at-point 'symbol t))))
         (new-name (read-string (format "Rename %s to: " old-name)))
         ;; The script needs the position of an occurrence of the variable, to determine its scope.
         (pos (cond (selection (car selection))
                    ((and symbol-bounds
                          (string= old-name (buffer-substring-no-properties (car symbol-bounds)
                                                                            (cdr symbol-bounds))))
                     (car symbol-bounds))
                    (t (or (js/nearest-symbol-position old-name)
                           (user-error "%s doesn't appear in this buffer." old-name))))))
    (when (evil-visual-state-p)
      (evil-normal-state))
    ;; The script edits the files on disk, so unsaved changes in the project must be saved first.
    (save-some-buffers t (lambda ()
                           (string-prefix-p project-root (expand-file-name buffer-file-name))))
    (let* ((line (line-number-at-pos pos t))
           (column (- pos (save-excursion (goto-char pos) (line-beginning-position))))
           (filename-arg (format "%s:%s:%s" (buffer-file-name) line column))
           (result (util/call-process-with-exit-status
                    script nil filename-arg old-name new-name project-root)))
      ;; Revert the changed buffers now, rather than leaving it to `global-auto-revert-mode`. That
      ;; mode can't revert until this command returns, so the buffers would briefly show the old
      ;; name and could be edited before reverting.
      (when (= (cl-first result) 0)
        (util/revert-buffers-changed-on-disk))
      (message "%s" (s-trim (cl-second result))))))
