;;; mlm-bench-measure.el --- Benchmarks of markdown-lite-mode -*- lexical-binding: t; -*-

;; mlm-bench.js loads this into a throwaway Emacs daemon, after a compiled markdown-lite-mode, and
;; calls `mlm-bench-measure/measure' there. It runs in a daemon rather than in batch mode, because
;; batch mode never turns on font-lock.
;;
;; Each operation starts in a fresh buffer, so it pays for whatever it needs computed (highlighting,
;; syntax properties, the outline structure), as it would in a newly opened file. The edits are
;; never saved.

(require 'cl-lib)
(require 'consult)
(require 'markdown-lite-mode)

(defconst mlm-bench-measure/item-with-continuation-regexp "^\\( *\\)\\* .*\n\\1  +[^ \n*+-]"
  "A list item marker line followed by a continuation line (e.g. the item's wrapped text).")

(defun mlm-bench-measure/fresh-buffer (file)
  "Returns a new buffer visiting FILE in markdown-lite-mode, killing any existing one."
  (let ((b (get-file-buffer file)))
    (when b
      (with-current-buffer b (set-buffer-modified-p nil))
      (kill-buffer b)))
  (with-current-buffer (find-file-noselect file)
    (unless (derived-mode-p 'markdown-lite-mode) (markdown-lite-mode))
    (current-buffer)))

(defmacro mlm-bench-measure/with-fresh-buffer (file &rest body)
  "Runs BODY in a fresh buffer of FILE, and kills it afterwards without saving."
  (declare (indent 1))
  `(with-current-buffer (mlm-bench-measure/fresh-buffer ,file)
     (unwind-protect (progn ,@body)
       (set-buffer-modified-p nil)
       (kill-buffer))))

(defmacro mlm-bench-measure/time-ms (&rest body)
  "Runs BODY after a garbage collection, so that a GC left over from earlier work doesn't land in
   the timing, and returns how long BODY took in ms."
  `(progn (garbage-collect)
          (let ((t0 (float-time)))
            ,@body
            (* 1000 (- (float-time) t0)))))

(defun mlm-bench-measure/fontify-chunks (start end)
  "Fontifies START to END a jit-lock chunk at a time, as scrolling through it would."
  (cl-loop for s from start below end by jit-lock-chunk-size
           do (jit-lock-fontify-now s (min end (+ s jit-lock-chunk-size)))))

(defun mlm-bench-measure/refontify-visible (n)
  "Fontifies N visible lines, starting 20 visible lines above point, approximating what redisplay
   does for a window around point. jit-lock-fontify-now skips text which is already fontified, so
   this only costs something for text an edit invalidated."
  ;; Jump over folded (invisible) text in one step, rather than line by line, which in a folded view
  ;; of a large buffer can be tens of thousands of lines, and would dominate the timing.
  (save-excursion
    (let ((i 0))
      (while (and (< i 20) (not (bobp)))
        (forward-line -1)
        (if (invisible-p (point))
            (progn (goto-char (previous-single-char-property-change (point) 'invisible))
                   (beginning-of-line))
          (cl-incf i))))
    (let ((i 0))
      (while (and (< i n) (not (eobp)))
        (if (invisible-p (point))
            (progn (goto-char (next-single-char-property-change (point) 'invisible))
                   (unless (bolp) (forward-line 1)))
          (jit-lock-fontify-now (point) (min (point-max) (+ (point) jit-lock-chunk-size)))
          (cl-incf i)
          (forward-line 1))))))

(defun mlm-bench-measure/goto-visible-line (frac regexp)
  "Moves to the start of the first visible match of REGEXP (which should start with \"^\") at or
   after FRAC of the buffer. Returns nil if there isn't one."
  (goto-char (floor (* frac (point-max))))
  (let (found)
    (while (and (not found) (re-search-forward regexp nil t))
      (goto-char (match-beginning 0))
      (if (invisible-p (point))
          (forward-line 1)
        (setq found t)))
    found))

(defun mlm-bench-measure/time-edit (fn)
  "Warms up the view around point, then times calling FN plus refontifying the view. Returns ms."
  (mlm-bench-measure/refontify-visible 60)
  (mlm-bench-measure/time-ms (funcall fn)
                             (mlm-bench-measure/refontify-visible 60)))

(defun mlm-bench-measure/time-typing (n)
  "Types N characters at the end of the line at point, refontifying the view around point after
   each one. Returns the average ms per keystroke."
  (mlm-bench-measure/refontify-visible 60)
  (/ (mlm-bench-measure/time-ms (dotimes (_ n)
                                  (end-of-line)
                                  (insert "x")
                                  (mlm-bench-measure/refontify-visible 60)))
     n))

(defun mlm-bench-measure/time-promote-ops (ops)
  "Times each of OPS, which are (NAME . COMMAND), on the list item at point. Returns (NAME . MS)
   pairs."
  (let ((pos (line-beginning-position))
        (results nil))
    (dolist (op ops)
      (goto-char pos)
      (back-to-indentation)
      (push (cons (car op) (mlm-bench-measure/time-edit (cdr op))) results)
      (setq pos (line-beginning-position)))
    (nreverse results)))

(defun mlm-bench-measure/measure (file)
  "Times each operation on FILE. Returns an alist of (NAME . MS), where each NAME is a symbol, e.g.
   `gh\\ menu', so that `json-serialize' turns the alist into a JSON object. An operation which
   doesn't apply to FILE (e.g. the gh menu, in a file without headings or list items) is left out."
  (let ((results nil))
    (cl-flet ((add (name ms) (when ms (push (cons (intern name) ms) results))))
      ;; The `gh' menu's candidates (`mlm/consult-outline'), highlighted, as the menu shows them.
      (mlm-bench-measure/with-fresh-buffer file
        (add "gh menu"
             (ignore-error user-error
               (mlm-bench-measure/time-ms (let ((consult-fontify-preserve t))
                                            (consult--outline-candidates))))))
      (mlm-bench-measure/with-fresh-buffer file
        (add "highlight whole file"
             (mlm-bench-measure/time-ms
              (mlm-bench-measure/fontify-chunks (point-min) (point-max)))))
      ;; Opening the file at its end, which highlights one screen (~3000 chars).
      (mlm-bench-measure/with-fresh-buffer file
        (add "first screen at end"
             (mlm-bench-measure/time-ms (jit-lock-fontify-now (max (point-min) (- (point-max) 3000))
                                                              (point-max)))))
      (dolist (level '(1 2))
        (mlm-bench-measure/with-fresh-buffer file
          (add (format "fold to level %d" level)
               (mlm-bench-measure/time-ms (mlm/show-level level)))))
      ;; Typing, per keystroke.
      (mlm-bench-measure/with-fresh-buffer file
        (goto-char (max (point-min) (- (point-max) 2000)))
        (add "type near end" (mlm-bench-measure/time-typing 20)))
      (mlm-bench-measure/with-fresh-buffer file
        (when (mlm-bench-measure/goto-visible-line
               0.5 mlm-bench-measure/item-with-continuation-regexp)
          (let ((pos (point)))
            (add "type on item" (mlm-bench-measure/time-typing 20))
            (goto-char pos)
            (forward-line 1)
            (add "type on continuation line" (mlm-bench-measure/time-typing 20)))))
      ;; Promote and demote, on a nested list item halfway through the file, unfolded and folded.
      (mlm-bench-measure/with-fresh-buffer file
        (when (mlm-bench-measure/goto-visible-line 0.5 "^  \\* ")
          (dolist (r (mlm-bench-measure/time-promote-ops
                      '(("demote" . mlm/markdown-demote)
                        ("promote" . mlm/markdown-promote)
                        ("demote subtree" . mlm/markdown-demote-subtree)
                        ("promote subtree" . mlm/markdown-promote-subtree))))
            (add (car r) (cdr r)))))
      (dolist (level '(1 2))
        (mlm-bench-measure/with-fresh-buffer file
          (mlm/show-level level)
          (when (mlm-bench-measure/goto-visible-line 0.5 "^ *\\* ")
            (dolist (r (mlm-bench-measure/time-promote-ops '(("demote" . mlm/markdown-demote)
                                                             ("promote" . mlm/markdown-promote))))
              (add (format "%s, folded to level %d" (car r) level) (cdr r)))))))
    (nreverse results)))

(provide 'mlm-bench-measure)
