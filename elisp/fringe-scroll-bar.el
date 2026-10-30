;;; -*- lexical-binding: t; -*-
;;
;; A scroll bar thumb drawn in the right fringe of each window, which shows where the window is in
;; its buffer. A replacement for the yascroll package, designed to cost nothing while typing.
;;
;; Emacs can't draw in the fringe directly, so the thumb is made of overlays, one per line of the
;; thumb, each with a fringe bitmap and limited to its window. Changing an overlay makes redisplay
;; redraw every window showing that buffer in full, rather than just the lines which changed, so
;; this avoids touching the overlays unless the thumb has moved:
;;
;; - It updates just before a window is redisplayed (and when redisplay scrolls a window), rather
;;   than from the buffer change hooks, so a command which edits many lines updates the thumb once.
;; - It only recomputes the thumb when the window has scrolled or resized, an edit has added or
;;   removed a newline, or outline folding has changed. Other edits (e.g. typing a character) can't
;;   move the thumb.
;; - Each window's thumb overlays are reused and moved, rather than deleted and recreated, and only
;;   moved when their positions change. While typing on a line, they don't change.
;; - Lines are counted with `line-number-at-pos', not `count-lines'. count-lines narrows the
;;   buffer, and narrowing during redisplay also makes redisplay redraw the window in full.
;;
;; The thumb's size and position are in visible lines: lines hidden by folding (by invisible
;; overlays, as outline-mode uses) aren't counted, so the thumb matches what can be scrolled to.
;;
;; Placing the thumb uses `vertical-motion', so that it counts screen lines (skipping folded text,
;; and counting wrapped lines). That lays out the text, which can font-lock it, but only text that
;; the window is about to show, which redisplay would font-lock next anyway. yascroll did the same
;; thing, but on every buffer change, so a command which edited many lines did it many times.
;;
(provide 'fringe-scroll-bar)
(require 'cl-lib)

(defface fsb/thumb '((t :foreground "#666666" :background "#666666"))
  "The face of the scroll bar thumb."
  :group 'scrolling)

(defvar fsb/disabled-modes '(image-mode)
  "Major modes whose windows don't show a scroll bar.")

(defvar fsb/windows (make-hash-table :test 'eq)
  "Maps each window which has shown a thumb to its state: a plist with :overlays, the thumb
   overlays, and :key, the inputs which the thumb's current position was computed from.")

(defvar-local fsb/newline-edits 0
  "How many edits have added or removed a newline in this buffer.")

(defvar-local fsb/deleting-newline nil
  "Whether the change about to happen deletes a newline.")

(defun fsb/before-change (beg end)
  (setq fsb/deleting-newline
        (and (< beg end) (save-excursion (goto-char beg) (search-forward "\n" end t)))))

(defun fsb/after-change (beg end _old-len)
  (when (or fsb/deleting-newline (save-excursion (goto-char beg) (search-forward "\n" end t)))
    (cl-incf fsb/newline-edits)))

(defvar-local fsb/visibility-changes 0
  "How many times outline folding has changed in this buffer.")

(defun fsb/note-visibility-change ()
  (cl-incf fsb/visibility-changes))

(defun fsb/hidden-lines (start)
  "Returns (BEFORE . TOTAL): how many lines invisible overlays hide before START, and in the whole
   buffer."
  (let ((before 0) (total 0) (covered (point-min))
        (overlays (seq-filter (lambda (o) (invisible-p (overlay-get o 'invisible)))
                              (overlays-in (point-min) (point-max)))))
    (dolist (overlay (sort overlays (lambda (a b) (< (overlay-start a) (overlay-start b)))))
      ;; Skip any part of this overlay which an earlier one already hides.
      (let ((beg (max (overlay-start overlay) covered))
            (end (overlay-end overlay)))
        (when (< beg end)
          ;; line-number-at-pos counts from the start of the buffer, so with hundreds of small folds,
          ;; subtracting line numbers is ~15x slower than counting each fold's newlines in a loop.
          ;; But for one large fold, the loop is ~10x slower.
          (let ((lines 0))
            (if (> (- end beg) 10000)
                (setq lines (- (line-number-at-pos end) (line-number-at-pos beg)))
              (save-excursion
                (goto-char beg)
                (while (search-forward "\n" end t)
                  (cl-incf lines))))
            (cl-incf total lines)
            (when (<= end start) (cl-incf before lines)))
          (setq covered end))))
    (cons before total)))

(defun fsb/thumb-positions (window start)
  "Returns the buffer positions of the lines where WINDOW's thumb goes, if WINDOW's display starts
   at START, or nil if it shouldn't have a thumb."
  (let* ((window-lines (window-body-height window))
         (hidden (fsb/hidden-lines start))
         (buffer-lines (- (line-number-at-pos (point-max)) (cdr hidden))))
    (when (and (< window-lines buffer-lines)
               (> (cadr (window-fringes window)) 0)
               (not (memq major-mode fsb/disabled-modes)))
      (let* ((top (- (line-number-at-pos start) 1 (car hidden)))
             (size (max 1 (floor (* window-lines (/ (float window-lines) buffer-lines)))))
             (thumb-line (floor (* window-lines (/ (float top) buffer-lines))))
             (positions nil))
        (save-excursion
          (goto-char start)
          (when (= thumb-line (vertical-motion thumb-line window))
            (push (point) positions)
            (while (and (< (length positions) size)
                        (= 1 (vertical-motion 1 window))
                        (not (eobp)))
              (push (point) positions))))
        (nreverse positions)))))

(defun fsb/make-overlay ()
  (let ((overlay (make-overlay 1 1)))
    (overlay-put overlay 'before-string
                 (propertize "." 'display '(right-fringe filled-rectangle fsb/thumb)))
    overlay))

(defun fsb/update (window &optional start)
  "Updates WINDOW's thumb, if its display starts at START (by default, its current start)."
  (let* ((start (or start (window-start window)))
         (state (gethash window fsb/windows))
         (key (list (current-buffer) start fsb/newline-edits fsb/visibility-changes
                    (window-body-height window)
                    (window-fringes window))))
    ;; The thumb only depends on these, so skip the line counting when none have changed.
    (unless (equal key (plist-get state :key))
      (let ((positions (unless (window-minibuffer-p window) (fsb/thumb-positions window start)))
            (overlays (plist-get state :overlays)))
        (while (< (length overlays) (length positions))
          (let ((overlay (fsb/make-overlay)))
            (overlay-put overlay 'window window)
            (setq overlays (append overlays (list overlay)))))
        (cl-loop for overlay in overlays
                 for pos = (pop positions)
                 do (cond ((null pos) (delete-overlay overlay))
                          ((not (and (eq (overlay-buffer overlay) (current-buffer))
                                     (= (overlay-start overlay) pos)))
                           (move-overlay overlay pos pos (current-buffer)))))
        (puthash window (list :overlays overlays :key key) fsb/windows)))))

(defun fsb/before-redisplay (window)
  (fsb/update window))

(defun fsb/after-scroll (window start)
  ;; Redisplay calls this when it scrolls the window, which happens after `fsb/before-redisplay',
  ;; e.g. when point has moved off the screen.
  (fsb/update window start))

(defun fsb/forget-deleted-windows ()
  (maphash (lambda (window state)
             (unless (window-live-p window)
               (mapc #'delete-overlay (plist-get state :overlays))
               (remhash window fsb/windows)))
           fsb/windows))

(define-minor-mode fringe-scroll-bar-mode
  "Shows a scroll bar thumb in the right fringe of each window."
  :global t
  :group 'scrolling
  (if fringe-scroll-bar-mode
      (progn
        (add-hook 'before-change-functions #'fsb/before-change)
        (add-hook 'after-change-functions #'fsb/after-change)
        (add-hook 'outline-view-change-hook #'fsb/note-visibility-change)
        (add-hook 'pre-redisplay-functions #'fsb/before-redisplay)
        (add-hook 'window-scroll-functions #'fsb/after-scroll)
        (add-hook 'window-configuration-change-hook #'fsb/forget-deleted-windows))
    (remove-hook 'before-change-functions #'fsb/before-change)
    (remove-hook 'after-change-functions #'fsb/after-change)
    (remove-hook 'outline-view-change-hook #'fsb/note-visibility-change)
    (remove-hook 'pre-redisplay-functions #'fsb/before-redisplay)
    (remove-hook 'window-scroll-functions #'fsb/after-scroll)
    (remove-hook 'window-configuration-change-hook #'fsb/forget-deleted-windows)
    ;; Remove the thumbs from every window. Without the hooks they'd no longer update, but they'd
    ;; stay drawn in the fringe.
    (maphash (lambda (_ state) (mapc #'delete-overlay (plist-get state :overlays))) fsb/windows)
    (clrhash fsb/windows)))
