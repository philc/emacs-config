;;; -*- lexical-binding: t; -*-
;;
;; Functions and configuration for window manipulation, management, and switching.
;;
(provide 'window-management)
(require 'dash)

;; When switching between windows using windmove, also jump across frames if there are multiple
;; frames.
(setq framemove-hook-into-windmove t)

;; Settings for window splits.
(setq split-height-threshold 40)
(setq split-width-threshold 200)
(setq split-window-preferred-function 'wm/split-window-sensibly-reverse)
;; Don't auto-focus the help window. I have my own shortcuts for switching to it and closing it.
(setq help-window-select "never")
;; Ensure these open in the currently selected window, not a new popup.
(setq same-window-buffer-names '("*magit-log*"))

;; I manage my windows in a 4x4 grid. I want ephemeral or status-based buffers to always show in the
;; lower-right or right window, in that order of preference.
(setq special-display-buffer-names
      '("*Help*" "*compilation*" "COMMIT_EDITMSG" "*Messages*"
        ;; Clojure
        "*clojure-simple*" "*inf-clojure*" "*Cljfmt Errors*"
        ;; Emacs lisp
        "*Backtrace*"
        ;; Coffeescript
        "*coffee-compiled*"
        ;; Go
        "*Compile-Log*" "*Gofmt Errors*"
        ;; Javascript
        "*Javascript REPL*"
        "*REPL*"
        "*eldoc*"
        "*Completions*"))
(setq special-display-regexps '("*cider.*"
                                "magit-process.*"
                                "*ghelp.*"))
;; I don't want these windows to be opened using my standard window opening logic, because I have
;; dedicated specific places that I like to place them, but I do want them to be closed by
;; wm/dismiss-ephemeral-windows.
(setq extra-ephemeral-window-regexps-to-close '("^magit-revision"))
(setq special-display-function 'wm/show-ephemeral-buffer-in-a-sensible-window)

;; Instructions for where to place specific buffer types.
;; See https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(add-to-list 'display-buffer-alist
             ;; Show magit-revision buffers (the contents of a diff, from magit's log view) to the
             ;; right of the log view window.
             '("^magit-revision:"
               (lambda (buffer action-alist)
                 (wm/show-buffer-rightward buffer))))

;; A list of "special" (ephemeral) buffer names which should be focused after they are shown. Used
;; by wm/show-ephemeral-buffer-in-a-sensible-window
(setq special-display-auto-focused-buffers '())

;; Whether we can show an ephemeral buffer in other frames, if, say, another frame already contains
;; another ephemeral buffer that we want to replace with a new one.
(setq show-ephemeral-buffer-in-other-frames t)

(defun wm/ephemeral-window-p (w)
  "True if the given window is an 'ephemeral' window."
  (let ((name (-> w window-buffer buffer-name)))
    (or (special-display-p name)
        (-first (lambda (r) (string-match r name))
                extra-ephemeral-window-regexps-to-close))))

(defun wm/get-visible-windows ()
  "The list of visible windows across all frames."
  (->> (visible-frame-list)
       (--mapcat (window-list it))))

(defun wm/get-ephemeral-windows ()
  "Returns a list of windows which are showing ephemeral buffers. Searches all visible frames."
  (->> (wm/get-visible-windows)
       (-filter 'wm/ephemeral-window-p)))

;; References, for context:
;; http://snarfed.org/emacs_special-display-function_prefer-other-visible-frame
;; http://stackoverflow.com/questions/1002091/how-to-force-emacs-not-to-display-buffer-in-a-specific-window
;; The implementation of this function is based on `special-display-popup-frame` in window.el.
(defun wm/show-ephemeral-buffer-in-a-sensible-window (buffer &optional buffer-data)
  "Given a buffer, show the window in a split on the right side of the frame. If the buffer is
   already showing in some window, do nothing. If there's another ephemeral buffer already showing
   in a window, show this new one on top of that one."
  ;; NOTE(philc): Be careful about invoking `print` statements in this function when debugging it.
  ;; For some reason it interferes with Emacs' window switching behavior.
  (let* ((original-window (selected-window))
         (window-showing-buffer (get-buffer-window buffer show-ephemeral-buffer-in-other-frames))
         (ephemeral-window (cl-first (wm/get-ephemeral-windows)))
         (rightmost-window (wm/farthest-window-in-direction 'right))
         (rightmost-frame (window-frame rightmost-window))
         (should-create-new-window (and (not window-showing-buffer)
                                        (not ephemeral-window)
                                        (< (wm/column-count rightmost-frame)
                                           (wm/max-column-count-for-frame-width rightmost-frame))))
         (window (or window-showing-buffer
                     ephemeral-window
                     (if should-create-new-window
                         (progn
                           (wm/switch-to-lower-right)
                           (wm/create-new-column))
                       (save-excursion
                         (wm/switch-to-lower-right) (selected-window))))))
    (display-buffer-record-window (if should-create-new-window 'window 'reuse) window buffer)
    (set-window-buffer window buffer)
    (when should-create-new-window (set-window-prev-buffers window nil))
    ;; NOTE(philc): The window manager won't focus the window if it's in a different frame.
    ;; If I want that behavior, invoke select-frame-set-input-focus.
    (select-window original-window)
    ;; TODO(philc): Make sure the frame where the new window is shown is raised.
    (when (member (buffer-name buffer) special-display-auto-focused-buffers)
      (select-window window))
    window))

(defun wm/show-buffer-in-direction (direction buffer)
  "Switches to the given buffer if it's showing already. If not, shows it in a window in the given
   direction, and creates a new column / window if there aren't enough columns yet."
  (let* ((original-window (selected-window))
         (window-showing-buffer (get-buffer-window buffer show-ephemeral-buffer-in-other-frames))
         (should-create-new-window (and (not window-showing-buffer)
                                        ;; TODO(philc): Change this to be < max column count
                                        (< (wm/column-count) 2)))
         (window (or window-showing-buffer
                     (when should-create-new-window
                       (wm/create-new-column))
                     ;; If we have enough columns already, select the window in the given direction,
                     ;; if there is one.
                     (wm/frame-window-in-direction direction (selected-window)))))
    (display-buffer-record-window (if should-create-new-window 'window 'reuse) window buffer)
    (set-window-buffer window buffer)
    (when should-create-new-window (set-window-prev-buffers window nil))
    (select-window window)
    (select-frame-set-input-focus (window-frame window))))

(defun wm/show-buffer-rightward (buffer)
  (wm/show-buffer-in-direction 'right buffer))

(defun wm/dismiss-ephemeral-window ()
  "Dismisses the leftmost visible ephemeral window in the current frame identifiedy by
   `special-display-buffer-names`, `special-display-regexps`, and
   `extra-ephemeral-window-regexps-to-close`. I use this to quickly dismiss help windows, compile
   output, etc."
  (interactive)
  (save-excursion
    (let ((original-window (selected-window)))
      (when-let ((w (cl-first (wm/get-ephemeral-windows))))
        (quit-window nil w))
      (select-window original-window))))

(defun wm/narrow-ephemeral-window ()
  "Narrows the ephemeral window (usually a REPL) so that 3 vertical splits can fit on a Mac
   Thunderbolt monitor: 2 splits which fit 110 chars without wrapping, and 1 narrower split with a
   REPL."
  (interactive)
  (when (cl-first (wm/get-ephemeral-windows))
    (let* ((shrink-by-amt 12)
           (total-width (-> (frame-root-window) window-total-width))
           (vertical-splits (wm/column-count))
           (ephemeral-width (- (/ total-width vertical-splits) shrink-by-amt))
           ;; Split the width from `shrink-by-amt` evently between the non-ephemeral windows.
           (non-ephemeral-width (-> (/ total-width vertical-splits)
                                    (+ (/ shrink-by-amt (- vertical-splits 1))))))
      (dolist (w (wm/get-visible-windows))
        (wm/set-window-width w (if (wm/ephemeral-window-p w) ephemeral-width non-ephemeral-width))))))

(defun wm/set-window-width (window width)
  "Resizes a window to be the given size.
   - horizontal: if true, resize the window horizontally, otherwise, vertically."
  ;; NOTE(philc): I couldn't find an Emacs function which takes an absolute window size.
  ;; shrink-window only takes deltas from the window's current width.
  (let ((delta (- (window-total-width window)
                  width)))
    (util/preserve-selected-window
     (lambda ()
       (select-window window)
       (shrink-window delta t)))))

(setq buffer-underneath-maximized-ephemeral-window nil)

(defun wm/toggle-maximize-lower-right-window ()
  "Toggles the vertical maximization of an ephemeral window, whereever it's showing. If there's a
   window above or below it, that window will be saved and will be restored if maximization is
   toggled."
  ;; I usually have a REPL or diff view showing in the lower right. Often I want to "maximize it"
  ;; vertically, to view a long stacktrace etc., without having to switch to the upper right and
  ;; close that window.
  (interactive)
  (window-in-direction 'above (get-buffer-window "*Messages*"))
  (let ((ephemeral-window (cl-first (wm/get-ephemeral-windows))))
    (when ephemeral-window
      (let ((covered-window (or (window-in-direction 'above ephemeral-window)
                                (window-in-direction 'below ephemeral-window))))
        (if covered-window
            (progn
              (setq buffer-underneath-maximized-ephemeral-window (window-buffer covered-window))
              (delete-window covered-window))
          (when buffer-underneath-maximized-ephemeral-window
            (util/preserve-selected-window
             (lambda ()
               (select-window ephemeral-window)
               (split-window-vertically)
               (set-window-buffer (selected-window)
                                  buffer-underneath-maximized-ephemeral-window)))))))))

(defun wm/swap-window-buffers (window-move-fn)
  "Swaps the current buffer with the buffer in the window active after invoking window-move-fn."
  (let ((src-window (get-buffer-window))
        (src-buffer (window-buffer)))
    (funcall window-move-fn)
    (set-window-buffer src-window (current-buffer))
    (set-window-buffer (get-buffer-window) src-buffer)))

(defun wm/swap-window-left () (interactive) (wm/swap-window-buffers 'windmove-left))
(defun wm/swap-window-right () (interactive) (wm/swap-window-buffers 'windmove-right))
(defun wm/swap-window-down () (interactive) (wm/swap-window-buffers 'windmove-down))
(defun wm/swap-window-up () (interactive) (wm/swap-window-buffers 'windmove-up))

;; When a window in a tab is maximized, we save its configuration per tab. Emacs doesn't give us an
;; ID number to identify a tab, so we make due by storing the window configuration by tab name.
(setq tab-name->window-config (make-hash-table))

(defun wm/toggle-window-maximize ()
  (interactive)
  (let ((current-tab-name (->> (tab-bar--current-tab)
                               (alist-get 'name))))
    (if (= 1 (length (window-list)))
        (when-let ((config (gethash current-tab-name tab-name->window-config)))
          (set-window-configuration config t)
          (remhash current-tab-name tab-name->window-config))
      (progn
        (puthash current-tab-name (current-window-configuration) tab-name->window-config)
        (delete-other-windows)))))

(defun wm/split-window-vertically-and-focus (&optional w)
  (interactive)
  (let ((w (split-window-vertically nil (or w (selected-window)))))
    (select-window w)))

(defun wm/split-window-horizontally-and-focus (&optional w)
  (interactive)
  (let ((w (split-window-horizontally nil (or w (selected-window)))))
    (select-window w)))

(defun wm/column-count (&optional frame)
  "Returns the number of vertical splits (or columns) in the current frame."
  (let* ((w (frame-first-window frame))
         (count 1))
    ;; Note that window-in-direction won't traverse frames.
    (while (setq w (window-in-direction 'right w))
      (setq count (1+ count)))
    count))

;; Generated generated by Claude 3.7 Sonnet.
(defun wm/get-leftmost-frame ()
  "Return the leftmost frame across all monitors."
  (let ((frames (frame-list))
        (leftmost-frame nil)
        (min-x most-positive-fixnum))
    (dolist (frame frames)
      (when (frame-visible-p frame)
        (let* ((frame-pos (frame-position frame))
               (frame-x (car frame-pos)))
          ;; Handle the special case of negative coordinates
          (when (listp frame-x)
            ;; Convert from the special (- pos) format to a negative number
            (setq frame-x (- (cadr frame-x))))
          (when (< frame-x min-x)
            (setq min-x frame-x)
            (setq leftmost-frame frame)))))
    leftmost-frame))

(defun wm/window-in-column (n)
  "Returns the topmost window in the nth column."
  (let* ((f (wm/get-leftmost-frame))
         (w (frame-first-window f))
         (last-valid-window w)
         (count 0))
    (while (and (< count n)
                (setq w (wm/frame-window-in-direction 'right w)))
      (setq last-valid-window w)
      (setq count (1+ count)))
    (wm/topmost-window last-valid-window)))

;; Generated generated by Claude 3.7 Sonnet.
(defun wm/frame-window-in-direction (direction w)
  "Like window-in-direction, but it will also traverse frames. Find a window in DIRECTION, possibly
   in another frame. DIRECTION must be one of 'above, 'below, 'left, or 'right. Returns the window
   object if found, nil otherwise."
  (let* ((current-window w)
         (current-frame (window-frame current-window))
         (current-frame-pos (frame-position current-frame))
         (current-frame-left (car current-frame-pos))
         (current-frame-top (cdr current-frame-pos))
         (current-edges (window-edges current-window))
         (current-left (+ current-frame-left (nth 0 current-edges)))
         (current-top (+ current-frame-top (nth 1 current-edges)))
         (current-right (+ current-frame-left (nth 2 current-edges)))
         (current-bottom (+ current-frame-top (nth 3 current-edges)))
         (frames (frame-list))
         (candidate-window nil)
         (min-distance most-positive-fixnum))
    ;; First try window-in-direction within the current frame
    (setq candidate-window (window-in-direction direction w))
    ;; If no window found in current frame, look in other frames
    (unless (and candidate-window (window-live-p candidate-window))
      (dolist (frame frames)
        (unless (eq frame current-frame)
          (let ((frame-pos (frame-position frame))
                (frame-left (car (frame-position frame)))
                (frame-top (cdr (frame-position frame))))
            (dolist (window (window-list frame 'no-minibuf))
              (let* ((window-edges (window-edges window))
                     (window-left (+ frame-left (nth 0 window-edges)))
                     (window-top (+ frame-top (nth 1 window-edges)))
                     (window-right (+ frame-left (nth 2 window-edges)))
                     (window-bottom (+ frame-top (nth 3 window-edges)))
                     (distance nil))
                (cond
                 ((eq direction 'left)
                  (when (< window-right current-left)
                    (setq distance (+ (abs (- current-top window-top))
                                      (abs (- current-left window-right))))))
                 ((eq direction 'right)
                  (when (> window-left current-right)
                    (setq distance (+ (abs (- current-top window-top))
                                      (abs (- window-left current-right))))))
                 ((eq direction 'above)
                  (when (< window-bottom current-top)
                    (setq distance (+ (abs (- current-left window-left))
                                      (abs (- current-top window-bottom))))))
                 ((eq direction 'below)
                  (when (> window-top current-bottom)
                    (setq distance (+ (abs (- current-left window-left))
                                      (abs (- window-top current-bottom)))))))
                (when (and distance (< distance min-distance))
                  (setq min-distance distance)
                  (setq candidate-window window))))))))
    candidate-window))

(defun wm/topmost-window (w)
  "Returns the topmost window in the direction above `w`, or `w` if it is the topmost."
  (while-let ((above (window-in-direction 'above w)))
    (setq w above))
  w)

(defun wm/switch-to-column (n)
  "Switches to the nth column. Does nothing if n >= the number of columns. Returns the window
   switched to."
  (let ((w (wm/window-in-column n)))
    (select-frame-set-input-focus (window-frame w)) ; Needed if window is in another frame.
    (select-window w)))

(defun wm/switch-to-column-or-change-row (n)
  "Switches to the nth column. If the current column is already the nth column, then switch to the
   next split below the current split, wrapping to the top split if needed."
  (interactive)
  ;; Before changing the active window, save the buffer. This isn't necessary, but I like having it
  ;; in my workflow.
  (util/save-buffer-if-dirty)
  (if (/= n (wm/column-number))
      (wm/switch-to-column n)
    (if-let ((split-below (window-in-direction 'below (selected-window))))
        (select-window split-below)
      (select-window (wm/topmost-window (selected-window))))))

(defun wm/column-number ()
  "Returns the column number (zero-based) of the current window."
  (interactive)
  (let* ((w (selected-window))
         (count 0))
    (while (setq w (wm/frame-window-in-direction 'left w))
      (setq count (1+ count)))
    count))

(defun wm/while-window-changes (f)
  "Run the given function until the selected window stops changing after each invocation. This is
   useful because the return value of `windmove-right` is not reliable when combined with framemove.
   It can return false even though the selected window was successfully changed."
  ;; TODO(philc): I think a better approach with less quirks is to remove framemove and to provide
  ;; advice to windmove-* myself.
  (let ((window nil))
    (while (not (eq window (selected-window)))
      (setq window (selected-window))
      (funcall f))))

(defun wm/farthest-window-in-direction (direction)
  (let ((current nil)
        (next (selected-window)))
    (while next
      (setq current next)
      (setq next (wm/frame-window-in-direction direction current)))
    current))

(defun wm/switch-to-lower-right ()
  (let ((w (wm/farthest-window-in-direction 'right)))
    ;; TODO(philc): Consider also traversing downward, from here.
    (select-window w)))

(defun wm/max-column-count-for-frame-width (&optional frame)
  "Returns the desired max number of columns for the current frame. This count depends on how wide
   the frame is."
  (let* ((column-width 100))
    (/ (frame-width frame) column-width)))

(defun wm/create-new-column ()
  "Creates a new column in my window layout by splitting the rightmost window and rebalancing
   windows. Returns the new window."
  (interactive)
  (let* ((is-part-of-vertical-combination (window-combined-p))
         (window-to-split (if is-part-of-vertical-combination
                              (window-parent (selected-window))
                            (selected-window)))
         (b (current-buffer))
         (new-window (wm/split-window-horizontally-and-focus window-to-split)))
    ;; Ensure that no matter where the window is created, it has the same buffer as the window prior
    ;; to creating the new one. Otherwise, the new window could have some random buffer in it,
    ;; making it difficult to use commands like open-in-project, for instance.
    (set-window-buffer (selected-window) b)
    (balance-windows)
    new-window))

(defadvice windmove-do-window-select (after windowmove-change-to-normal-mode)
  "Ensure we reset to Evil's normal mode when switching windows."
  (evil-change-to-initial-state))
(ad-activate 'windmove-do-window-select)

(defun wm/split-window-sensibly-reverse (&optional window)
  "Identical to the built-in function split-window-sensibly, but prefers horizontal splits over
   vertical."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
       ;; Split window horizontally.
       (with-selected-window window
         (split-window-right)))
  (and (window-splittable-p window)
       ;; Split window vertically.(column-marker-1 80)
       (with-selected-window window
         (split-window-below)))
  (and (eq window (frame-root-window (window-frame window)))
       (not (window-minibuffer-p window))
       ;; If WINDOW is the only window on its frame and is not the minibuffer window, try to split
       ;; it vertically disregarding the value of `split-height-threshold'.
       (let ((split-height-threshold 0))
         (when (window-splittable-p window)
     (with-selected-window window
       (split-window-below))))))))

(defun wm/quarter-height ()
  "Set the current window to be a quarter of the frame height."
  (interactive)
   (let ((quarter-height (/ (frame-height) 4)))
     (enlarge-window (- quarter-height (window-height)))))
