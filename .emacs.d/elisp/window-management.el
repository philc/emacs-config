;;
;; Functions and configuration for window manipulation, management, and switching.
;;
(provide 'window-management)
(require 'dash)
(require 'framemove)

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
                                "magit-revision.*"
                                "*ghelp.*"))
(setq special-display-function 'wm/show-ephemeral-buffer-in-a-sensible-window)

;; A list of "special" (ephemeral) buffer names which should be focused after they are shown. Used
;; by wm/show-ephemeral-buffer-in-a-sensible-window
(setq special-display-auto-focused-buffers '())

;; Whether we can show an ephemeral buffer in other frames, if, say, another frame already contains
;; another ephemeral buffer that we want to replace with a new one.
(setq show-ephemeral-buffer-in-other-frames t)

(defun wm/ephemeral-window-p (w)
  "True if the given window is an 'ephemeral' window."
  (-> w window-buffer buffer-name special-display-p))

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
  "Given a buffer, shows the window in a split on the right side of the frame. If the buffer is
   already showing in some window, do nothing. If there's another ephemeral buffer already showing
   in a window, show this new one on top of that one."
  ;; NOTE(philc): Be careful about invoking `print` statements in this function when debugging it.
  ;; For some reason it interferes with Emacs' window switching behavior.
  (let* ((original-window (selected-window))
         (window-showing-buffer (get-buffer-window buffer show-ephemeral-buffer-in-other-frames))
         (ephemeral-window (first (wm/get-ephemeral-windows)))
         (should-create-new-window (and (not window-showing-buffer)
                                        (not ephemeral-window)
                                        (< (wm/column-count)
                                           (wm/max-column-count-for-frame-width))))
         (window (or window-showing-buffer
                     ephemeral-window
                     (if should-create-new-window
                         (progn
                           (wm/switch-to-lower-right)
                           (wm/create-new-column))
                       (save-excursion (wm/switch-to-lower-right) (selected-window))))))
    (display-buffer-record-window (if should-create-new-window 'window 'reuse) window buffer)
    (set-window-buffer window buffer)
    (when should-create-new-window (set-window-prev-buffers window nil))
    (select-window original-window)
    (when (member (buffer-name buffer) special-display-auto-focused-buffers)
      (select-window window))
    window))

(defun wm/switch-to-buffer-other-window (buffer)
  "Switches to the given buffer if it's showing already. If not, shows it on column 1 or 2, creating
   a new column (window) if there is only one column."
  (let* ((original-window (selected-window))
         (window-showing-buffer (get-buffer-window buffer show-ephemeral-buffer-in-other-frames))
         (should-create-new-window (and (not window-showing-buffer)
                                        (< (wm/column-count) 2)))
         (window (or window-showing-buffer
                     (when should-create-new-window
                         (wm/create-new-column))
                     ;; If we already have two columns, show the window in the column which is
                     ;; opposite to the current window.
                     (if (= (wm/column-number) 1)
                         (wm/window-in-column 0)
                         (wm/window-in-column 1)))))
    (display-buffer-record-window (if should-create-new-window 'window 'reuse) window buffer)
    (set-window-buffer window buffer)
    (when should-create-new-window (set-window-prev-buffers window nil))
    (select-window window)))

(defun wm/dismiss-ephemeral-windows ()
  "Dismisses any visible windows in the current frame identifiedy by `special-display-buffer-names`
   and `special-display-regexps`. I use this to quickly dismiss help windows, compile output, etc."
  (interactive)
  (save-excursion
    (let ((original-window (selected-window)))
      (dolist (w (wm/get-ephemeral-windows))
        (quit-window nil w))
      (select-window original-window))))

(defun wm/narrow-ephemeral-window ()
  "Narrows the ephemeral window (usually a REPL) so that 3 vertical splits can fit on a Mac
   Thunderbolt monitor: 2 splits which fit 110 chars without wrapping, and 1 narrower split with a
   REPL."
  (interactive)
  (when (first (wm/get-ephemeral-windows))
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
  (let ((ephemeral-window (first (wm/get-ephemeral-windows))))
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

(defun wm/column-count ()
  "Returns the number of vertical splits (or columns) in the current frame."
  (let* ((w (frame-first-window))
         (count 1))
    (while (setq w (window-in-direction 'right w))
      (setq count (1+ count)))
    count))

(defun wm/window-in-column (n)
  "Returns the topmost window in the nth column."
  (let* ((w (frame-first-window))
         (last-valid-window w)
         (count 0))
    (while (and (< count n)
                (setq w (window-in-direction 'right w)))
      (setq last-valid-window w)
      (setq count (1+ count)))
    (wm/topmost-window w)))

(defun wm/topmost-window (w)
  "Returns the topmost window in the direction above `w`, or `w` if it is the topmost."
  (while-let ((above (window-in-direction 'above w)))
    (setq w above))
  w)

(defun wm/switch-to-column (n)
  "Switches to the nth column. Does nothing if n >= the number of columns. Returns the window
   switched to."
  (-?> (wm/window-in-column n) select-window))

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
    (while (setq w (window-in-direction 'left w))
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

(defun wm/switch-to-upper-left ()
  (wm/while-window-changes (lambda () (ignore-errors (windmove-left 1))))
  (wm/while-window-changes (lambda () (ignore-errors (windmove-up 1)))))

(defun wm/switch-to-lower-right ()
  (wm/while-window-changes (lambda () (ignore-errors (windmove-right 1))))
  (wm/while-window-changes (lambda () (ignore-errors (windmove-down 1)))))

(defun wm/max-column-count-for-frame-width ()
  "Returns the desired max number of columns for the current frame. This count depends on how wide
   the frame is."
  (let* ((column-width 100))
    (/ (frame-width) column-width)))

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
