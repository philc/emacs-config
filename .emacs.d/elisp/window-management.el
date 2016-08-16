;;
;; Functions and configuration for window manipulation, management, and switching.
;;
(provide 'window-management)
(require 'dash)

;; Settings for window splits.
(setq split-height-threshold 40)
(setq split-width-threshold 200)
(setq split-window-preferred-function 'split-window-sensibly-reverse)
;; Don't auto-focus the help window. I have my own shortcuts for switching to it and closing it.
(setq help-window-select "never")
;; Ensure these open in the currently selected window, not a new popup.
(setq same-window-buffer-names '("*magit-log*"))

;; I manage my windows in a 4x4 grid. I want ephemeral or status-based buffers to always show in the
;; lower-right or right window, in that order of preference.
(setq special-display-buffer-names '("*Help*" "*compilation*" "COMMIT_EDITMSG" "*Messages*"
                                     ;; Clojure
                                     "*inf-clojure*" "*Cljfmt Errors*"
                                     ;; Magit
                                     "*magit-process*" "*magit-commit*"
                                     ;; Emacs lisp
                                     "*Backtrace*"
                                     ;; Coffeescript
                                     "*coffee-compiled*"
                                     ;; Go
                                     "*Compile-Log*" "*Gofmt Errors*"
                                     ;; Mu4e (email)
                                     "*mu4e-update*" ; Note this doesn't work; it shows wherever it wants.
                                     "*Completions*"))
(setq special-display-regexps '("*cider.*"))
(setq special-display-function 'show-ephemeral-buffer-in-a-sensible-window)

;; A list of "special" (ephemeral) buffer names which should be focused after they are shown. Used by
;; show-ephemeral-buffer-in-a-sensible-window
(setq special-display-auto-focused-buffers '())

;; Whether we can show an ephemeral buffer in other frames, if, say, another frame already contains another
;; ephemeral buffer that we want to replace with a new one.
(setq show-ephemeral-buffer-in-other-frames t)

(defun ephemeral-window-p (w)
  "True if the given window is an 'ephemeral' window."
  (-> w window-buffer buffer-name special-display-p))

(defun get-visible-windows ()
  "The list of visible windows across all frames."
  (->> (visible-frame-list)
       (--mapcat (window-list it))))

(defun get-ephemeral-windows ()
  "Returns a list of windows which are showing ephemeral buffers. Searches all visible frames."
  (->> (get-visible-windows)
       (-filter 'ephemeral-window-p)))

;; References, for context:
;; http://snarfed.org/emacs_special-display-function_prefer-other-visible-frame
;; http://stackoverflow.com/questions/1002091/how-to-force-emacs-not-to-display-buffer-in-a-specific-window
;; The implementation of this function is based on `special-display-popup-frame` in window.el.
(defun show-ephemeral-buffer-in-a-sensible-window (buffer &optional buffer-data)
  "Given a buffer, shows the window in a split on the right side of the frame. If the buffer is already
   showing in some window, do nothing. If there's another ephemeral buffer already showing in a window,
   show this new one on top of that one."
  ;; NOTE(philc): Be careful about invoking `print` statements in this function when debugging it. For some
  ;; reason it interfaces with the window switching behavior.
  (let* ((original-window (selected-window))
         (window-showing-buffer (get-buffer-window buffer show-ephemeral-buffer-in-other-frames))
         (ephemeral-window (first (get-ephemeral-windows)))
         (should-create-new-window (and (not window-showing-buffer)
                                        (not ephemeral-window)
                                        (one-window-p)))
         (window (or window-showing-buffer
                     ephemeral-window
                     (if should-create-new-window
                         (split-window-sensibly-reverse)
                       (save-excursion (switch-to-lower-right) (selected-window))))))
    (display-buffer-record-window (if should-create-new-window 'window 'reuse) window buffer)
    (set-window-buffer window buffer)
    (when should-create-new-window (set-window-prev-buffers window nil))
    (select-window original-window)
    (when (member (buffer-name buffer) special-display-auto-focused-buffers)
      (select-window window))
    window))

(defun dismiss-ephemeral-windows ()
  "Dismisses any visible windows in the current frame identifiedy by `special-display-buffer-names` and
   `special-display-regexps`. I use this to quickly dismiss help windows, compile output, etc."
  (interactive)
  (save-excursion
    (let ((original-window (selected-window)))
      (dolist (w (get-ephemeral-windows))
        (quit-window nil w))
      (select-window original-window))))

(defun narrow-ephemeral-window ()
  "Narrows the ephemeral window (usually a REPL) so that 3 vertical splits can fit on a Mac Thunderbolt
   monitor: 2 splits which fit 110 chars without wrapping, and 1 narrower split with a REPL."
  (interactive)
  (when (first (get-ephemeral-windows))
    (lexical-let* ((shrink-by-amt 12)
                   (total-width (-> (frame-root-window) window-total-width))
                   (vertical-splits (vertical-split-count))
                   (ephemeral-width (- (/ total-width vertical-splits) shrink-by-amt))
                   ;; Split the width from `shrink-by-amt` evently between the non-ephemeral windows.
                   (non-ephemeral-width (-> (/ total-width vertical-splits)
                                            (+ (/ shrink-by-amt (- vertical-splits 1))))))
      (dolist (w (get-visible-windows))
        (set-window-width w (if (ephemeral-window-p w) ephemeral-width non-ephemeral-width))))))

(defun set-window-width (window width)
  "Resizes a window to be the given size.
   - horizontal: if true, resize the window horizontally, otherwise, vertically."
  ;; NOTE(philc): I couldn't find an Emacs function which takes an absolute window size. shrink-window
  ;; only takes deltas from the window's current width.
  (let ((delta (- (window-total-width window)
                  width)))
    (util/preserve-selected-window
     (lambda ()
       (select-window window)
       (shrink-window delta t)))))

(setq buffer-underneath-maximized-ephemeral-window nil)

(defun toggle-maximize-lower-right-window ()
  "Toggles the vertical maximization of an ephemeral window, whereever it's showing. If there's a window above
   or below it, that window will be saved and will be restored if maximization is toggled."
  ;; I usually have a REPL or diff view showing in the lower right. Often I want to "maximize it" vertically,
  ;; to view a long stacktrace etc., without having to switch to the upper right and close that window.
  (interactive)
  (window-in-direction 'above (get-buffer-window "*Messages*"))
  (let ((ephemeral-window (first (get-ephemeral-windows))))
    (when ephemeral-window
      (lexical-let ((covered-window (or (window-in-direction 'above ephemeral-window)
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
               (set-window-buffer (selected-window) buffer-underneath-maximized-ephemeral-window)))))))))

(defun swap-window-buffers (window-move-fn)
  "Swaps the current buffer with the buffer in the window active after invoking window-move-fn."
  (let ((src-window (get-buffer-window))
        (src-buffer (window-buffer)))
    (funcall window-move-fn)
    (set-window-buffer src-window (current-buffer))
    (set-window-buffer (get-buffer-window) src-buffer)))

(defun swap-window-left () (interactive) (swap-window-buffers 'windmove-left))
(defun swap-window-right () (interactive) (swap-window-buffers 'windmove-right))
(defun swap-window-down () (interactive) (swap-window-buffers 'windmove-down))
(defun swap-window-up () (interactive) (swap-window-buffers 'windmove-up))

(defun toggle-window-maximize ()
  (interactive)
  (if (= 1 (length (window-list)))
      ;; winner-undo undoes the last change you made to the state of your widnows.
      ;; This isn't an exact inverse of "delete-other-windows", but it works OK for me in practice.
      (winner-undo)
      (delete-other-windows)))

(defun split-window-vertically-and-focus ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun split-window-horizontally-and-focus ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun vertical-split-count ()
  "Returns the number of vertical splits (or columns) in the current frame."
  (util/preserve-selected-window
   (lambda ()
     (select-window (frame-first-window))
     (let ((i 1))
       (while (ignore-errors (windmove-right 1))
         (setq i (+ i 1)))
       i))))

(defun switch-to-lower-right ()
  (while (ignore-errors (windmove-right 1)))
  (while (ignore-errors (windmove-down 1))))

(defun create-new-column ()
  "Creates a new column in my window layout by splitting the rightmost window and rebalancing windows."
  (interactive)
  (lexical-let* ((w (selected-window))
                 (b (current-buffer)))
    (while (ignore-errors (windmove-right 1)))
    (if (not (window-splittable-p (selected-window)))
        (select-window w)
      (progn
        (split-window-horizontally-and-focus)
        ;; Ensure that no matter where the window is created, it has the same buffer as the window prior to
        ;; creating the new one. Otherwise, the new window could have some random buffer in it, making it
        ;; difficult to use commands like open-in-project, for instance.
        (set-window-buffer (selected-window) b)
        (balance-windows)))))

(defun create-window-in-next-logical-spot ()
  "Creates a window in the next slot in my standard 2x2 configuration. So for instance, if I have only 1
   window open, it will split the screen into two vertical windows."
  (interactive)
  (let ((window-count (length (window-list)))
        (buffer (current-buffer)))
    (case window-count
      (1 (split-window-horizontally-and-focus))
      (2 (progn
           (switch-to-upper-right)
           (split-window-vertically-and-focus)))
      (3 (progn
           (switch-to-upper-left)
           (if (window-in-direction 'below)
               (progn
                (switch-to-upper-right)
                (split-window-vertically-and-focus))
           (split-window-vertically-and-focus)))))
    ;; Ensure that no matter where the window is created, it has the same buffer as the window prior to
    ;; creating the new one. Otherwise, the new window could have some random buffer in it, making it
    ;; difficult to use commands like open-in-project, for instance.
    (set-window-buffer (selected-window) buffer)))

(defadvice windmove-do-window-select (after windowmove-change-to-normal-mode)
  "Ensure we reset to Evil's normal mode when switching windows."
  (evil-change-to-initial-state))
(ad-activate 'windmove-do-window-select)

(defun split-window-sensibly-reverse (&optional window)
  "Identical to the built-in function split-window-sensibly, but prefers horizontal splits over vertical."
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
       ;; If WINDOW is the only window on its frame and is not the
       ;; minibuffer window, try to split it vertically disregarding
       ;; the value of `split-height-threshold'.
       (let ((split-height-threshold 0))
         (when (window-splittable-p window)
     (with-selected-window window
       (split-window-below))))))))

;; Taken from http://www.emacswiki.org/emacs/misc-cmds.el.
(defun kill-buffer-and-its-windows (buffer)
  "Kill BUFFER and delete its windows. Default is `current-buffer'. BUFFER may be either a buffer or its name"
  (interactive (list (read-buffer "Kill buffer: " (current-buffer) 'existing)))
  (setq buffer (get-buffer buffer))
  (if (buffer-live-p buffer)
      (let ((windows (get-buffer-window-list buffer nil t)))
        (when (kill-buffer buffer)
          (dolist (window windows)
            (when (window-live-p window)
              ;; Ignore error, in particular,;; "Attempt to delete the sole visible or iconified frame".
              (condition-case nil (delete-window window) (error nil))))))
    (when (interactive-p)
      (error "Cannot kill buffer.  Not a live buffer: `%s'" buffer))))
