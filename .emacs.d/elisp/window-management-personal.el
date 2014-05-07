;;
;; Functions & configuration for window manipulation, switching, & management.
;;
(provide 'window-management-personal)

;; Settings for window splits.
(setq split-height-threshold 40)
(setq split-width-threshold 200)
(setq split-window-preferred-function 'split-window-sensibly-reverse)
;; Don't auto-focus the help window. I have my own shortcuts for switching to it and closing it.
(setq help-window-select "never")
;; Ensure these open in the selected window, not a new popup.
(setq same-window-buffer-names '("*magit-log*"))

;; I manage my windows in a 4x4 grid. I want ephemeral or status-based buffers to always show in the
;; lower-right or right window, in that order of preference.
(setq special-display-buffer-names '("*Help*" "*compilation*" "COMMIT_EDITMSG" "*Messages*"
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

;; References, for context:
;; http://snarfed.org/emacs_special-display-function_prefer-other-visible-frame
;; http://stackoverflow.com/questions/1002091/how-to-force-emacs-not-to-display-buffer-in-a-specific-window
;; The implementation of this function is based on `special-display-popup-frame` in window.el.
(defun show-ephemeral-buffer-in-a-sensible-window (buffer &optional buffer-data)
  "Given a buffer, shows the window in a right-side split."
  (let* ((original-window (selected-window))
         (create-new-window (one-window-p))
         (window (if create-new-window
                     (split-window-sensibly-reverse)
                   (save-excursion (switch-to-lower-right) (selected-window)))))
    (display-buffer-record-window (if create-new-window 'window 'reuse) window buffer)
    (set-window-buffer window buffer)
    (when create-new-window (set-window-prev-buffers window nil))
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
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (when (special-display-p (buffer-name buffer))
            (quit-window nil window))))
      (select-window original-window))))

(setq buffer-underneath-maximized-ephemeral-window nil)

(defun toggle-maximize-lower-right-window ()
  "Toggles the maximization of an ephemeral window showing in the lower right quadrant."
  ;; I usually have a REPL or diff view showing in the lower right. Often I want to "maximize it" vertically,
  ;; to view a long stacktrace etc., without having to switch to the upper right and close that window.
  (interactive)
  (preserve-selected-window
   (lambda ()
     (switch-to-lower-right)
     (lexical-let ((w (window-in-direction 'above)))
       (if w
           (progn
             (setq buffer-underneath-maximized-ephemeral-window (window-buffer w))
             (delete-window w))
         (progn
           (when buffer-underneath-maximized-ephemeral-window
             (split-window-vertically)
             (set-window-buffer (selected-window) buffer-underneath-maximized-ephemeral-window))))))))

(defun swap-window-buffers (window-move-fn)
  "Swaps the current buffer with the buffer in the window active after invoking window-move-fn."
  (let ((src-window (get-buffer-window))
        (src-buffer (window-buffer)))
    (funcall window-move-fn)
    (set-window-buffer src-window (current-buffer))
    (set-window-buffer (get-buffer-window) src-buffer)))

(defun swap-window-with-upper-left () (interactive) (swap-window-buffers 'switch-to-upper-left))
(defun swap-window-with-lower-left () (interactive) (swap-window-buffers 'switch-to-lower-left))
(defun swap-window-with-upper-right () (interactive) (swap-window-buffers 'switch-to-upper-right))
(defun swap-window-with-lower-right () (interactive) (swap-window-buffers 'switch-to-lower-right))

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

(defun switch-to-upper-left () (interactive) (select-window (frame-first-window)))
(defun switch-to-lower-left () (interactive) (switch-to-upper-left) (ignore-errors (windmove-down)))
(defun switch-to-upper-right () (interactive) (switch-to-upper-left) (ignore-errors (windmove-right 1)))
(defun switch-to-lower-right () (interactive) (switch-to-upper-right) (ignore-errors
                                                                        (windmove-down)
                                                                        (windmove-right)))

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
