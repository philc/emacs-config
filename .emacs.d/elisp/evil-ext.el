;;
;; Extensions to Evil mode, mostly to improve the usability of certain editing operations.
;;
(provide 'evil-ext)
(require 'evil)

;; Evil uses the current file's mode's definition of a paragraph, which is often surprising. For instance, in
;; Markdown mode, a single item in a bulleted list consistutes a paragraph. Instead, I've defined a paragraph
;; to be hunks of text separated by newlines. That's typically what I would expect of a paragraph.
(evil-define-text-object evil-paragraph-from-newlines (count &optional beg end type)
  "Select a paragraph separated by newlines."
  :type line
  ;; The implementation of evil-select-an-object will invoke 'forward-evil-paragraph-from-newlines, which is
  ;; defined below.
  ;; NOTE(philc): I used to instead change `paragraph-start` and `paragraph-separate`, but that approach is
  ;; more complicated and didn't work in go-mode. See http://stackoverflow.com/q/9923540.
  (let ((paragraph-start "\f\\|[     ]*$")
        (paragraph-separate "[  ]*$"))
    (evil-select-an-object 'evil-paragraph-from-newlines beg end type count)))

(defun forward-evil-paragraph-from-newlines (&optional count)
  "Move forward COUNT paragraphs, where paragraphs are separated by newlines. Moves point COUNT paragraphs
   forward or (- COUNT) paragraphs backward if COUNT is negative."
  ;; Adapted from the implementation of forward-evil-paragraph from evil-common.el
  (evil-motion-loop (dir (or count 1))
    (cond
     ((> dir 0)
      (while (and (not (eobp))
                  (not (string/blank? (util/get-current-line))))
        (forward-line 1))
      (while (and (not (eobp))
                  (string/blank? (util/get-current-line)))
        (forward-line 1)))
     ((not (bobp))
      (while (and (not (bobp))
                  (not (string/blank? (util/get-current-line))))
        (forward-line -1))))))

;; NOTE(philc): With the latest Evil, evil-scroll-down does not show the last few lines of the buffer
;; when you reach the bottom. This is a workaround for this issue.
;; This implementation is based on evil-scroll-down.
(evil-define-command evil-ext/scroll-down-patched (count)
  "Scrolls the window and the cursor COUNT lines downwards. The default is half the screen."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let* ((p (point))
           (c (or count (/ (evil-num-visible-lines) 2)))
           (scrollable (- c (save-excursion (forward-line c)))))
      (save-excursion
        (scroll-up scrollable))
      (forward-line c)
      (let ((win-beg (window-start))
            (win-end (window-end nil 'update)))
        ;; If we're at end of buffer, let the last line be at the bottom:
        (when (= win-end (point-max))
          ;; NOTE(philc): these lines do not work, but they do the same thing as "View-scroll-to-buffer-end",
          ;; which is an Emacs function which does work.
          ;; (scroll-down (- (evil-num-visible-lines)
          ;;                 (count-lines win-beg win-end)))))
          (View-scroll-to-buffer-end)))
      (when (= 0 (count-lines p (point)))
        (signal 'end-of-buffer nil)))))

(defun evil-ext/fill-inside-paragraph ()
  "Fills (reflows/linewraps) the current paragraph. Equivalent to gqip in vim."
  (interactive)
  ;; When we line wrap (fill) the paragraph, the default behavior is to put your cursor at the end of the
  ;; newly-wrapped paragraph. Better would be to keep your cursor where it was when you triggered this
  ;; command, so you don't need ot navigate back there if you want to continue typing.
  ;; This tries to estimate where your cursor should be after lines get wrapped.
  (lexical-let* ((estimated-col (mod (current-column) fill-column))
                 (estimated-line (+ (line-number-at-pos)
                                    (/ (current-column) fill-column))))
    (util/preserve-line-and-column
     (lambda ()
       (let ((region (if (use-region-p)
                         (list (region-beginning) (region-end))
                       (evil-inner-paragraph))))
         (evil-fill (first region) (second region)))))
    (goto-line estimated-line)
    (move-to-column estimated-col)))

(defun evil-ext/indent-inside-paragraph ()
  "Indents the current paragraph. Equivalent to =ip in vim."
  (interactive)
  ;; Even though evil-indent-without-move preserves the cursor position, we must also do it here.
  (util/preserve-line-and-column
   (lambda ()
     (let ((region (if (use-region-p)
                       (list (region-beginning) (region-end))
                     (evil-inner-paragraph))))
       (evil-ext/indent-without-move (first region) (second region))))))

(evil-define-operator evil-ext/indent-without-move (beg end)
  "Indent text."
  :move-point nil
  :type line
  (util/preserve-line-and-column (lambda () (evil-indent beg end))))
