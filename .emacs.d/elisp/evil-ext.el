;;
;; Extensions to Evil mode, mostly to improve the usability of certain editing operations.
;;
(provide 'evil-ext)
(require 'evil)

;; Evil uses the current file's mode's definition of a paragraph, which is often surprising. For
;; instance, in Markdown mode, a single item in a bulleted list consistutes a paragraph. Instead,
;; I've defined a paragraph to be hunks of text separated by newlines. That's typically what I would
;; expect of a paragraph.
(evil-define-text-object evil-paragraph-from-newlines (count &optional beg end type)
  "Select a paragraph separated by newlines."
  :type line
  ;; The implementation of evil-select-an-object will invoke 'forward-evil-paragraph-from-newlines,
  ;; which is defined below.
  ;; NOTE(philc): I used to change `paragraph-start` and `paragraph-separate`, but that approach is
  ;; more complicated and didn't work in go-mode. See http://stackoverflow.com/q/9923540.
  (let ((paragraph-start "\f\\|[     ]*$")
        (paragraph-separate "[  ]*$"))
    (evil-select-an-object 'evil-paragraph-from-newlines beg end type count)))

(defun forward-evil-paragraph-from-newlines (&optional count)
  "Move forward COUNT paragraphs, where paragraphs are separated by newlines. Moves point COUNT
   paragraphs forward or (- COUNT) paragraphs backward if COUNT is negative."
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

(defun evil-ext/preserve-cursor-after-fill (f)
  "Estimates where the cursor position should be after a fill operation, and puts it there."
  ;; When we line wrap (fill) the paragraph, the default behavior is to put your cursor at the end
  ;; of the newly-wrapped paragraph. Better would be to keep your cursor where it was when you
  ;; triggered this command, so you don't need to navigate back there if you want to continue
  ;; typing. This tries to estimate where your cursor should be after lines get wrapped.
  (lexical-let* ((estimated-col (mod (current-column) fill-column))
                 (estimated-line (+ (line-number-at-pos)
                                    (/ (current-column) fill-column))))
    (funcall f)
    (goto-line estimated-line)
    (move-to-column estimated-col)))

;; This regexp matches lines starting with these chars:
;; ["//", ";;", "/*", "* ", "*/"].
(setq evil-ext/comment-regexp "^\\\s*\\(\/\/\\|;;\\|\\/\\*\\|\\*\\/\\|\\* \\).*$")

;; I couldn't get "comment block" working as a first-class evil text object. The code below didn't
;; work as intended when writing this in a similar style to forward-evil-paragraph-from-newlines.
;; Probably because I don't understand how evil-motion-loop is working. However, for my purposes,
;; writing this as a regular function is sufficient, and it's easier to understand because it has no
;; interaction with Evil.
(defun evil-ext/get-comment-block-region ()
  "Returns a list containing the start and end of the comment block surrounding the cursor."
  (let* ((start nil)
         (end nil))
    (util/preserve-line-and-column
     (lambda ()
       ;; Expand the selection forward
       (end-of-line)
       (while (and (not (eobp))
                   (string-match evil-ext/comment-regexp (util/get-line 1)))
         (forward-line 1)
         (end-of-line))
       (setq end (point))
       ;; Expand the selection backward
       (beginning-of-line)
       (while (and (not (bobp))
                   (string-match evil-ext/comment-regexp (util/get-line -1)))
         (forward-line -1))
       (setq start (point))
       (list start end)))))

(defun evil-ext/fill-comment-block ()
  (interactive)
  (print "fill-comment-block")
  (lexical-let ((region (evil-ext/get-comment-block-region)))
    (evil-ext/preserve-cursor-after-fill
     (lambda ()
       (evil-fill (first region) (second region))))))

(defun evil-ext/fill-inside-string ()
  "Fills the current quote surrounded string. Equivalent to gqi\"."
  (interactive)
  (let ((region (evil-a-double-quote)))
    (evil-fill (first region) (second region))))

(defun evil-ext/fill-inside-paragraph ()
  "Fills (reflows/linewraps) the current paragraph. Equivalent to gqip in vim."
  (interactive)
  (lexical-let ((region (if (use-region-p)
                            (list (region-beginning) (region-end))
                          (util/preserve-line-and-column 'evil-inner-paragraph))))
    (evil-ext/preserve-cursor-after-fill
     (lambda ()
       (evil-fill (first region) (second region))))))

(defun evil-ext/fill-inside-paragraph-or-comment-block ()
  "If the cursor is inside a comment block in a programming mode, fill the surrounding comment.
   Otherwise, fill the paragraph. If a region is selected, fill the text inside the region."
  (interactive)
  (if (or (use-region-p)
          (not (derived-mode-p 'prog-mode))
          (not (string-match evil-ext/comment-regexp (util/get-current-line))))
      (call-interactively 'evil-ext/fill-inside-paragraph)
    (call-interactively 'evil-ext/fill-comment-block)))

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
