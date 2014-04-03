;;
;; Markdown
;;

(provide 'markdown-mode-personal)

(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

(defun markdown-insert-list-item-below ()
  "Inserts a new list item under the current one. markdown-insert-list-item inserts above, by default."
  (interactive)
  (end-of-line)
  (call-interactively 'markdown-insert-list-item)
  (evil-append nil))

(defun insert-markdown-header (header-line-text)
  "With the cursor focused on the header's text, insert a setext header line below that text.
   header-line-text: either '===' or '---'"
  (end-of-line)
  (insert (concat "\n" header-line-text))
  (markdown-complete)
  ;; markdown-complete inserts a newline after the header. Remove it and move the cursor to a logical place.
  (next-line)
  (next-line)
  (delete-backward-char 1)
  (next-line)
  (beginning-of-line))

(defun preview-markdown ()
  "Pipes the buffer's contents into a script which renders the markdown as HTML and opens in a browser."
  (interactive)
  (call-process-region (point-min) (point-max) "/bin/bash" nil nil nil "-c" "markdown_page.rb | bcat"))

(defun markdown-get-list-item-region ()
  "Returns '(start, end) for the markdown list item under the cursor, excluding subtrees."
  (interactive)
  (save-excursion
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      (next-line)
      ;; Stop the search at left-aligned text (which is an approximation for detecting headings).
      (while (not (or (string/blank? (util/get-current-line))
                      (string-match "^[ ]*\\*" (util/get-current-line))
                      (string-match "^[^ *]" (util/get-current-line))))
        (setq end (line-end-position))
        (next-line))
      (list start end))))

(defun markdown-perform-promote (should-promote)
  "Promotes the list item under the cursor, excluding subtrees"
  (let* ((region (markdown-get-list-item-region))
         (indent-amount (if should-promote -2 2)))
    (indent-rigidly (first region) (second region) indent-amount)))

(defun markdown-perform-promote-subtree (should-promote)
  "Promotes hte list under under the cursor, and also promotes all subtrees."
  ;; This show-subtree call is important because this indentation code does not work with collapsed subtrees.
  ;; They are converted into raw ellipses characters, and so their contents would otherwise b elost.
  (show-subtree)
  (let* ((line (util/get-current-line))
         (start-level (util/line-indentation-level line))
         (indent-amount (if should-promote -2 2))
         (indent-fn (lambda ()
                      (indent-rigidly (line-beginning-position) (line-end-position) indent-amount))))
    (save-excursion
      (funcall indent-fn)
      (next-line)
      (while (and (setq line (util/get-current-line))
                  (or (string/blank? line)
                      (> (util/line-indentation-level line) start-level)))
        (when (not (string/blank? line))
          (funcall indent-fn))
        (next-line)))))

(defun markdown-promote () (interactive) (markdown-perform-promote t))
(defun markdown-demote () (interactive) (markdown-perform-promote nil))
(defun markdown-promote-subtree () (interactive) (markdown-perform-promote-subtree t))
(defun markdown-demote-subtree () (interactive) (markdown-perform-promote-subtree nil))

(defun setup-markdown-buffer ()
  (interactive)
  ;; markdown-mode has support for outline mode, but the implementations is that headings are folded. For my
  ;; purposes, I like instead to fold subtrees of lists.
  (setq-local outline-regexp "[ ]*\\*") ; matches a leading bullet point
  ;; markdown-mode sets this to its own function, but this lisp-outline-level is more correct with our regexp.
  (setq-local outline-level 'lisp-outline-level))

(defun setup-markdown-mode ()
  ;; I use markdown heavily for outlining. outline-magic provides handy functions for cycling the visibility
  ;; of subtrees, the same way org mode doe sit.
  (require 'outline-magic)
  ;; NOTE(philc): For some reason I can't get evil-leaer/set-leader-for-key to work with gfm-mode.
  (evil-define-key 'normal markdown-mode-map
    ";l" 'markdown-cleanup-list-numbers
    ";vv" 'preview-markdown)

  (define-key markdown-mode-map (kbd "<tab>") nil) ; Normally bound to markdown-cycle.

  (evil-define-key 'normal markdown-mode-map
    ;; Autocomplete setext headers by typing "==" or "--" on the header's line in normal mode.
    (kbd "==") '(lambda () (interactive) (insert-markdown-header "=="))
    (kbd "--") '(lambda () (interactive) (insert-markdown-header "--"))
    (kbd "TAB") '(lambda () (interactive) (save-excursion (outline-cycle)))
    (kbd "C-S-L") 'markdown-demote
    (kbd "C-S-H") 'markdown-promote)
  (evil-define-key 'insert markdown-mode-map
    (kbd "C-S-H") 'markdown-promote
    (kbd "C-S-L") 'markdown-demote)
  (mapc (lambda (state)
          (evil-define-key state markdown-mode-map
            (kbd "C-S-K") 'markdown-move-up
            (kbd "C-S-J") 'markdown-move-down
            ;; M-return creates a new todo item and enters insert mode.
            (kbd "<C-return>") 'markdown-insert-list-item-below))
        '(normal insert)))

(add-hook 'markdown-mode-hook 'setup-markdown-buffer)
(eval-after-load 'markdown-mode '(setup-markdown-mode))
