;;; -*- lexical-binding: t; -*-
;;
;; Markdown lite mode
;; TODO(philc): Document what this mode does.
;;
;; Performance notes:
;;
;; This mode becomes surprisingly slow due to font locking on some operations, like indenting a
;; block of text in a large file, when some list items are collapsed.
;; There are many functions that can be made more efficient by reducing features that I don't
;; personally use. mlm/markdown-search-backward-baseline for instance can be made more efficient.
;;

(provide 'markdown-lite-mode)
(require 'evil)
(require 's)
(require 'markdown-tables)

(define-derived-mode markdown-lite-mode text-mode "Markdown-lite"
  "Major mode for editing Markdown files."
  (setq tab-width 4)
  ;; Comments
  (set (make-local-variable 'mlm/markdown-mode-font-lock-keywords) nil)
  (set (make-local-variable 'font-lock-defaults) nil)
  (set (make-local-variable 'font-lock-multiline) t)
  ;; TODO(philc): re-fontifys buffer
  ;; (markdown-reload-extensions)

  ;; Extensions
  (make-local-variable 'mlm/markdown-enable-math)
  (add-hook 'hack-local-variables-hook 'mlm/markdown-reload-extensions)

  ;; Paragraph filling
  (set (make-local-variable 'paragraph-start)
       "\f\\|[ \t]*$\\|[ \t]*[*+-] \\|[ \t]*[0-9]+\\.[ \t]\\|[ \t]*: ")
  (set (make-local-variable 'paragraph-separate)
       "\\(?:[ \t\f]*\\|.*  \\)$")
  (set (make-local-variable 'adaptive-fill-first-line-regexp)
       "\\`[ \t]*>[ \t]*?\\'")

  (set (make-local-variable 'adaptive-fill-function)
       'mlm/markdown-adaptive-fill-function)

  ;; Outline mode
  (make-local-variable 'outline-regexp)
  ;; markdown-mode has support for outline mode, but in that implementations, headings are folded.
  ;; My preference is to instead fold subtrees in bulleted lists, akin to Org mode and Workflowly.
  (setq outline-regexp "[ ]*\\*") ; matches a leading bullet point

  (make-local-variable 'outline-level)
  (setq outline-level 'lisp-outline-level)

  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))

  ;; Indentation and filling
  (make-local-variable 'fill-nobreak-predicate)
  (add-hook 'fill-nobreak-predicate 'mlm/markdown-nobreak-p)

  )

(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-lite-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-lite-mode))

(defun mlm/get-next-list-marker (list-marker)
  "When appending a new item to an existing list, this returns the character to be used for that
   list item. If we're appending to a numbered list, increment the list-marker by one."
  (cond
   ;; Ordered list
   ((string-match "[0-9]" list-marker)
    (concat (int-to-string (+ 1 (string-to-number list-marker))) "."))
   ;; Unordered list
   ((string-match "[\\*\\+-]" list-marker)
    (concat (s-trim-right list-marker)))))

(defun mlm/insert-list-item-below ()
  "Inserts a new list item under the current one. If the cursor is not in a list item, an error
   message is shown."
  (interactive)
  ;; When the current list item has a trailing space at the end of the current line, the end of the
  ;; list item is not properly calculated, and so ultimately an exception is thrown. This is
  ;; disruptive. It was more expedient to just trim the current line than to change the way list
  ;; items are detected, which proved involved.
  (delete-trailing-whitespace (line-beginning-position) (line-end-position))
  (let* ((bounds (mlm/markdown-cur-list-item-bounds))
         (indent (nth 2 bounds))
         (marker (nth 4 bounds))
         (new-marker (-?> marker mlm/get-next-list-marker))
         (inside-list-item (not (null new-marker))))
    (if (not inside-list-item)
        (message "Not currently inside a list item.")
      (let* ((space-char 32)
             (new-indent (make-string indent space-char))
             (is-collapsed (invisible-p (cl-second bounds))))
        (goto-char (+ (cl-second bounds) (if is-collapsed 1 0)))
        (newline)
        (insert new-indent new-marker " ")
        (evil-append nil)))))

(defun mlm/markdown-create-list-item ()
  "Takes the current visualize line and makes it into a list item."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert "* ")))

(defun mlm/insert-markdown-setext-header (setext-type)
  "With the cursor focused on the header's text, insert a setext header line below that text.
   setet-string: either '==' or '--'"
  (let* ((text-of-line (lambda ()
                         (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
         (line-length (length (funcall text-of-line)))
         (setext-str (make-string line-length (get-byte 0 setext-type)))
         (next-line (save-excursion (forward-line) (funcall text-of-line))))
    ;; If there's already a setext header, delete it, so we don't add two. It's useful to support
    ;; this because the existing setext header may be the incorrect length (i.e. not equal to
    ;; line-length).
    (when (or (s-starts-with? "--" next-line) (s-starts-with? "==" next-line))
      (save-excursion
        (forward-line 1)
        (beginning-of-line)
        (delete-region (- (point) 1) (line-end-position))))
    (save-excursion
      (end-of-line)
      (insert (concat "\n" setext-str)))))

(defun mlm/preview-markdown (&optional markdown-stylesheet show-in-browser)
  (interactive)
  "Pipes the buffer's contents into a script which renders the markdown as HTML."
  (let* ((beg (if (use-region-p)
                  (region-beginning)
                (point-min)))
         (end (if (use-region-p)
                  (region-end)
                (point-max)))
         (stylesheet (or markdown-stylesheet "github"))
         ;; I used to need to copy from an actual browser window to get a good
         ;; export ready for Google Docs, but I think this is no longer the case.
         ;; (use-clipboard (not (string= stylesheet "google-docs")))
         (use-clipboard (not show-in-browser))
         ;; NOTE(philc): line-number-at-pos is 1-indexed.
         (command (format "~/scripts/publishing/markdown_page.rb %s --css %s --scroll-to-line %s"
                          (if use-clipboard "--clipboard" "")
                          stylesheet
                          (- (line-number-at-pos) 1)))
         (markdown (buffer-substring-no-properties beg end))
         ;; This will throw an error if there's any issue with the mardkown->html conversion.
         (html (util/call-process-and-check "/bin/bash" markdown "-c" command)))
    ;; Show markdown in a browser
    (when (not use-clipboard)
      (util/call-process-and-check "browser" html))))

(defun mlm/markdown-get-list-item-region ()
  "Returns '(start, end) for the markdown list item under the cursor, excluding subtrees."
  (interactive)
  (save-excursion
    (let ((start (line-beginning-position))
          (end (line-end-position))
          (end-of-file nil))
      (forward-line 1)
      ;; Stop the search at left-aligned text (which is an approximation for detecting headings).
      (while (not (or ; (string/blank? (util/get-current-line)) ; TODO(philc): Remove this.
                   end-of-file
                   (string-match "^[ ]*\\*" (util/get-current-line))
                   (string-match "^[^ *]" (util/get-current-line))))
        (setq end (line-end-position))
        (when (> (forward-line 1) 0) (setq end-of-file t)))
      (list start end))))

(defun mlm/markdown-perform-promote (should-promote)
  "Promotes the list item under the cursor, excluding subtrees"
  (let* ((region (mlm/markdown-get-list-item-region))
         (indent-amount (if should-promote -2 2))
         (is-collapsed-subtree (invisible-p (cl-second region))))
    (if is-collapsed-subtree
        (progn
          (outline-show-subtree)
          (mlm/markdown-perform-promote-subtree should-promote)
          (outline-hide-subtree))
      (indent-rigidly (cl-first region) (cl-second region) indent-amount))))

(defun mlm/markdown-perform-promote-subtree (should-promote)
  "Promotes thes list under under the cursor, and also promotes all subtrees."
  ;; This show-subtree call is important because this indentation code does not work with collapsed
  ;; subtrees, which are represented as overlays. The hidden overlays get lost upon indention.
  (outline-show-subtree)
  (let* ((line (util/get-current-line))
         (start-level (util/line-indentation-level line))
         (indent-amount (if should-promote -2 2))
         (indent-fn (lambda ()
                      (indent-rigidly (line-beginning-position) (line-end-position) indent-amount))))
    (save-excursion
      (funcall indent-fn)
      (forward-line 1)
      (while (and (setq line (util/get-current-line))
                  (or (string/blank? line)
                      (> (util/line-indentation-level line) start-level)))
        (when (not (string/blank? line))
          (funcall indent-fn))
        (forward-line 1)))))

(defun mlm/markdown-promote () (interactive) (mlm/markdown-perform-promote t))
(defun mlm/markdown-demote () (interactive) (mlm/markdown-perform-promote nil))
(defun mlm/markdown-promote-subtree () (interactive) (mlm/markdown-perform-promote-subtree t))
(defun mlm/markdown-demote-subtree () (interactive) (mlm/markdown-perform-promote-subtree nil))

(defun mlm/markdown-up-heading ()
  (interactive)
  (outline-up-heading 1)
  (back-to-indentation)) ; Move the cursor to the first non-whitespace character.

(defun mlm/bounds-of-space-delimitted-word ()
  "Returns a cons list of coordinates of the boundary of the word under the cursor, where 'word' is
   defined as any sequence of non-whitespace characters."
  (let* ((start nil)
         (end nil))
    (save-excursion
      (while (and (not (bolp))
                  (not (string= (char-to-string (char-before)) " ")))
        (backward-char))
      (setq start (point)))
    (save-excursion
      (while (and (not (eolp))
                  (not (string= (char-to-string (char-after)) " "))
                  (not (string= (char-to-string (char-after)) "\n")))
        (forward-char))
      (setq end (point)))
    (cons start end)))

(defun mlm/replace-region-with-contents (fn)
  "Replace the currently selected region (or the current word if no region is selected) with the
   given text. `fn` takes the currently selected text and returns the text it should be replaced
   with."
  ;; TODO(philc): I think a better approach is to change mlm/bounds-of-space-delimitted-word to
  ;; ignore punctuation.
  (let* ((is-region (region-active-p))
         (word-boundary (mlm/bounds-of-space-delimitted-word))
         (start (if is-region (region-beginning) (car word-boundary)))
         (end (if is-region (region-end) (cdr word-boundary)))
         (last-char (buffer-substring-no-properties (- end 1) end))
         ;; Typical usage for e.g. markdown's bold markers is to surround the contents of
         ;; the replaced region before any punctuation characters, not after them. "\n" is
         ;; here because it allows you to visually select the whole line and surround it
         ;; with text without having the surrounded text appear on the next line.
         (special-last-char? (-contains? '("\n" ":" "." "," ";") last-char))
         (contents (buffer-substring-no-properties start end))
         (contents (if special-last-char?
                       (->> contents (s-chop-suffix last-char) s-trim-right)
                     contents)))
    (delete-region start end)
    (insert (concat (funcall fn contents)
                    (when special-last-char? last-char)))))

(defun mlm/markdown-create-link ()
  "Converts the currently selected text into a link, using what's the clipboard as the URL."
  (interactive)
  (let ((url (s-trim (util/call-process-and-check "pbpaste" nil))))
    (mlm/replace-region-with-contents (lambda (existing-text)
                                        (concat "[" existing-text "](" url ")")))))

(defun mlm/markdown-bold ()
  "Surrounds the currently selected text or the word under the cursor in bold asterisks."
  (interactive)
  (mlm/replace-region-with-contents (lambda (existing-text)
                                      (concat "**" existing-text "**"))))

(defun mlm/setup-markdown-mode ()
  (interactive)
  (define-leader-keys 'markdown-lite-mode-map
    "0" 'outline-show-all
    "1" '(lambda () (interactive) (mlm/show-level 1))
    "2" '(lambda () (interactive) (mlm/show-level 2))
    "3" '(lambda () (interactive) (mlm/show-level 3))
    "4" '(lambda () (interactive) (mlm/show-level 4))
    "5" '(lambda () (interactive) (mlm/show-level 5))
    "l" 'mlm/markdown-create-link
    "ad" 'mlm/markdown-insert-date
    "re" (lambda ()
           (interactive)
           (mlm/preview-markdown "gmail"));
    "rd" (lambda ()
           (interactive)
           (mlm/preview-markdown "google-docs"));
    "rr" (lambda ()
           (interactive)
            (mlm/preview-markdown nil t)))

  (evil-define-key 'visual markdown-lite-mode-map
    (kbd "M-b") 'mlm/markdown-bold)

  (evil-define-key 'normal markdown-lite-mode-map
    ;; Autocomplete setext headers by typing "==" or "--" on the header's line in normal mode.
    (kbd "==") '(lambda () (interactive) (mlm/insert-markdown-setext-header "=="))
    (kbd "--") '(lambda () (interactive) (mlm/insert-markdown-setext-header "--"))
    (kbd "C-k") 'mlm/backward-same-level
    (kbd "C-j") 'mlm/forward-same-level
    (kbd "A-h") 'outline-previous-visible-heading
    (kbd "A-l") 'outline-next-visible-heading
    (kbd "TAB") 'mlm/markdown-cycle
    (kbd "A-o") 'mlm/markdown-cycle
    (kbd "C-S-L") 'mlm/markdown-demote
    (kbd "C-S-H") 'mlm/markdown-promote
    (kbd "C-S-A-L") 'mlm/markdown-demote-subtree
    (kbd "C-S-A-H") 'mlm/markdown-promote-subtree
    "gh" 'mlm/navigate-to-top-level-heading
    "gH" 'mlm/navigate-to-any-heading
    "gu" 'mlm/markdown-up-heading)

  (evil-define-key 'insert markdown-lite-mode-map
    (kbd "C-S-H") 'mlm/markdown-promote
    (kbd "C-S-L") 'mlm/markdown-demote)
  (mapc (lambda (state)
          (evil-define-key state markdown-lite-mode-map
            (kbd "M-b") 'mlm/markdown-bold
            (kbd "C-S-K") 'mlm/markdown-move-list-item-up
            (kbd "C-S-J") 'mlm/markdown-move-list-item-down
            ;; Note for this C-S-I keybinding to work, you must (define-key input-decode-map [?\C-i]
            ;; [C-i]) https://emacs.stackexchange.com/a/221
            (kbd "C-S-I") 'mlm/markdown-create-list-item
            ;; M-return creates a new list item and enters insert mode.
            (kbd "<C-return>") 'mlm/insert-list-item-below))
        '(normal insert)))

(mlm/setup-markdown-mode)

(defun mlm/show-level (indent-level)
  "Show all list items which are less than or equal to `indent-level`."
  (outline-hide-sublevels (* indent-level 2)))

(defun mlm/markdown-cycle ()
  "Cycle the visibility of the list under the cursor."
  (interactive)
  (save-excursion (outline-cycle)))

(defun mlm/forward-same-level ()
  (interactive)
  (condition-case nil (outline-forward-same-level 1)
    (error (outline-next-visible-heading 1)))
  ;; Move the cursor to the first non-whitespace character.
  (back-to-indentation))

(defun mlm/backward-same-level ()
  (interactive)
  (condition-case nil (outline-backward-same-level 1)
    (error (outline-previous-visible-heading 1)))
  ;; Move the cursor to the first non-whitespace character.
  (back-to-indentation))

;;
;; Much of this code is taken from markdown-mode.el.
;;

(defconst mlm/markdown-regex-list-item1
  "^\\* .*$")

(defconst mlm/markdown-regex-list-item2
  "^\\(  \\|\t\\)\\* .*$")

(defconst mlm/markdown-regex-list-item3
  "^\\(  \\|\t\\)\\{2\\}\\* .*$")

(defconst mlm/markdown-regex-list-item4
  "^\\(  \\|\t\\)\\{3\\}\\* .*$")

(defconst mlm/markdown-regex-list-item5
  "^\\(  \\|\t\\)\\{4\\}\\* .*$")

(defconst mlm/markdown-regex-list-item6
  "^\\(  \\|\t\\)\\{5,\\}\\* .*$")

(defconst mlm/markdown-regex-list
  "^\\([ \t]*\\)\\([0-9]+\\.\\|[\\*\\+-]\\)\\([ \t]+\\)"
  "Regular expression for matching list items.")

(defconst mlm/markdown-regex-code
  "\\(\\`\\|[^\\]\\)\\(\\(`+\\)\\(\\(.\\|\n[^\n]\\)*?[^`]\\)\\3\\)\\([^`]\\|\\'\\)"
  "Regular expression for matching inline code fragments.

The first group ensures that the leading backquote character
is not escaped.  The group \\(.\\|\n[^\n]\\) matches any
character, including newlines, but not two newlines in a row.
The final group requires that the character following the code
fragment is not a backquote.")

(defconst mlm/markdown-regex-pre
  "^\\(    \\|\t\\).*$"
  "Regular expression for matching preformatted text sections.")

(defconst mlm/markdown-regex-line-break
  "[^ \n\t][ \t]*\\(  \\)$"
  "Regular expression for matching line breaks.")

(defconst mlm/markdown-regex-blockquote
  "^[ \t]*\\(>\\).*$"
  "Regular expression for matching blockquote lines.")

(defconst mlm/markdown-regex-block-separator
  "\\(\\`\\|\\(\n[ \t]*\n\\)[^\n \t]\\)"
  "Regular expression for matching block boundaries.")

(defconst mlm/markdown-regex-header
  "^\\(?:\\(.+\\)\n\\(=+\\)\\|\\(.+\\)\n\\(-+\\)\\|\\(#+\\)\\s-*\\(.*?\\)\\s-*?\\(#*\\)\\)$"
  "Regexp identifying Markdown headers.")

(defconst mlm/markdown-regex-header-1-atx
  "^\\(#\\)[ \t]*\\(.+?\\)[ \t]*\\(#*\\)$"
  "Regular expression for level 1 atx-style (hash mark) headers.")

(defconst mlm/markdown-regex-header-2-atx
  "^\\(##\\)[ \t]*\\(.+?\\)[ \t]*\\(#*\\)$"
  "Regular expression for level 2 atx-style (hash mark) headers.")

(defconst mlm/markdown-regex-header-3-atx
  "^\\(###\\)[ \t]*\\(.+?\\)[ \t]*\\(#*\\)$"
  "Regular expression for level 3 atx-style (hash mark) headers.")

(defconst mlm/markdown-regex-header-4-atx
  "^\\(####\\)[ \t]*\\(.+?\\)[ \t]*\\(#*\\)$"
  "Regular expression for level 4 atx-style (hash mark) headers.")

(defconst mlm/markdown-regex-header-5-atx
  "^\\(#####\\)[ \t]*\\(.+?\\)[ \t]*\\(#*\\)$"
  "Regular expression for level 5 atx-style (hash mark) headers.")

(defconst mlm/markdown-regex-header-6-atx
  "^\\(######\\)[ \t]*\\(.+?\\)[ \t]*\\(#*\\)$"
  "Regular expression for level 6 atx-style (hash mark) headers.")

(defconst mlm/markdown-regex-header-1-setext
  "^\\(.*\\)\n\\(=+\\)$"
  "Regular expression for level 1 setext-style (underline) headers.")

(defconst mlm/markdown-regex-header-2-setext
  "^\\(.*\\)\n\\(-+\\)$"
  "Regular expression for level 2 setext-style (underline) headers.")

(defconst mlm/markdown-regex-header-setext
  "^\\(.+\\)\n\\(\\(?:=\\|-\\)+\\)$"
  "Regular expression for generic setext-style (underline) headers.")

(defconst mlm/markdown-regex-header-atx
  "^\\(#+\\)[ \t]*\\(.*?\\)[ \t]*\\(#*\\)$"
  "Regular expression for generic atx-style (hash mark) headers.")

;; (defconst markdown-regex-line-break
;;   "[^ \n\t][ \t]*\\(  \\)$"
;;   "Regular expression for matching line breaks.")

;; TODO(philc): This has been replaced with mlm/insert-list-item-below.
(defun mlm/markdown-insert-list-item (&optional arg)
  "Insert a new list item.
If the point is inside unordered list, insert a bullet mark.  If
the point is inside ordered list, insert the next number followed
by a period.  Use the previous list item to determine the amount
of whitespace to place before and after list markers.

With a \\[universal-argument] prefix (i.e., when ARG is (4)),
decrease the indentation by one level.

With two \\[universal-argument] prefixes (i.e., when ARG is (16)),
increase the indentation by one level."
  (interactive "p")
  (let (bounds item-indent marker indent new-indent new-loc)
    (save-match-data
      ;; Look for a list item on current or previous non-blank line
      (save-excursion
        (while (and (not (setq bounds (mlm/markdown-cur-list-item-bounds)))
                    (not (bobp))
                    (mlm/markdown-cur-line-blank-p))
          (forward-line -1)))
      (when bounds
        (cond ((save-excursion
                 (skip-chars-backward " \t")
                 (looking-at mlm/markdown-regex-list))
               (beginning-of-line)
               (insert "\n")
               (forward-line -1))
              ((not (mlm/markdown-cur-line-blank-p))
               (newline)))
        (setq new-loc (point)))
      ;; Look ahead for a list item on next non-blank line
      (unless bounds
        (save-excursion
          (while (and (null bounds)
                      (not (eobp))
                      (mlm/markdown-cur-line-blank-p))
            (forward-line)
            (setq bounds (mlm/markdown-cur-list-item-bounds))))
        (when bounds
          (setq new-loc (point))
          (unless (mlm/markdown-cur-line-blank-p)
            (newline))))
      (if (not bounds)
          ;; When not in a list, start a new unordered one
          (progn
            (unless (mlm/markdown-cur-line-blank-p)
              (insert "\n"))
            (insert "* "))
        ;; Compute indentation for a new list item
        (setq item-indent (nth 2 bounds))
        (setq marker (nth 4 bounds))
        (setq indent (cond
                      ((= arg 4) (max (- item-indent 4) 0))
                      ((= arg 16) (+ item-indent 4))
                      (t item-indent)))
        (setq new-indent (make-string indent 32))
        (goto-char new-loc)
        (cond
         ;; Ordered list
         ((string-match "[0-9]" marker)
          (if (= arg 16) ;; starting a new column indented one more level
              (insert (concat new-indent "1. "))
            ;; travel up to the last item and pick the correct number.  If
            ;; the argument was nil, "new-indent = item-indent" is the same,
            ;; so we don't need special treatment. Neat.
            (save-excursion
              (while (not (looking-at (concat new-indent "\\([0-9]+\\)\\.")))
                (forward-line -1)))
            (insert (concat new-indent
                            (int-to-string (1+ (string-to-number (match-string 1))))
                            ". "))))
         ;; Unordered list
         ((string-match "[\\*\\+-]" marker)
          (insert new-indent marker)))))))

(defun mlm/markdown-move-list-item-up ()
  "Move the current list item up in the list when possible."
  (interactive)
  (outline-show-subtree) ; When moving list items, expand any collaped subtrees to avoid corruption.
  (let (cur prev old)
    (when (setq cur (mlm/markdown-cur-list-item-bounds))
      (setq old (point))
      (goto-char (nth 0 cur))
      (if (mlm/markdown-prev-list-item (nth 3 cur))
          (progn
            (setq prev (mlm/markdown-cur-list-item-bounds))
            (condition-case nil
                (progn
                  (outline-show-subtree)
                  (transpose-regions (nth 0 prev) (nth 1 prev)
                                     (nth 0 cur) (nth 1 cur) t)
                  (goto-char (+ (nth 0 prev) (- old (nth 0 cur)))))
              ;; Catch error in case regions overlap.
              (error (goto-char old))))
        (goto-char old)))))

(defun mlm/markdown-move-list-item-down ()
  "Move the current list item down in the list when possible."
  (interactive)
  (outline-show-subtree) ; When moving list items, expand any collaped subtrees to avoid corruption.
  (let (cur next old)
    (when (setq cur (mlm/markdown-cur-list-item-bounds))
      (setq old (point))
      (if (mlm/markdown-next-list-item (nth 3 cur))
          (progn
            (outline-show-subtree)
            (setq next (mlm/markdown-cur-list-item-bounds))
            (condition-case nil
                (progn
                  (transpose-regions (nth 0 cur) (nth 1 cur)
                                     (nth 0 next) (nth 1 next) nil)
                  (goto-char (+ old (- (nth 1 next) (nth 1 cur)))))
              ;; Catch error in case regions overlap.
              (error (goto-char old))))
        (goto-char old)))))

(defun mlm/markdown-prev-list-item (level)
  "Search backward from point for a list item with indentation LEVEL.
Set point to the beginning of the item, and return point, or nil
upon failure."
  (let (bounds indent prev)
    (setq prev (point))
    (forward-line -1)
    (setq indent (current-indentation))
    (while
        (cond
         ;; Stop at beginning of buffer
         ((bobp) (setq prev nil))
         ;; Continue if current line is blank
         ((mlm/markdown-cur-line-blank-p) t)
         ;; List item
         ((and (looking-at mlm/markdown-regex-list)
               (setq bounds (mlm/markdown-cur-list-item-bounds)))
          (cond
           ;; Continue at item with greater indentation
           ((> (nth 3 bounds) level) t)
           ;; Stop and return point at item of equal indentation
           ((= (nth 3 bounds) level)
            (setq prev (point))
            nil)
           ;; Stop and return nil at item with lesser indentation
           ((< (nth 3 bounds) level)
            (setq prev nil)
            nil)))
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the next is blank
         ((and (< indent level)
               (mlm/markdown-next-line-blank-p))
          (setq prev nil))
         ;; Stop at a header
         ((looking-at mlm/markdown-regex-header) (setq prev nil))
         ;; Stop at a horizontal rule
         ((looking-at mlm/markdown-regex-hr) (setq prev nil))
         ;; Otherwise, continue.
         (t t))
      (forward-line -1)
      (setq indent (current-indentation)))
    prev))

(defun mlm/markdown-next-line-blank-p ()
  "Return t if the next line is blank and nil otherwise.
   If we are at the last line, then consider the next line to be blank."
  (or (= (line-end-position) (point-max))
      (save-excursion
        (forward-line 1)
        (mlm/markdown-cur-line-blank-p))))

(defun mlm/markdown-next-list-item (level)
  "Search forward from point for the next list item with indentation LEVEL.
Set point to the beginning of the item, and return point, or nil
upon failure."
  (let (bounds indent prev next)
    (setq next (point))
    (forward-line)
    (setq indent (current-indentation))
    (while
        (cond
         ;; Stop at end of the buffer.
         ((eobp) (setq prev nil))
         ;; Continue if the current line is blank
         ((mlm/markdown-cur-line-blank-p) t)
         ;; List item
         ((and (looking-at mlm/markdown-regex-list)
               (setq bounds (mlm/markdown-cur-list-item-bounds)))
          (cond
           ;; Continue at item with greater indentation
           ((> (nth 3 bounds) level) t)
           ;; Stop and return point at item of equal indentation
           ((= (nth 3 bounds) level)
            (setq next (point))
            nil)
           ;; Stop and return nil at item with lesser indentation
           ((< (nth 3 bounds) level)
            (setq next nil)
            nil)))
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the previous line was blank.
         ((and (< indent level)
               (mlm/markdown-prev-line-blank-p))
          (setq next nil))
         ;; Stop at a header
         ((looking-at mlm/markdown-regex-header) (setq next nil))
         ;; Stop at a horizontal rule
         ((looking-at mlm/markdown-regex-hr) (setq next nil))
         ;; Otherwise, continue.
         (t t))
      (forward-line)
      (setq indent (current-indentation)))
    next))

(defun mlm/markdown-cur-list-item-bounds ()
  "Return bounds and indentation of the current list item.
Return a list of the form (begin end indent nonlist-indent marker).
If the point is not inside a list item, return nil.
Leave match data intact for `mlm/markdown-regex-list'."
  (let (cur prev-begin prev-end indent nonlist-indent marker)
    ;; Store current location
    (setq cur (point))
    ;; Verify that cur is between beginning and end of item
    (save-excursion
      (end-of-line)
      (when (re-search-backward mlm/markdown-regex-list nil t)
        (setq prev-begin (match-beginning 0))
        (setq indent (length (match-string 1)))
        (setq nonlist-indent (length (match-string 0)))
        (setq marker (concat (match-string 2) (match-string 3)))
        (save-match-data
          (mlm/markdown-cur-list-item-end nonlist-indent)
          (setq prev-end (point)))
        (when (and (>= cur prev-begin)
                   (<= cur prev-end)
                   nonlist-indent)
          (list prev-begin prev-end indent nonlist-indent marker))))))

(defun mlm/markdown-cur-list-item-end (level)
  "Move to the end of the current list item with nonlist indentation LEVEL.
If the point is not in a list item, do nothing."
  (let (indent)
    (forward-line)
    (setq indent (current-indentation))
    (while
        (cond
         ;; Stop at end of the buffer.
         ((eobp) nil)
         ;; Continue if the current line is blank
         ((mlm/markdown-cur-line-blank-p) t)
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the previous line was blank.
         ((and (< indent level)
               (mlm/markdown-prev-line-blank-p))
          nil)
         ;; Stop at a new list item of the same or lesser indentation
         ((looking-at mlm/markdown-regex-list) nil)
         ;; Stop at a header
         ((looking-at mlm/markdown-regex-header) nil)
         ;; Stop at a horizontal rule
         ;; ((looking-at mlm/markdown-regex-hr) nil)
         ;; Otherwise, continue.
         (t t))
      (forward-line)
      (setq indent (current-indentation)))
    ;; Don't skip over whitespace for empty list items (marker and
    ;; whitespace only), just move to end of whitespace.
    (if (looking-back (concat mlm/markdown-regex-list "\\s-*"))
          (goto-char (match-end 3))
      (skip-syntax-backward "-"))))

(defun mlm/markdown-cur-line-blank-p ()
  "Return t if the current line is blank and nil otherwise."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "^\\s *$" (line-end-position) t)))

(defun mlm/markdown-nobreak-p ()
  "Return nil if it is acceptable to break the current line at the point."
  ;; inside in square brackets (e.g., link anchor text)
  (looking-back "\\[[^]]*"))

(defun mlm/markdown-adaptive-fill-function ()
  "Return prefix for filling paragraph or nil if not determined."
  (cond
   ;; List item inside blockquote
   ((looking-at "^[ \t]*>[ \t]*\\([0-9]+\\.\\|[*+-]\\)[ \t]+")
    (replace-regexp-in-string
     "[0-9\\.*+-]" " " (match-string-no-properties 0)))
   ;; Blockquote
   ((looking-at "^[ \t]*>[ \t]*")
    (match-string-no-properties 0))
   ;; List items
   ((looking-at mlm/markdown-regex-list)
    (match-string-no-properties 0))
   ;; No match
   (t nil)))

(defvar mlm/markdown-mode-font-lock-keywords nil
  "Default highlighting expressions for Markdown mode.
This variable is defined as a buffer-local variable for dynamic
extension support.")

(defvar markdown-lite-mode-map
  "Keymap for Markdown lite major mode."
  (mark-keymap))

(defun mlm/markdown-prev-line-blank-p ()
  "Return t if the previous line is blank and nil otherwise.
If we are at the first line, then consider the previous line to be blank."
  (or (= (line-beginning-position) (point-min))
      (save-excursion
        (forward-line -1)
        (mlm/markdown-cur-line-blank-p))))


;;; Font Lock =================================================================

(require 'font-lock)

(defvar markdown-italic-face 'markdown-italic-face
  "Face name to use for italic text.")

(defvar markdown-bold-face 'markdown-bold-face
  "Face name to use for bold text.")

(defvar markdown-header-delimiter-face 'markdown-header-delimiter-face
  "Face name to use as a base for header delimiters.")

(defvar markdown-header-rule-face 'markdown-header-rule-face
  "Face name to use as a base for header rules.")

(defvar markdown-header-face 'markdown-header-face
  "Face name to use as a base for headers.")

(defvar markdown-header-face-1 'markdown-header-face-1
  "Face name to use for level-1 headers.")

(defvar markdown-header-face-2 'markdown-header-face-2
  "Face name to use for level-2 headers.")

(defvar markdown-header-face-3 'markdown-header-face-3
  "Face name to use for level-3 headers.")

(defvar markdown-header-face-4 'markdown-header-face-4
  "Face name to use for level-4 headers.")

(defvar markdown-header-face-5 'markdown-header-face-5
  "Face name to use for level-5 headers.")

(defvar markdown-header-face-6 'markdown-header-face-6
  "Face name to use for level-6 headers.")

(defvar markdown-inline-code-face 'markdown-inline-code-face
  "Face name to use for inline code.")

(defvar markdown-list-face 'markdown-list-face
  "Face name to use for list markers.")

(defvar markdown-blockquote-face 'markdown-blockquote-face
  "Face name to use for blockquote.")

(defvar markdown-pre-face 'markdown-pre-face
  "Face name to use for preformatted text.")

(defvar markdown-language-keyword-face 'markdown-language-keyword-face
  "Face name to use for programming language identifiers.")

(defvar markdown-link-face 'markdown-link-face
  "Face name to use for links.")

(defvar markdown-missing-link-face 'markdown-missing-link-face
  "Face name to use for links where the linked file does not exist.")

(defvar markdown-reference-face 'markdown-reference-face
  "Face name to use for reference.")

(defvar markdown-footnote-face 'markdown-footnote-face
  "Face name to use for footnote identifiers.")

(defvar markdown-url-face 'markdown-url-face
  "Face name to use for URLs.")

(defvar markdown-link-title-face 'markdown-link-title-face
  "Face name to use for reference link titles.")

(defvar markdown-line-break-face 'markdown-line-break-face
  "Face name to use for hard line breaks.")

(defvar markdown-comment-face 'markdown-comment-face
  "Face name to use for HTML comments.")

(defvar markdown-math-face 'markdown-math-face
  "Face name to use for LaTeX expressions.")

(defvar markdown-metadata-key-face 'markdown-metadata-key-face
  "Face name to use for metadata keys.")

(defvar markdown-metadata-value-face 'markdown-metadata-value-face
  "Face name to use for metadata values.")

(defgroup markdown-faces nil
  "Faces used in Markdown Mode"
  :group 'markdown
  :group 'faces)

(defface markdown-italic-face
  '((t (:inherit font-lock-variable-name-face :slant italic)))
  "Face for italic text."
  :group 'markdown-faces)

(defface markdown-bold-face
  '((t (:inherit font-lock-variable-name-face :weight bold)))
  "Face for bold text."
  :group 'markdown-faces)

(defface markdown-header-rule-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers rules."
  :group 'markdown-faces)

(defface markdown-header-delimiter-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers hash delimiter."
  :group 'markdown-faces)

(defface markdown-header-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers."
  :group 'markdown-faces)

(defface markdown-header-face-1
  '((t (:inherit markdown-header-face)))
  "Face for level-1 headers."
  :group 'markdown-faces)

(defface markdown-header-face-2
  '((t (:inherit markdown-header-face)))
  "Face for level-2 headers."
  :group 'markdown-faces)

(defface markdown-header-face-3
  '((t (:inherit markdown-header-face)))
  "Face for level-3 headers."
  :group 'markdown-faces)

(defface markdown-header-face-4
  '((t (:inherit markdown-header-face)))
  "Face for level-4 headers."
  :group 'markdown-faces)

(defface markdown-header-face-5
  '((t (:inherit markdown-header-face)))
  "Face for level-5 headers."
  :group 'markdown-faces)

(defface markdown-header-face-6
  '((t (:inherit markdown-header-face)))
  "Face for level-6 headers."
  :group 'markdown-faces)

(defface markdown-inline-code-face
  '((t (:inherit font-lock-constant-face)))
  "Face for inline code."
  :group 'markdown-faces)

(defface markdown-list-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for list item markers."
  :group 'markdown-faces)

(defface markdown-blockquote-face
  '((t (:inherit font-lock-doc-face)))
  "Face for blockquote sections."
  :group 'markdown-faces)

(defface markdown-pre-face
  '((t (:inherit font-lock-constant-face)))
  "Face for preformatted text."
  :group 'markdown-faces)

(defface markdown-language-keyword-face
  '((t (:inherit font-lock-type-face)))
  "Face for programming language identifiers."
  :group 'markdown-faces)

(defface markdown-link-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for links."
  :group 'markdown-faces)

(defface markdown-missing-link-face
  '((t (:inherit font-lock-warning-face)))
  "Face for missing links."
  :group 'markdown-faces)

(defface markdown-reference-face
  '((t (:inherit font-lock-type-face)))
  "Face for link references."
  :group 'markdown-faces)

(defface markdown-footnote-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for footnote markers."
  :group 'markdown-faces)

(defface markdown-url-face
  '((t (:inherit font-lock-string-face)))
  "Face for URLs."
  :group 'markdown-faces)

(defface markdown-link-title-face
  '((t (:inherit font-lock-comment-face)))
  "Face for reference link titles."
  :group 'markdown-faces)

(defface markdown-line-break-face
  '((t (:inherit font-lock-constant-face :underline t)))
  "Face for hard line breaks."
  :group 'markdown-faces)

(defface markdown-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face for HTML comments."
  :group 'markdown-faces)

(defface markdown-math-face
  '((t (:inherit font-lock-string-face)))
  "Face for LaTeX expressions."
  :group 'markdown-faces)

(defface markdown-metadata-key-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for metadata keys."
  :group 'markdown-faces)

(defface markdown-metadata-value-face
  '((t (:inherit font-lock-string-face)))
  "Face for metadata values."
  :group 'markdown-faces)

;; TODO(philc):
(defface markdown-list-item1-face
  '((t (:inherit font-lock-builtin-face)))
  "For first-level list items."
  :group 'markdown-faces)

(defface markdown-list-item2-face
  '((t (:inherit font-lock-type-face)))
  "For second-level list items."
  :group 'markdown-faces)

(defface markdown-list-item3-face
  '((t (:inherit font-lock-builtin-face)))
  "For second-level list items."
  :group 'markdown-faces)

(defface markdown-list-item4-face
  '((t (:inherit font-lock-type-face)))
  "For second-level list items."
  :group 'markdown-faces)

(defface markdown-list-item5-face
  '((t (:inherit font-lock-builtin-face)))
  "For second-level list items."
  :group 'markdown-faces)

(defface markdown-list-item6-face
  '((t (:inherit font-lock-type-face)))
  "For second-level list items."
  :group 'markdown-faces)

(defvar mlm/markdown-mode-font-lock-keywords-basic
  (list
   (cons 'mlm/markdown-match-pre-blocks '((0 markdown-pre-face)))
   (cons 'mlm/markdown-match-fenced-code-blocks '((0 markdown-pre-face)))
   ;; (cons markdown-regex-blockquote 'markdown-blockquote-face)
   (cons mlm/markdown-regex-header-1-setext '((1 markdown-header-face-1)
                                          (2 markdown-header-rule-face)))
   (cons mlm/markdown-regex-header-2-setext '((1 markdown-header-face-2)
                                          (2 markdown-header-rule-face)))
   (cons mlm/markdown-regex-header-6-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-6)
                                       (3 markdown-header-delimiter-face)))
   (cons mlm/markdown-regex-header-5-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-5)
                                       (3 markdown-header-delimiter-face)))
   (cons mlm/markdown-regex-header-4-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-4)
                                       (3 markdown-header-delimiter-face)))
   (cons mlm/markdown-regex-header-3-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-3)
                                       (3 markdown-header-delimiter-face)))
   (cons mlm/markdown-regex-header-2-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-2)
                                       (3 markdown-header-delimiter-face)))
   (cons mlm/markdown-regex-header-1-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-1)
                                       (3 markdown-header-delimiter-face)))
   (cons mlm/markdown-regex-list-item1 '((0 'markdown-list-item1-face)))
   (cons mlm/markdown-regex-list-item2 '((0 'markdown-list-item2-face)))
   (cons mlm/markdown-regex-list-item3 '((0 'markdown-list-item3-face)))
   (cons mlm/markdown-regex-list-item4 '((0 'markdown-list-item4-face)))
   (cons mlm/markdown-regex-list-item5 '((0 'markdown-list-item5-face)))
   (cons mlm/markdown-regex-list-item6 '((0 'markdown-list-item6-face)))
   ;; (cons 'mlm/markdown-match-multimarkdown-metadata '((1 markdown-metadata-key-face)
   ;;                                                (2 markdown-metadata-value-face)))
   ;; (cons 'mlm/markdown-match-pandoc-metadata '((1 markdown-comment-face)
   ;;                                         (2 markdown-metadata-value-face)))
   ;; (cons mlm/markdown-regex-hr 'markdown-header-face)
   ;; (cons 'mlm/markdown-match-comments '((0 markdown-comment-face)))
   (cons 'mlm/markdown-match-code '((0 markdown-inline-code-face)))
   ;; (cons mlm/markdown-regex-angle-uri 'markdown-link-face)
   ;; (cons mlm/markdown-regex-uri 'markdown-link-face)
   ;; (cons mlm/markdown-regex-email 'markdown-link-face)
   ;; (cons mlm/markdown-regex-list '(2 markdown-list-face))
   ;; (cons mlm/markdown-regex-footnote 'markdown-footnote-face)
   ;; (cons mlm/markdown-regex-link-inline '((1 markdown-link-face t t)
   ;;                                    (2 markdown-link-face t)
   ;;                                    (4 markdown-url-face t)
   ;;                                    (6 markdown-link-title-face t t)))
   ;; (cons mlm/markdown-regex-link-reference '((1 markdown-link-face t t)
   ;;                                       (2 markdown-link-face t)
   ;;                                       (4 markdown-reference-face t)))
   ;; (cons mlm/markdown-regex-reference-definition '((1 markdown-reference-face t)
   ;;                                             (2 markdown-url-face t)
   ;;                                             (3 markdown-link-title-face t)))
   ;; (cons mlm/markdown-regex-bold '(2 markdown-bold-face))
   ;; (cons mlm/markdown-regex-line-break '(1 markdown-line-break-face prepend))
   )
  "Syntax highlighting for Markdown files.")

(defun mlm/markdown-match-code (last)
  "Match inline code from the point to LAST."
  (unless (bobp)
    (backward-char 1))
  (cond ((re-search-forward mlm/markdown-regex-code last t)
         (set-match-data (list (match-beginning 2) (match-end 2)
                               (match-beginning 4) (match-end 4)))
         (goto-char (match-end 0))
         t)
        (t (forward-char 2) nil)))

(defun mlm/markdown-match-fenced-code-blocks (last)
  "Match fenced code blocks from the point to LAST."
  (cond ((search-forward-regexp "^\\([~]\\{3,\\}\\)" last t)
         (beginning-of-line)
         (let ((beg (point)))
           (forward-line)
           (cond ((search-forward-regexp
                   (concat "^" (match-string 1) "~*") last t)
                  (set-match-data (list beg (point)))
                  t)
                 (t nil))))
        (t nil)))

(defun mlm/markdown-match-pre-blocks (last)
  ;; (interactive)
  "Match Markdown pre blocks from point to LAST."
  (let ((levels (mlm/markdown-calculate-list-levels))
        indent pre-regexp end-regexp begin end stop)
    (while (and (< (point) last) (not end))
      ;; Search for a region with sufficient indentation
      (if (null levels)
          (setq indent 1)
        (setq indent (1+ (length levels))))
      (setq pre-regexp (format "^\\(    \\|\t\\)\\{%d\\}" indent))
      (setq end-regexp (format "^\\(    \\|\t\\)\\{0,%d\\}\\([^ \t]\\)" (1- indent)))

      (cond
       ;; If not at the beginning of a line, move forward
       ((not (bolp)) (forward-line))
       ;; Move past blank lines
       ((mlm/markdown-cur-line-blank-p) (forward-line))
       ;; At headers and horizontal rules, reset levels
       ((mlm/markdown-new-baseline-p) (forward-line) (setq levels nil))
       ;; If the current line has sufficient indentation, mark out pre block
       ((looking-at pre-regexp)
        (setq begin (match-beginning 0))
        (while (and (or (looking-at pre-regexp) (mlm/markdown-cur-line-blank-p))
                    (not (eobp)))
          (forward-line))
        (setq end (point)))
       ;; If current line has a list marker, update levels, move to end of block
       ((looking-at mlm/markdown-regex-list)
        (setq levels (mlm/markdown-update-list-levels
                      (match-string 2) (current-indentation) levels))
        (mlm/markdown-end-of-block-element))
       ;; If this is the end of the indentation level, adjust levels accordingly.
       ;; Only match end of indentation level if levels is not the empty list.
       ((and (car levels) (looking-at end-regexp))
        (setq levels (mlm/markdown-update-list-levels
                      nil (current-indentation) levels))
        (mlm/markdown-end-of-block-element))
       (t (mlm/markdown-end-of-block-element))))

    (if (not (and begin end))
        ;; Return nil if no pre block was found
        nil
      ;; Set match data and return t upon success
      (set-match-data (list begin end))
      t)))

(defun mlm/markdown-end-of-block-element ()
  "Move the point to the start of the next block unit.
Stops at blank lines, list items, headers, and horizontal rules."
  (interactive)
  (forward-line)
  (while (and (or (not (mlm/markdown-prev-line-blank-p))
                  (mlm/markdown-cur-line-blank-p))
              (not (or (looking-at mlm/markdown-regex-list)
                       (looking-at mlm/markdown-regex-header)
                       ;; (looking-at mlm/markdown-regex-hr)
                       ))
              (not (eobp)))
    (forward-line)))

(defun mlm/markdown-calculate-list-levels ()
  "Calculate list levels at point.
Return a list of the form (n1 n2 n3 ...) where n1 is the
indentation of the deepest nested list item in the branch of
the list at the point, n2 is the indentation of the parent
list item, and so on.  The depth of the list item is therefore
the length of the returned list.  If the point is not at or
immediately  after a list item, return nil."
  (save-excursion
    (let ((first (point)) levels indent pre-regexp)
      ;; Find a baseline point with zero list indentation
      (mlm/markdown-search-backward-baseline)
      ;; Search for all list items between baseline and LOC
      (while (and (< (point) first)
                  (re-search-forward mlm/markdown-regex-list first t))
        (setq pre-regexp (format "^\\(    \\|\t\\)\\{%d\\}" (1+ (length levels))))
        (beginning-of-line)
        (cond
         ;; Make sure this is not a header or hr
         ((mlm/markdown-new-baseline-p) (setq levels nil))
         ;; Make sure this is not a line from a pre block
         ((looking-at pre-regexp))
         ;; If not, then update levels
         (t
          (setq indent (current-indentation))
          (setq levels (mlm/markdown-update-list-levels (match-string 2)
                                                        indent levels))))
        (end-of-line))
      levels)))

(defun mlm/markdown-search-backward-baseline ()
  "Search backward baseline point with no indentation and not a list item."
  (end-of-line)
  (let (stop)
    (while (not (or stop (bobp)))
      (re-search-backward mlm/markdown-regex-block-separator nil t)
      (when (match-end 2)
        (goto-char (match-end 2))
        (cond
         ((mlm/markdown-new-baseline-p)
          (setq stop t))
         ((looking-at mlm/markdown-regex-list)
          (setq stop nil))
         (t (setq stop t)))))))

(defun mlm/markdown-update-list-levels (marker indent levels)
  "Update list levels given list MARKER, block INDENT, and current LEVELS.
Here, MARKER is a string representing the type of list, INDENT is an integer
giving the indentation, in spaces, of the current block, and LEVELS is a
list of the indentation levels of parent list items.  When LEVELS is nil,
it means we are at baseline (not inside of a nested list)."
  (cond
   ;; New list item at baseline.
   ((and marker (null levels))
    (setq levels (list indent)))
   ;; List item with greater indentation (four or more spaces).
   ;; Increase list level.
   ((and marker (>= indent (+ (car levels) 4)))
    (setq levels (cons indent levels)))
   ;; List item with greater or equal indentation (less than four spaces).
   ;; Do not increase list level.
   ((and marker (>= indent (car levels)))
    levels)
   ;; Lesser indentation level.
   ;; Pop appropriate number of elements off LEVELS list (e.g., lesser
   ;; indentation could move back more than one list level).  Note
   ;; that this block need not be the beginning of list item.
   ((< indent (car levels))
    (while (and (> (length levels) 1)
                (< indent (+ (cadr levels) 4)))
      (setq levels (cdr levels)))
    levels)
   ;; Otherwise, do nothing.
   (t levels)))

(defun mlm/markdown-new-baseline-p ()
  "Determine if the current line begins a new baseline level."
  (save-excursion
    (beginning-of-line)
    (save-match-data
      (or (looking-at mlm/markdown-regex-header)
          ;; (looking-at mlm/markdown-regex-hr)
          (and (null (mlm/markdown-cur-non-list-indent))
               (= (current-indentation) 0)
               (mlm/markdown-prev-line-blank-p))))))

(defun mlm/markdown-cur-non-list-indent ()
  "Return beginning position of list item text (not including the list marker).
Return nil if the current line is not the beginning of a list item."
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward mlm/markdown-regex-list (line-end-position) t)
        (current-column)))))

(defun mlm/markdown-reload-extensions ()
  "Check settings, update font-lock keywords, and re-fontify buffer."
  (interactive)
  (when (eq major-mode 'markdown-lite-mode)
    ;; (setq mlm/markdown-mode-font-lock-keywords
    ;;       (append
    ;;        (when mlm/markdown-enable-math
    ;;          markdown-mode-font-lock-keywords-math)
    ;;        markdown-mode-font-lock-keywords-basic
    ;;        markdown-mode-font-lock-keywords-core))
    (setq font-lock-defaults '(mlm/markdown-mode-font-lock-keywords-basic))
    (font-lock-refresh-defaults)))

(defvar mlm/heading-regexp "^\s*\\* ")
(defvar mlm/top-heading-regexp "^\\* ")

(defun mlm/get-headings (regexp)
  (let ((headings '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let ((heading-text (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))))
          (push heading-text headings))))
    (nreverse headings)))

(defun mlm/goto-heading (heading-text)
  "Navigates to the given heading, if it exists. `heading` should include the list item character."
  (let ((heading-line-number nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (null heading-line-number)
                  (re-search-forward mlm/heading-regexp nil t))
        (let ((text (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (when (string= heading-text text)
            (setq heading-line-number (line-number-at-pos))))))
    (when heading-line-number
      (util/goto-line heading-line-number))))

(defun mlm/navigate-to-heading (top-level)
  "Show a menu of headings and jump to the one selected."
  (interactive)
  (let* ((regexp (if top-level mlm/top-heading-regexp mlm/heading-regexp))
         (headings (mlm/get-headings regexp))
         (vertico-original-styles completion-styles)
         (vertico-original-sort-function vertico-sort-function)
         (selected-string nil))
    ;; Normally, Vertico will match completions using fuzzy matching. For headings
    ;; in Markdown, that's not appropriate, given the headings can be very long.
    ;; Use plain substring matching instead.
    (setq completion-styles (list 'substring))
    ;; Disable Vertico sorting: show the headings in the order they are supplied.
    (setq vertico-sort-function nil)
    (unwind-protect
        (setq selected-string (completing-read "Heading: " headings nil t))
      (setq completion-styles vertico-original-styles)
      (setq vertico-sort-function vertico-original-sort-function))
    (let ((selected-heading (-first (lambda (s) (s-ends-with? selected-string s))
                                    headings)))
      (mlm/goto-heading selected-heading)
      (recenter))))

(defun mlm/navigate-to-top-level-heading ()
  (interactive)
  (mlm/navigate-to-heading t))

(defun mlm/navigate-to-any-heading ()
  (interactive)
  (mlm/navigate-to-heading nil))

(defun mlm/testing ()
  (interactive)
  (mlm/navigate-to-heading "Cursor position"))
