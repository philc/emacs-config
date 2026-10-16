;;; -*- lexical-binding: t; -*-
;;
;; Markdown lite mode
;; TODO(philc): Document what this mode does.
;;
;; Performance notes:
;;
;; indent-rigidly fires after-change-functions once per line. The yascroll package hooks into
;; after-change-functions and synchronously redraws the scroll bar on every line.
;; yascroll:window-height is advised (in init.el) to avoid one expensive part of that redraw
;; (line-pixel-height forcing jit-lock fontification), but yascroll:show-scroll-bar-internal's
;; positioning of the thumb overlay adds up across thousands of lines. To avoid this, use
;; inhibit-modification-hooks around bulk indent-rigidly calls, followed by font-lock-flush to queue
;; one lazy refontification pass.
;;

(provide 'markdown-lite-mode)
(require 'evil)
(require 's)
(require 'markdown-tables)
(require 'emacs-utils)
(require 'outline)
(require 'cl-lib)
(require 'consult) ; For consult-outline.

;; Spell checking. Don't spell check preformatted/code blocks (mlm-pre), or the URL
;; portion of inline image links, e.g. the "images/keylayout-column-staggered.svg" in
;; ![Column-staggered](images/keylayout-column-staggered.svg). That text is fontified with
;; mlm-url by mlm/markdown-mode-font-lock-keywords-basic, below.
(defvar jinx-exclude-faces)
(with-eval-after-load 'jinx
  (add-to-list 'jinx-exclude-faces '(markdown-lite-mode mlm-pre mlm-url)))

(defconst mlm/outline-regexp "[ ]*\\*\\|#+ "
  "The `outline-regexp' for markdown-lite-mode. It matches a leading bullet point, or an ATX
   heading.")

(defconst mlm/outline-search-regexp (concat "^\\(?:" mlm/outline-regexp "\\)")
  "`mlm/outline-regexp' at the start of a line.")

(define-derived-mode markdown-lite-mode text-mode "Markdown-lite"
  "Major mode for editing Markdown files."
  (setq tab-width 4)
  (setq-local font-lock-defaults '(mlm/markdown-mode-font-lock-keywords-basic))
  (setq-local font-lock-multiline t)

  ;; Paragraph filling
  (set (make-local-variable 'paragraph-start)
       "\f\\|[ \t]*$\\|[ \t]*[*+-] \\|[ \t]*[0-9]+\\.[ \t]\\|[ \t]*: ")
  (set (make-local-variable 'paragraph-separate)
       "\\(?:[ \t\f]*\\|.*  \\)$")
  (set (make-local-variable 'adaptive-fill-first-line-regexp)
       "\\`[ \t]*>[ \t]*?\\'")
  ;; Without this, Emacs's default adaptive-fill-regexp (which treats "*" as a
  ;; bullet/comment-continuation character) can misparse a "**bold**" span that
  ;; starts a wrapped line as a list marker, corrupting the asterisks on refill.
  (set (make-local-variable 'adaptive-fill-regexp) "\\s-*")

  (set (make-local-variable 'adaptive-fill-function)
       'mlm/markdown-adaptive-fill-function)

  ;; Outline mode
  ;; markdown-mode has support for outline mode, but in that implementations, only headings are
  ;; folded. My preference is to also fold subtrees in bulleted lists, akin to Org mode and
  ;; Workflowy.
  (setq-local outline-regexp mlm/outline-regexp)

  (make-local-variable 'outline-level)
  (setq outline-level 'mlm/outline-level)
  (setq-local outline-search-function 'mlm/outline-search)

  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))

  ;; Indentation and filling
  (make-local-variable 'fill-nobreak-predicate)
  (add-hook 'fill-nobreak-predicate 'mlm/markdown-nobreak-p)

  ;; Compute and store list levels, used by mlm/markdown-calculate-list-levels.
  (setq-local syntax-propertize-function 'mlm/syntax-propertize)

  )

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-lite-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-lite-mode))

(defun mlm/get-next-list-marker (list-marker)
  "When appending a new item to an existing list, this returns the character to be used for that
   list item. If we're appending to a numbered list, increment the list-marker by one."
  (cond
   ;; Ordered list
   ((string-match "[0-9]" list-marker)
    (concat (int-to-string (+ 1 (string-to-number list-marker))) "."))
   ;; Unordered list
   ((string-match "[*+-]" list-marker)
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
  "Pipes the buffer's contents into a script which renders the markdown as HTML."
  (interactive)
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
         (command (format "~/scripts/publishing/markdown_page.js %s --css %s --scroll-to-line %s"
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
  "Returns (start end) for the markdown list item under the cursor, excluding subtrees."
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
        (let ((inhibit-redisplay t))
          (outline-show-subtree)
          (mlm/markdown-perform-promote-subtree should-promote)
          (outline-hide-subtree))
      (let ((inhibit-modification-hooks t))
        (indent-rigidly (cl-first region) (cl-second region) indent-amount))
      ;; inhibit-modification-hooks also bypasses the invalidation of syntax properties, including
      ;; the list levels set by mlm/syntax-propertize.
      (syntax-ppss-flush-cache (cl-first region))
      ;; Note: Don't cache (point-max) before indent-rigidly. When promoting, indent-rigidly shrinks
      ;; the buffer, and so a cached point-max could we be stale.
      (font-lock-flush (cl-first region) (min (cl-second region) (point-max))))))

(defun mlm/markdown-perform-promote-subtree (should-promote)
  "Promotes the list item under the cursor, and also promotes all subtrees."
  ;; This show-subtree call is important because this indentation code does not work with collapsed
  ;; subtrees, which are represented as overlays. The hidden overlays get lost upon indention.
  (outline-show-subtree)
  (let* ((line (util/get-current-line))
         (start-level (util/line-indentation-level line))
         (indent-amount (if should-promote -2 2))
         (region-start (line-beginning-position))
         (region-end (line-end-position)))
    (save-excursion
      (forward-line 1)
      (while (and (not (eobp))
                  (setq line (util/get-current-line))
                  (or (string/blank? line)
                      (> (util/line-indentation-level line) start-level)))
        (unless (string/blank? line)
          (setq region-end (line-end-position)))
        (forward-line 1)))
    ;; Use inhibit-modification-hooks to prevent after-change hooks (notably yascroll) from firing
    ;; once per line during indent-rigidly. Without this, yascroll redraws the scroll bar
    ;; synchronously on every line, and that redraw's move-to-window-line call (positioning the
    ;; thumb overlay) is expensive enough on its own to add up across a large subtree.
    (let ((inhibit-modification-hooks t))
      (indent-rigidly region-start (min (1+ region-end) (point-max)) indent-amount))
    ;; inhibit-modification-hooks also bypasses the invalidation of syntax properties, including
    ;; the list levels set by mlm/syntax-propertize.
    (syntax-ppss-flush-cache region-start)
    ;; inhibit-modification-hooks prevents jit-lock from being notified of the change, so
    ;; manually queue a lazy refontification of the modified region.
    ;; Note: (point-max) is intentionally not cached before indent-rigidly. When promoting,
    ;; indent-rigidly shrinks the buffer, so a pre-modification point-max would be stale
    ;; and cause an args-out-of-range error here.
    (font-lock-flush region-start (min (1+ region-end) (point-max)))))

(defun mlm/markdown-promote () (interactive) (mlm/markdown-perform-promote t))
(defun mlm/markdown-demote () (interactive) (mlm/markdown-perform-promote nil))
(defun mlm/markdown-promote-subtree () (interactive) (mlm/markdown-perform-promote-subtree t))
(defun mlm/markdown-demote-subtree () (interactive) (mlm/markdown-perform-promote-subtree nil))

(defun mlm/markdown-up-heading ()
  (interactive)
  (outline-up-heading 1)
  (back-to-indentation)) ; Move the cursor to the first non-whitespace character.

(defun mlm/bounds-of-space-delimitted-word ()
  "Returns a cons list of coordinates of the boundary of the word under the cursor, where a word is
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
    "1" (lambda () (interactive) (mlm/show-level 1))
    "2" (lambda () (interactive) (mlm/show-level 2))
    "3" (lambda () (interactive) (mlm/show-level 3))
    "4" (lambda () (interactive) (mlm/show-level 4))
    "5" (lambda () (interactive) (mlm/show-level 5))
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
    (kbd "==") (lambda () (interactive) (mlm/insert-markdown-setext-header "=="))
    (kbd "--") (lambda () (interactive) (mlm/insert-markdown-setext-header "--"))
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

(defun mlm/atx-heading-level (hashes)
  "The outline level of an ATX heading beginning with HASHES. \"#\" and \"##\" are both treated as
   level 1, since \"#\" is typically only used for the document's title, and \"##\" for its
   sections."
  (pcase hashes
    ("#" 1)
    ("##" 1)
    (_ (1- (length hashes)))))

(defvar-local mlm/outline-cache nil
  "(TICK HEADINGS . FENCED-BLOCKS). This is a cache of `mlm/outline-structure'. It becomes stale
   when the buffer is modified and (buffer-chars-modified-tick) advances.

   HEADINGS is a vector with a (POS . LEVEL) pair for each ATX heading in the buffer. POS is the
   position of the start of the heading's line, and LEVEL is its outline level, per
   `mlm/atx-heading-level'. E.g. for this buffer:

     # Title
     ## A
     * a1
     ### A.1

   HEADINGS is [(1 . 1) (9 . 1) (19 . 2)].

   FENCED-BLOCKS is a vector with a (START . END) pair for each fenced code block, from the start of
   its opening fence's line to the end of its closing fence's line, including the newline.")

(defun mlm/outline-structure ()
  "Returns (HEADINGS . FENCED-BLOCKS); see `mlm/outline-cache'. A \"#\" line in a fenced code block
   isn't a heading."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eql (car mlm/outline-cache) tick)
      (let (headings blocks fence block-start)
        (save-excursion
          (save-match-data
            (goto-char (point-min))
            ;; Visit each line which could be a heading or a fence.
            (while (re-search-forward "^\\(?:#+ \\|[ \t]*\\(?:```\\|~~~\\)\\)" nil t)
              (beginning-of-line)
              (if (and (null fence) (looking-at "\\(#+\\) "))
                  (push (cons (point) (mlm/atx-heading-level (match-string 1))) headings)
                (let ((fence-after (mlm/markdown-fence-after-line fence)))
                  (cond ((and (null fence) fence-after)
                         (setq block-start (point)))
                        ((and fence (null fence-after))
                         (push (cons block-start (line-beginning-position 2)) blocks)))
                  (setq fence fence-after)))
              (forward-line 1))
            (when fence
              (push (cons block-start (point-max)) blocks))))
        (setq mlm/outline-cache (cons tick (cons (vconcat (nreverse headings))
                                                 (vconcat (nreverse blocks))))))))
  (cdr mlm/outline-cache))

(defun mlm/atx-headings ()
  "A vector of (POS . LEVEL) for each of the buffer's ATX headings, in buffer order."
  (car (mlm/outline-structure)))

(defun mlm/in-fenced-code-block-p (pos blocks)
  "Whether POS is in one of BLOCKS, the buffer's fenced code blocks (see `mlm/outline-cache')."
  ;; The number of blocks which start at or before POS.
  (let ((i (mlm/bisect-left blocks (1+ pos) #'car)))
    (and (> i 0) (< pos (cdr (aref blocks (1- i)))))))

(defun mlm/outline-search (&optional bound move backward looking-at)
  "The `outline-search-function' for markdown-lite-mode. It searches for `mlm/outline-regexp' like
   outline-mode does by default, but skips the lines in fenced code blocks."
  ;; outline-mode calls this for every heading, e.g. when folding the whole buffer with
  ;; `mlm/show-level', and so does the `gh' menu. Since this file isn't byte-compiled, every form
  ;; here costs noticeably, and so most buffers, which have no fenced code blocks, take a fast path.
  ;; Neither path needs `save-match-data': `mlm/outline-structure' wraps its own searches in it.
  (let ((blocks (cdr (mlm/outline-structure)))
        (noerror (if move 'move t)))
    (cond
     ((= (length blocks) 0)
      (cond (looking-at (looking-at mlm/outline-regexp))
            (backward (re-search-backward mlm/outline-search-regexp bound noerror))
            (t (re-search-forward mlm/outline-search-regexp bound noerror))))
     (looking-at
      (and (looking-at mlm/outline-regexp)
           (not (mlm/in-fenced-code-block-p (point) blocks))))
     (t
      (let (found)
        (while (and (not found)
                    (if backward
                        (re-search-backward mlm/outline-search-regexp bound noerror)
                      (re-search-forward mlm/outline-search-regexp bound noerror)))
          (setq found (not (mlm/in-fenced-code-block-p (match-beginning 0) blocks))))
        found)))))

(defun mlm/bisect-left (vec value &optional key)
  "Returns the number of elements of the sorted vector VEC which are less than VALUE, i.e. the index
   at which VALUE must be inserted to keep VEC sorted. KEY, if given, is applied to each element to
   get the number to compare with VALUE. Modeled after Python's bisect.bisect_left."
  (let ((lo 0)
        ;; Invariant: elements before lo are < VALUE; elements from hi on aren't.
        (hi (length vec)))
    (while (< lo hi)
      (let* ((mid (/ (+ lo hi) 2))
             (elem (aref vec mid)))
        (if (< (if key (funcall key elem) elem) value)
            (setq lo (1+ mid))
          (setq hi mid))))
    lo))

(defun mlm/enclosing-atx-heading-level (pos)
  "The level of the last ATX heading before POS, or 0 if there isn't one."
  ;; A binary search is materially faster than a linear search in a document with many headings.
  (let* ((headings (mlm/atx-headings))
         (i (mlm/bisect-left headings pos #'car)))
    (if (> i 0) (cdr (aref headings (1- i))) 0)))

(defun mlm/outline-level ()
  "The `outline-level' of the heading or list item on the current line. List items are considered
   to be on the level of the ATX heading they appear under, and then further based on their
   indentation.

   Outline modes commands call this function for every heading and list item they visit, so this
   function needs to be fast. Most of a line's level can be read off the line itself: a heading's
   \"#\"s, or a list item's indentation. The one exception is the heading a list item appears under,
   which could be anywhere earlier in the buffer. That's the only part which isn't local to the
   line, and so it's looked up from a per-buffer cache of the headings; see
   `mlm/outline-cache'."
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (if (looking-at "\\(#+\\) ")
          (mlm/atx-heading-level (match-string 1))
        (+ (mlm/enclosing-atx-heading-level (point))
           1
           (/ (current-indentation) 2))))))

(defun mlm/show-level (level)
  "Show all headings and list items at or above LEVEL. Level 1 is the top-level headings, or the
   top-level list items if the doc has no headings. Each list item's indentation adds another level."
  (outline-hide-sublevels level))

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
  "^\\([ \t]*\\)\\([0-9]+\\.\\|[*+-]\\)\\([ \t]+\\)"
  "Regular expression for matching list items.")

(defconst mlm/markdown-regex-list-or-blank-line
  "^[ \t]*\\(?:$\\|\\(?:[0-9]+\\.\\|[*+-]\\)[ \t]\\)"
  "Regular expression matching a list item's marker line, or a blank line. These are the lines which
   end a run of list item continuation lines (see `mlm/markdown-match-list-item-continuation').")

(defconst mlm/markdown-list-item-faces
  `((,mlm/markdown-regex-list-item1 . mlm-list-item-1)
    (,mlm/markdown-regex-list-item2 . mlm-list-item-2)
    (,mlm/markdown-regex-list-item3 . mlm-list-item-3)
    (,mlm/markdown-regex-list-item4 . mlm-list-item-4)
    (,mlm/markdown-regex-list-item5 . mlm-list-item-5)
    (,mlm/markdown-regex-list-item6 . mlm-list-item-6))
  "The regexps which match the marker lines of highlighted list items, and the face of each.")

(defconst mlm/markdown-regex-code
  "\\(\\`\\|[^\\]\\)\\(\\(`+\\)\\(\\(.\\|\n[^\n]\\)*?[^`]\\)\\3\\)\\([^`]\\|\\'\\)"
  "Regular expression for matching inline code fragments.

The first group ensures that the leading backquote character
is not escaped.  The group \\(.\\|\n[^\n]\\) matches any
character, including newlines, but not two newlines in a row.
The final group requires that the character following the code
fragment is not a backquote.")

(defconst mlm/markdown-regex-image-url
  "!\\[[^]\n]*\\](\\([^)\n]*\\))"
  "Regular expression matching the URL portion of an inline image link,
e.g. the \"images/foo.svg\" in ![alt](images/foo.svg).")

(defconst mlm/markdown-regex-block-separator
  "\\(\\`\\|\\(\n[ \t]*\n\\)[^\n \t]\\)"
  "Regular expression for matching block boundaries.")

(defconst mlm/markdown-regex-hr
  (rx line-start
      (group (or (and (repeat 3 (and "*" (? " "))) (* (any "* ")))
                 (and (repeat 3 (and "-" (? " "))) (* (any "- ")))
                 (and (repeat 3 (and "_" (? " "))) (* (any "_ ")))))
      line-end)
  "Regular expression for matching Markdown horizontal rules.")

(defconst mlm/markdown-regex-fence
  "^[ \t]*\\(?:\\(`\\{3,\\}\\)[^`\n]*$\\|\\(~\\{3,\\}\\)\\)"
  "Regular expression for matching the line which opens a fenced code block. Group 1 or 2 is the
   fence. A backtick fence can't be followed by another backtick on its line, so that e.g. ```x```
   at the start of a line is inline code.")

(defconst mlm/markdown-regex-header
  "^\\(?:\\(.+\\)\n\\(=+\\)\\|\\(.+\\)\n\\(-+\\)\\|\\(#+\\)[ \t]+\\(.*?\\)\\s-*?\\(#*\\)\\)$"
  "Regexp identifying Markdown headers.")

(defconst mlm/markdown-regex-header-atx
  "^\\(#\\{1,6\\}\\)[ \t]+\\(.+?\\)[ \t]*\\(#*\\)$"
  "Regular expression for atx-style (hash mark) headers. The number of \"#\"s in group 1 is the
   header's level.")

(defconst mlm/markdown-header-faces
  [mlm-header-1 mlm-header-2 mlm-header-3
   mlm-header-4 mlm-header-5 mlm-header-6]
  "The face for each level of header, from level 1 to 6.")

(defconst mlm/markdown-regex-header-1-setext
  "^\\(.*\\)\n\\(=+\\)$"
  "Regular expression for level 1 setext-style (underline) headers.")

(defconst mlm/markdown-regex-header-2-setext
  "^\\(.*\\)\n\\(-+\\)$"
  "Regular expression for level 2 setext-style (underline) headers.")

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
         ;; List item
         ((and (looking-at mlm/markdown-regex-list)
               (setq bounds (mlm/markdown-cur-list-item-bounds)))
          (cond
           ;; Stop and return point at item of equal indentation
           ((= (nth 3 bounds) level)
            (setq prev (point))
            nil)
           ;; Stop and return nil at item with lesser indentation
           ((< (nth 3 bounds) level)
            (setq prev nil)
            nil)
           ;; Continue at item with greater indentation, unless we've
           ;; hit the beginning of the buffer and can't go further
           ((bobp) (setq prev nil))
           (t t)))
         ;; Stop at beginning of buffer
         ((bobp) (setq prev nil))
         ;; Continue if current line is blank
         ((mlm/markdown-cur-line-blank-p) t)
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
  (let (bounds indent next)
    (setq next (point))
    (forward-line)
    (setq indent (current-indentation))
    (while
        (cond
         ;; Stop at end of the buffer.
         ((eobp) nil)
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
    (if (looking-back (concat mlm/markdown-regex-list "\\s-*") (line-beginning-position))
        (goto-char (match-end 3))
      (skip-syntax-backward "-")
      ;; If that backed up into an empty list item on a previous line, the skip also consumed the
      ;; whitespace after its marker, which is where point usually sits. Include it in the item.
      (when (looking-back "^[ \t]*\\([0-9]+\\.\\|[*+-]\\)" (line-beginning-position))
        (skip-chars-forward " \t")))))

(defun mlm/markdown-cur-line-blank-p ()
  "Return t if the current line is blank and nil otherwise."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "^\\s *$" (line-end-position) t)))

(defun mlm/markdown-nobreak-p ()
  "Return nil if it is acceptable to break the current line at the point."
  ;; inside in square brackets (e.g., link anchor text)
  (looking-back "\\[[^]]*" (line-beginning-position)))

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
   ;; The line at point is a list item's marker line: continuation lines should align under the
   ;; item's text, not repeat the marker itself.
   ((looking-at mlm/markdown-regex-list)
    (make-string (length (match-string-no-properties 0)) ?\s))
   ;; A line inside an existing list item that doesn't itself start with a marker (e.g. a wrapped or
   ;; pasted continuation line). Align it under the item's text instead of preserving whatever
   ;; indentation it happened to already have -- unless the line is already indented deeper than
   ;; that (e.g. an indented/code sub-block nested inside the list item), in which case that deeper
   ;; indentation is intentional and should be kept rather than collapsed to the item's text column.
   ((let ((bounds (mlm/markdown-cur-list-item-bounds)))
      (when bounds
        (make-string (max (nth 3 bounds) (current-indentation)) ?\s))))
   ;; No match
   (t nil)))

(defun mlm/markdown-prev-line-blank-p ()
  "Return t if the previous line is blank and nil otherwise.
If we are at the first line, then consider the previous line to be blank."
  (or (= (line-beginning-position) (point-min))
      (save-excursion
        (forward-line -1)
        (mlm/markdown-cur-line-blank-p))))


;;; Font Lock =================================================================

(defgroup mlm-faces nil
  "Faces used in markdown-lite-mode."
  :group 'faces)

(defface mlm-header-rule
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers rules."
  :group 'mlm-faces)

(defface mlm-header-delimiter
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers hash delimiter."
  :group 'mlm-faces)

(defface mlm-header
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers."
  :group 'mlm-faces)

(defface mlm-header-1
  '((t (:inherit mlm-header)))
  "Face for level-1 headers."
  :group 'mlm-faces)

(defface mlm-header-2
  '((t (:inherit mlm-header)))
  "Face for level-2 headers."
  :group 'mlm-faces)

(defface mlm-header-3
  '((t (:inherit mlm-header)))
  "Face for level-3 headers."
  :group 'mlm-faces)

(defface mlm-header-4
  '((t (:inherit mlm-header)))
  "Face for level-4 headers."
  :group 'mlm-faces)

(defface mlm-header-5
  '((t (:inherit mlm-header)))
  "Face for level-5 headers."
  :group 'mlm-faces)

(defface mlm-header-6
  '((t (:inherit mlm-header)))
  "Face for level-6 headers."
  :group 'mlm-faces)

(defface mlm-inline-code
  '((t (:inherit font-lock-constant-face)))
  "Face for inline code."
  :group 'mlm-faces)

(defface mlm-pre
  '((t (:inherit font-lock-constant-face)))
  "Face for preformatted text."
  :group 'mlm-faces)

(defface mlm-url
  '((t (:inherit font-lock-string-face)))
  "Face for URLs."
  :group 'mlm-faces)

;; TODO(philc):
(defface mlm-list-item-1
  '((t (:inherit font-lock-builtin-face)))
  "For first-level list items."
  :group 'mlm-faces)

(defface mlm-list-item-2
  '((t (:inherit font-lock-type-face)))
  "For second-level list items."
  :group 'mlm-faces)

(defface mlm-list-item-3
  '((t (:inherit font-lock-builtin-face)))
  "For second-level list items."
  :group 'mlm-faces)

(defface mlm-list-item-4
  '((t (:inherit font-lock-type-face)))
  "For second-level list items."
  :group 'mlm-faces)

(defface mlm-list-item-5
  '((t (:inherit font-lock-builtin-face)))
  "For second-level list items."
  :group 'mlm-faces)

(defface mlm-list-item-6
  '((t (:inherit font-lock-type-face)))
  "For second-level list items."
  :group 'mlm-faces)

(defvar mlm/markdown-mode-font-lock-keywords-basic
  (list
   (cons 'mlm/markdown-match-pre-blocks '((0 'mlm-pre)))
   (cons 'mlm/markdown-match-fenced-code-blocks '((0 'mlm-pre)))
   (cons mlm/markdown-regex-header-1-setext '((1 'mlm-header-1)
                                              (2 'mlm-header-rule)))
   (cons mlm/markdown-regex-header-2-setext '((1 'mlm-header-2)
                                              (2 'mlm-header-rule)))
   (cons mlm/markdown-regex-header-atx
         '((1 'mlm-header-delimiter)
           (2 (aref mlm/markdown-header-faces (1- (length (match-string 1)))))
           (3 'mlm-header-delimiter)))
   (cons mlm/markdown-regex-list-item1 '((0 'mlm-list-item-1)))
   (cons mlm/markdown-regex-list-item2 '((0 'mlm-list-item-2)))
   (cons mlm/markdown-regex-list-item3 '((0 'mlm-list-item-3)))
   (cons mlm/markdown-regex-list-item4 '((0 'mlm-list-item-4)))
   (cons mlm/markdown-regex-list-item5 '((0 'mlm-list-item-5)))
   (cons mlm/markdown-regex-list-item6 '((0 'mlm-list-item-6)))
   (cons 'mlm/markdown-match-list-item-continuation '((0 mlm/markdown-list-item-continuation-face)))
   (cons 'mlm/markdown-match-code '((0 'mlm-inline-code)))
   (cons mlm/markdown-regex-image-url '((1 'mlm-url))))
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

;; A list item's text can continue onto the lines after its marker line, e.g. when it's wrapped.
;; These continuation lines are highlighted with the item's face. A continuation line is in the run
;; of non-blank, non-marker lines after the item's marker line, and is indented at least to the
;; item's text. A paragraph after a blank line within an item isn't highlighted.
;;
;; Editing a marker line can change the highlighting of the lines after it. font-lock doesn't
;; refontify those lines right away, but jit-lock does after `jit-lock-context-time': this mode's
;; font-lock does syntactic fontification, which turns on `jit-lock-contextually'.

(defvar mlm/markdown-list-item-continuation-face nil
  "The face for the line matched by `mlm/markdown-match-list-item-continuation'.")

(defun mlm/markdown-match-list-item-continuation (last)
  "Match the next list item continuation line from point to LAST, and set
   `mlm/markdown-list-item-continuation-face' to the item's face. This matches one line at a time,
   and leaves point at the end of the line, because font-lock refontifies a multiline match as a
   whole when any part of it is edited (see `font-lock-multiline')."
  (let (face)
    (while (and (not face) (re-search-forward "^[ \t]+[^ \t\n]" last t))
      (let ((indent (current-indentation)))
        (save-excursion
          (forward-line 0)
          (when (and (not (looking-at mlm/markdown-regex-list))
                     (not (bobp))
                     ;; Find the line which starts this run of lines.
                     (progn (backward-char)
                            (re-search-backward mlm/markdown-regex-list-or-blank-line nil t))
                     (looking-at mlm/markdown-regex-list)
                     (>= indent (mlm/markdown-cur-non-list-indent)))
            (setq face (cl-loop for (regexp . f) in mlm/markdown-list-item-faces
                                when (looking-at regexp) return f))))))
    (when face
      (set-match-data (list (line-beginning-position) (line-end-position)))
      (end-of-line)
      (setq mlm/markdown-list-item-continuation-face face)
      t)))

;; A fenced code block starts at a line of 3 or more backticks or tildes, and ends at a line of at
;; least as many of the same character, or at the end of the buffer if there's no such line. Whether
;; a line is in a fenced code block depends on all of the lines before it, so `mlm/syntax-propertize'
;; stores the fence each line starts in, in an `mlm-fence' text property.
;;
;; Like `mlm/markdown-match-list-item-continuation', the font-lock matcher matches one line at a
;; time, so that an edit refontifies only its own line, and a block can be highlighted in jit-lock
;; chunks. Editing a fence line changes the highlighting of the lines after it, which jit-lock
;; refontifies after `jit-lock-context-time'.

(defun mlm/markdown-fence-after-line (fence)
  "Given the FENCE at the start of the line at point (the string of backticks or tildes which opened
   the enclosing fenced code block, or nil if there isn't one), returns the fence after that line."
  (cond
   ((null fence)
    (when (looking-at mlm/markdown-regex-fence)
      (or (match-string-no-properties 1) (match-string-no-properties 2))))
   ((looking-at (concat "[ \t]*" (regexp-quote fence) (substring fence 0 1) "*[ \t]*$"))
    nil)
   (t fence)))

(defun mlm/markdown-fence-at-line-start ()
  "The fence (see `mlm/markdown-fence-after-line') at the start of the line at point."
  (if (and (eq syntax-propertize-function 'mlm/syntax-propertize)
           (< (point) (point-max))
           ;; syntax-propertize can't propertize text outside of a narrowed region.
           (not (buffer-narrowed-p)))
      (progn
        (syntax-propertize (1+ (point)))
        (get-text-property (point) 'mlm-fence))
    (let ((end (point))
          fence)
      (save-excursion
        (goto-char (point-min))
        (while (< (point) end)
          (setq fence (mlm/markdown-fence-after-line fence))
          (forward-line 1)))
      fence)))

(defun mlm/markdown-match-fenced-code-blocks (last)
  "Match the next non-blank line of a fenced code block, including its fence lines, from point to
   LAST. This matches one line at a time, and leaves point at the end of the line."
  (unless (bolp)
    (forward-line 1))
  (let ((fence (when (< (point) last) (mlm/markdown-fence-at-line-start)))
        found)
    (while (and (not found) (< (point) last))
      (cond
       ;; Outside of a block, skip to the line which opens the next one. Most text isn't in a block,
       ;; and one regexp search is much faster than checking each line.
       ((null fence)
        (if (re-search-forward mlm/markdown-regex-fence last t)
            (progn (beginning-of-line)
                   (setq found t))
          (goto-char last)))
       ((looking-at "[ \t]*$") (forward-line 1))
       (t (setq found t))))
    (when found
      (set-match-data (list (point) (line-end-position)))
      (end-of-line)
      t)))

(defun mlm/markdown-match-pre-blocks (last)
  "Match Markdown pre blocks from point to LAST."
  (let ((levels (mlm/markdown-calculate-list-levels))
        required-column begin end)
    (while (and (< (point) last) (not end))
      ;; The column a line must be indented to in order to count as pre-formatted text here.
      (setq required-column (mlm/markdown-required-pre-column levels))

      (cond
       ;; If not at the beginning of a line, move forward
       ((not (bolp)) (forward-line))
       ;; Move past blank lines
       ((mlm/markdown-cur-line-blank-p) (forward-line))
       ;; At headers and horizontal rules, reset levels
       ((mlm/markdown-new-baseline-p) (forward-line) (setq levels nil))
       ;; If the current line has sufficient indentation, mark out pre block
       ((>= (current-indentation) required-column)
        (setq begin (point))
        (while (and (or (>= (current-indentation) required-column)
                        (mlm/markdown-cur-line-blank-p))
                    (not (eobp)))
          (forward-line))
        (setq end (point)))
       ;; If current line has a list marker, update levels, move to end of block
       ((looking-at mlm/markdown-regex-list)
        (setq levels (mlm/markdown-update-list-levels
                      (mlm/markdown-cur-non-list-indent) (current-indentation) levels))
        (mlm/markdown-end-of-block-element))
       ;; If this is the end of the indentation level, adjust levels accordingly.
       ;; Only match end of indentation level if levels is not the empty list.
       ((and (car levels) (< (current-indentation) required-column))
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
  "Calculate list levels at point. Return a list of (MARKER-COLUMN . CONTENT-COLUMN) pairs (see
   `mlm/markdown-update-list-levels'), one per open list level: the first pair is the deepest
   nested list item in the branch of the list at the point, the second is its parent list item, and
   so on. The depth of the list item is therefore the length of the returned list. If the point is
   not at or immediately after a list item, return nil."
  (save-excursion
    (if (and (eq syntax-propertize-function 'mlm/syntax-propertize)
             (bolp)
             (< (point) (point-max))
             ;; syntax-propertize can't propertize text outside of a narrowed region.
             (not (buffer-narrowed-p)))
        (let ((pos (point)))
          (if (or (< pos syntax-propertize--done)
                  ;; Before propertizing up to pos, look for a baseline after the propertized
                  ;; region. If there's one, replaying from it is cheaper than propertizing the rest
                  ;; of the region, e.g. when a large buffer is first opened at its end.
                  (not (mlm/markdown-search-backward-baseline
                        (max (point-min) (1- syntax-propertize--done)))))
              (progn
                (syntax-propertize (1+ pos))
                (get-text-property pos 'mlm-list-levels))
            (mlm/markdown-replay-list-levels pos nil)))
      (let ((end (point)))
        (mlm/markdown-search-backward-baseline)
        (mlm/markdown-replay-list-levels end nil)))))

;; Computing the list levels at a position from scratch requires replaying every list item from the
;; preceding baseline, which in a long list (e.g. a notes file which is one big outline) can be the
;; start of the buffer. Font-lock needs the list levels for every region it fontifies (see
;; `mlm/markdown-match-pre-blocks'), which makes fontifying a large list quadratic.
;;
;; Instead, `mlm/syntax-propertize' computes the list levels in a single forward pass, and stores
;; them as a text property on each line. Emacs runs it lazily, from the last propertized position up
;; to wherever is needed, and discards the results from the start of a buffer change onward (see
;; `syntax-propertize'). The list levels at a line depend only on that line and the text before it,
;; so the results before a change remain valid.
;;
;; Changes made with `inhibit-modification-hooks' bound bypass that invalidation, so after such
;; changes, call `syntax-ppss-flush-cache' from the start of the change.

(defun mlm/syntax-propertize (start end)
  "The `syntax-propertize-function' for markdown-lite-mode. For each line from START to END, sets
   the `mlm-list-levels' text property on the line to the value of
   `mlm/markdown-calculate-list-levels' at the start of the line, and the `mlm-fence' text property
   to the fenced code block's fence at the start of the line (see
   `mlm/markdown-fence-after-line')."
  (goto-char start) ; `syntax-propertize-wholelines' ensures this is the start of a line.
  ;; `syntax-propertize' turns this on before calling us. It makes every syntax-aware operation
  ;; (e.g. a regexp search using \s-, as `delete-trailing-whitespace' does on save) propertize up to
  ;; wherever it reaches, which in a large buffer can mean the rest of the buffer. This mode doesn't
  ;; set `syntax-table' properties, so it doesn't need them looked up.
  (setq-local parse-sexp-lookup-properties nil)
  (let (;; When first viewing the end of a large buffer, this propertizes all of it. Most of that
        ;; time would be spent garbage collecting at the default threshold.
        (gc-cons-threshold (max gc-cons-threshold (* 64 1024 1024)))
        levels fence)
    (unless (bobp)
      ;; Continue from the previous line, which has already been propertized.
      (save-excursion
        (forward-line -1)
        (setq levels (mlm/markdown-list-levels-after-line
                      (get-text-property (point) 'mlm-list-levels)))
        (setq fence (mlm/markdown-fence-after-line (get-text-property (point) 'mlm-fence)))))
    (while (< (point) end)
      (let ((line-start (point)))
        (when (mlm/markdown-baseline-line-p)
          (setq levels nil))
        (let ((line-levels levels)
              (line-fence fence))
          (setq levels (mlm/markdown-list-levels-after-line levels))
          (setq fence (mlm/markdown-fence-after-line fence))
          (forward-line 1)
          ;; Set the properties on the whole line, not just its first character. Consecutive lines
          ;; often have the same (eq) properties, and then Emacs merges them into a single text
          ;; property interval. With one interval per line, operations which scan text
          ;; properties, like skipping over folded text during redisplay, get noticeably slower in
          ;; large buffers.
          (add-text-properties line-start (max (point) (1+ line-start))
                               (list 'mlm-list-levels line-levels 'mlm-fence line-fence)))))))

(defun mlm/markdown-baseline-line-p ()
  "Whether the line at point is a baseline: one where list levels reset to nil. This is where
   `mlm/markdown-search-backward-baseline' stops: an unindented line after a blank line, which is a
   header or not a list item."
  (and (looking-at "[^\n \t]")
       (save-excursion
         (and (= 0 (forward-line -1))
              (looking-at "[ \t]*$")
              ;; The blank line can't be the first line; `mlm/markdown-regex-block-separator'
              ;; requires a newline before it.
              (not (bobp))))
       (or (mlm/markdown-new-baseline-p)
           (not (looking-at mlm/markdown-regex-list)))))

(defun mlm/markdown-list-levels-after-line (levels)
  "Given the LEVELS at the start of the line at point, returns the levels after that line."
  (if (not (looking-at mlm/markdown-regex-list))
      levels
    (cond
     ;; Make sure this is not a header or hr
     ((mlm/markdown-new-baseline-p) nil)
     ;; Make sure this is not a line from a pre block
     ((>= (current-indentation) (mlm/markdown-required-pre-column levels)) levels)
     ;; If not, then update levels
     (t (mlm/markdown-update-list-levels
         (mlm/markdown-cur-non-list-indent) (current-indentation) levels)))))

(defun mlm/markdown-replay-list-levels (end levels)
  "Update LEVELS (see `mlm/markdown-calculate-list-levels') for each of the list items from point
   to END, and return the result. Point should be at a baseline, or at a position where LEVELS were
   the list levels."
  ;; Search for all list items between the start and END.
  (while (and (< (point) end)
              (re-search-forward mlm/markdown-regex-list end t))
    (beginning-of-line)
    (setq levels (mlm/markdown-list-levels-after-line levels))
    (end-of-line))
  levels)

(defun mlm/markdown-search-backward-baseline (&optional limit)
  "Search backward to a baseline point with no indentation and not a list item. Returns non-nil,
   with point at the baseline, if one is found (the start of the buffer counts as one). If LIMIT is
   given and the search reaches it first, returns nil, with point at LIMIT."
  (end-of-line)
  (let (found)
    (while (not (or found (bobp)))
      (if (re-search-backward mlm/markdown-regex-block-separator limit t)
          (when (match-end 2)
            (goto-char (match-end 2))
            (cond
             ((mlm/markdown-new-baseline-p) (setq found t))
             ((looking-at mlm/markdown-regex-list))
             (t (setq found t))))
        (goto-char limit)
        (setq found 'limit)))
    (not (eq found 'limit))))

(defun mlm/markdown-update-list-levels (content-column indent levels)
  "Update the stack of open list levels given a new line at column INDENT.

   LEVELS is a list of (MARKER-COLUMN . CONTENT-COLUMN) pairs, one per currently open list level,
   innermost first. CONTENT-COLUMN is the column at which a nested list item or code block must
   start to belong to that level. This is computed per level from that level's own marker and
   trailing whitespace (see `mlm/markdown-cur-non-list-indent'), rather than assumed to be a fixed
   number of columns past the marker: list markers are not all the same width (\"* \" is 2 columns,
   \"10. \" is 4), and nesting conventions vary (this project nests lists 2 columns per level, via
   dprint, rather than the traditional 4). LEVELS is nil at baseline (outside of any list).

   If CONTENT-COLUMN (the argument) is non-nil, the current line is itself a new list item marker,
   whose own content begins at that column; INDENT is its marker's column. Otherwise, the current
   line is plain, non-marker text, used only to detect that indentation has dropped below an open
   level's content column, ending it."
  (cond
   ;; A new list item at baseline (no list open yet): open the first level.
   ((and content-column (null levels))
    (list (cons indent content-column)))
   ;; A new list item whose marker starts at or past the innermost open
   ;; level's content column: it's a child of that level. Open a new, deeper
   ;; level.
   ((and content-column (>= indent (cdar levels)))
    (cons (cons indent content-column) levels))
   ;; A new list item whose marker starts at the same column as the
   ;; innermost open level's own marker: a sibling item at the same level.
   ;; Leave the stack as-is.
   ((and content-column (>= indent (caar levels)))
    levels)
   ;; Indentation has dropped below the innermost open level's own marker
   ;; column (whether or not this line is itself a list item): close levels
   ;; until we reach one this line still belongs to. A level is closed when
   ;; the line isn't indented enough to be nested inside that level's parent,
   ;; i.e. it's to the left of the parent's content column.
   ((and levels (< indent (caar levels)))
    (while (and (> (length levels) 1)
                (< indent (cdr (cadr levels))))
      (setq levels (cdr levels)))
    levels)
   ;; Otherwise, do nothing.
   (t levels)))

(defun mlm/markdown-required-pre-column (levels)
  "Return the column at which an indented code block must start, given the currently open list
   LEVELS (see `mlm/markdown-update-list-levels'). Per CommonMark, a code block nested in a list
   item requires 4 columns past that item's own content column; outside of any list, the ordinary
   top-level rule of 4 columns applies."
  (+ 4 (if levels (cdar levels) 0)))

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

(defun mlm/consult-outline (&optional level)
  "Show a menu of outline headings (from `mlm/outline-level') and jump to the one selected. If
   LEVEL is given, the menu starts out narrowed to headings at or above that level."
  ;; Normally, Vertico will match completions using fuzzy matching. For headings in Markdown, that's
  ;; not appropriate, given the headings can be very long. Use plain substring matching instead.
  (let ((completion-styles '(substring)))
    (consult-outline level)))

(defun mlm/navigate-to-top-level-heading ()
  (interactive)
  (mlm/consult-outline 1))

(defun mlm/navigate-to-any-heading ()
  (interactive)
  (mlm/consult-outline))
