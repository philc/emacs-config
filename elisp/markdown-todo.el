;;; -*- lexical-binding: t; -*-
;;
;; A minor mode for my todo list, a Markdown file of list items under "#" section headings. On top of
;; markdown-lite-mode, which marks items done with "t" in any Markdown file, it adds:
;;
;; - T, which cycles an item through the in progress and waiting status markers ("→" and "‖")
;; - archiving an item to the todo list's archive file, under the same section heading, with ;e,
;;   Gmail's key for archiving
;; - capturing a new item into a section without leaving the current one
;; - quieter colors: top-level items are plain, since in a todo list they're tasks, not sections

(require 'cl-lib)
(require 'face-remap)
(require 'evil)
(require 'emacs-utils)
(require 'markdown-lite-mode)

(provide 'markdown-todo)

(defvar mtodo/file "~/Dropbox/notes/tasks/tasks.md"
  "The todo list, which `markdown-todo-mode' is turned on for.")

(defvar mtodo/capture-sections
  '((?c . "Civ")
    (?b . "Base")
    (?j . "Journal")
    (?n . "Entertainment")
    (?m . "Emacs")
    (?h . "Handbook")
    (?v . "Vimium"))
  "The sections which `mtodo/capture' offers, as (KEY . HEADING).")

(defface mtodo-status-marker
  '((t (:inherit shadow)))
  "For the in progress and waiting status markers, \"→\" and \"‖\"."
  :group 'mlm-faces)

(defconst mtodo/font-lock-keywords
  `((,mlm/todo-prefix-regexp
     (5 (unless (equal (match-string 5) "✓") 'mtodo-status-marker) t t))))

(defvar-local mtodo/face-remaps nil
  "The cookies of the face remappings which `markdown-todo-mode' made in this buffer.")

(define-minor-mode markdown-todo-mode
  "A todo list in Markdown. See markdown-todo.el."
  :keymap (make-sparse-keymap)
  (if markdown-todo-mode
      (progn
        (font-lock-add-keywords nil mtodo/font-lock-keywords 'append)
        (setq mtodo/face-remaps
              (list (face-remap-add-relative 'mlm-list-item-1 'mlm-list-item-2)
                    (face-remap-add-relative 'mlm-list-marker-1 'mlm-list-marker-2))))
    (font-lock-remove-keywords nil mtodo/font-lock-keywords)
    (mapc #'face-remap-remove-relative mtodo/face-remaps)
    (setq mtodo/face-remaps nil))
  (font-lock-flush)
  ;; Evil doesn't activate a minor mode's normal-state keys until some later command refreshes its
  ;; keymaps, so keys typed right after the mode is turned on would otherwise fall through.
  (evil-normalize-keymaps))

(defun mtodo/maybe-enable ()
  "Turns on `markdown-todo-mode' if the current buffer is visiting `mtodo/file'."
  (when (and buffer-file-name
             (string= (file-truename buffer-file-name) (file-truename mtodo/file)))
    (markdown-todo-mode 1)))

(add-hook 'markdown-lite-mode-hook #'mtodo/maybe-enable)

(defun mtodo/cycle-status ()
  "Cycles the status of the list item at point from open, to in progress, to waiting, and back to
   open. A done item becomes in progress."
  (interactive)
  (mlm/todo-set-status (pcase (mlm/todo-status)
                         ('nil "→")
                         ("→" "‖")
                         ("‖" nil)
                         ("✓" "→"))))

(defun mtodo/section-regexp (heading)
  "Matches the line of the \"#\" section heading HEADING."
  (concat "^# +" (regexp-quote heading) "[ \t]*$"))

(defun mtodo/current-section ()
  "The heading of the \"#\" section which point is in, or nil if it isn't in one."
  (save-excursion
    (end-of-line)
    (when (re-search-backward "^# +\\(.*?\\)[ \t]*$" nil t)
      (match-string-no-properties 1))))

(defun mtodo/dedent (text n)
  "TEXT with up to N spaces removed from the start of each of its lines."
  (replace-regexp-in-string (format "^ \\{0,%d\\}" n) "" text))

(defun mtodo/archive-file ()
  "The archive of the todo list in the current buffer: tasks.md's is tasks_archive.md."
  (concat (file-name-sans-extension buffer-file-name) "_archive.md"))

(defun mtodo/append-to-section (heading text)
  "Inserts TEXT, which ends in a newline, at the end of the current buffer's section HEADING, or in
   a new section at the end of the buffer if there's no such section."
  (goto-char (point-min))
  (if (re-search-forward (mtodo/section-regexp heading) nil t)
      (let ((heading-end (point)))
        (goto-char (if (re-search-forward "^# " nil t) (match-beginning 0) (point-max)))
        (skip-chars-backward " \t\n")
        ;; Leave a blank line after the heading.
        (insert (if (= (point) heading-end) "\n\n" "\n") (string-trim-right text "\n")))
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (delete-region (point) (point-max))
    (insert (if (bobp) "" "\n\n") "# " heading "\n\n" text)))

(defun mtodo/archive ()
  "Moves the list item at point, with its children, to the end of the same section in the archive
   file (see `mtodo/archive-file'), and saves the archive. The archived item starts with today's
   date: \"* 2026-11-25 ✓ Trip: Hawaii\"."
  (interactive)
  (let* ((heading (or (mtodo/current-section) (user-error "Not in a # section")))
         (range (mlm/a-list-item))
         (beg (evil-range-beginning range))
         (end (evil-range-end range))
         (indent (save-excursion (goto-char beg) (current-indentation)))
         (item (buffer-substring-no-properties beg end))
         (text (with-temp-buffer
                 (insert (mtodo/dedent item indent))
                 (unless (bolp) (insert "\n"))
                 (goto-char (point-min))
                 (looking-at mlm/markdown-regex-list)
                 (goto-char (match-end 0))
                 (insert (format-time-string "%Y-%m-%d") " ")
                 (buffer-string))))
    (with-current-buffer (find-file-noselect (mtodo/archive-file))
      (save-excursion
        (save-restriction
          (widen)
          (mtodo/append-to-section heading text)))
      (save-buffer))
    (delete-region beg end)
    (message "Archived to %s" heading)))

(defun mtodo/read-capture-section ()
  "Prompts for one of `mtodo/capture-sections' by its key, and returns its heading."
  (let* ((prompt (mapconcat (lambda (s) (format "[%c] %s" (upcase (car s)) (cdr s)))
                            mtodo/capture-sections "  "))
         (key (read-char-choice (concat prompt " ") (mapcar #'car mtodo/capture-sections))))
    (alist-get key mtodo/capture-sections)))

(defun mtodo/capture (&optional heading item)
  "Prompts for a section and a new item, and adds the item as the section's first. HEADING and
   ITEM, if given, are used instead of prompting."
  (interactive)
  (let* ((heading (or heading (mtodo/read-capture-section)))
         (item (or item (read-from-minibuffer (concat heading " todo: "))))
         ;; A marker which advances when text is inserted at it, so that point stays on the same
         ;; text, even if the new item goes right where point is.
         (pos (copy-marker (point) t)))
    (goto-char (point-min))
    (unless (re-search-forward (mtodo/section-regexp heading) nil t)
      (goto-char pos)
      (user-error "No section named %s" heading))
    (forward-line 1)
    (while (and (not (eobp)) (looking-at "[ \t]*$"))
      (forward-line 1))
    ;; A heading on the buffer's last line, without a newline.
    (unless (bolp)
      (insert "\n\n"))
    (insert "* " item "\n")
    ;; In an empty section, keep a blank line before the next one.
    (when (looking-at "# ")
      (insert "\n"))
    (goto-char pos)
    (set-marker pos nil)
    (message "Added")))

(evil-define-key 'normal markdown-todo-mode-map
  "T" 'mtodo/cycle-status)

(define-leader-keys 'markdown-todo-mode-map
  "e" 'mtodo/archive
  "c" 'mtodo/capture)
