;;; -*- lexical-binding: t; -*-
;;
;;; My additions and customizations to org-mode, based loosely on evil-org-mode as a starting point.
;;; Provides an evil-org-mode minor mode.
;;; https://github.com/edwtjo/evil-org-mode

(require 'evil)
(require 'org)
(require 's)
;; This org mode customization uses functions from my markdown-lite-mode.
(require 'markdown-lite-mode)

(provide 'org-mode-personal)

(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap (make-sparse-keymap) ; defines evil-org-mode-map
  :group 'evil-org)

(add-hook 'org-mode-hook 'evil-org-mode) ;; only load with org-mode

;; Highlight the TODO keywords using various colors.
;; See http://orgmode.org/manual/Faces-for-TODO-keywords.html.
(setq org-todo-keyword-faces
      '(("WAITING" . (:foreground "#999999"))
        ("TODO" . (:foreground "orange")) ; The default red used for TODO is opressive
        ("IP" .(:foreground "#85C7FF"))))

(defun init-org-mode-personal ()
  ;; This enables "clean mode", such that sublists use whitespace for indentation (ala markdown)
  ;; instead of many stars.
  (setq org-startup-indented t))

(with-eval-after-load "org" (init-org-mode-personal))

(defun init-org-mode-buffer ()
  ;; Since I don't use fill mode on Org Mode headers, don't highlight long lines.
  (setq-local whitespace-style '(face trailing)))

(add-hook 'org-mode-hook 'init-org-mode-buffer)

;; normal state shortcuts
(evil-define-key 'normal evil-org-mode-map
  "t" 'org-todo
  ";vt" 'org-show-todo-and-done-tree
  "o" '(lambda () (interactive) (evil-org-eol-call 'always-insert-item))
  ;; "O" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))
  "<" 'org-metaleft
  ">" 'org-metaright
  "gh" 'org-goto-top-level-heading
  "gu" 'outline-up-heading
  ; Normally these go backwards-and-forward by paragraphs but skipping between headings is more
  ; useful.
  "{" 'org-backward-heading-same-level
  "}" 'org-forward-heading-same-level
  (kbd "<C-tab>") 'org-expand-top-level-parent
  ;; For some mode, org-mode maps TAB to org-cycle, and outline-mode maps <tab> to outline-cycle.
  ;; <tab> supercedes and shadows <tab>. One difference between the two functions is that org-cycle
  ;; retains the cursor position when cycling, which is desirable.
  (kbd "<tab>") 'org-cycle
  (kbd "TAB") 'org-cycle)

(defun preview-org ()
  "Pipes the buffer's contents into a script which renders the markdown as HTML and opens in a
   browser."
  (interactive)
  ;; This convert_org_to_markdown.rb is a primitive script I've written which fits my needs.
  (call-process-region (point-min) (point-max) "/bin/bash" nil nil nil "-c"
                       "convert_org_to_markdown.rb | markdown_page.rb | bcat"))


(define-leader-keys 'org-mode-map
  "a" 'org-archive-subtree
  "c" 'org-capture-item-and-prepend-to-subtree
  ;; Leader-space is bound to evil-ext/fill-inside-paragraph-or-comment-block globally, but I
  ;; haven't yet made that command work well in org, and it wreaks havoc in huge org mode files.
  "SPC" (lambda () (interactive))
  "vv" 'preview-org)

;; normal & insert state shortcuts.
(mapc (lambda (state)
        (evil-define-key state evil-org-mode-map
          (kbd "C-S-L") 'org-metaright
          (kbd "C-S-H") 'org-metaleft
          (kbd "C-S-K") 'org-metaup
          (kbd "C-S-J") 'org-metadown
          (kbd "C-S-A-H") 'org-promote-subtree
          (kbd "C-S-A-L") 'org-demote-subtree
          ; M-return creates a new todo item and enters insert mode.
          (kbd "<C-return>") '(lambda () (interactive)
                                (org-insert-heading-after-current)
                                (evil-append nil))))
      '(normal insert))

(defun always-insert-item ()
  (if (not (org-in-item-p))
      (insert "\n")
    (org-insert-item)))

(defun evil-org-eol-call (fun)
  (end-of-line)
  (funcall fun)
  (evil-append nil))

;; Moves the current heading (and all of its children) into the matching parent note in the archive
;; file. I think this is the most sensible way to archive TODOs in org mode files.
;; http://orgmode.org/worg/org-hacks.html
(defun my-org-archive-subtree (orig-fn &rest args)
  (let ((org-archive-location
         (if (save-excursion (org-back-to-heading)
                             (> (org-outline-level) 1))
             (concat (car (split-string org-archive-location "::"))
                     "::* "
                     (car (org-get-outline-path)))
           org-archive-location)))
    (apply orig-fn args)))
(advice-add 'org-archive-subtree :around #'my-org-archive-subtree)

(defun org-show-todo-and-done-tree ()
  "Shows only subtrees which are TODOs or DONE items. Similar to org-show-todo-tree, but it matches
   DONE items as well."
  (interactive)
  (save-excursion
    ;; Note that these tags are case insensitive.
    (org-occur "\\(TODO\\|DONE\\|INPROGRESS\\|WAITING\\)")
    ;; org-occur highlights every TODO and DONE string match in the doc, which is distracting.
    ;; Remove it.
    (org-remove-occur-highlights)))

;; When I've narrowed a subtree (e.g. via org-show-todo-and-done-tree), this allows me to quickly
;; expand the tree around the cursor to show all items again, not just TODO and DONE.
(defun org-expand-top-level-parent ()
  "Shows the children of the top-level parent for the tree under the cursor."
  (interactive)
  (save-excursion
    (outline-up-heading 4)
    (show-children)))

(defun org-text-of-current-line ()
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-beginning-position 2)))

(defun org-get-current-heading ()
  "Assumes the cursor is currently on a heading. TODO: return nil if the cursor isn't on a heading."
  (-> (org-text-of-current-line) s-trim (split-string "* ") cl-second))

;; heading-arg and todo-arg are used for programmatic testing.
(defun org-capture-item-and-prepend-to-subtree (&optional heading-arg todo-arg)
  "Prompts for a TODO and the name of a top-level heading, and adds the TODO as the first child to
   the heading."
  (interactive)
  ;; NOTE(philc): These are personalized to the way I organize my org mode TODO list.
  (message "[L] Liftoff  [B] Base  [S] Study  [N] Entertainment  [M] Emacs [J] Journal")
  (when-let ((heading
              (or heading-arg
                  (pcase (read-char)
                    (?l "Liftoff")
                    (?b "Base")
                    (?s "Study")
                    (?j "Journal")
                    (?n "Entertainment")
                    (?m "Emacs")
                    (?h "Handbook")
                    (?v "Vimium")))))
    (let* ((former-line (util/get-line))
           (former-col (current-column))
           (new-todo (or todo-arg
                         (read-from-minibuffer (concat heading " TODO: ")))))
      (util/preserve-line-and-column
       (lambda ()
         (util/preserve-scroll-position
          (lambda ()
            (mlm/goto-heading (concat "* " heading))
            ;; NOTE(philc): We can't just insert a new line here, because if the heading is
            ;; folded, the insertion behavior becomes incorrect. I don't understand the mechanics
            ;; of folded outline-mode headings, but moving to the first sub-item under the current
            ;; heading resolves the issue.
            (re-search-forward "^\\*\\* ")
            (beginning-of-line)
            (insert (concat "** " new-todo "\n"))))))
      ;; If we inserted text above us in the buffer, the cursor will now be on the wrong line;
      ;; in that case; advance one line.
      (when (not (string= former-line (util/get-line)))
        (next-line)
        (move-to-column former-col))
      (message "Added"))))

(defvar org/top-heading-regexp "^\\* ")

(defun org-goto-top-level-heading (&optional heading-arg)
  (interactive)
  "Prompts for the name of a top-level heading and jumps to there."
  (let* ((heading
          (or heading-arg
              (let* ((headings (mlm/get-headings org/top-heading-regexp))
                     (headings (mapcar (lambda (s) (replace-regexp-in-string org/top-heading-regexp "" s))
                                       headings)))
                (completing-read "Heading: " headings nil t))))
         (heading-with-prefix (concat "* " heading)))
    (mlm/goto-heading heading-with-prefix)
    (recenter-no-redraw)))
