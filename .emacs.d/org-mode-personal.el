;;; My additions and customizations to org-mode.
;;; Based loosely on evil-org-mode as a starting point.
;;; Provides an evil-org-mode minor mode.
;;; https://github.com/edwtjo/evil-org-mode

(require 'evil)
(require 'org)

(provide 'org-mode-personal)

(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap (make-sparse-keymap) ; defines evil-org-mode-map
  :group 'evil-org)

(add-hook 'org-mode-hook 'evil-org-mode) ;; only load with org-mode

(defun init-org-mode-buffer ()
  ;; This enables "clean mode", such that sublists use whitespace for indentation (ala markdown) instead of
  ;; many stars.
  (setq org-startup-indented t))

(eval-after-load 'org '(init-org-mode-buffer))

;; normal state shortcuts
(evil-define-key 'normal evil-org-mode-map
  "t" 'org-todo
  "T" '(lambda () (interactive) (evil-org-eol-call '(org-insert-todo-heading nil)))
  "H" 'org-beginning-of-line
  "L" 'org-end-of-line
  ";vt" 'org-show-todo-and-done-tree
  "o" '(lambda () (interactive) (evil-org-eol-call 'always-insert-item))
  ; "O" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))
  "$" 'org-end-of-line
  "^" 'org-beginning-of-line
  "<" 'org-metaleft
  ">" 'org-metaright
  ";a" '(lambda () (interactive)
          (org-archive-subtree)
          ;; For some reason org-archive-subtree aggressively scrolls the window down. Re-center the window on
          ;; the cursor.
          (call-interactively 'evil-scroll-line-to-center))
  ";g" 'org-set-tags-command
  ";va" 'org-agenda
  "-" 'org-cycle-list-bullet
  "gu" 'outline-up-heading
  ; Normally these go backwards-and-forward by paragraphs but skipping between headings is more useful.
  "{" 'org-backward-heading-same-level
  "}" 'org-forward-heading-same-level
  (kbd "<C-tab>") 'org-expand-top-level-parent
  (kbd "TAB") 'org-cycle)

;; normal & insert state shortcuts.
(mapc (lambda (state)
        (evil-define-key state evil-org-mode-map
          (kbd "C-S-L") 'org-metaright
          (kbd "C-S-H") 'org-metaleft
          (kbd "C-S-K") 'org-metaup
          (kbd "C-S-J") 'org-metadown
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

;; Moves the current heading (and all of its children) into the matching parent note in the archive file.
;; I think this is the most sensible way to archive TODOs in org mode files.
;; http://orgmode.org/worg/org-hacks.html
(defadvice org-archive-subtree (around my-org-archive-subtree activate)
  (let ((org-archive-location
         (if (save-excursion (org-back-to-heading)
                             (> (org-outline-level) 1))
             (concat (car (split-string org-archive-location "::"))
                     "::* "
                     (car (org-get-outline-path)))
           org-archive-location)))
    ad-do-it))

(defun org-show-todo-and-done-tree ()
  "Shows only subtrees which are TODOs or DONE items. Similar to org-show-todo-tree, but it matches DONE items
   as well."
  (interactive)
  (save-excursion
    ;; Note that these tags are case insensitive.
    (org-occur "\\(TODO\\|DONE\\|INPROGRESS\\|WAITING\\)")
    ;; org-occur highlights every TODO and DONE string match in the doc, which is distracting. Remove it.
    (org-remove-occur-highlights)))

;; When I've narrowed a subtree (e.g. via org-show-todo-and-done-tree), this allows me to quickly expand the
;; tree around the cursor to show all items again, not just TODO and DONE.
(defun org-expand-top-level-parent ()
  "Shows the children of the top-level parent for the tree under the cursor."
  (interactive)
  (save-excursion
    (outline-up-heading 4)
    (show-children)))
