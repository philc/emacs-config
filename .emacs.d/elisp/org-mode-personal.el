;;; -*- lexical-binding: t; -*-
;;
;;; My additions and customizations to org-mode.
;;; Based loosely on evil-org-mode as a starting point.
;;; Provides an evil-org-mode minor mode.
;;; https://github.com/edwtjo/evil-org-mode

(require 'evil)
(require 'org)
(require 's)

(provide 'org-mode-personal)

(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap (make-sparse-keymap) ; defines evil-org-mode-map
  :group 'evil-org)

(add-hook 'org-mode-hook 'evil-org-mode) ;; only load with org-mode

;; Highlight the TODO keywords using various colors
;; See http://orgmode.org/manual/Faces-for-TODO-keywords.html.
(setq org-todo-keyword-faces '(("WAITING" . (:foreground "#999999"))
                               ("TODO" . (:foreground "orange")) ; The default red used for TODO is opressive
                               ("IP" .(:foreground "#85C7FF"))))

(defun init-org-mode-personal ()
  ;; This enables "clean mode", such that sublists use whitespace for indentation (ala markdown) instead of
  ;; many stars.
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
  ; Normally these go backwards-and-forward by paragraphs but skipping between headings is more useful.
  "{" 'org-backward-heading-same-level
  "}" 'org-forward-heading-same-level
  (kbd "<C-tab>") 'org-expand-top-level-parent
  ;; For some mode, org-mode maps TAB to org-cycle, and outline-mode maps <tab> to outline-cycle.
  ;; <tab> supercedes and shadows <tab>. One difference between the two functions is that org-cycle
  ;; retains the cursor position when cycling, which is desirable.
  (kbd "<tab>") 'org-cycle
  (kbd "TAB") 'org-cycle)

(defun preview-org ()
  "Pipes the buffer's contents into a script which renders the markdown as HTML and opens in a browser."
  (interactive)
  ;; This convert_org_to_markdown.rb is a primitive script I've written which fits my needs.
  (call-process-region (point-min) (point-max) "/bin/bash" nil nil nil "-c"
                       "convert_org_to_markdown.rb | markdown_page.rb | bcat"))


(define-leader-keys 'org-mode-map
  "a" 'org-archive-subtree
  "c" 'org-capture-item-and-prepend-to-subtree
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
      ;; TODO(philc): Make S-C-enter insert a heading above
          ;; (kbd "<C-S-return>") '(lambda () (interactive)
          ;;                       (org-insert-subheading-as-first-child)
          ;;                       (evil-append nil))))
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

(defun text-of-current-line ()
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-beginning-position 2)))

(defun org-get-current-heading ()
  "Assumes the cursor is currently on a heading. TODO: return nil if the cursor isn't on a heading."
  (-> (text-of-current-line) s-trim (split-string "* ") cl-second))

(defun org-move-to-heading (heading-name)
  (let ((heading-has-changed nil)
        (heading-has-been-found nil)
        (current-heading nil))
    (while (not (or (eq heading-has-changed 't)
                    (eq heading-has-been-found 't)))
      (setq current-heading (org-get-current-heading))
      (setq heading-has-been-found (string= current-heading heading-name))
      (when (not heading-has-been-found)
        (org-forward-heading-same-level nil)
        (setq heading-has-changed (string= current-heading (org-get-current-heading)))))))

(defun org-insert-subheading-as-first-child (subheading-text)
  "Inserts the given text as the first child of the heading which is currently under the cursor."
  (org-insert-heading-after-current)
  (insert subheading-text)
  (org-demote)
  ;; TODO(philc): This works because org-move-subtree-up throws an error (using user-error) when it can no
  ;; longer move up. Change this so we only invoke org-move-subtree-up "current-depth" times.
  (while t
    ;; This will throw an exception once we can no longer move the subtree up.
    (org-move-subtree-up)))

(defun org-capture-item-and-prepend-to-subtree ()
  "Prompts for a TODO and the name of a top-level heading, and adds the TODO as a child to the heading."
  (interactive)
  ;; NOTE(philc): These are personalized to the way I organize my org mode TODO file.
  (message "[L] Liftoff  [B] Base  [S] Study  [N] Entertainment  [M] Emacs [J] Journal")
  (let ((subheading (pcase (read-char)
                      (?l "Liftoff")
                      (?b "Base")
                      (?s "Study")
                      (?j "Journal")
                      (?n "Entertainment")
                      (?m "Emacs")
                      (?h "Handbook")
                      (?p "Side projects")
                      (?v "Vimium")
                      (?d "PD"))))
    (when subheading
      (let ((new-todo (read-from-minibuffer (concat subheading " TODO: "))))
        (save-excursion
          (goto-char 0)
          (org-move-to-heading subheading)
          (org-insert-subheading-as-first-child new-todo))))))

(defun org-goto-top-level-heading ()
  (interactive)
  "Prompts for the name of a top-level heading and jumps to there."
  ;; TODO(philc): Populate these completions with the top-level headers from the buffer.
  (let* (; I got this elaborate esnippet from online.
         (headings (org-map-entries (lambda () (fifth (org-heading-components))) "LEVEL=1"))
         (heading (completing-read "Heading: " headings nil t)))
    (goto-char 0)
    (org-move-to-heading heading)
    (recenter-no-redraw)))
