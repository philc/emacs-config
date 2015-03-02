;;
;; notmuch - email & gmail in Emacs.
;;

(provide 'notmuch-mode-personal)

(require 'notmuch)

;; Don't keep message buffers around.
(setq message-kill-buffer-on-exit t)

(setq my-email-address "philc3@gmail.com")

(evil-set-initial-state 'notmuch-search-mode 'normal)
(evil-set-initial-state 'notmuch-tree-mode 'normal)
(evil-set-initial-state 'notmuch-hello-mode 'normal)
(evil-set-initial-state 'notmuch-show-mode 'normal)

;; Sort messages newest first.
(set 'notmuch-search-oldest-first nil) ; The default is t.

(evil-define-key 'normal notmuch-search-mode-map
  "o" 'notmuch-search-show-thread-in-other-window
  "q" 'vimlike-quit
  ;; "t" (lambda () (interactive) (print (notmuch-search-get-result) ))
  "t" (lambda () (interactive) (print (notmuch-search-find-thread-id t)))
  ;; I'm using "Y" here to archive, so that I can still copy text from the thread view if I want. Will I ever
  ;; do that?
  ;; "Y" 'notmuch-search-archive-thread
  "Y" 'archive-message
  "D" 'delete-thread
  ;; I'm using these custom-scroll functions for page-up and page-down because the built-in ones in Emacs
  ;; switch to the buffer when yout try to scroll up past the beginning of the window.
  "u" (lambda () (interactive) (within-message-view (lambda ()
                                                      (condition-case nil (scroll-down)
                                                        (beginning-of-buffer (goto-char (point-min)))))))
  "d" (lambda () (interactive) (within-message-view (lambda ()
                                                      (condition-case nil (scroll-up)
                                                        (end-of-buffer (goto-char (point-max))))))))

(evil-leader/set-key-for-mode 'notmuch-hello-mode
  "gi" (lambda ()
         (interactive)
         (notmuch-search "tag:inbox")))

(evil-leader/set-key-for-mode 'notmuch-search-mode
  "gli" (lambda ()
         (interactive)
         (notmuch-go-to-inbox))
  "gls" (lambda ()
         (interactive)
         (notmuch-go-to-sent))
  "gl1" (lambda ()
         (interactive)
         (notmuch-go-to-label-1action))
  "t" (lambda ()
        (interactive)
        (print  (notmuch-search-find-thread-id)))
  "r" 'notmuch-refresh-this-buffer
  "1" (lambda ()
         (interactive)
         (move-thread "mylabel")))

;;
;; Settings for show mode
;;
(setq notmuch-message-headers '("To")) ; The default list is '("Subject" "To" "Cc" "Date").
(setq notmuch-show-indent-messages-width 2) ; The default is 1.

;; Show HTML mail by default, and keep the text/plain hidden.
;; (setq notmuch-multipart/alternative-discouraged '("text/plain" "text/html"))
;; (setq notmuch-multipart/alternative-discouraged '("text/html" "text/plain"))

;; By default the "show hidden multipart" buttons are very bright (and distracting) in my color scheme.
;; Make them be the same color as the email's body text.
(set-face-foreground 'message-mml (face-attribute 'default :foreground))

(evil-define-key 'normal notmuch-show-mode-mode
  "r" 'notmuch-show-reply-sender
  "R" 'notmuch-show-reply)

(defun get-message-view ()
  ;; TODO(philc): Select the notmuch-search buffer, then move to its right.
  )

;; (setq view-scroll-auto-exit nil)
;; (setq scroll-error-top-bottom t)

(defun within-message-view
    (f)
  "Execute the given function within the message view (the window to the right of the notmuch-search window."
  (lexical-let* ((f f)
                 (win (window-in-direction 'right)))
    (if win
        (util/preserve-selected-window (lambda ()
                                         (select-window win)
                                         (funcall f)))
      (message "No Notmuch messages view window is visible."))))

;; Useful functions
;; notmuch-poll-and-refresh-this-buffer

(defun notmuch-go-to-inbox ()
  (interactive)
  (notmuch-search "folder:Inbox"))

(defun notmuch-go-to-sent ()
  (interactive)
  ;; This is a workaround. I couldn't for the life of me determine how to search for [Gmail]/.Sent mail
  (notmuch-search (concat "from:" my-email-address)))

(defun notmuch-go-to-label-1action ()
  (interactive)
  (notmuch-search "folder:1action"))

;; TODO(philc): Move these into utils.
;; Sample invocation: (process-exit-code-and-output "ls" "-h" "-l" "-a") ;; => (0 "-r-w-r-- 1 ...")
(defun call-process-with-exit-status (program &rest args)
  "Runs a command and returns a list containing the status code and output string."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun call-process-and-check (program &rest args)
  "Calls the given program and raises an error if the exist status is non-zero."
  (lexical-let ((result (apply 'call-process-with-exit-status program args)))
    (if (= (first result) 0)
        (second result)
      (throw nil (concat "This command exited with status "
                         (number-to-string (first result))
                         ": `"
                         (mapconcat 'identity (append (list program) args) " ")
                         "`")))))

(defun get-messages-to-move (thread-id include-special-gmail-folders)
  (lexical-let ((filter-fn (if include-special-gmail-folders
                               (lambda (s) nil)
                             (lambda (s) (search "[Gmail]" s)))))
    (->>
     ;;"thread:0000000000000490"
     thread-id
     list
     (append '("search" "--output=files" "--format=sexp"))
     (apply 'notmuch-call-notmuch-sexp)
     (remove-if filter-fn)
     )))

(defun archive-message ()
  "Intended to be called from the search view."
  (interactive)
  (->> (get-messages-to-move (notmuch-search-find-thread-id) nil)
       (mapcar 'delete-file))
  ;; Now that some files have been removed from the disk, ask notmuch to update its database.
  (notmuch-call-notmuch-process "new")
  (notmuch-refresh-this-buffer))

(defun string-chomp-end (str)
  "Chomp tailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any " \t\n")) eos)
                            ""
                            str))

(defun get-notmuch-db-path ()
  (string-chomp-end (call-process-and-check "notmuch" "config" "get" "database.path")))

(defun delete-thread ()
  (interactive)
  (->> (get-messages-to-move (notmuch-search-find-thread-id) t)
       (mapcar (lambda (m) (perform-delete-message m))))
  ;; Now that some files have been removed from the disk, ask notmuch to update its database.
  (notmuch-call-notmuch-process "new")
  (notmuch-refresh-this-buffer))

(defun perform-delete-message (file)
  "Moves an email into a .deletions directory at the root of your imap email folder."
  ;; Another way to handle deletions would be to move the message into Gmail's trash folder, but that would
  ;; require syncing the Gmail trash folder, which is something I'm not doing now with mbsync due to the
  ;; unnecessary overhead.
  (lexical-let ((deletions-folder (concat (get-notmuch-db-path) "/.deletions/")))
    (unless (file-exists-p deletions-folder)
      (make-directory deletions-folder))
    ;; (print (concat "deleting" file))
    ;; (print "exists?")
    ;; (print (file-exists-p file))
    ;; (print (concat deletions-folder (file-name-nondirectory file)))
    ;; (call-process-and-check "mv" file (concat deletions-folder (file-name-nondirectory file)))
    (rename-file file (concat deletions-folder (file-name-nondirectory file)))))
    ;; ))

(defun notmuch-search-quick-refresh-view ()
  ;; TODO(philc): document/remove
  (let ((target-line (line-number-at-pos))
        (oldest-first notmuch-search-oldest-first)
        (query notmuch-search-query-string))
    (notmuch-bury-or-kill-this-buffer)
    (notmuch-search query oldest-first nil target-line)
    ;; TODO(philc): Remove this goto-char?
    ;; (goto-char (point-min))
    (goto-line target-line)
    ))

(defun move-thread (dest-folder)
  (perform-move-thread (notmuch-search-find-thread-id) dest-folder)
  (notmuch-call-notmuch-process "new")
  (notmuch-refresh-this-buffer))

(defun perform-move-thread (thread-id dest-folder)
  (lexical-let ((destination (concat (get-notmuch-db-path) "/" dest-folder "/cur/")))
    (->> (get-messages-to-move thread-id nil)
         (mapcar (lambda (m) (rename-file m destination))))))

(add-hook 'notmuch-search-hook
          (lambda ()
            ;; I have global-visual-line-mode enabled in my init.el, but we don't want word-wrapping in the
            ;; search view. One message per line, no matter how long the subject.
            (visual-line-mode -1)
            (toggle-truncate-lines 1)))

;; TODO(philc): Make the date strings shorter. They're long.
;; (setq notmuch-search-result-format
;;       ;; This is the default, but with the subject truncated so it always fits in my splits
;;       ;; `(("date" . "%8s ")
;;       `(("date" . "%12s ")
;;         ("count" . "%-7s ")
;;         ("authors" . "%-20s ")
;;         ("subject" . "%s ")
;;         ("tags" . "(%s)")))


(defun notmuch-search-show-thread-in-other-window (&optional elide-toggle)
  "Display the currently selected thread."
  (interactive "P")
  (let ((thread-id (notmuch-search-find-thread-id))
        (subject (notmuch-search-find-subject))
        (b (current-buffer)))
    (if (> (length thread-id) 0)
        (util/preserve-selected-window
         (lambda ()
           (let ((right-window (or (window-in-direction 'right)
                                   (split-window-horizontally))))
             (select-window right-window))
           (notmuch-show thread-id
                         elide-toggle
                         b
                         nil ; This should probably be notmuch-search-query-string, but that is a private var.
                         ;; Name the buffer based on the subject.
                         (concat "*" (truncate-string-to-width subject 30 nil nil t) "*"))))
      (message "End of search results."))))



    ;; (set-keymap-parent map notmuch-common-keymap)
    ;; (define-key map "Z" 'notmuch-tree-from-show-current-query)
    ;; (define-key map (kbd "<C-tab>") 'widget-backward)
    ;; (define-key map (kbd "M-TAB") 'notmuch-show-previous-button)
    ;; (define-key map (kbd "<backtab>") 'notmuch-show-previous-button)
    ;; (define-key map (kbd "TAB") 'notmuch-show-next-button)
    ;; (define-key map "f" 'notmuch-show-forward-message)
    ;; (define-key map "r" 'notmuch-show-reply-sender)
    ;; (define-key map "R" 'notmuch-show-reply)
    ;; (define-key map "|" 'notmuch-show-pipe-message)
    ;; (define-key map "w" 'notmuch-show-save-attachments)
    ;; (define-key map "V" 'notmuch-show-view-raw-message)
    ;; (define-key map "c" 'notmuch-show-stash-map)
    ;; (define-key map "h" 'notmuch-show-toggle-visibility-headers)
    ;; (define-key map "*" 'notmuch-show-tag-all)
    ;; (define-key map "-" 'notmuch-show-remove-tag)
    ;; (define-key map "+" 'notmuch-show-add-tag)
    ;; (define-key map "X" 'notmuch-show-archive-thread-then-exit)
    ;; (define-key map "x" 'notmuch-show-archive-message-then-next-or-exit)
    ;; (define-key map "A" 'notmuch-show-archive-thread-then-next)
    ;; (define-key map "a" 'notmuch-show-archive-message-then-next-or-next-thread)
    ;; (define-key map "N" 'notmuch-show-next-message)
    ;; (define-key map "P" 'notmuch-show-previous-message)
    ;; (define-key map "n" 'notmuch-show-next-open-message)
    ;; (define-key map "p" 'notmuch-show-previous-open-message)
    ;; (define-key map (kbd "M-n") 'notmuch-show-next-thread-show)
    ;; (define-key map (kbd "M-p") 'notmuch-show-previous-thread-show)
    ;; (define-key map (kbd "DEL") 'notmuch-show-rewind)
    ;; (define-key map " " 'notmuch-show-advance-and-archive)
    ;; (define-key map (kbd "M-RET") 'notmuch-show-open-or-close-all)
    ;; (define-key map (kbd "RET") 'notmuch-show-toggle-message)
    ;; (define-key map "#" 'notmuch-show-print-message)
    ;; (define-key map "!" 'notmuch-show-toggle-elide-non-matching)
    ;; (define-key map "$" 'notmuch-show-toggle-process-crypto)
    ;; (define-key map "<" 'notmuch-show-toggle-thread-indentation)
    ;; (define-key map "t" 'toggle-truncate-lines)
    ;; (define-key map "." 'notmuch-show-part-map)
    ;; map)


;; (evil-define-key

;; (defvar notmuch-search-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map notmuch-common-keymap)
;;     (define-key map "x" 'notmuch-bury-or-kill-this-buffer)
;;     (define-key map (kbd "<DEL>") 'notmuch-search-scroll-down)
;;     (define-key map "b" 'notmuch-search-scroll-down)
;;     (define-key map " " 'notmuch-search-scroll-up)
;;     (define-key map "<" 'notmuch-search-first-thread)
;;     (define-key map ">" 'notmuch-search-last-thread)
;;     (define-key map "p" 'notmuch-search-previous-thread)
;;     (define-key map "n" 'notmuch-search-next-thread)
;;     (define-key map "r" 'notmuch-search-reply-to-thread-sender)
;;     (define-key map "R" 'notmuch-search-reply-to-thread)
;;     (define-key map "o" 'notmuch-search-toggle-order)
;;     (define-key map "c" 'notmuch-search-stash-map)
;;     (define-key map "t" 'notmuch-search-filter-by-tag)
;;     (define-key map "f" 'notmuch-search-filter)
;;     (define-key map [mouse-1] 'notmuch-search-show-thread)
;;     (define-key map "*" 'notmuch-search-tag-all)
;;     (define-key map "a" 'notmuch-search-archive-thread)
;;     (define-key map "-" 'notmuch-search-remove-tag)
;;     (define-key map "+" 'notmuch-search-add-tag)
;;     (define-key map (kbd "RET") 'notmuch-search-show-thread)
;;     (define-key map "Z" 'notmuch-tree-from-search-current-query)
;;     map)

;; (with-eval-after-load "mu4e"
;;   (evil-make-overriding-map mu4e-main-mode-map 'normal t)
;;   (evil-define-key 'normal mu4e-main-mode-map
;;     "q" 'vimlike-quit
;;     ";" nil ; Ensure my evil-leader key works unhindered.
;;     "j" nil ; originally "jump to maildir".
;;     "gl" 'mu4e~headers-jump-to-maildir)

;;   (evil-make-overriding-map mu4e-headers-mode-map 'normal t)
;;   (evil-define-key 'normal mu4e-headers-mode-map
;;     "j" 'evil-next-line
;;     "k" 'evil-previous-line
;;     "n" (lambda () (interactive) (if (mu4e-view-is-below?) (mu4e-headers-next) (mu4e-headers-view-message)))
;;     "p" (lambda () (interactive) (if (mu4e-view-is-below?) (mu4e-headers-prev) (mu4e-headers-view-message)))
;;     "#" 'mu4e-headers-mark-for-trash
;;     "d" 'mu4e-view-mark-for-trash
;;     "y" 'mu4e-headers-mark-for-refile
;;     "/" 'mu4e-headers-search-edit
;;     "z" 'mu4e-headers-mark-for-unmark
;;     "x" 'mu4e-headers-mark-for-something
;;     "gl" 'mu4e~headers-jump-to-maildir
;;     ;; consider calling this with t, for "no confirmation".
;;     "e" 'mu4e-mark-execute-all
;;     "q" 'vimlike-quit
;;     (kbd "RET") 'mu4e-headers-view-message
;;     "o" 'mu4e-headers-view-message
;;     "ESC" nil
;;     "a" 'mu4e-reply-all
;;     "r" 'mu4e-compose-reply
;;     ;; TODO(philc): mu4e-headers-toggle-full-search - show all results or just up until the cap.
;;     ;; TODO(philc): mu4e-view-action opens URL
;;     "f" 'mu4e-compose-forward
;;     ;; By default, run this in the background. Hit ";vv to show the buffer with the fetch status.
;;     (kbd "M-r") (lambda () (interactive) (mu4e-update-mail-and-index t))
;;     "c" 'mu4e-compose-new)

;;   (evil-make-overriding-map mu4e-view-mode-map 'normal t)
;;   (evil-define-key 'normal mu4e-view-mode-map
;;     "j" 'evil-next-line
;;     "k" 'evil-previous-line
;;     "n" 'mu4e-view-headers-next
;;     "p" 'mu4e-view-headers-prev
;;     "#" 'mu4e-view-mark-for-trash
;;     "d" 'mu4e-view-mark-for-trash
;;     "y" 'mu4e-view-mark-for-refile
;;     "/" 'mu4e-view-search-edit
;;     "x" 'mu4e-view-mark-for-something
;;     "z" 'mu4e-view-mark-for-unmark
;;     "q" 'vimlike-quit
;;     "a" 'mu4e-compose-reply
;;     ;; Opens the URL under the cursor.
;;     (kbd "RET") (lambda () (interactive) (execute-kbd-macro (kbd "M-RET")))
;;     "go" nil
;;     "go1" (lambda () (interactive) (mu4e-view-go-to-url 1))
;;     "go2" (lambda () (interactive) (mu4e-view-go-to-url 2))
;;     "go3" (lambda () (interactive) (mu4e-view-go-to-url 3))
;;     "go4" (lambda () (interactive) (mu4e-view-go-to-url 4))
;;     "gl" (lambda ()
;;            (interactive)
;;            (switch-to-buffer-other-window "*mu4e-headers*")
;;            (call-interactively 'mu4e~headers-jump-to-maildir))
;;     ;; consider calling this with t, for "no confirmation".
;;     "e" 'mu4e-view-marked-execute
;;     (kbd "SPC") 'evil-ace-jump-word-mode
;;     "ESC" nil
;;     "a" 'mu4e-reply-all
;;     "r" 'mu4e-compose-reply
;;     "f" 'mu4e-compose-forward
;;     (kbd "M-r") '(lambda () (interactive) (mu4e-update-mail-and-index t))
;;     "c" 'mu4e-compose-new)

;;   (evil-leader/set-key-for-mode 'mu4e-headers-mode
;;     "vv" 'mu4e-show-fetch-progress)
;;   (evil-leader/set-key-for-mode 'mu4e-view-mode
;;     "vv" 'mu4e-show-fetch-progress
;;     "vr" 'mu4e-view-raw-message)

;;   (evil-make-overriding-map mu4e-compose-mode-map 'normal t)
;;   (evil-define-key 'normal mu4e-compose-mode-map
;;     "c" nil)
;;   (evil-leader/set-key-for-mode 'mu4e-compose-mode
;;     ;; Emacs always prompts me "Fix continuation lines?" when sending an email. I don't know what this prompt
;;     ;; means. It's in message-send-mail in message.el. Answer "y" automatically.
;;     "s" (lambda () (interactive) (util/without-confirmation 'message-send-and-exit))))

;; RET             notmuch-search-show-thread
;;   (that binding is currently shadowed by another mode)
;; SPC             notmuch-search-scroll-up
;;   (that binding is currently shadowed by another mode)
;; *               notmuch-search-tag-all
;;   (that binding is currently shadowed by another mode)
;; +               notmuch-search-add-tag
;;   (that binding is currently shadowed by another mode)
;; -               notmuch-search-remove-tag
;;   (that binding is currently shadowed by another mode)
;; <               notmuch-search-first-thread
;;   (that binding is currently shadowed by another mode)
;; =               notmuch-refresh-this-buffer
;;   (that binding is currently shadowed by another mode)
;; >               notmuch-search-last-thread
;;   (that binding is currently shadowed by another mode)
;; ?               notmuch-help
;;   (that binding is currently shadowed by another mode)
;; G               notmuch-poll-and-refresh-this-buffer
;;   (that binding is currently shadowed by another mode)
;; R               notmuch-search-reply-to-thread
;;   (that binding is currently shadowed by another mode)
;; Z               notmuch-tree-from-search-current-query
;;   (that binding is currently shadowed by another mode)
;; a               notmuch-search-archive-thread
;;   (that binding is currently shadowed by another mode)
;; b               notmuch-search-scroll-down
;;   (that binding is currently shadowed by another mode)
;; c               notmuch-search-stash-map
;;   (that binding is currently shadowed by another mode)
;; f               notmuch-search-filter
;;   (that binding is currently shadowed by another mode)
;; j               notmuch-jump-search
;;   (that binding is currently shadowed by another mode)
;; m               notmuch-mua-new-mail
;;   (that binding is currently shadowed by another mode)
;; n               notmuch-search-next-thread
;;   (that binding is currently shadowed by another mode)
;; o               notmuch-search-toggle-order
;;   (that binding is currently shadowed by another mode)
;; p               notmuch-search-previous-thread
;;   (that binding is currently shadowed by another mode)
;; r               notmuch-search-reply-to-thread-sender
;;   (that binding is currently shadowed by another mode)
;; s               notmuch-search
;;   (that binding is currently shadowed by another mode)
;; t               notmuch-search-filter-by-tag
;;   (that binding is currently shadowed by another mode)
;; x               notmuch-bury-or-kill-this-buffer
;;   (that binding is currently shadowed by another mode)
;; z               notmuch-tree
;;   (that binding is currently shadowed by another mode)
;; DEL             notmuch-search-scroll-down

;; Settings for sending mail.
;; (require 'smtpmail)
;; (setq message-send-mail-function 'smtpmail-send-it)
;; (setq smtpmail-stream-type 'ssl)
;; (setq smtpmail-smtp-server "smtp.gmail.com")
;; (setq smtpmail-smtp-service 465)
