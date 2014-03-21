;;
;; mu4e - email & gmail in Emacs.
;;
;; References:
;; * the mu4e manual
;; * https://groups.google.com/forum/#!topic/mu-discuss/qJ2zvyLPBX0

(provide 'mu4e-mode-personal)

(add-to-list 'load-path "/usr/local/Cellar/mu/0.9.9.5/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(setq mu4e-mu-binary "/usr/local/Cellar/mu/0.9.9.5/bin/mu")
(setq mu4e-maildir "~/.mail/work") ; Note to self: update your ~/.authinfo when changing accounts.
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-refile-folder  "/[Gmail].All Mail")
;; Note that I'm not using the Gmail Drafts and Trash folders. Sometimes offlineimap tries to create these
;; directories for some reason, which fails, since it already exists in Gmail.
;; (setq mu4e-trash-folder  "/[Gmail].Trash")
;; (setq mu4e-drafts-folder "/Drafts")

;; Use offline imap when fetching and reindexing mail.
(setq mu4e-get-mail-command "offlineimap -a work")
(setq mu4e-update-interval 60) ; Fetch and index new maile very 60s.

;; Folder shortcuts for the "jump-to-maildir" command.
(setq mu4e-maildir-shortcuts
    '(("/INBOX"               . ?i)
      ("/[Gmail].Sent Mail"   . ?s)
      ("/1action"             . ?1)
      ("/2hold"               . ?2)
      ("/[Gmail].All Mail"    . ?a)))

(setq user-mail-address "phil@liftoff.io"
      user-full-name  "Phil Crosby")

;; This shouldn't be necessary, but see https://github.com/djcb/mu/issues/399.
(setq mu4e-user-mail-address-list (list user-mail-address))

;; Don't save messages to Sent Messages, Gmail/IMAP takes care of this.
(setq mu4e-sent-messages-behavior 'delete)

;; Don't keep message buffers around.
(setq message-kill-buffer-on-exit t)

(setq mu4e-attachment-dir "~/Downloads")

;; Attempt to show images when viewing messages
(setq mu4e-view-show-images t
      mu4e-view-image-max-width 700)

;; Only complete the email addresses of people who directly sent me an email.
(setq mu4e-my-email-addresses user-mail-address)
(setq mu4e-compose-complete-only-personal t)

;; https://groups.google.com/forum/#!searchin/mu-discuss/html/mu-discuss/7WwtyrCBeDg/nr0vK9fT7BEJ
(setq mu4e-html2text-command "w3m -dump -cols 110 -T text/html")
;; (setq mu4e-html2text-command "html2text | grep -v '&nbsp_place_holder;'")

(add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; TODO(philc): what does this do?
(setq mu4e-compose-dont-reply-to-self t)

;; How tall to make the headers view when viewing headers+mail as a split.
(setq mu4e-headers-visible-lines 22)

;; Trim down the types of columns we show, to leave more room for the sender & subject.
(setq mu4e-headers-fields '((:human-date . 12)
                            ;; (:flags . 6)
                            (:from-or-to . 22)
                            (:subject . 74)))

(defun mu4e-view-is-below? ()
  "True if mu4e-view is displayed below the current buffer."
  (let ((window-below (window-in-direction 'below)))
    (and window-below
         (string= (buffer-name (window-buffer window-below)) "*mu4e-view*"))))

(eval-after-load 'mu4e
  '(progn
     (evil-make-overriding-map mu4e-main-mode-map 'normal t)
     (evil-define-key 'normal mu4e-main-mode-map
       "q" 'vimlike-quit
       ";" nil ; Ensure my evil-leader key works unhindered.
       "j" nil ; originally "jump to maildir".
       "gl" 'mu4e~headers-jump-to-maildir)

     (evil-make-overriding-map mu4e-headers-mode-map 'normal t)
     (evil-define-key 'normal mu4e-headers-mode-map
       "j" 'evil-next-line
       "k" 'evil-previous-line
       "n" (lambda () (interactive) (if (mu4e-view-is-below?) (mu4e-headers-next) (mu4e-headers-view-message)))
       "p" (lambda () (interactive) (if (mu4e-view-is-below?) (mu4e-headers-prev) (mu4e-headers-view-message)))
       "#" 'mu4e-headers-mark-for-trash
       "d" 'mu4e-view-mark-for-trash
       "y" 'mu4e-headers-mark-for-refile
       "/" 'mu4e-headers-search-edit
       "z" 'mu4e-headers-mark-for-unmark
       "x" 'mu4e-headers-mark-for-something
       "gl" 'mu4e~headers-jump-to-maildir
       ;; consider calling this with t, for "no confirmation".
       "e" 'mu4e-mark-execute-all
       "q" 'vimlike-quit
       (kbd "RET") 'mu4e-headers-view-message
       "o" 'mu4e-headers-view-message
       "ESC" nil
       "a" 'mu4e-reply-all
       "r" 'mu4e-compose-reply
       ;; TODO(philc): mu4e-headers-toggle-full-search - show all results or just up until the cap.
       ;; TODO(philc): mu4e-view-action opens URL
       "f" 'mu4e-compose-forward
       ;; By default, run this in the background. Hit ";vv to show the buffer with the fetch status.
       (kbd "M-r") (lambda () (interactive) (mu4e-update-mail-and-index t))
       "c" 'mu4e-compose-new)

     (evil-make-overriding-map mu4e-view-mode-map 'normal t)
     (evil-define-key 'normal mu4e-view-mode-map
       "j" 'evil-next-line
       "k" 'evil-previous-line
       "n" 'mu4e-view-headers-next
       "p" 'mu4e-view-headers-prev
       "#" 'mu4e-view-mark-for-trash
       "d" 'mu4e-view-mark-for-trash
       "y" 'mu4e-view-mark-for-refile
       "/" 'mu4e-view-search-edit
       "x" 'mu4e-view-mark-for-something
       "z" 'mu4e-view-mark-for-unmark
       "q" 'vimlike-quit
       "a" 'mu4e-compose-reply
       ;; Opens the URL under the cursor.
       (kbd "RET") (lambda () (interactive) (execute-kbd-macro (kbd "M-RET")))
       "go" nil
       "go1" (lambda () (interactive) (mu4e-view-go-to-url 1))
       "go2" (lambda () (interactive) (mu4e-view-go-to-url 2))
       "go3" (lambda () (interactive) (mu4e-view-go-to-url 3))
       "go4" (lambda () (interactive) (mu4e-view-go-to-url 4))
       "gl" (lambda ()
              (interactive)
              (switch-to-buffer-other-window "*mu4e-headers*")
              (call-interactively 'mu4e~headers-jump-to-maildir))
       ;; consider calling this with t, for "no confirmation".
       "e" 'mu4e-view-marked-execute
       (kbd "SPC") 'evil-ace-jump-word-mode
       "ESC" nil
       "a" 'mu4e-reply-all
       "r" 'mu4e-compose-reply
       "f" 'mu4e-compose-forward
       (kbd "M-r") '(lambda () (interactive) (mu4e-update-mail-and-index t))
       "c" 'mu4e-compose-new)

     (evil-leader/set-key-for-mode 'mu4e-headers-mode
       "vv" 'mu4e-show-fetch-progress)
     (evil-leader/set-key-for-mode 'mu4e-view-mode
       "vv" 'mu4e-show-fetch-progress
       "vr" 'mu4e-view-raw-message)

     (evil-make-overriding-map mu4e-compose-mode-map 'normal t)
     (evil-define-key 'normal mu4e-compose-mode-map
       "c" nil)
     (evil-leader/set-key-for-mode 'mu4e-compose-mode
       ;; Emacs always prompts me "Fix continuation lines?" when sending an email. I don't know what this prompt
       ;; means. It's in message-send-mail in message.el. Answer "y" automatically.
       "s" (lambda () (interactive) (without-confirmation 'message-send-and-exit)))))

;; Trim the number of fields shown in the email view. This is customizable. See mu4e-view.el for a full list.
(setq mu4e-view-fields '(:from :to  :cc :subject :date :tags :attachments))

;; NOTE(philc): This doesn't work. Tracking at
;; https://github.com/djcb/mu/issues/373
(defun mu4e-reply-all ()
  (interactive)
  (mu4e-compose-reply))
  ;; (flet ((mu4e~draft-user-wants-reply-all (&rest args) t))
    ;; (mu4e-compose-reply)))

(defun mu4e-show-fetch-progress ()
  (interactive)
  (mu4e-update-mail-and-index nil)
  (let ((buffer "*mu4e-update*"))
    ;; This update window shows up in a random place. Kill it.
    (delete-window (get-buffer-window buffer))
    (show-ephemeral-buffer-in-a-sensible-window (get-buffer buffer))))

(evil-set-initial-state 'mu4e-mode 'normal)
(evil-set-initial-state 'mu4e-main-mode 'normal)
(evil-set-initial-state 'mu4e-headers-mode 'normal)
(evil-set-initial-state 'mu4e-view-mode 'normal)
(evil-set-initial-state 'mu4e-compose-mode 'normal)

;; Settings for sending mail.
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-stream-type 'ssl)
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 465)
