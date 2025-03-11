;;; -*- lexical-binding: t; -*-
;;
;; notmuch - email & gmail in Emacs.
;;
;; These are the major common data structures used in this file:
;;
;; * message-id: an ID from the email header used for identifying and grouping messages into threads. Used by
;;   notmuch commands.
;; * notmuch-message: a message returned by `notmuch-show`, in plist form.
;; * message: a string containing a message-header and message-body.
;; * message-header: the text section of a message containing fields like To: and From:
;; * message-body: the whole text section below the email header, including the reply-text and quoted-text.
;; * reply-text: the mesage text excluding the quoted history and any "attribution lines" (e.g. "On Tues Oct 4
;;   Phil wrote:"). For new emails which have no reply history, `reply-text` is equal to `message-body`.
;; * quoted-text: the text of all quoted history following `reply-text`. This does not include any quoted text
;;   (which can occur when writing "inline replies") which is part of the reply-text.
;;
;; References
;; * http://en.wikipedia.org/wiki/Posting_style
;;

(provide 'notmuch-ext)
(require 'notmuch)
(require 'json)
(require 'dash)
(require 'dash-functional)
(require 's)

(defun in-notmuch (f)
  "Execute the given function within the notmuch view. Used for REPL-based development."
  (util/preserve-selected-window
   (let* ((f f))
     (fn ()
       (let ((w (select-window (get-buffer-window "*notmuch-search-folder:Inbox*"))))
         (when (not w)
           (throw "Can't switch to the given notmuch window" nil))
         (funcall f))))))

(defun notmuch-ext/get-body-parts-from-notmuch-message (notmuch-message)
  "Returns a flattened list of all parts of a notmuch-message.
   Not that notmuch-messages can optionally contain multiple bodies with different content-types, sometimes
   nested, e.g. a multipart body which contains plaintext and text/HTML children.
   - notmuch-message: a plist of the form:
   (:body ((:content-type 'multipart' :body ((:content-type 'html' :content 'the-html-text') ...))))"
  (setq queue (plist-sget notmuch-message :body))
  (setq result '())
  (while queue
    (let* ((item (pop queue))
           (content (plist-sget item :content)))
      (push item result)
      ;; Content can either be a string (in the case of text/html) or a list (in the case of
      ;; multipart/alternative)).
      (when (and content (not (stringp content)))
        (setq queue (append queue content)))))
  result)

(defun notmuch-ext/remove-empty-envelopes (list)
  "Removes any empty containing lists surrounding a plist. notmuch show --format=sexp returns results with
   many empty surrounding lists, which sometimes include sparse nils (when using --entire-thread=false for
   instance) and sublists containing just nils.
   Example: (remove-empty-envelopes '(((nil (nil (:a 1))))) => '(:a 1)."
  (setq current list)
  (while (not (-> current -non-nil first symbolp))
    (setq current (-> current -non-nil first)))
  current)

(defun notmuch-ext/get-html-body (message-id)
  "Returns the HTML body for the given message ID. Returns nil if there is no HTML body (e.g. if there's only
   a plaintext body.)"
  (let* ((message (->> message-id
                       (util/call-process-and-check "notmuch" nil "show" "--format=sexp"
                                                    "--entire-thread=false" "--include-html")
                       read
                       ;; Since we have --entire-thread=false, this list of messages can have nil
                       ;; entries.
                       notmuch-ext/remove-empty-envelopes))
         (parts (notmuch-ext/get-body-parts-from-notmuch-message message))
         (html-part (-?>> parts
                          (--filter (string= (plist-sget it :content-type) "text/html"))
                          first)))
    (-?> html-part (plist-sget :content))))

(defun notmuch-ext/extract-message-id (message-body)
  "Extracts the message ID which is identified by the field 'In-Reply-To: <the-message-id@gmail.com>'."
  (string-match "In-Reply-To: <\\(.*\\)>" message-body)
  ;; TODO(philc): To robustify, assert that message-body doesn't contain "References:".
  (match-string 1 message-body))

(defun notmuch-ext/assemble-multipart-mml (plaintext-body html-body)
  "Formats the plaintext + HTML text into a multipart message using MML syntax, which is understood by Emac's
   message-mode. See http://gnus.org/manual/emacs-mime_10.html#Simple-MML-Example for details."
  (->> (list "<#multipart type=alternative>"
             plaintext-body
             "<#part type=text/html>"
             html-body
             "<#/multipart>")
       (s-join "\n")))

(setq notmuch-ext/header-section-separator "--text follows this line--")
(setq notmuch-ext/attribution-line-regexp ".+ \<.+\> writes:")

(defun notmuch-ext/get-message-parts (message)
  "Splits a message into its parts. Returns a plist of :header, :reply-text, :quoted-text, :attribution-line."
  (let* ((parts (split-string message notmuch-ext/header-section-separator))
         (header (-> parts first s-trim))
         (message-body (-> parts cl-second s-trim))
         (body-lines (s-split "\n" message-body))
         (attribution-line-index (--find-last-index
                                  (string-match notmuch-ext/attribution-line-regexp it)
                                  body-lines))
         ;; TODO(philc): If there's no attribution line in a reply message, we should instead just
         ;; split on the starting index of the contiguous quoted region at the bottom of the email.
         (reply-text (--> body-lines (-slice it 0 attribution-line-index) (s-join "\n" it)))
         (quoted-text (when attribution-line-index
                        (--> body-lines (-slice it (+ 1 attribution-line-index)) (s-join "\n" it)))))
    (list :header header
          :attribution-line (-?> attribution-line-index (nth body-lines))
          ;; TODO(philc): Rename this to compose-text.
          :reply-text reply-text
          :quoted-text quoted-text)))

(defun notmuch-ext/build-response-from-markdown (message-body)
  "Takes in a compose/reply buffer and returns a multipart response (plaintext and HTML) where the HTML
   portion contains the plaintext reply, converted to markdown.
   Returns a plist of header, plaintext, html."
  (let* ((message-id (notmuch-ext/extract-message-id message-body)) ; nil if this msg isn't a reply.
         (parts (notmuch-ext/get-message-parts message-body))
         (quoted-text (plist-get parts :quoted-text))
         (attribution-line (plist-get parts :attribution-line))
         (plaintext-response (->> (list (plist-get parts :reply-text)
                                        attribution-line
                                        quoted-text)
                                  -non-nil
                                  (s-join "\n\n")))
         ;; TODO(philc): do I need Gmail CSS, i.e. do I need to pass "--css" "gmail" to this command?
         (markdown-reply-text (->> (plist-get parts :reply-text)
                                   (util/call-process-and-check notmuch-ext/markdown-to-html-command)))
         (html-quoted-text (when message-id
                             (notmuch-ext/get-html-body (concat "id:" message-id))))
         ;; There may be no quoted-text if this is a new message with no reply history.
         (quoted-text-as-html
          (cond
           (html-quoted-text (concat "<blockquote>\n" html-quoted-text "\n<blockquote>"))
           (quoted-text (concat "<pre>\n" quoted-text "\n</pre>"))))
         (html-response (concat (s-trim markdown-reply-text)
                                (when attribution-line "\n<br/>\n")
                                attribution-line
                                (when quoted-text-as-html "\n<br/>\n")
                                quoted-text-as-html)))
    (list :header (plist-get parts :header)
          :plaintext plaintext-response
          :html html-response)))

(setq notmuch-ext/stylesheet-for-previews
      "html { width: 1000px; margin: 10px auto 0 auto; font-family: Helvetica Neue;}")

(defun notmuch-ext/render-message-in-browser (html)
  ;; TODO(philc): Make this command configurable.
  (let ((styled-html (concat "<style>" notmuch-ext/stylesheet-for-previews "</style>" html)))
    (util/call-process-and-check "browser" styled-html)))

(defun notmuch-ext/view-message-in-browser ()
  (interactive)
  (-> (buffer-substring-no-properties (point-min) (point-max))
      notmuch-ext/build-response-from-markdown
      (plist-get :html)
      notmuch-ext/render-message-in-browser))

;; When composing an email in Emacs message mode, you can write both HTML and plaintext versions of the same
;; message using MML. This generates said MML for the contents of the current buffer.
;; References:
;; http://edward.oconnor.cx/2008/01/html-email-composition-in-emacs
;; http://gnus.org/manual/emacs-mime_9.html#SEC9
(defun notmuch-ext/get-mml-for-buffer ()
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (response (notmuch-ext/build-response-from-markdown text)))
    (->> (list (plist-get response :header)
               notmuch-ext/header-section-separator
               (notmuch-ext/assemble-multipart-mml (plist-get response :plaintext)
                                                   (plist-get response :html)))
         (s-join "\n"))))

(defun notmuch-ext/convert-to-markdown-and-send ()
  (interactive)
  (util/replace-buffer-text (notmuch-ext/get-mml-for-buffer))
  (message-send-and-exit))

(defun notmuch-preview-html-reply-in-browser ()
  (interactive)
  (let* ((separator "--text follows this line--")
         (buffer (buffer-substring-no-properties (point-min) (point-max)))
         (body (second (split-string buffer separator)))
         ;; TODO(philc): Consider centering this content in the browser window for a nicer preview.
         (html-body (util/call-process-and-check notmuch-markdown-to-html body "--css" "gmail")))
    (util/call-process-and-check "browser" html-body)))

;; Possible options ot customize
;; (defcustom message-directory "~/Mail/"
;; (defcustom message-max-buffers 10

;; add Cc and Bcc headers to the message buffer
;; (setq message-default-mail-headers "Cc: \n")
;; postponed message is put in the following draft file

;; Where the temporary "*message*" buffers created by the message mode are saved.
(setq message-auto-save-directory "~/.mail/drafts")

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

(defun notmuch-ext/get-selected-message-id ()
  "Returns the message ID of the selected thread. Works in both notmuch-search and notmuch-show modes."
  (cond ((s-equals? major-mode "notmuch-show-mode")
         (notmuch-show-get-message-id t))
        ((s-equals? major-mode "notmuch-search-mode")
         (-> (notmuch-search-find-thread-id) get-newest-message-in-thread))
        (t (throw "This is not a recognized notmuch mode in get-selected-message-id" nil))))

(defun notmuch-query-for-newest-message-in-thread ()
  ;; (print (notmuch-ext/get-selected-message-id))
  (->> (notmuch-ext/get-selected-message-id)
       (concat "id:")))

(defun notmuch-reply-to-newest-in-thread ()
  (interactive)
  (notmuch-mua-new-reply (notmuch-query-for-newest-message-in-thread) nil nil))

(defun notmuch-reply-all-to-newest-in-thread ()
  (interactive)
  (notmuch-mua-new-reply (notmuch-query-for-newest-message-in-thread) nil t))

(defun within-message-view (f)
  "Execute the given function within the message view (the window to the right of the notmuch-search window. For REPL development."
  (let* ((f f)
         (win (window-in-direction 'right)))
    (if win
        (util/preserve-selected-window (fn ()
                                         (select-window win)
                                         (funcall f)))
      (message "No Notmuch messages view window is visible."))))

(defun notmuch-go-to-inbox ()
  (interactive)
  (notmuch-search "folder:Inbox"))

(defun notmuch-go-to-sent ()
  (interactive)
  ;; This is a workaround. I couldn't for the life of me determine how to search for [Gmail]/.Sent mail
  (notmuch-search (concat "from:" user-mail-address)))

(defun notmuch-go-to-label-1action ()
  (interactive)
  (notmuch-search "folder:1action"))

(defun get-messages-to-move (thread-id include-special-gmail-folders)
  (let ((filter-fn (if include-special-gmail-folders
                       (fn (s) nil)
                     (fn (s) (search "[Gmail]" s)))))
    (->>
     ;;"thread:0000000000000490"
     thread-id
     list
     (append '("search" "--output=files" "--format=sexp"))
     (apply 'notmuch-call-notmuch-sexp)
     (-remove filter-fn))))

(defun get-newest-message-in-thread (thread-id)
  (->> (list "search" "--output=messages" "--format=sexp" "--sort=newest-first" thread-id)
       (apply 'notmuch-call-notmuch-sexp)
       first))

(defun archive-message ()
  "Intended to be called from the search view."
  (interactive)
  (->> (get-messages-to-move (notmuch-search-find-thread-id) nil)
       (-map 'delete-file))
  ;; Now that some files have been removed from the disk, ask notmuch to update its database.
  (notmuch-call-notmuch-process "new")
  (notmuch-refresh-this-buffer))

(defun get-notmuch-db-path ()
  (s-trim-right (util/call-process-and-check "notmuch" nil "config" "get" "database.path")))

(defun delete-thread ()
  (interactive)
  (->> (get-messages-to-move (notmuch-search-find-thread-id) t)
       (-map 'perform-delete-message))
  ;; Now that some files have been removed from the disk, ask notmuch to update its database.
  (notmuch-call-notmuch-process "new")
  (notmuch-refresh-this-buffer))

(defun perform-delete-message (file)
  "Moves an email into a .deletions directory at the root of your imap email folder."
  ;; Another way to handle deletions would be to move the message into Gmail's trash folder, but that would
  ;; require syncing the Gmail trash folder, which is something I'm not doing now with mbsync due to the
  ;; unnecessary overhead.
  (let ((deletions-folder (concat (get-notmuch-db-path) "/.deletions/")))
    (unless (file-exists-p deletions-folder)
      (make-directory deletions-folder))
    ;; (print (concat "deleting" file))
    ;; (print "exists?")
    ;; (print (file-exists-p file))
    ;; (print (concat deletions-folder (file-name-nondirectory file)))
    ;; (util/call-process-and-check "mv" file (concat deletions-folder (file-name-nondirectory file)))
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
  (let ((destination (concat (get-notmuch-db-path) "/" dest-folder "/cur/")))
    (->> (get-messages-to-move thread-id nil)
         (--map (rename-file it destination)))))

(add-hook 'notmuch-search-hook
          (fn ()
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
         (fn ()
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


;;
;; Configuration
;;

;; Don't keep message buffers around.
(setq message-kill-buffer-on-exit t)

(setq notmuch-poll-script "~/scripts/mail/fetch_email")

;; Settings for composing and sending emails.
;; http://chrisdone.com/posts/emacs-mail
;; Credentials are stored in ~/.authoinfo. See here for the format:
;; http://emacswiki.org/emacs/GnusAuthinfo
;; Note that you need to use a site-specific password if you have Google's two-factor auth enabled.
(setq user-full-name "Phil Crosby")
;; (setq user-mail-address "phil.crosby@gmail.com")
(setq user-mail-address "phil@liftoff.io")
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-stream-type 'ssl)
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 465)
;; TODO(philc): This can be nil. I have a folder here so that draft buffers are backed by disk. Is that
;; necessary?
(setq notmuch-fcc-dirs ".sentmail")

;; Sort messages newest first.
(set 'notmuch-search-oldest-first nil) ; The default is t.

;; The command used for converting plaintext markdown emails into HTML emails.
(setq notmuch-ext/markdown-to-html-command "markdown_page.rb")

(evil-set-initial-state 'notmuch-search-mode 'normal)
(evil-set-initial-state 'notmuch-tree-mode 'normal)
(evil-set-initial-state 'notmuch-hello-mode 'normal)
(evil-set-initial-state 'notmuch-show-mode 'normal)

(evil-define-key 'normal notmuch-search-mode-map
  "o" 'notmuch-search-show-thread-in-other-window
  "q" 'vimlike-quit
  "c" 'notmuch-mua-new-mail
  ;; "t" (fn () (interactive) (print (notmuch-search-get-result) ))
  "t" (fn () (interactive) (print (notmuch-search-find-thread-id t)))
  "T" (fn () (interactive) (notmuch-ext/reply))
  ;; I'm using uppercase Y here to archive, so that I can still copy text from the thread view if I want. Will
  ;; I ever do that, or should I switch this to being lowercase y?
  "Y" 'archive-message
  "D" 'delete-thread
  "r" 'notmuch-reply-to-newest-in-thread
  "R" 'notmuch-reply-all-to-newest-in-thread
  ;; I'm using these custom-scroll functions for page-up and page-down because the built-in ones in Emacs
  ;; switch to the buffer when you try to scroll up past the beginning of the window.
  "u" (fn () (interactive)
        (within-message-view (fn ()
                               (condition-case nil (scroll-down)
                                 (beginning-of-buffer (goto-char (point-min)))))))
  "d" (fn () (interactive)
        (within-message-view (fn ()
                               (condition-case nil (scroll-up)
                                 (end-of-buffer (goto-char (point-max))))))))

(evil-leader/set-key-for-mode 'notmuch-search-mode
  "gli" (fn () (interactive) (notmuch-go-to-inbox))
  "gls" (fn () (interactive) (notmuch-go-to-sent))
  "gl1" (fn () (interactive) (notmuch-go-to-label-1action))
  "t" (fn () (interactive) (print (notmuch-search-find-thread-id)))
  "r" 'notmuch-refresh-this-buffer
  "R" 'notmuch-poll-and-refresh-this-buffer
  "1" (fn () (interactive) (move-thread "1action")))

(evil-define-key 'normal notmuch-show-mode-map
  "r" 'notmuch-reply-to-newest-in-thread
  "R" 'notmuch-reply-all-to-newest-in-thread)

(evil-leader/set-key-for-mode 'notmuch-hello-mode
  "gi" (fn () (interactive) (notmuch-search "tag:inbox")))

(evil-leader/set-key-for-mode 'message-mode ; The compose window.
  "rr" 'notmuch-ext/view-message-in-browser
  "x" (fn () (interactive) (util/without-confirmation 'message-kill-buffer))
  "S" 'message-send-and-exit
  "s" 'notmuch-ext/convert-to-markdown-and-send)

;; Initialize the Emacs message mode for sending emails for use with notmuch-mode.
(defun init-message-mode-settings ()
  ;; Use markdown-mode for composing emails.
  (markdown-lite-mode)
  ;; TODO(philc): Not sure why this is needed, but without it, syntax highlighting isn't updated.
  (markdown-reload-extensions)
  (setq tab-width 2) ; This is set to 4 by message-mode.
  ;; TODO(philc): This mutates all of the evil keybindings in markdown mode. I haven't been able to figure out
  ;; how to limit these changes to the current buffer.
  (util/define-keys
   evil-normal-state-local-map
   ";rr" 'notmuch-ext/view-message-in-browser
   ";x" (fn () (interactive) (util/without-confirmation 'message-kill-buffer))
   ";S" 'message-send-and-exit ; For when you want to send a plaintext message, not markdown.
   ";s" 'notmuch-ext/convert-to-markdown-and-send))

(add-hook 'message-mode-hook 'init-message-mode-settings)
