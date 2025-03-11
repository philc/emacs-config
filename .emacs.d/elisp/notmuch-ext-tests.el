(require 'cl-lib)
(require 'dash)
(require 'dash-functional)
(require 's)
(require 'notmuch-ext)

(defun trim-left-lines (s)
  "Trims the leading whitespace from every line in the given string."
  (replace-regexp-in-string "^ +" "" s))

(defun test-remove-empty-envelopes ()
  (assert (equal (notmuch-ext/remove-empty-envelopes '((nil (nil (:a 1) nil))))
                 '(:a 1)) t)
  (assert (equal (notmuch-ext/remove-empty-envelopes '((:a 1) (nil nil)))
                 '(:a 1)) t))

(defun test-get-message-parts-for-replies ()
  (lexical-let* ((fixture (trim-left-lines
                           "From: a@a.com
                            --text follows this line--
                            Reply line 1

                            Reply line 2
                            Someone <a@b.com> writes:
                            > a quoted message."))
                 (parts (notmuch-ext/get-message-parts fixture)))
    (assert (string= "From: a@a.com" (plist-get parts :header)) t)
    (assert (string= "Reply line 1\n\nReply line 2" (plist-get parts :reply-text)) t)
    (assert (string= "> a quoted message." (plist-get parts :quoted-text)) t)))

(defun test-get-message-parts-for-non-replies ()
  (lexical-let* ((fixture (trim-left-lines
                           "From: a@a.com
                            --text follows this line--
                            New message"))
                 (parts (notmuch-ext/get-message-parts fixture)))
    (assert (not (plist-get parts :attribution-line)))
    (assert (not (plist-get parts :quoted-text)))
    (assert (string= "New message" (plist-get parts :reply-text)))))

(defun test-extract-message-id ()
  (lexical-let ((fixture "\nIn-Reply-To: <message123@gmail.com>\n..."))
    (assert (string= (notmuch-ext/extract-message-id fixture)
                     "message123@gmail.com") t)))

(defun test-build-response-from-markdown-for-composing-new-messages ()
  (lexical-let* ((fixture (trim-left-lines
                           "From: a@a.com
                            --text follows this line--
                            New message"))
                 (parts (notmuch-ext/build-response-from-markdown fixture)))
    (assert (string= "<p>New message</p>" (plist-get parts :html)))
    (assert (string= "New message" (plist-get parts :plaintext)))))

(defun notmuch-ext/run-tests ()
  (test-extract-message-id)
  (test-get-message-parts-for-replies)
  (test-get-message-parts-for-non-replies)
  (test-remove-empty-envelopes)
  (test-build-response-from-markdown-for-composing-new-messages))

(notmuch-ext/run-tests)
