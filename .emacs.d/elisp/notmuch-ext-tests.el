(require 'cl)
(require 'dash)
(require 's)
(require 'notmuch-ext)

(defun trim-left-lines (s)
  "Trims the leading whitespace from every line in the given string."
  (replace-regexp-in-string "^ +" "" s))

(defun test-remove-empty-envelopes ()
  (assert (equal (notmuch-ext/remove-empty-envelopes '((nil (nil (1 2 3 nil) nil))))
                 '(1 2 3 nil)) t))

(defun test-get-message-parts ()
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

(defun test-extract-message-id ()
  (lexical-let ((fixture "\nIn-Reply-To: <message123@gmail.com>\n..."))
    (assert (string= (notmuch-ext/extract-message-id fixture)
                     "message123@gmail.com") t)))

(defun notmuch-ext/run-tests ()
  (test-extract-message-id)
  (test-get-message-parts)
  (test-remove-empty-envelopes))

(notmuch-ext/run-tests)
