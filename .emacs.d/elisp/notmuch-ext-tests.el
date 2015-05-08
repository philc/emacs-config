
(require 'cl)
(require 'dash)
(require 'notmuch-ext)

(defun trim-leading-whitespace (s)
  "Trims the leading whitespace from every line in the given string."
  (replace-regexp-in-string "^ +" "" s))

(defun test-remove-empty-envelopes ()
  (assert (equal (notmuch-ext/remove-empty-envelopes '((nil (nil (1 2 3 nil) nil))))
                 '(1 2 3 nil)) t))

(defun test-strip-quoted-text ()
  (lexical-let ((fixture (trim-leading-whitespace
                          "line 1
                          > quote 1
                          line 2
                          > quote 2
                          > quote 3")))
    (assert (string= (notmuch-ext/strip-quoted-text fixture)
                     "line 1\n> quote 1\nline 2") t)))

(defun test-get-message-parts ()
  (lexical-let* ((fixture (trim-leading-whitespace
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
  (test-strip-quoted-text)
  (test-remove-empty-envelopes))

(notmuch-ext/run-tests)
