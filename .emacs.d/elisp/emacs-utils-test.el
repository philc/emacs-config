;;; -*- lexical-binding: t; -*-
;; Run these with `make test`.

(require 'ert)
(require 'cl-lib)
(require 'emacs-utils)

(defun emacs-utils-test/paste-with-fn (clipboard-text fn)
  "Puts CLIPBOARD-TEXT on the kill ring, calls FN in a new temp buffer to paste/transform it, and
   returns the buffer's contents afterwards."
  (kill-new clipboard-text)
  (with-temp-buffer
    (funcall fn)
    (buffer-string)))

(ert-deftest emacs-utils-test/clipboard-yank-and-remove-email-headers-strips-headers ()
  "Leading From/To/Date/Subject headers and the blank lines around them are stripped, leaving only
   the body."
  (let ((clipboard-text (concat "From: Alice <alice@example.com>\n"
                                 "To:   bob@example.com\n"
                                 "Date: Sunday, August 23 2026 at 12:57 PM PDT\n"
                                 "\n"
                                 "Subject: Some subject line\n"
                                 "\n"
                                 "This is the body.\n"
                                 "It has two lines.")))
    (should (string= (emacs-utils-test/paste-with-fn
                       clipboard-text #'util/clipboard-yank-and-remove-email-headers)
                      "This is the body.\nIt has two lines."))))

(ert-deftest emacs-utils-test/clipboard-yank-and-remove-email-headers-no-headers ()
  "Text with no leading headers is pasted through unchanged."
  (let ((clipboard-text "Just a plain paste, no headers here."))
    (should (string= (emacs-utils-test/paste-with-fn
                       clipboard-text #'util/clipboard-yank-and-remove-email-headers)
                      clipboard-text))))

(ert-deftest emacs-utils-test/clipboard-yank-and-remove-query-string-strips-query-and-hash ()
  "Everything from the first '?' or '#' in a pasted URL is removed."
  (should (string= (emacs-utils-test/paste-with-fn
                     "https://example.com/path?utm_source=foo&x=1#section"
                     #'util/clipboard-yank-and-remove-query-string)
                    "https://example.com/path")))
