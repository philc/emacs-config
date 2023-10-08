;; These functions are minor modifications of code taken from go-mode.el
;; (version 1.3.1), which includes the following copyright notice and license
;; information:
;;
;;   Copyright 2013 The go-mode Authors. All rights reserved.
;;   Use of this source code is governed by a BSD-style
;;   license that can be found in the LICENSE file.
;;
;; The LICENSE file referred to above is available in this directory as
;; go-mode-LICENSE. See https://github.com/dominikh/go-mode.el for the full
;; source.

(require 'cl)

(defvar cljfmt-command "cljfmt"
  "The 'cljfmt' command.")

(defvar cljfmt-show-errors 'buffer
  "Where to display cljfmt error output.
It can either be displayed in its own buffer, in the echo area, or not at all.

Please note that Emacs outputs to the echo area when writing
files and will overwrite cljfmt's echo output if used from inside
a `before-save-hook'.")

(defun cljfmt--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun cljfmt--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

(defun cljfmt--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in cljfmt--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (cljfmt--goto-line (- from line-offset))
                (incf line-offset len)
                (cljfmt--delete-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in cljfmt--apply-rcs-patch")))))))))

(defun cljfmt ()
  "Format the current buffer according to the cljfmt tool."
  (interactive)
  (let ((tmpfile (make-temp-file "cljfmt" nil ".go"))
        (patchbuf (get-buffer-create "*Cljfmt patch*"))
        (errbuf (if cljfmt-show-errors (get-buffer-create "*Cljfmt Errors*")))
        (former-col (current-column))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))

    (save-restriction
      (widen)
      (if errbuf
          (with-current-buffer errbuf
            (setq buffer-read-only nil)
            (erase-buffer)))
      (with-current-buffer patchbuf
        (erase-buffer))

      (write-region nil nil tmpfile)

      ;; We're using errbuf for the mixed stdout and stderr output. This
      ;; is not an issue because cljfmt -w does not produce any stdout
      ;; output in case of success.
      (if (zerop (call-process cljfmt-command nil errbuf nil "-w" tmpfile))
          (progn
            (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                (message "Buffer is already cljfmted")
              (cljfmt--apply-rcs-patch patchbuf)
              (message "Applied cljfmt"))
            (if errbuf (cljfmt--kill-error-buffer errbuf)))
        (message "Could not apply cljfmt")
        (if errbuf (cljfmt--process-errors (buffer-file-name) tmpfile errbuf)))

      (kill-buffer patchbuf)
      (delete-file tmpfile)
      (move-to-column former-col))))

(defun cljfmt--process-errors (filename tmpfile errbuf)
  (with-current-buffer errbuf
    (if (eq cljfmt-show-errors 'echo)
        (progn
          (message "%s" (buffer-string))
          (cljfmt--kill-error-buffer errbuf))
      ;; Convert the cljfmt stderr to something understood by the compilation mode.
      (goto-char (point-min))
      (insert "cljfmt errors:\n")
      (while (search-forward-regexp (concat "^\\(" (regexp-quote tmpfile) "\\):") nil t)
        (replace-match (file-name-nondirectory filename) t t nil 1))
      (compilation-mode)
      (display-buffer errbuf))))

(defun cljfmt--kill-error-buffer (errbuf)
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (kill-buffer errbuf))))

(defun cljfmt-before-save ()
  "Add this to .emacs to run cljfmt on the current buffer when saving:
 (add-hook 'before-save-hook 'cljfmt-before-save).

Note that this will cause go-mode to get loaded the first time
you save any file, kind of defeating the point of autoloading."

  (interactive)
  (when (or (eq major-mode 'clojure-mode)
            (eq major-mode 'clojurec-mode)
            (eq major-mode 'clojurescript-mode))
    (cljfmt)))
