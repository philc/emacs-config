; http://martintrojer.github.io/clojure/2015/02/14/clojure-and-emacs-without-cider-redux/
; https://gist.github.com/levand/b1012bb7bdb5fcc6486f
; https://github.com/clojure-emacs/cider/issues/1472
; https://github.com/clojure-emacs/cider/issues/1305

(provide 'clojure-mode-simple)
(require 'emacs-utils-personal)
(require 'inf-clojure)
(require 'thingatpt)
(require 'smartparens)

(defun setup-clojure-buffer ()
  ;; Count hyphens, etc. as word characters in lisps
  (modify-syntax-entry ?- "w" clojure-mode-syntax-table)
  ;; Comment lines using only one semi-colon instead of two.
  (setq indent-line-function 'lisp-indent-line-single-semicolon-fix)
  (setq comment-add 0)
  )

(add-hook 'clojure-mode-hook 'setup-clojure-buffer)
(add-hook 'clojure-mode-hook 'inf-clojure-minor-mode)

; "A saved function to execute later. Saved via `mark-current-buffer`"
(setq marked-function nil)

(defun clj/mark-current-buffer ()
  (interactive)
  (lexical-let ((b (current-buffer)))
    (setq marked-function (lambda ()
                            (with-current-buffer b
                              (clj/load-buffer)))))
  (message "Buffer saved for later evaluation."))

(defun mark-execute ()
  "Executes any saved (marked) evaluation functions."
  (interactive)
  (if (not marked-function)
      (message "You haven't yet marked (saved) anything to execute.")
    (progn
      (message "Executing mark.")
      (funcall marked-function))))

;; Don't ask confirmation for closing any open REPL subprocesses when exiting Emacs.
;; http://stackoverflow.com/q/2706527/46237
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(defun init-comint-mode-settings ()
  (define-key comint-mode-map (kbd "C-d") nil)
  ;; Comint mode uses a local keymap which overrides Evil's keymaps.
  ;; See here for the bindings it creates: http://www.cs.indiana.edu/pub/scheme-repository/utl/comint.el
  (local-set-key (kbd "C-d") 'evil-scroll-down))

(add-hook 'comint-mode-hook 'init-comint-mode-settings)

(defun clj/show-doc-for-symbol-at-point ()
  (interactive)
  (-if-let (s (thing-at-point 'symbol))
      ;; inf-clojure-show-var-documentation will interactively prompt you for the variable.
      (inf-clojure-show-var-documentation s)
    (message "There's no symbol under the cursor to look up documentation for.")))

(defun clj/quit ()
  (interactive)
  (-?> (ignore-errors (inf-clojure-proc)) delete-process)
  (-?> (get-buffer "*inf-clojure*") kill-buffer))

(defun clj/load-file (file-name)
  "Load a Clojure file FILE-NAME into the inferior Clojure process."
  (comint-send-string (inf-clojure-proc)
                      (format inf-clojure-load-command file-name)))

;; TODO(philc): Different from eval-buffer -- doesn't eval each statement individually and send the output to the REPL.
(defun clj/load-buffer ()
  (interactive)
  (util/save-buffer-if-dirty)
  (clj/load-file (buffer-file-name)))

(defun clj/on-inf-clojure-buffer-created ()
  "Perform any setup you desire to newly created inf-clojure buffers. This exists because the inf clojure
   buffer has no major mode, so it's hard to customize."
  ;; (print "setting up")
  ;; (print inf-clojure-buffer)
  (with-current-buffer inf-clojure-buffer
    ;; (print "in buffer")
    (evil-normal-state)
    )
  )

(defun clj/restart-repl ()
  "Starts the REPL if it's not running; otherwise resetarts it by killing and recreating the REPL buffer."
  (interactive)
  (save-excursion
    (clj/quit)
    (message "Starting REPL...")
    (clj/show-repl)
    (clj/on-inf-clojure-buffer-created)
    ))

(defun clj/show-repl ()
  "Shows the REPL in a popup window but does not switch to it."
  (interactive)
  (util/preserve-selected-window
   (lambda ()
     (inf-clojure-switch-to-repl nil)
     ; Scroll to the bottom of the window.
     (goto-char (point-max)))))

(defun clj/in-repl-buffer (fn)
  (util/preserve-selected-window
   (lambda ()
     (inf-clojure-switch-to-repl nil)
     (funcall fn))))

(defun clj/clear-repl ()
  (interactive)
  ; inf-clojure-clear-repl-buffer assumes the REPL buffer is currently focused.
  (clj/in-repl-buffer 'inf-clojure-clear-repl-buffer))

(defun clj/ns-of-buffer (&optional buffer)
  "Returns the namespace of the clojure file (as defined in the `(ns)` form) or nil if none could be found."
  (-let* ((b (or buffer (current-buffer)))
          (contents (with-current-buffer b
                      (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-match "(ns \\(.+?\\)[ \n)]" contents)
      (match-string 1 contents))))

(defun clj/eval-in-current-ns (str)
  (-let* ((ns (or (clj/ns-of-buffer) "user"))
          (s (concat "(binding [*ns* '" ns "] " str ")")))
    ;; TODO(philc): Only switch to `ns` if it's not already the current ns. This will save an extra
    ;; nil from being printed to the REPL.
    (inf-clojure-eval-string (concat "(do (clojure.core/in-ns '" ns ") nil)"))
    (inf-clojure-eval-string str)))

(defun clj/get-last-sexp-str ()
  (buffer-substring-no-properties (save-excursion (backward-sexp) (point))
                                  (point)))

; (progn
;   (unwind-protect
;       (let* ((proc (inf-clojure-proc))
;              (f (process-filter proc))
;              (kept "")
;              )
;         (progn
;           (set-process-filter proc (lambda (proc string)
;                                      (print string)
;                                      (setq kept (concat kept string))))
;           (process-send-string proc "(+ 1 3 4)\n")
;           (while (and (not (string-match inf-clojure-prompt kept))
;                       (accept-process-output proc 1)))
;           ; (accept-process-output proc 1)
;           (print kept))
;         ; (let ((eldoc-snippet (format inf-clojure-arglist-command fn)))
;         ;   (process-send-string proc eldoc-snippet)
;         ;   (while (and (not (string-match inf-clojure-prompt kept))
;         ;               (accept-process-output proc 2)))
;         ;   ; some nasty #_=> garbage appears in the output
;         ;   (setq eldoc (and (string-match "(.+)" kept) (match-string 0 kept)))
;         ;   ))
;         (set-process-filter proc f)))
;   nil)


(defun clj/eval-sexp ()
  (interactive)
  (clj/eval-in-current-ns (thing-at-point 'list t)))

(defun clj/eval-defun ()
  (interactive)
  (clj/eval-in-current-ns (thing-at-point 'defun t)))
  ;; (let ((str (buffer-substring-no-properties (save-excursion
  ;;                                              (beginning-of-defun)
  ;;                                              (proint))
  ;;                                              (let ((end (point))
  ;;                                                    (case-fold-search t))
  ;;                                                (beginning-of-defun)))

  ;;             (save-excursion
  ;;                                              (end-of-defun))
  ;;                                            )))
  ;; (print str)))

(defun inf-clojure-eval-defun (&optional and-go)
  "Send the current defun to the inferior Clojure process.
Prefix argument AND-GO means switch to the Clojure buffer afterwards."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (let ((end (point)) (case-fold-search t))
      (beginning-of-defun)
      (inf-clojure-eval-region (point) end)))
  (if and-go (inf-clojure-switch-to-repl t)))


;; (clj/eval-in-current-ns str)))
;;     (inf-clojure-eval-region (point) end)))
;; (clj/eval-in-current-ns (thing-at-point 'list t)))

; "mf" (lambda () (interactive) (with-nrepl-connection-of-current-buffer 'my-cider-mark-recent-form))
; "mb" 'my-cider-mark-current-buffer
; ;; Shortcuts for printing the results of expressions. These eval functions take a second param which prints
; ;; result of the expression.
; "eps" (lambda () (interactive) (with-nrepl-connection-of-current-buffer
;                                 (lambda () (my-cider-eval-current-sexp t))))
; "epx" (lambda () (interactive) (with-nrepl-connection-of-current-buffer
;                                 (lambda () (my-cider-eval-defun-at-point t)))))

(defun lisp-indent-line-single-semicolon-fix (&optional whole-exp)
  "Identical to the built-in function lisp-indent-line,
but doesn't treat single semicolons as right-hand-side comments."
  (interactive "P")
  (let ((indent (calculate-lisp-indent)) shift-amt end
        (pos (- (point-max) (point)))
        (beg (progn (beginning-of-line) (point))))
    (skip-chars-forward " \t")
    (if (or (null indent) (looking-at "\\s<\\s<\\s<"))
        ;; Don't alter indentation of a ;;; comment line
        ;; or a line that starts in a string.
        ;; FIXME: inconsistency: comment-indent moves ;;; to column 0.
        (goto-char (- (point-max) pos))
      (if (listp indent) (setq indent (car indent)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
          nil
        (delete-region beg (point))
        (indent-to indent)))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))))

; (evil-define-key 'normal clojure-mode-map "gf" 'cider-jump-to-var)
; (evil-define-key 'normal clojure-mode-map "gb" 'cider-jump-back)
(evil-define-key 'normal clojure-mode-map "K" 'clj/show-doc-for-symbol-at-point)
(evil-define-key 'normal clojure-mode-map "(" 'sp-backward-up-sexp)
(evil-define-key 'normal clojure-mode-map ")" 'sp-forward-sexp)
(evil-define-key 'normal clojure-mode-map (kbd "C-S-H") 'shift-sexp-backward)
(evil-define-key 'normal clojure-mode-map (kbd "C-S-L") 'shift-sexp-forward)
(evil-define-key 'normal clojure-mode-map (kbd "C-S-K") 'shift-sexp-backward)
(evil-define-key 'normal clojure-mode-map (kbd "C-S-J") 'shift-sexp-forward)

(evil-leader/set-key-for-mode 'clojure-mode
  ; "eap" (lambda () (interactive) (with-nrepl-connection-of-current-buffer 'cider-eval-paragraph))
  "ee" 'clj/show-repl
  "ek" 'clj/clear-repl
  "en" 'clj/restart-repl
  "es" 'clj/eval-sexp
  "mb" 'clj/mark-current-buffer
  ; "es" 'inf-clojure-eval-last-sexp
  ;; "ex" 'inf-clojure-eval-defun
  "ex" 'clj/eval-defun
  "eb" 'clj/load-buffer)

;; Make it possible to eval any marked buffer from any window -- a clojure window need not be focused.
(evil-leader/set-key
  "me" 'mark-execute)

;; Clojure indentation rules
(with-eval-after-load "clojure-mode"
  (define-clojure-indent
    (send-off 1) (cli 1) (go-loop 1)                                  ; Core
    (ANY 2) (GET 2) (POST 2) (PUT 2) (PATCH 2) (DELETE 2) (context 2) ; Compojure
    (OPTIONS 2)
    (select 1) (insert 1) (update 1) (where 1) (set-fields 1)         ; Korma
    (values 1) (delete 1) (upsert 1) (subselect 1)
    (clone-for 1)                                                     ; Enlive
    (up 1) (down 1) (alter 1) (table 1) (create 1)                    ; Lobos
    (with-eligible-values 1) (when-eligible 1) (check 4)              ; Personal
    (url-of-form 1)                                                   ; Personal
    ))
