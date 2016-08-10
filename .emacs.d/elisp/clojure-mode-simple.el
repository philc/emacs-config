; http://martintrojer.github.io/clojure/2015/02/14/clojure-and-emacs-without-cider-redux/
; https://gist.github.com/levand/b1012bb7bdb5fcc6486f
; https://github.com/clojure-emacs/cider/issues/1472
; https://github.com/clojure-emacs/cider/issues/1305

(provide 'clojure-mode-simple)
(require 'emacs-utils-personal)
(require 'inf-clojure)
(require 'thingatpt)
(require 'dash)
(require 's)
(require 'ht)
(require 'smartparens)

;; Use clojure.pprint when printing data structures to the REPL.
(setq use-pprint t)

(defun setup-clojure-buffer ()
  ;; Count hyphens, etc. as word characters in lisps
  (modify-syntax-entry ?- "w" clojure-mode-syntax-table)
  ;; Comment lines using only one semi-colon instead of two.
  (setq indent-line-function 'lisp-indent-line-single-semicolon-fix)
  (setq comment-add 0))

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

(defun clj/open-clojure-docs-for-symbol-at-point ()
  (interactive)
  (-if-let (s (thing-at-point 'symbol))
      (let ((clojure-docs-url
             (concat "http://www.google.com/search?btnI=Im+Feeling+Lucky&q=site:clojuredocs.org+" s)))
        (util/open-in-browser clojure-docs-url))
    (message "There's no symbol under the cursor to look up documentation for.")))

(defvar clj/commands
  (->>
   ;; This returns output of the form: "/home/username/shared_lib/core.clj,123"
   ;; Inspiration taken from  https://github.com/clojure/clojure/blob/master/src/clj/clojure/repl.clj
   ;; Note that the file metadata for a symbol can be rooted if it's already been loaded in the REPL.
   (ht ("get-src-file"
        "(let [m (-> '%s resolve meta)
               path (:file m)
               full-path (when (not= (subs path 0 1) \"/\")
                           (->> path (.getResource (RT/baseLoader)) .getPath))]
          (str (or full-path path) \":\" (:line m)))"))
   ;; NOTE(philc): we strip newlines from all commands we send to the REPL, because this makes the output
   ;; coming back from the REPL process have multiple #_=> prompts embedded in it. We may be able to reliably
   ;; strip those from the output, but for now I'm just collapsing all clojure code snippets to one line.
   (ht-map (lambda (k v) (list k (s-collapse-whitespace v))))
   -flatten
   ht<-plist))

(defvar clj/buffers-before-jump '())

;; TODO(philc):
;; * Auto-eval the NS of the current file when invoking jump-to-var and it hasn't yet been eval'd.
;; * Handle symbols which have no definition (currently throws a runtime exception).
(defun clj/jump-to-var ()
  (interactive)
  (-if-let (s (-> (thing-at-point 'symbol) substring-no-properties))
      (let* ((output (-> (format (ht-get clj/commands "get-src-file") s)
                         clj/wrap-sexp-in-current-ns
                         clj/eval-and-capture-output
                         clj/remove-surrounding-quotes)))
        ;; `output` is of the form "/home/USER/shared_lib/core.clj:123"
        ;; If the function is in a JAR, output will be e.g. file:/home/USER/the-jar.jar!/path/in/jar
        (message output)
        (if (s-contains? "jar!" output)
            (message (format "The var %s is defined in a JAR. Viewing source inside of a JAR is unimplemented." s))
          (let ((file (->> output (s-split ":") (nth 0)))
                (line (->> output (s-split ":") (nth 1) string-to-number)))
            (message (buffer-file-name))
            (message file)
            (when (not (string= (buffer-file-name)
                                file))
              (push (list (current-buffer) (line-number-at-pos)) clj/buffers-before-jump)
              (find-file file))
            (goto-line line)
            (recenter 0))))
    (message "No symbol is under the cursor.")))

(defun clj/jump-back ()
  (interactive)
  (when (> (length clj/buffers-before-jump) 0)
    (let* ((item (pop clj/buffers-before-jump))
           (buf (nth 0 item))
           (line (nth 1 item)))
      ;; (message item)
      (set-window-buffer (selected-window) buf)
      (goto-line line)
      (recenter 0))))

(defun clj/chomp-last-line (s)
  "Removes the last line from the string s."
  (-?>> s s-lines (-drop-last 1) (s-join "\n")))

(defun clj/remove-surrounding-quotes (s)
  (->> s (s-chop-prefix "\"") (s-chop-suffix "\"")))

(defun clj/eval-and-capture-output (command)
  (interactive)
  (message "Evaluating:")
  (message command)
  ;; Taken from inf-clojure-show-arglist.
  (let* ((command (concat command "\n"))
         (proc (inf-clojure-proc))
         (comint-filt (process-filter proc)))
    (set-process-filter proc (lambda (proc string) (setq kept (concat kept string))))
    (let* ((result (unwind-protect
                       (let ((kept ""))
                         (process-send-string proc command)
                         (while (and (not (string-match inf-clojure-prompt kept))
                                     (accept-process-output proc 2)))
                         ;; TODO(philc): I couldn't get this code working.
                         ;; some nasty #_=> garbage appears in the output
                         ;; (setq result (and (string-match "(.+)" kept) (match-string 0 kept))))
                         kept)
                     (set-process-filter proc comint-filt)))
           ;; The last line of output is a REPL prompt. Remove it.
           (result (clj/chomp-last-line result)))

      (when result
        (message "%s" result)))))

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

(defun clj/wrap-sexp-in-current-ns (str)
  (-let* ((ns (or (clj/ns-of-buffer) "user"))
          (s (concat "(binding [*ns* '" ns "] " str ")")))
    (format "(do (clojure.core/in-ns '%s) %s)" ns str)))

(defun clj/eval-in-current-ns (str)
  (let ((str (if use-pprint
                 (concat "(do (println \"\n\") (clojure.pprint/pprint " str "))")
               str)))
    (inf-clojure-eval-string (clj/wrap-sexp-in-current-ns str))))

(defun clj/pretty-print-last-stack-trace ()
  (interactive)
  ;; TODO(philc): Consider embedding this helper file as a string in this .el."
  (let ((helpers-file "/Users/phil/.lein/repl_helpers.clj")) ; TODO(philc): Make this path relative/configurable.
    (-> (concat "(do (eval (read-string (slurp \"" helpers-file "\"))) (my-pst))")
        (inf-clojure-eval-string))))

(defun clj/get-last-sexp-str ()
  (buffer-substring-no-properties (save-excursion (backward-sexp) (point))
                                  (point)))

(defun clj/eval-sexp ()
  (interactive)
  (clj/eval-in-current-ns (thing-at-point 'list t)))

(defun clj/eval-defun ()
  (interactive)
  (clj/eval-in-current-ns (thing-at-point 'defun t)))

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

; (evil-define-key 'normal clojure-mode-map "gb" 'cider-jump-back)
(evil-define-key 'normal clojure-mode-map "K" 'clj/show-doc-for-symbol-at-point)
(evil-define-key 'normal clojure-mode-map "gf" 'clj/jump-to-var)
(evil-define-key 'normal clojure-mode-map "gb" 'clj/jump-back)
;; (evil-define-key 'normal clojure-mode-map "gf" 'clj/eval-and-capture-output)
(evil-define-key 'normal clojure-mode-map (kbd "A-k") 'clj/open-clojure-docs-for-symbol-at-point)
(evil-define-key 'normal clojure-mode-map "(" 'sp-backward-up-sexp)
(evil-define-key 'normal clojure-mode-map ")" 'sp-forward-sexp)
(evil-define-key 'normal clojure-mode-map (kbd "C-S-H") 'shift-sexp-backward)
(evil-define-key 'normal clojure-mode-map (kbd "C-S-L") 'shift-sexp-forward)
(evil-define-key 'normal clojure-mode-map (kbd "C-S-K") 'shift-sexp-backward)
(evil-define-key 'normal clojure-mode-map (kbd "C-S-J") 'shift-sexp-forward)

(evil-leader/set-key-for-mode 'clojure-mode
  ;; "eap" (lambda () (interactive) (with-nrepl-connection-of-current-buffer 'cider-eval-paragraph))
  "ee" 'clj/show-repl
  "ek" 'clj/clear-repl
  "en" 'clj/restart-repl
  "es" 'clj/eval-sexp
  "mb" 'clj/mark-current-buffer
  "eP" 'clj/pretty-print-last-stack-trace
  ;; "es" 'inf-clojure-eval-last-sexp
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


;;
;; cljfmt -- automatic formatting of Clojure code. This configuration is Liftoff-specific.
;;

(load "/Users/phil/src/liftoff/exp/emacs/cljfmt.el")

;; Note that `cljfmt-before-save` triggers this save-hook for some reason, so we lock on clj-in-progress to
;; to protect from infinite recurision.
(setq cljfmt-in-progress nil)
(defun cljfmt-before-save-mutually-exclusive ()
  (interactive)
  (when (and (eq major-mode 'clojure-mode)
             (not cljfmt-in-progress))
    (setq cljfmt-in-progress 't)
    (cljfmt)
    (setq cljfmt-in-progress nil)))

(add-hook 'before-save-hook 'cljfmt-before-save-mutually-exclusive nil)
