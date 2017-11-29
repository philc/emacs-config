;; My experiment ground for a product and simple-to-debug clojure mode.
;; I think where I'll eventually end up is using nrepl, so that I can keep track of asynchronous output from
;; commands and cleanly kill expensive commands.
;;
;; Frustrations with cider which caused me to start this experiment:
;;
;; http://martintrojer.github.io/clojure/2015/02/14/clojure-and-emacs-without-cider-redux/
;; https://gist.github.com/levand/b1012bb7bdb5fcc6486f
;; https://github.com/clojure-emacs/cider/issues/1472
;; https://github.com/clojure-emacs/cider/issues/1305

(provide 'clojure-mode-simple)
(require 'emacs-utils)
(require 'inf-clojure)
(require 'thingatpt)
(require 'dash)
(require 's)
(require 'ht)
(require 'smartparens)

;; Use clojure.pprint when printing data structures to the REPL.
(setq use-pprint t)

;;
;; Buffer setup
;;

(setq clojure-simple-output-prefix "clojure-simple::")

(defun setup-clojure-buffer ()
  ;; Count hyphens, etc. as word characters in lisps
  (modify-syntax-entry ?- "w" clojure-mode-syntax-table)
  ;; Comment lines using only one semi-colon instead of two.
  (setq indent-line-function 'lisp-indent-line-single-semicolon-fix)
  (setq comment-add 0))

(defun remove-junk-from-inf-clojure-output (output-str)
  "Strips nested prompt symbols and the vast amounts of white space that inf-clojure is returning back."
  (let ((str (-?>> output-str
                   s-trim-left
                   (replace-regexp-in-string inf-clojure-prompt "")
                   (replace-regexp-in-string "#_=>" "")
                   s-trim-left))) ; Remove sub-prompts.
    (if (string= "" (s-trim str))
        ""
      str)))

(defun init-repl-settings ()
  "Customize the way the REPL buffer works."
  ;; Normally we maintain a margin of N lines between the cursor and the edge of the window, but in the REPL
  ;; buffer, the cursor should always be at the bottom of the window.
  (setq-local scroll-margin 0)
  (setq-local scroll-conservatively 0)
  (setq-local scroll-step 1))

;; This redefines the inf-clojure-preoutput-filter defined in inf-clojure.
;; TODO(philc): Can I get rid of this?
(defun inf-clojure-preoutput-filter (str)
  "Preprocess the output (`str`) from the clojure process, removing whitespace etc.."
  ;; NOTE(philc): When debugging this function, you can't write to stdout using print, because it will mess up
  ;; the process filter... until I find a better way, my strategy for debugging this is to store intermediate
  ;; values into variables for later inspection outside of this function.
  (let ((str (remove-junk-from-inf-clojure-output str)))
    (setq output-debug str)
    (clj/append-to-repl-buffer str)
    str))

(defun init-comint-mode-settings ()
  (define-key comint-mode-map (kbd "C-d") nil)
  ;; Comint mode uses a local keymap which overrides Evil's keymaps.
  ;; See here for the bindings it creates: http://www.cs.indiana.edu/pub/scheme-repository/utl/comint.el
  (local-set-key (kbd "C-d") 'evil-scroll-down))

(add-hook 'clojure-mode-hook 'setup-clojure-buffer)
(add-hook 'clojure-mode-hook 'inf-clojure-minor-mode)
(add-hook 'inf-clojure-mode-hook 'init-repl-settings)
(add-hook 'comint-mode-hook 'init-comint-mode-settings)

;; Don't ask confirmation for closing any open REPL subprocesses when exiting Emacs.
;; http://stackoverflow.com/q/2706527/46237
(add-hook 'comint-exec-hook
          (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

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

;;
;; General commands
;;

(defvar clj/buffers-before-jump '())

;; NOTE(philc): in all of our snippets, we strip newlines from all commands we send to the REPL, because this
;; makes the output coming back from the REPL process have multiple #_=> prompts embedded in it. We may be
;; able to reliably strip those from the output, but for now I'm just collapsing all clojure code snippets to
;; one line.
(defun clj/snippet-get-source-file (symbol-name)
  "Returns output of the form: /home/username/the_project/core.clj:123"
  ;; Inspiration taken from  https://github.com/clojure/clojure/blob/master/src/clj/clojure/repl.clj
  ;; Note that the file metadata for a symbol can be rooted if it's already been loaded by the REPL.
  (-> "(when-let [m (-> '%s resolve meta)]
         (let [path (:file m)
               full-path (when (not= (subs path 0 1) \"/\")
                           (->> path (.getResource (clojure.lang.RT/baseLoader)) .getPath))]
           (str (or full-path path) \":\" (:line m))))"
      s-collapse-whitespace
      (format symbol-name)))

(defun clj/remove-surrounding-quotes (s)
  (->> s (s-chop-prefix "\"") (s-chop-suffix "\"")))

;; TODO(philc):
;; * Auto-eval the NS of the current file when invoking jump-to-var and it hasn't yet been eval'd.
(defun clj/jump-to-var (&optional symbol-name)
  (interactive)
  (let ((s (or symbol-name
               (-> (thing-at-point 'symbol) substring-no-properties))))
    (if (not s)
        (message "No symbol is under the cursor.")
      (let* ((output (-> (clj/snippet-get-source-file s)
                         clj/wrap-sexp-in-current-ns
                         (clj/eval-and-capture-output t)
                         clj/remove-surrounding-quotes)))
        ;; `output` is of the form "/home/USER/shared_lib/core.clj:123"
        ;; If the function is in a JAR, output will be e.g. file:/home/USER/the-jar.jar!/path/in/jar
        (message output)
        (cond
         ((string= output "nil")
          (message (format "Could find a definition for the symbol %s" s)))
         ((s-contains? "jar!" output)
          (message (format "The var %s is defined in a JAR. Viewing source inside of a JAR is unimplemented." s)))
         (t
          (let ((file (->> output (s-split ":") (nth 0)))
                (line (->> output (s-split ":") (nth 1) string-to-number)))
            ;; Save which buffer we are jumping from, so clj/jump-back can take us back.
            (when (not (string= (buffer-file-name)
                                file))
              (push (list (current-buffer) (line-number-at-pos) (current-column))
                    clj/buffers-before-jump)
              (find-file file))
            (goto-line line)
            (recenter))))))))

(defun clj/jump-back ()
  (interactive)
  (when (> (length clj/buffers-before-jump) 0)
    (let* ((item (pop clj/buffers-before-jump))
           (buf (nth 0 item))
           (line (nth 1 item))
           (col (nth 2 item)))
      ;; (message item)
      (set-window-buffer (selected-window) buf)
      (with-selected-window (selected-window)
        (goto-line line)
        (move-to-column col)
        (recenter)))))

(defun clj/restart-repl ()
  "Starts the REPL if it's not running; otherwise resetarts it by killing and recreating the REPL buffer."
  (interactive)
  (save-excursion
    (util/preserve-selected-window
     (lambda ()
       (clj/clear-repl)
       (clj/quit)
       (message "Starting REPL...")
       (clj/show-repl)
       (run-clojure inf-clojure-program)
       ;; Hide the inf-clojure buffer which pops up.
       (with-selected-window (get-buffer-window inf-clojure-buffer t)
         (quit-window))))))

(defun clj/quit ()
  (interactive)
  (-?> (ignore-errors (inf-clojure-proc)) delete-process)
  (-?> (get-buffer "*inf-clojure*") kill-buffer))

(defun clj/scroll-to-repl-buffer-end ()
  (let ((w (get-buffer-window (clj/repl-buffer) t)))
    (when w
      (with-selected-window w
        (View-scroll-to-buffer-end)))))

(defun clj/append-to-repl-buffer (str)
  (clj/in-repl-buffer
   (lambda ()
     (save-excursion
       (goto-char (point-max))
       (insert str))))
  (clj/scroll-to-repl-buffer-end))

(defun clj/show-repl ()
  "Shows the REPL in a popup window but does not switch to it."
  (interactive)
  (util/preserve-selected-window
   (lambda ()
     (pop-to-buffer (clj/repl-buffer) nil t)
     (clj/scroll-to-repl-buffer-end))))

(defun clj/clear-repl ()
  (interactive)
  (clj/in-repl-buffer (lambda () (erase-buffer))))


;;
;; Evaluation functions
;;

(defun clj/chomp-last-line (s)
  "Removes the last line from the string s."
  (-?>> s s-lines (-drop-last 1) (s-join "\n")))

(defun clj/eval-and-capture-output (command &optional silence)
  "This will run the command, progressively output its stdout to our clj buffer as it comes in, and return the
   full output string once the command has finished executing."
  (interactive)
  ;; Taken from inf-clojure-show-arglist.
  (let* ((command (concat command "\n"))
         (proc (inf-clojure-proc))
         (comint-filt (process-filter proc))
         (kept "")
         (last-string "")
         (process-fn (lambda (proc string)
                       (setq last-string string)
                       (let ((s (remove-junk-from-inf-clojure-output string)))
                         ;; TODO(philc): describe the asynchronous nature here
                         (when (not silence)
                           (clj/append-to-repl-buffer s))
                         (setq kept (concat kept s))))))
    (progn (print ">>>> command") (prin1 command t))
    (set-process-filter proc process-fn)
    (let* ((result (unwind-protect
                       (progn
                         (process-send-string proc command)
                         (while (and (not (string-match inf-clojure-prompt last-string))
                                     (accept-process-output proc 0.5)))
                         kept)
                     (set-process-filter proc comint-filt)))
           ;; The last line of output is a REPL prompt. Remove it.
           (result (clj/chomp-last-line result))))
    kept))

(defun clj/load-file (file-name)
  "Load a Clojure file FILE-NAME into the inferior Clojure process."
  (clj/eval-str (format "(clojure.core/load-file \"%s\")\n" file-name)))

;; TODO(philc): Different from eval-buffer -- doesn't eval each statement individually and send the output to the REPL.
(defun clj/load-buffer ()
  (interactive)
  (util/save-buffer-if-dirty)
  (clj/load-file (buffer-file-name)))

(defun clj/ns-of-buffer (&optional buffer)
  "Returns the namespace of the clojure file (as defined in the `(ns)` form) or nil if none could be found."
  (-let* ((b (or buffer (current-buffer)))
          (contents (with-current-buffer b
                      (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-match "(ns \\(.+?\\)[ \n)]" contents)
      (match-string 1 contents))))

(defun clj/wrap-sexp-in-current-ns (str)
  (-let* ((ns (or (clj/ns-of-buffer) "user"))
          (s (format "(binding [*ns* '%s] %s)" ns str))
          (require-statement (format "(clojure.core/require '%s)" ns)))
    ;; We first require the namespace before switching to it in case it hasn't yet been loaded.
    (format "(do %s (clojure.core/in-ns '%s) %s)" require-statement ns str)))

(defun clj/print-separator ()
  ;; This marker here is to separate the output from each command.
  (clj/append-to-repl-buffer "\n------\n"))

(defun clj/eval-str (str)
  (clj/print-separator)
  (inf-clojure-eval-string str))

(defun clj/wrap-sexp (str pprint wrap-ns &optional dont-record-exceptions)
  "Wraps the given sexp with pretty printing, execution in the current file's namespace, and sets
   the `_last-exception` var in the clojure process if this statement causes an exception.
   - dont-record-exceptions: don't modify _last-exception as a result of evaluating `str`."
  (when pprint
    (setq str (concat "(do (clojure.pprint/pprint " str "))")))
  (when (not dont-record-exceptions)
    (setq str (format "(do (intern 'user '_last-exception nil)
                           (try %s
                             (catch Exception e
                               (intern 'user '_last-exception e)
                               (println \"got exception\")
                               (println \"%sexception-occurred\")
                               (throw e)
                             )))"
                      str
                      clojure-simple-output-prefix)))
  (when wrap-ns
    (setq str (clj/wrap-sexp-in-current-ns str)))
  str)
;; (catch RuntimeException
                             ;; (catch CompilerException e (intern 'user '_last-exception e) (println \"BB!\") (throw e))

(defun clj/wrap-with-repl-helpers-file (str)
  ;; TODO(philc): Make this path relative/configurable.
  (let ((helpers-file "/Users/phil/.emacs.d/elisp/clojure_repl_helpers.clj"))
    (format "(do (eval (read-string (slurp \"%s\"))) %s)" helpers-file str)))

(defun clj/pretty-print-last-stack-trace ()
  (interactive)
  (clj/eval-str (clj/wrap-with-repl-helpers-file "(my-pst)")))

(setq clj/backtrace-cursor-linenum nil)

(defun clj/print-any-exceptions ()
  "If an exception was caused by the last evaled Clojure statement, print just its backtrace."
  (let ((exception-str (-> "user/_last-exception" (clj/eval-and-capture-output t) s-trim)))
    ;; NOTE(philc): I'm not sure why, but for the first command in a new REPL, _last-exception
    ;; is returned by inf-clojure as an empty string.
    (progn (print ">>>> exception-str") (prin1 exception-str t))
    (when (not (or (string= "nil" exception-str)
                   (string= "" exception-str)))
      ;; Reset the exception navigation cursor.
      (setq clj/backtrace-cursor-linenum nil)
      ;; We're omitting the message on the exception because the message (but not the stacktrace) has
      ;; already been printed to the REPL by nREPL. It is not possible to disable this exception output
      ;; from nREPL, to as a workaround, we just print the backtrace.
      ;; See https://dev.clojure.org/jira/browse/CLJ-2040
      (-> "(my-pst user/_last-exception true)" clj/wrap-with-repl-helpers-file inf-clojure-eval-string))))

(defun clj/file-of-backtrace-line (line)
  "`line` should be of the form:
    'the-ns.class/the-fn-name (the-file-name.clj:213)'.
    Returns a list containing file name and line number."
  (string-match ".+ (\\(.+\\))" line)
  (let* ((tuple-str (match-string 1 line))
         (tuple (s-split ":" tuple-str)))
    (list (first tuple) (string-to-number (second tuple)))))

(defun clj/update-backtrace-cursor (direction)
  (let* ((exception-str (-> "(my-pst user/_last-exception true)"
                            clj/wrap-with-repl-helpers-file
                            (clj/eval-and-capture-output t)))
         (lines (->> exception-str
                     s-trim
                     (s-split "\n")
                     (-drop-last 1) ; the last line is the string "nil"
                     (-map 's-trim))))
    (if (not clj/backtrace-cursor-linenum)
        ;; Find the first line in the backtrace which corresponds to the current buffer, and start there.
        (let* ((index (-find-index (lambda (x) (string= (buffer-name) (first (clj/file-of-backtrace-line x))))
                                   lines)))
          (if index
              (setq clj/backtrace-cursor-linenum index)
            (message "No stack frame found which corresponds to the current buffer.")))
      (setq clj/backtrace-cursor-linenum (+ clj/backtrace-cursor-linenum direction)))
    (let* ((line (nth clj/backtrace-cursor-linenum lines))
           ;; This removes any non-top-level functions from the symbol name in the exception outout.
           ;; So "the.ns/fn/inner-fn" becomes "the.ns/fn".
           (clj-symbol (->> line (s-split " ") first (s-split "/") (-take 2) (s-join "/")))
           (file-and-linenum (clj/file-of-backtrace-line line))
           (linenum (second file-and-linenum)))
      ;; Assume this is an anonymous eval form, like "ns/eval1234", which we can't easily resolve.
      (if (s-contains? "/eval" clj-symbol)
          (message "Jumping to anonymous top-level forms like %s isn't implemented." clj-symbol)
        (progn
          (clj/jump-to-var clj-symbol)
          (goto-line linenum)
          (recenter) ; Put the line in the center of the screen: for errors, you need context above & below.
          (message "Jumped to: %s:%s" (first file-and-linenum) linenum))))))

(defun clj/goto-next-exception () (interactive) (clj/update-backtrace-cursor 1))

(defun clj/goto-prev-exception () (interactive) (clj/update-backtrace-cursor -1))

(defun clj/eval-in-current-ns (str)
  (clj/print-separator)
  (clj/eval-and-capture-output (clj/wrap-sexp str t t nil))
  ;; (clj/print-any-exceptions))
  )

(defun clj/load-file (file-name)
  "Load and run a Clojure file. This uses clojure.core/load-file. This is better than evaluating the string
   contents of a file, because load-file preserves file and line-number metadata, which makes exception
   backtraces intelligible."
  (clj/print-separator)
  (-> (format "(clojure.core/load-file \"%s\")\n" file-name)
      (clj/wrap-sexp t nil)
      clj/eval-and-capture-output)
  ;; (clj/print-any-exceptions)
  )

(defun clj/load-buffer ()
  (interactive)
  (util/save-buffer-if-dirty)
  (clj/load-file (buffer-file-name)))

(defun clj/get-last-sexp-str ()
  (buffer-substring-no-properties (save-excursion (backward-sexp) (point))
                                  (point)))

(defun clj/parse-fn-name-from-defn (str)
  ;; This regexp must capture the fn-name for defn, defn-, and defns with metadata, e.g.
  ;; (defn- {:a 1} fn-name\n "the doctring" [])
  (when (string-match "(defn-? +\\(?:\\^{.+?} +\\)?\\([^ ]+\\)" str)
    (match-string 1 str)))

(defun clj/correct-defn-file-metadata (exp thing)
  "If exp starts with a defn, this will wrap exp and set its file metadata.
   - thing: the same arg you would pass to thing-at-point (e.g. 'list, 'sexp, 'defun)."
  (let ((fn-name (clj/parse-fn-name-from-defn exp)))
    (if fn-name
        (format "(do %s (alter-meta! #'%s assoc :file \"%s\" :line %s :column 0))"
                exp fn-name (buffer-file-name)
                (-> (bounds-of-thing-at-point thing) first line-number-at-pos))
      exp)))

(defun clj/eval-sexp ()
  (interactive)
  (-> (thing-at-point 'list t)
      (clj/correct-defn-file-metadata 'list)
      clj/eval-in-current-ns))

(defun clj/strip-commented-form (s)
  "Strips #_ from #_(exp)"
  (if (s-starts-with? "#_" s)
      (s-chop-prefix "#_" s)
    s))

(defun clj/eval-defun ()
  (interactive)
  (-> (thing-at-point 'defun t)
      ;; If the user evals a reader-commented form, e.g. #_(+ 1 1), assume they want to run the form
      ;; that's commented out, not eval a comment.
      clj/strip-commented-form
      (clj/correct-defn-file-metadata 'defun)
      clj/eval-in-current-ns))

(defun clj/on-inf-clojure-buffer-created ()
  "Perform any setup you desire to newly created inf-clojure buffers. This exists because the inf clojure
   buffer has no major mode, so it's hard to customize."
  ;; (print "setting up")
  ;; (print inf-clojure-buffer)
  (with-current-buffer inf-clojure-buffer
    ;; (print "in buffer")
    (evil-normal-state)))

(defun clj/repl-buffer ()
  (lexical-let ((b (get-buffer-create "*clojure-simple*")))
    (with-current-buffer b
      (setq-local scroll-margin 1)
    b)))

(defun clj/in-repl-buffer (fn)
  (with-current-buffer (clj/repl-buffer)
    (funcall fn)))

;;
;; Indenting and moving sexps.
;;

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

;; Clojure indentation rules
(with-eval-after-load "clojure-mode"
  (define-clojure-indent
    (send-off 1) (cli 1) (go-loop 1) (assoc 1)                        ; Core
    (ANY 2) (GET 2) (POST 2) (PUT 2) (PATCH 2) (DELETE 2) (context 2) ; Compojure
    (OPTIONS 2)
    (select 1) (insert 1) (update 1) (where 1) (set-fields 1)         ; Korma
    (values 1) (delete 1) (upsert 1) (subselect 1)
    (clone-for 1)                                                     ; Enlive
    (cache-get 1) (time 1)                                            ; Workbench
    (with-eligible-values 1) (when-eligible 1) (check 4)              ; Personal
    (url-of-form 1) (construct-partial 1)                             ; Personal
    ))

(defun move-to-start-of-word ()
  (let ((word-boundary (bounds-of-space-delimitted-word)))
    (if (not (= (car word-boundary) (cdr word-boundary)))
        (goto-char (car word-boundary)))))

;; TODO(philc): These commands need work. I think paredit's model of expanding/contracting the sexp under the
;; cursor may be better.

(defun slurp-into-prev-sexp ()
  (interactive)
  (move-to-start-of-word)
  (util/preserve-line-and-column
   (lambda ()
     (sp-backward-sexp)
     (forward-char)
     (sp-forward-slurp-sexp))))

(defun slurp-into-next-sexp ()
  (interactive)
  (move-to-start-of-word)
  (util/preserve-line-and-column
   (lambda ()
     (sp-backward-sexp)
     (forward-char)
     (sp-forward-slurp-sexp))))

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
(evil-define-key 'normal clojure-mode-map (kbd "C-A-h") 'slurp-into-prev-sexp)

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
  "eb" 'clj/load-buffer
  "rr" 'reload-active-chrome-tab
  "ta" 'clj/run-all-tests
  "tf" 'clj/run-tests-in-ns
  "tt" 'clj/run-test-at-point
  "SPC" 'evil-ext/fill-inside-string
  "n" 'clj/goto-next-exception
  "N" 'clj/goto-prev-exception)

;; Make it possible to eval any marked buffer from any window -- a clojure window need not be focused.
(evil-leader/set-key
  "me" 'mark-execute)

;;
;; Functions for working with clojure.test mode.
;;

; TODO(philc): Consider removing this. Do I use it?
(defun clj/run-all-tests ()
  (interactive)
  (util/save-buffer-if-dirty)
  (-> (format (concat "(do (clojure.core/load-file \"%s\")"
                      "(clojure.test/run-all-tests))")
              (buffer-file-name))
      inf-clojure-eval-string))

(defun clj/run-tests-in-ns ()
  (interactive)
  (util/save-buffer-if-dirty)
  (-> (format (concat "(do (clojure.core/load-file \"%s\")"
                      "(clojure.test/run-tests))")
              (buffer-file-name))
      clj/eval-in-current-ns))

(defun clj/defun-name-at-point ()
  "Returns the name of the function at point, and nil if it can't be found."
  ;; This should work with both defn and deftest.
  (let* ((form (thing-at-point 'defun t))
         (result (string-match "(def[^ ]* \\([^ ]*\\)" form)))
    (when result
      (match-string 1 form))))

(defun clj/run-test-at-point ()
  "Runs the clojure.test under the cursor by invoking the function defined by the test in the cider repl."
  (interactive)
  ;; Note that prior to running the test, we eval the test's definition in case we've edited its source since
  ;; our last eval. We load the entire buffer rather than just evaling the test's definition because loading
  ;; the buffer properly sets the file metadata for the function definition, so that test failure output has
  ;; the correct source file and line number of the failing test.
  (-when-let (fn-name (clj/defun-name-at-point))
    (clj/load-buffer)
    (-> (format "(clojure.test/test-vars [#'%s])" fn-name)
        clj/eval-in-current-ns)))

;;
;; cljfmt -- automatic formatting of Clojure code. This configuration is Liftoff-specific.
;;

(load "/Users/phil/src/liftoff/exp/emacs/cljfmt.el")

(setq cljfmt-show-errors nil)

;; Note that `cljfmt-before-save` triggers this save-hook for some reason, so we lock on clj-in-progress to
;; to protect from infinite recurision.
(setq cljfmt-in-progress nil)
(defun cljfmt-before-save-mutually-exclusive ()
  (interactive)
  (when (and (eq major-mode 'clojure-mode)
             (not cljfmt-in-progress))
    (setq cljfmt-in-progress 't)
    (unwind-protect (cljfmt)
      (setq cljfmt-in-progress nil))))

(add-hook 'before-save-hook 'cljfmt-before-save-mutually-exclusive nil)
;; Run this again after save so we see any formatting error messages in the Emacs echo area,
;; because they get clobbered by Emacs's "Wrote [file]" message.
(add-hook 'after-save-hook 'cljfmt-before-save-mutually-exclusive nil)
