;;
;; Utility functions for writing Emacs lisp.
;;
(provide 'lisp-utils)
(require 'dash)

;; Use Clojure's convention of `fn` for anonymous functions. It's shorter.
(defalias 'fn 'lambda)

(defun flatten (mylist)
  (cond
   ((null mylist) nil)
   ((atom mylist) (list mylist))
   (t
    (append (flatten (car mylist)) (flatten (cdr mylist))))))

(defun inc (i)
  (+ i 1))

(defun dec (i)
  (- i 1))

(defun constantly (x)
  (lexical-let ((x x))
    (lambda (&rest args) x)))

;;
;; Threading (thrush) macros, ported from Clojure.
;; Taken from https://github.com/sroccaserra/emacs/blob/master/tools.el
;;
(defmacro -?> (x form &rest more)
  (cond ((not (null more)) `(-?> (-?> ,x ,form) ,@more))
        (t (if (sequencep form)
               `(if (null ,x) nil
                  (,(first form) ,x ,@(rest form)))
             `(if (null ,x) nil
                ,(list form x))))))

(defmacro -?>> (x form &rest more)
  (cond ((not (null more)) `(-?>> (-?>> ,x ,form) ,@more))
        (t (if (sequencep form)
               `(if (null ,x) nil
                  (,(first form) ,@(rest form) ,x))
             `(if (null ,x) nil
                ,(list form x))))))

(defun string/ends-with (s ending)
  "return non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
    (if (> elength (length s))
      nil
      (string= (substring s (- 0 elength)) ending))))

(defun string/starts-with (s start)
  "returns non-nil if string S starts with ARG.  Else nil."
  (if (> (length start) (length s))
      nil
    (string-equal (substring s 0 (length start)) start)))

(defun string/join (strings separator)
  (mapconcat 'identity strings separator))

(defun string/blank? (s)
  (string= (chomp s) ""))

;; Taken from http://www.emacswiki.org/emacs/ElispCookbook
;; Also called "strip" or "trim".
(defun chomp (str)
  "Chomp leading and tailing whitespace from str."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
  str)

;; Taken from https://groups.google.com/forum/#!topic/gnu.emacs.help/_p2-GXAANgw. Requires CL.
(defun partition (l n)
  "Return a list of L's consecutive sublists of length N."
  (assert (zerop (mod (length l) n)))
  (loop for l on l by #'(lambda (l) (nthcdr n l)) collect (subseq l 0 n)))

(defun plist-sget (plist prop)
  "Retrives the prop value in plist. Throws and error if plist does not contain prop."
  (when (not (plist-member plist prop))
    (throw (concat "plist is missing property: " (prin1-to-string prop) ". " (prin1-to-string plist)) nil))
  (plist-get plist prop))
