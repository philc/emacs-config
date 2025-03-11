;;
;; Utility functions for writing Emacs lisp.
;;
(provide 'lisp-utils)
(require 'dash)
(require 's)

;; Use Clojure's convention of `fn` for anonymous functions. It's shorter.
(defalias 'fn 'lambda)

(defun flatten (mylist)
  (cond
   ((null mylist) nil)
   ((atom mylist) (list mylist))
   (t
    (append (flatten (car mylist)) (flatten (cdr mylist))))))

(defun inc (i) (+ i 1))

(defun dec (i) (- i 1))

(defun constantly (x)
  (let ((x x))
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

(defun string/blank? (s)
  (string= (s-trim s) ""))

;; Taken from https://groups.google.com/forum/#!topic/gnu.emacs.help/_p2-GXAANgw. Requires CL.
(defun partition (l n)
  "Return a list of L's consecutive sublists of length N."
  (cl-assert (zerop (mod (length l) n)))
  (cl-loop for l on l by #'(lambda (l) (nthcdr n l)) collect (subseq l 0 n)))

(defun plist-sget (plist prop)
  "Retrives the prop value in plist. Throws and error if plist does not contain prop."
  (when (not (plist-member plist prop))
    (throw (concat "plist is missing property: " (prin1-to-string prop) ". "
                   (prin1-to-string plist)) nil))
  (plist-get plist prop))

(defmacro setq-temporarily (var value form)
  "Uses setq to set the variable `var` temporarily for the duration of form, and then restores it to its
   former value. Returns the value that `form` returns."
  `(let* ((old-val (symbol-value ',var))
          (_ (setq ,var ,value))
          (ret-val ,form))
     (setq ,var old-val)
     ret-val))

;; This is useful because `print` only prints its first arg. `message` accepts a formatter string
;; and so can print multiple arguments, but it also prints to the echo area.
(defun printall (&rest args)
  (dolist (arg args)
    (princ arg)
    (princ " "))
  (princ "\n"))
