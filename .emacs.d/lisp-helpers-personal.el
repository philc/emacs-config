;;
;; Utility functions for writing lisp in Emacs.
;;
(provide 'lisp-helpers-personal)

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

;;
;; Threading (thrush) macros, ported from Clojure.
;; Taken from https://github.com/sroccaserra/emacs/blob/master/tools.el
;;
(defmacro -> (x &optional form &rest more)
  (cond ((not (null more))
         `(-> (-> ,x ,form) ,@more))
        ((not (null form))
         (if (sequencep form)
             `(,(first form) ,x ,@(rest form))
           (list form x)))
        (t x)))

(defmacro ->> (x form &rest more)
  (cond ((not (null more)) `(->> (->> ,x ,form) ,@more))
        (t (if (sequencep form)
               `(,(first form) ,@(rest form) ,x)
             (list form x)))))

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
