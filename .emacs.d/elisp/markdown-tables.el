(provide 'markdown-tables)

(defun tables/parse-columns (row)
  (let* ((lines (->> (s-split "\n" row)
                     (-map (lambda (s) (s-chop-prefix "|" s)))))
         (columns (list)))
    (dolist (line lines)
      (let* ((column-values (s-split "\|" line)))
        ;; (progn (print ">>>> line") (prin1 line t))
        ;; (progn (print ">>>> column-values") (prin1 column-values t))
        (dotimes (i (length column-values))
          (let* ((line-content (nth i column-values))
                 (contents (or (nth i columns)
                               "")))
            ;; (progn (print ">>>> line-content") (prin1 line-content t))
            ;; (progn (print ">>>> contents") (prin1 contents t))
            (setq columns (-replace-at i (concat contents line-content) columns))))))
    columns))

(defun tables/parse-rows (text)
  "Returns a list of rows, each containing cells."
  (let* ((rows (s-split "\n?\|[\-\|]+\n" text)))
    ;; Remove any table borders at the very bottom of the table.
    (if (string= (car (last rows)) "")
        (-drop-last 1 rows)
      rows)))

(defun tables/parse-table (text)
  (->> text
       tables/parse-rows
       (-map 'tables/parse-columns)))

;; (let ((a (->> (concat "|a |b\n"
;;                       "|--|-\n"
;;                       "|1 |5 \n"
;;                       "|2 |6"
;;                       "|--|-\n"
;;                       )
;;               tables/parse-rows
;;               ;; last
;;               ;; car
;;               )))
;;   (progn (print ">>>> a") (prin1 a t))
;;   ;; (string= "" a)
;;   )
;;      ;; (-take 2)
;;      ;; (-map 'tables/parse-columns)


(setq tables/max-width 30)
(setq tables/cell-padding 0)

(defun tables/reflow-table (rows)
  ;; (let* ((column-count (->> rows first length))
  ;;        (width-without-boders (- tables/max-width (+ column-count
  ;;                                                     (* column-count tables/cell-padding 2))))
  ;;        (column-width (/ width-without-boders column-count)))
  (tables/to-string rows))

(defun tables/to-string (rows)
  "Returns a string representation of a table."
  ;; TODO(philc): Figure out who is computing column widths.
  (let* ((column-count (->> rows first length))
         (width-without-boders (- tables/max-width (+ column-count
                                                      (* column-count tables/cell-padding 2))))
         (column-width (/ width-without-boders column-count))
         (line-divider (->> (number-sequence 0 (- column-count 1))
                            (--map (s-pad-right column-width "-" ""))
                            (s-join "|")
                            (s-prepend "|")))
         )
    (progn (print ">>>> rows") (prin1 rows t))
    (->> rows
         (-map (lambda (row)
                 (let* ((lines (--map (s-split "\n" it) row))
                        (line-count (-max (-map 'length lines))))
                   (->> (number-sequence 0 (- line-count 1))
                        (-map (lambda (line-num)
                                (->> lines
                                     (--map (->> (or (nth line-num it) "")
                                                 (s-pad-right column-width " ")))
                                     (s-join "|")
                                     s-trim-right
                                     (s-prepend "|")
                                     )))
                        (s-join "\n")
                        ))))
         (s-join (format "\n%s\n" line-divider))
         )))


(->> (concat "|a |b\n"
             "|--|-\n"
             "|1 |5 \n"
             "|2 |6"
             "|--|-\n"
             )
     tables/parse-table
     tables/reflow-table
     ;; (-take 2)
     ;; (-map 'tables/parse-columns)
     )

(defun tables/reflow ()
  (interactive)
  ;; Assume the cursor is in a table.
  (let* ((table-text (substring-no-properties (thing-at-point 'paragraph)))
         (bounds (bounds-of-thing-at-point 'paragraph)))
    ;; (progn (print ">>>> bounsd") (prin1 bounds t))
    ;; (progn (print ">>>> (first bounds") (prin1 (first bounds t)))
    ;; (progn (print ">>>> (second bounds)") (prin1 (second bounds) t))
    (delete-region (car bounds) (cdr bounds))
    (->> table-text
         tables/parse-table
         tables/reflow-table
         (s-prepend "\n")
         insert
         )))


   ;; (setq bds (bounds-of-thing-at-point 'word)) )
   ;;  (setq p1 (car bds) )
   ;;  (setq p2 (cdr bds) )
   ;;  ;; grab the string
   ;;  (setq inputStr (buffer-substring-no-properties p1 p2)  )
   ;;  (setq resultStr (s-lower-camel-case inputStr))
   ;;  (message inputStr)

    ;; (delete-region p1 p2 ) ; delete the region
    ;; (insert resultStr) ; insert new string
