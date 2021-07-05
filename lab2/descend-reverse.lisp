;;:: Checks if given list is descending and reverses it
;;;; if it is not

(defun is-descending (l)
  "Checks if given list is in descending order"
  (cond ((< (length l) 2) T)
        ((<= (first l) (second l)) NIL)
        (T (is-descending (rest l)))))

(defun decreasing (l)
  "Reverses list if it is not in non-ascending order"
  (if (is-descending l)
      l
      (reverse l)))
