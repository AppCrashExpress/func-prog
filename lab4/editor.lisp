(defun check-lowercase (letter)
  "Checks if letter is lowercase"
  (cond ((position letter "абвгдеёжзийклмнопрстуфхцчшщъыьэюя") T)
        ((lower-case-p letter) T)
        (T NIL)))

(defun edit-lower (letter)
  "If letter is lowercase, returns 3 (the letter otherwise)"
  (if (check-lowercase letter)
      #\3
      letter))

(defun edit-string (str)
  "Replaces lower case letters before * with 3"
  (let ((star-pos (position #\* str)))
    (if star-pos
        (concatenate
          'string
          (map 'string #'edit-lower (subseq str 0 star-pos))
          (subseq str star-pos))
        str)))


(defun edit-full-string (str)
  "Same as edit-string but for the whole string"
  (map 'string #'edit-lower str))

(defun edit-text (text) 
  "Replaces lower case letters in strings before * with 3"
  (let ((star-line (position-if (lambda (str) (position #\* str)) text)))
    (if star-line
        (concatenate
          'list
          (map 'list #'edit-full-string (subseq text 0 star-line))
          (list (edit-string (nth star-line text)))
          (subseq text (+ 1 star-line)))
        text)))
