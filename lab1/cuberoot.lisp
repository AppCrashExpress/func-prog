;;;; Calculates a cubic root of value

(defconstant +eps+ short-float-epsilon
  "Defines an epsilon, which will be used to compare two float variables.")

(defun to-cube (x) (* x x x))

(defun eps-equal (a b) 
  "Checks if two values are close enough to be considered equal
  (float-point comparison)"
  (<= (abs (- a b)) +eps+))

(defun calculate-next (previous-x original-x)
  "Calculates next value in Newton method"
  (/ 
    (+ 
      (/ original-x (* previous-x previous-x)) 
      (* 2 previous-x))
    3))

(defun cuberoot-iteration (previous-x original-x) 
  "Checks if cube of current value is close enough to 
  be considered cube root of original"
  (if (eps-equal (to-cube previous-x) original-x)
      previous-x
      (cuberoot-iteration 
        (calculate-next previous-x original-x) 
        original-x)))

(defun cuberoot (x) 
  "Calculates the cube root of the value 'x'"
  (cuberoot-iteration (float x) (float x)))

