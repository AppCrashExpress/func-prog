;; Helper

(defun square (value) (* value value))

;; Cartesian point definition

(defclass cartesian () 
  ((x 
     :initarg :x
     :initform 0
     :reader x-val)
   (y 
     :initarg :y
     :initform 0
     :reader y-val)))

(defmethod radius ((c cartesian))
  (sqrt (+ (square (x-val c))
           (square (y-val c)))))

(defmethod angle ((c cartesian))
  (if (= (x-val c) 0)
      (/ pi 2)
      (atan (y-val c) (x-val c))))

;; Polar point definition

(defclass polar () 
  ((r 
     :initarg :r
     :initform 0
     :reader radius)
   (phi 
     :initarg :phi
     :initform 0
     :reader angle)))

(defmethod x-val ((p polar))
  (* (radius p) (cos (angle p))))

(defmethod y-val ((p polar))
  (* (radius p) (sin (angle p))))

;; Converters between point types

(defgeneric to-polar (arg)
 (:documentation "Convert point to polar")
 (:method ((p polar))
  p)
 (:method ((c cartesian))
  (make-instance 'polar
                 :r   (radius c)
                 :phi (angle c))))

(defgeneric to-cartesian (arg)
 (:documentation "Convert point to cartesian")
 (:method ((c cartesian))
  c)
 (:method ((p polar))
  (make-instance 'cartesian
                 :x (x-val p)
                 :y (y-val p))))

;; Functions, assisting in calculation of height

(defun apply-cartesians (func a b)
  (make-instance 'cartesian
                 :x (funcall func (x-val a) (x-val b)) 
                 :y (funcall func (y-val a) (y-val b))))

(defun apply-scalar (func a coef) 
  (make-instance 'cartesian
                 :x (funcall func (x-val a) coef)
                 :y (funcall func (y-val a) coef)))

(defun dot-product (a b)
  (+ (* (x-val a) (x-val b)) (* (y-val a) (y-val b))))

;; Shapes

(defclass line ()
 ((start
    :initarg :start 
    :reader line-start)
  (end
    :initarg :end
    :accessor line-end)))

(defclass triangle ()
 ((vertex1 :initarg :1 :reader vertex1)
  (vertex2 :initarg :2 :reader vertex2)
  (vertex3 :initarg :3 :reader vertex3)))

;; Function, calculating the height line

(defmethod height ((tri triangle))
  (let* ((cart1     (to-cartesian (vertex1 tri))) 
         (cart2     (to-cartesian (vertex2 tri))) 
         (cart3     (to-cartesian (vertex3 tri)))
         (BC        (apply-cartesians #'- cart3 cart2))
         (BA        (apply-cartesians #'- cart1 cart2))
         (coef      (/ (dot-product BA BC) (dot-product BC BC)))
         (end-point (apply-cartesians #'+ cart2 (apply-scalar #'* BC coef))))
    (if (typep (vertex1 tri) 'cartesian)
        (make-instance 'line :start cart1 :end end-point)
        (make-instance 'line :start (vertex1 tri) :end (to-polar end-point)))))

;; Class printers

(defmethod print-object ((c cartesian) stream)
  (format stream "[CART x ~d y ~d]"
          (x-val c) (y-val c)))

(defmethod print-object ((p polar) stream)
  (format stream "[POLAR radius ~d angle ~d]"
          (radius p) (angle p)))

(defmethod print-object ((lin line) stream)
  (format stream "[LINE ~s ~s]"
          (line-start lin) (line-end lin)))

(defmethod print-object ((tri triangle) stream)
  (format stream "[TRIANGLE ~s ~s ~s]"
          (vertex1 tri) (vertex2 tri) (vertex3 tri)))

