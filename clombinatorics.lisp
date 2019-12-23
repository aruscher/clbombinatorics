(defpackage #:clombinatorix
  (:use #:cl))

(in-package #:clombinatorix)

;; Src: http://www.math.kit.edu/iag6/lehre/co2015s/media/script.pdf
(declaim (optimize (debug 3)))

(defclass clombinatorix ()
  ((elements :accessor elements)))

(defgeneric count-elements (clombinatorix)
  (:documentation "Counts number of elements."))

(defmethod count-elements ((l list))
  (list-length l))

(defgeneric compute-elements (clombinatorix)
  (:documentation "Computes the elements."))

(defmethod elements ((l list))
  l)

(defmethod elements :before ((c clombinatorix))
  (unless (slot-boundp c 'elements)
    (setf (slot-value c 'elements)
          (compute-elements c))))

(defclass natural-numbers (clombinatorix)
  ((from :initarg :from :reader from)
   (to :initarg :to :reader to)))

(defun natural-numbers (&key from to)
  (make-instance 'natural-numbers :from from :to to))

(defmethod count-elements ((n natural-numbers))
  (+ (- (to n) (from n)) 1))

(defmethod compute-elements ((n natural-numbers))
  (loop :for i :from (from n) :upto (to n) :collect i))

(defclass set-partition (clombinatorix)
  ((sets :initarg :sets :accessor sets)))

(defmethod compute-elements ((p set-partition))
  (apply #'append
         (loop :for set :in (sets p)
              :collect (elements set))))

(defmethod count-elements ((p set-partition))
  (reduce #'+ (loop :for set :in (sets p)
                 :collect (count-elements set))))

(defun set-partition (&rest sets)
  (make-instance '%set-partition :sets sets))


(defclass set-product (clombinatorix)
  ((sets :initarg :sets :accessor sets)))

(defmethod compute-elements ((p set-product))
  (n-cartesian-product (sets p)))

(defmethod count-elements ((p set-product))
  (reduce #'* (loop :for set :in (sets p)
                 :collect (count-elements set))))

(defun set-product (&rest sets)
  (make-instance 'set-product :sets sets))

(defun n-cartesian-product (l)
  (if (null l)
      (list nil)
      (loop for x in (car l)
            nconc (loop for y in (n-cartesian-product (cdr l))  
                        collect (cons x y)))))




(defparameter *foo*
  (set-product (natural-numbers :from 1 :to 9)
                      (natural-numbers :from 10 :to 20)))
