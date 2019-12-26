(defpackage #:clombinatorix
  (:use #:cl)
  (:import-from #:alexandria
                :copy-array
                :nconcf))

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
   (upto :initarg :upto :reader upto)))

(defun natural-numbers (from upto)
  (make-instance 'natural-numbers :from from :upto upto))

(defmethod count-elements ((n natural-numbers))
  (+ (- (upto n) (from n)) 1))

(defmethod compute-elements ((n natural-numbers))
  (loop :for i :from (from n) :upto (upto n) :collect i))

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
  (n-cartesian-product (loop :for set :in (sets p)
                            :collect (elements set))))

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

; Permutation with repetition; permutation wihtout repetition; k out of n
(defclass permutation (clombinatorix)
  ((set :initarg :set :reader pset)
   (check :initarg :repetition-check :reader repetition-check)))

(defun permutation (set &optional (repetition-check nil))
  (make-instance 'permutation
                 :set set
                 :repetition-check repetition-check))

(defmethod count-elements ((p permutation))
  (factorial (count-elements (pset p))))

(defmethod compute-elements ((p permutation))
  (let* ((elements (elements (pset p)))
         (array (coerce elements 'vector))
         (n (length elements)))
    (heaps-algorithm n array )))


(defun heaps-algorithm (n array)
  (let ((result nil))
    (if (= n 1)
        (list (copy-array array))
        (progn
          (dotimes (i n result)
                  (append (heaps-algorithm (- n 1) array) result)
                  (if (evenp n)
                      (swap i (- n 1) array)
                      (swap 0 (- n 1) array)))))))

(defun factorial (n)
  (reduce #'* (loop :for i :from 2 :upto n :collect i)))

(defun swap (what with array)
  (rotatef (aref array what)
           (aref array with)))

(defparameter *foo*
  (set-product (natural-numbers 1 9)
                      (natural-numbers 10 20)))

(defparameter *foo-permut*
  (permutation (natural-numbers 1 3)))
