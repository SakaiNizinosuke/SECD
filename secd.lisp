(defparameter *S* :uninitialized)
(defparameter *E* :uninitialized)
(defparameter *C* :uninitialized)
(defparameter *D* :uninitialized)

(defun unitlist (x)
  (cons x nil))

(defun identifier (x)
  (and (stringp x) (= (length x) 1)))

(defun lambda_exp (x)
  (char= (char x 1) #\@))

(defun bv (x)
  (subseq x 2 3))

(defun body (x)
  (subseq x 4 (- (length x) 1)))
