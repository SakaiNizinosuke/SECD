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

(defun rand (x)
  (block nil
	(if (not (char= (char x 1) #\())
  		(return (subseq x 3 (- (length x) 1))))
	(let ((count_pt 0))
		(loop for i from 1 to (- (length x) 1) do
			  (if (char= (char x i) #\() (setq count_pt (+ count_pt 1)))
			  (if (char= (char x i) #\)) (setq count_pt (- count_pt 1)))
			  (if (= count_pt 0) (return (subseq x (+ i 2) (- (length x) 1))))))))

(defun rator (x)
  (block nil
	(if (not (char= (char x 1) #\())
  		(return (subseq x 1 2)))
	(let ((count_pt 0))
		(loop for i from 1 to (- (length x) 1) do
			  (if (char= (char x i) #\() (setq count_pt (+ count_pt 1)))
			  (if (char= (char x i) #\)) (setq count_pt (- count_pt 1)))
			  (if (= count_pt 0) (return (subseq x 1 (+ i 1))))))))

(defun construct_closure (E bv body)
  (cons E (cons bv (cons body nil))))

(defun my_assoc (x y)
  (cons x y))

(defun derive (x E)
  (cons E x))
