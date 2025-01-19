(defparameter *S* nil)
(defparameter *E* nil)
(defparameter *C* nil)
(defparameter *D* '("D0"))

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
  (push x E))

(defun location (X)
  (let ((pair (assoc-if (lambda (key) (equal key X)) *E*)))
	(if pair
	  (cdr pair)
	  X)))

(defun closure (x)
  (= (length x) 3))

(defun print_secd ()
  (format t "S: ~a~%E: ~a~%C: ~a~%D: ~a~%~%" *S* *E* *C* *D*))

(defun input ()
  (format t "input lambda: ")
  (let ((input_string (read-line)))
  (setq *C* (unitlist input_string))))

(defun transform ()
  (input)
  (print_secd)

  (block nil
		 (loop
		   ;; 1
		   (when (null *C*)
			 (when (equal *D* '(nil nil nil "D0"))
				 (setq *S* (cons (first *S*) (first *D*)))
				 (setq *E* (second *D*))
				 (setq *C* (third *D*))
				 (setq *D* (unitlist (fourth *D*)))
				 (print_secd)
				 (return))
			 (setq *S* (list (first *S*) (first *D*)))
			 (setq *E* (second *D*))
			 (setq *C* (unitlist (third *D*)))
			 (setq *D* (unitlist (fourth *D*)))
			 (print_secd))

		   ;; 2
		   (unless (null *C*)
			 (let ((X (first *C*)))
			   (cond
				 ;; 2a
				 ((identifier X)
				  (setq *S* (cons (location X) *S*))
				  (setq *C* (rest *C*))
				  (print_secd))
				 ;; 2b
				 ((lambda_exp X)
				  (setq *S* (cons (construct_closure *E* (bv X) (body X)) *S*))
				  (setq *C* (rest *C*))
				  (print_secd))
				 ;; 2c
				 ((string= X "ap")
				  (let ((hs (first *S*)))
					;; 2c1
					(when (closure hs)
					  (let ((S_copy (rest (rest *S*))) (E_copy *E*) (C_copy (rest *C*)) (D_copy *D*))
						(when (null S_copy)
						  (setq S_copy (unitlist nil)))
						(when (null E_copy)
						  (setq E_copy (unitlist nil)))
						(when (null C_copy)
						  (setq C_copy (unitlist nil)))
						(when (null D_copy)
						  (setq D_copy (unitlist nil)))
					    (setq *D* (append S_copy (append E_copy (append C_copy D_copy)))))
					  (setq *E* (derive (my_assoc (second hs) (second *S*)) (first hs)))
					  (setq *S* nil)
					  (setq *C* (unitlist (third hs)))
					  (print_secd))
					;; 2c2
					(unless (closure hs)
					  (setq *S* (cons (cons (first *S*) (cons (second *S*) nil)) (rest (rest *S*))))
					  (setq *C* (rest *C*))
					  (print_secd))))
				 ;; 2d
				 (t
				   (setq *C* (cons (rand X) (cons (rator X) (cons "ap" (rest *C*)))))
				   (print_secd))))))))
					  
(defun clean ()
  (setq *S* nil)
  (setq *E* nil)
  (setq *C* nil)
  (setq *D* '("D0")))
