(ql:quickload "cl-actors")

(use-package 'cl-actors) 

(defun circular (&rest items)
  (rplacd (last items) items)
  items)

(defmacro send-list (l m)
  `(map nil send l (circular '(m))))

(defun has-label (label msg)
  (if (eq msg (car label))
      t))

(defun get-contents (msg)
  (cdr msg))

(defmacro defsource (name ioname &rest body)
  `(progn (defactor ,ioname () (m)
	    (loop (send ,name (progn ,@body))))
	  (defactor ,name (subscribers '()) (m)
	    (cond ((has-label 'subscribe m)
		   (push (get-contents msg) subscribers))
		  ((has-label 'unsubscribe m)
		   (remove (get-contents msg) subscribers))
		  ((has-label 'io-data m)
		   (send-list l m))))
	  (setf ,name (,name))
	  (setf ,ioname (,ioname))))

(defsource terminal io-terminal
  (read-line))
  
