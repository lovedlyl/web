(in-package #:web)
(defun newline()
  (format nil "~A" #\Newline))

(defun compress(string)
  "for compress CSS and JavaScript"
  (remove-if (lambda(char) (char-equal char #\Space)) string))
;;;;;;;;CSS

;;;;;;;;;;;;;for using SASS format
(defun css2sass(raw);;;may be improved only allow child selector(not descendent) for performance
  "Convert SASS format in raw to cl-css use"
  (reduce #'append (mapcar (lambda (lst)
			     (if (nest-p lst)
				 (flatten lst)
				 (list lst)))
			   raw)))

(defun flatten(nest &optional tags result)  
  "flatten a SASS like css format to flatten lists to be used by cl-css"
  (cond ((null nest) result)	
	((not (nest-p nest)) (append result
				     (list 
				      (cons (convert-selector tags (first nest))
					    (rest nest)))))
	(t (multiple-value-bind (clean lists)
	       (clean-css nest)    
	     (cond ((null lists) result)		  
		   (t (flatten (cdr lists)
			       (append tags (when clean (list (first clean))))
			       (flatten (car lists) 					
					(append tags (when clean (list (first clean))))
				        (append result (when (and clean (not (equal (length clean) 1)))
							 (list (cons (convert-selector tags (first clean))
								     (rest clean)))))))))))))


(defun convert-selector(tags tag)
  (format nil "~{~A~^ ~}" (mapcar #'Convert-case (group (append tags (list tag))))))

(defun convert-case(x)
  (if (symbolp x) 
      (string-downcase (symbol-name x))		  
      x))

(defun group(lst)
  (labels((pseud-class-p (x)
	    (or (and (symbolp x) (find #\& (symbol-name x)))
		(and (stringp x) (find #\& x)))))
    (do*((list lst (cdr list))
	 (current (car list) (car list))
	 (result nil))
	((null list) (reverse result))
      (if (pseud-class-p current)	     
	  (push (format nil "~A:~A"
			(convert-case (pop result))
			(if (stringp current)
			    (subseq current 1)
			    (string-downcase (subseq (symbol-name current) 1))))
		result)
	  (push current result)))))
	    	    

(defun clean-css(nest)
  (let (clean lists)
    (dolist (item nest)
      (if (or (symbolp item)
	      (stringp item)
	      (numberp item))
	  (push item clean)
	  (push item lists)))
    (values (reverse clean) (reverse lists))))

(defun nest-p(list)
  (find-if (lambda(item) (listp item)) list))

;;;;;for cross browers compatible
(defmacro with-gensym((&rest name) &body body)
  `(let ,(loop for n in name collect `(,n (gensym)))
     ,@body))

;;(with-gensym (a b c))
(defmacro cross-broswer(property value &optional(brower-names '(-moz- -webkit- -ms- -o-)))
  (with-gensym (val)
    `(let ((,val ,value))
       (list  ,property ,val 
	      ,@(loop for name in brower-names
		   collect (intern (format nil "~A~A" name property) :keyword)
		   collect val)))))


;;;for html


;;;;;;;;;;universal util
(defmacro while(test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro do-while(test &body body)
  `(progn
     ,@body
     (do()
	((not ,test))
       ,@body)))
;;(ps:import-macros-from-lisp 'do-while)

(defun new-symbol (&rest args)
  (intern (format nil "~{~A~}" args)))

;-------------------------------------------------------
