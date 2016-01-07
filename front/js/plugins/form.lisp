(in-package #:web)

;;A stabdard set of rules and descriptive error 
;;messages for building a basic validation engine
(defpslibmacro (form)
    `(Create
      err-msg 
      (create
       ;;required
       required
       (create
	msg "This filed is required."
	test (lambda(obj)	      
	       (defun inputs-by-name(name)
		 ;;the array of input elements that will be matched
		 (var results (array))
		 ;;keep track of how many of them were checked
		 (setf (@ results num-checked) 0)
		 ;;find all the input elements in the document
		 (var input (chain document (get-elements-by-tag-name "input")))
		 ;;(console-log input)
		 (dotimes(i (@ input length))
		   ;;find all the fields that have the specified name
		   (when (equal (@ (elt input i) name) name)
		     ;;(console-log name)
		     ;;Save the result, to be returned later
		     (chain results (push (elt input i)))
		     ;;Remember how many of the fields were checked
		     (when(@ (elt input i) checked)
		       (incf (@ results num-checked)))))
		 results)
	       (var value (@ obj value))
	       (cond ((or (= (@ obj type) "checkbox")
			  (= (@ obj type) "radio"))
		      ;;(add-note obj)
		      (@ (inputs-by-name (@ obj name)) num-checked))
		     (t ;;(add-note obj) 
		      (and (> (@ value length) 0)
			   (not (= value (@ obj default-value)))
			   value)))))



     ;;;email
       email
       (create
	msg "Not a valid email address."
	test (lambda(obj)
	       ;; (console-log "      validating email")
	       (var value (@ obj value))
	       (and value
		    (chain (regex "/^[a-z0-9_+.-]+\\@([a-z0-9-]+\\.)+[a-z0-9]{2,4}$/i")
			   (test value)))))
       ;;phone number
       phone
       (create
	msg "Not a valid phone number."
	test (lambda(obj)
	       (var value (@ obj value))
	       (var m (chain (regex "/(\\d{4}).*(\\d{7})/")
			     (exec value)))
	       (when m
		 (var matched (+ "(" (elt m 1) ")"  (elt m 2) )))
	       (and (not (= value ""))
		    m
		    matched)))
       
       ;;cell-phone number
       cell-phone
       (create 
	msg "Not a valid cell phone number."
	test (lambda(obj)
	       (var value (@ obj value))
	       (and (not (= value ""))
		    (chain (regex "/\\d{11}/") (test value))
		    value)))
       
       ;;date
       date
       (create
	msg "Not a valid date."
	test (lambda(obj)
	       (var value (@ obj value))
	       (and (not (= value ""))
		    (chain (regex "/^\\d{2,4}.*\\d{1,2}.*\\d{1,2}$/")
			   (test value))
		    value)))
       ;;url
       url
       (create
	msg "Not a valid URL"
	test (lambda(obj)
	       (var value (@ obj value))
	       (and (not (= value ""))
		    (not (= value "http://"))
		    (chain (regex "/^https?:\\/\\/([a-z0-9]+\\.)+[a-z0-9]{2,4}.*$/")
			   (test value))
		    value)))
       )
     ;;;;;;end errMsg

     ;;;;;;;;methods
      validate-form
      (lambda(form)
	;;(console-log "validating from...")
	(var valid true)
	(var elements (@ form elements))
	(dotimes(i (@ elements length))
	  (var elem (elt elements i))
	  ;;(chain ,*lib-name* form (hide-errors elem))
	  (unless (chain ,*lib-name* form (validate-field elem))
	    (setf valid false)))
	valid)
      ;;valide a single field field's content
      validate-field
      (lambda(elem)
	(chain ,*lib-name* form (hide-errors elem)) ;;;for validate each field individually
	(var errors (array))
	(for-in (name (@ ,*lib-name* form err-msg))	       
		(var re (new (*reg-exp (+ "(^|\\s)" name "(\\s|$)")))) ;;;regex 尾部索引不断变化 所以每次创建新的
		(when (and (chain re (test (@ elem class-name)))
			   (not (chain (elt (@ ,*lib-name* form err-msg) name)
				       (test elem))))
		  ;;(console-log "    validating " name elem)			
		  (chain errors (push (@ (elt (@ ,*lib-name* form err-msg) name)
					 msg)))))
	;;(console-log "errors: " (@ errors length) errors)
	(when (@ errors length)
	  (chain ,*lib-name* form (show-errors elem errors)))
	;;0 for no error >0 for errors
	(> (@ errors length) 0))
      ;;hide-errors remove duplicated error messages
      hide-errors
      (lambda(elem)
	(var next (@ elem next-sibling))
	(when (and next (= (@ next node-name) "UL")
		   (= (@ next class-name) "errors"))
	  (chain elem parent-node (remove-child next))))
      ;;show errors
      show-errors
      (lambda(elem errors)
	(var next (@ elem next-sibling))       
	(when (or (not next)
		  (not (= (@ next node-name) "UL"))
		  (not (= (@ next class-name) "errors")))
	  (var new-next (chain document (create-element "ul")))
	  (setf (@ new-next class-name) "errors")
	  (if next
	      (chain elem parent-node (insert-before new-next (@ elem next-sibling)))
	      (chain elem parent-node (append-child new-next))))
	(dotimes(i (@ errors length))
	  (var li (chain document (create-element "li")))
	  (setf (@ li inner-h-t-m-l) (elt errors i))
	  (chain new-next (append-child li))))
     ;;;;;3 Ways for validating a form: onload onsubmit on-field-changed
      ;;duplicatedly definr add-event
      add-event
      (lambda(elem type handler)
	(cond ((@ elem add-event-listener)
	       (chain elem (add-event-listener type handler)))
	      ((@ elem attach-event)
	       (chain elem (attach-event (+ "on" type) handler)))
	      (t (setf (elt elem (+ "on" type)) handler))))
     ;;;validate on submit
      watch-form
      (lambda(form)
	(chain ,*lib-name* form 
	       (add-event form "submit" (lambda(e) 
					  (chain e (prevent-default))					 
					  (chain ,*lib-name* form (validate-form  form))))))
     ;;;validate on field change
      watch-fields
      (lambda(form)
	(var elems (@ form elements))
	(dotimes(i (@ elems length))
	  (var elem (elt elems i))
	  (add-event 
	   (elt elems i) "blur"
	   (lambda() (chain ,*lib-name* form (validate-field this))))))
     ;;;initial notice
      init-required-notice
      (lambda(form)
	(var elems (@ form elements))
	(dotimes(i (@ elems length))
	  (var elem (elt elems i))
	  (var prev (@ elem previous-sibling))
	  (when (and (chain (regex "/(^|\\s)required(\\s|$)/")
			    (test (@ elem class-name)))
		     ;;(or (= (@ elem value) ""))
		     (not (= (@ prev inner-h-t-m-l) "*")))
	    (var span (Chain document (create-element "span")))
	    (setf (@ span inner-h-t-m-l) "*")
	    (setf (@ span style "color") "red")
	    (setf (@ span style "margin-right") "4px")
	    (chain elem parent-node (insert-before span elem)))))
      
      


      ))



(defpsmacro validate-form(form)
  `(chain ,*lib-name* form (validate-form ,form)))

(defpsmacro watch-form(form)
  `(chain ,*lib-name* form (watch-form ,form)))

(defpsmacro watch-fields(form)
  `(chain ,*lib-name* form (watch-fields ,form)))

(defpsmacro init-required-notice(form)
  `(chain ,*lib-name* form (init-required-notice ,form)))



;;;-----------------------
`(html2file
 "~/test.html"
 :title "Form Test"
 :body
 (<:form :id "form1" :action "http://www.baidu.com" :method "get"
	 (<:textarea :id "area" :rows 10 :cols 10 "inital value")
	 (<:input :name "name" :type "text" :placeholder "your name")
	 (<:ul (loop for i from 1 to 3 collect (<:li (<:input :type "radio" :name "color" :value (format nil "~R" i)))))
	 (<:input :class "phone" :name "pass" :type "password" :placeholder "your pass word")
	 (<:input :name "submit" :type "submit" :value "submit")
	 (<:input :name "reset"  :type "reset" :value "reset form"))
 :internal-script
 ((define-front-lib form dom)
  (var form (by "#form1"))
  (watch-form form)))

