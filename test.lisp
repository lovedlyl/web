(ql:quickload "web")
(in-package #:Web)
(truename ".")
(html2file
 "~/test.html"
 :body (<:div :id "main"
	      (<:ul :id "ul" 
		    (loop for i from 1 to 100
			 collect (<:li (format nil "~R" i))))
	      (<:p :id "p" "this is a para"))
 ;;:scripts ("dom.js")
 :internal-script
 ((define-front-lib dom)
  (var main (by "#main"))
  (var ul (chain document (query-selector "#ul")))
  (var p (by "#p"))
  (var fragment (create-fragement))
  ;;(var ul (create-element "ul"))
  (dotimes(i 3)
    (var li (create-element "li"))
    (var text (create-text-node (+ "Item: " i)))
    (insert-node-at li text "last")
    (insert-node-at fragment li "last"))		    
  ;;(insert-node-at fragment p "last")
  (insert-node-at ul fragment "last")
  ;;(print (by "li"))
  (print (focused-element))
  (print p)
  (focus-element p)
  (print (= p (focused-element)))
  (print (focused-element))
  ))

