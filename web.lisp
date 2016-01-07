;;;; web.lisp

(in-package #:web)

;;; "web" goes here. Hacks and glory await!


;;util functions for front

;;;;;for parenscript
(setf *js-string-delimiter* #\')

;;;;;;;JavaScript


;;(let((*js-string-delimiter* #\"))
(defmacro js2string(&body js-body)
  `(ps ,@js-body))

(defmacro exec-js2string(&body js-body)
  `(format nil "~A~%~A" "#! /usr/bin/nodejs" (ps ,@js-body)))

(defmacro json2string(&body json-body)
  `(substitute #\" #\' (string-trim '(#\; #\( #\) ) (ps ,@json-body))))


(defmacro js2file(filename &body js-body)
  `(progn
     (with-open-file (out ,filename 
			  :direction :output
			  :if-exists :supersede 
			  :if-does-not-exist :create)
       (format out (js2string ,@js-body)))
     (format t "~%Done! js->: ~A from ~A" ,filename (truename "."))))




(defmacro json2file(filename &body js-body)
  `(progn
     (with-open-file (out ,filename 
			  :direction :output
			  :if-exists :supersede 
			  :if-does-not-exist :create)
       (format out (json2string ,@js-body)))
     (format t "~%Done! JSON->: ~A from ~A" ,filename (truename "."))))


(defmacro exec-js2file(filename &body js-body)
  `(progn
     (with-open-file (out ,filename 
			  :direction :output
			  :if-exists :supersede 
			  :if-does-not-exist :create)
       (format out (exec-js2string ,@js-body)))
     (format t "~%Done! EXEC-js->: ~A from ~A" ,filename (truename "."))))




(defun css2string(css-raw)
  (cl-css:css (css2sass css-raw)))

(defun css2file(filename css-raw)
  "write css to file"
  (cl-css:compile-css filename (css2sass css-raw))
  (format t "~%Done! css->: ~A from ~A" filename (truename ".")))


;;----------------------------------------------
(<:augment-with-doctype "html" "")
(defmacro html-frame(context)
  (let*((title (getf context :title))
	(css-s (remove-duplicates (getf context :css-s) :test #'string=))
	(internal-css (getf context :internal-css))
	(scripts (remove-duplicates (getf context :scripts) :test #'string=))
	(internal-script (getf context :internal-script))
	(body (getf context :body)))
    `(<:html
      :lang "zh-CN"
      (<:head
       (<:meta :charset "utf-8")
       (newline)
       ;;Always put the document into the most recent document mode available.???
       (<:meta :http-equiv "X-UA-Compatible" :content "IE=Edge")
       (when ,title (<:title ,title))
       (newline)
       (<:!-- "external css")
       (newline)
       (loop for css in ',css-s
	  collect
	    (<:link :rel "stylesheet" :type "text/css" :href css))
       (newline)
       (<:!-- "internal css")
       (newline)
       (when ,internal-css
	 (<:style :type "text/css" (css2string ,internal-css)))
       (newline)    
       (<:!-- "Dynamic external script (can be with internal scripts)")
       (newline)
       ;;外联脚本
       (loop for script in ',scripts
	  collect (list (<:script :src script) (newline)))
       ;;内联脚本
       ;;测试库时放在这里，以后放在head中
       (when ',internal-script       
	 (<:script :type "text/javascript"  
		   (format nil  "~A//<![CDATA[~A ~a ~A//]]>~A"
			   (newline)(newline);;;for xhtml
			   (js2string ,@internal-script)
			   (newline)(newline))))

       )
      ;;end head
      (<:!-- "body")
      (newline)
      (<:body ,body))))


(defun frame(cxt)
  (<:html
   (<:head)
   (<:body (getf cxt :body))))

(defmacro node-frame(cxt)
  (let((html (frame cxt)))
    `(progn ,html)))
    
(frame `(:body ,(<:div :id "shit")))


(defmacro html2file(filename &rest context)
   "write html5 to file.
context: title body internal-css css-s internal-script scripts"
   `(progn
      (with-open-file (out ,filename 
			   :direction :output
			   :if-exists :supersede 
			   :if-does-not-exist :create)
	(format out (html-frame ,context)))
      (format t "~%Done! html->: ~A from ~A" ,filename (truename "."))))

