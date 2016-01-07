(in-package #:web)

(extend-lib
    (ajax)
  `(progn 
     (chain ,$ 
	    (extend
	     (create
	      ;;创建XHR对象
	      ;;只支持到IE>7 所以XMLHttpRequest通用
	      create-x-h-r
	      (lambda() (new (*x-m-l-http-request)))
	      
	      ;;;
	      ajax-options
	      (create type "GET" url "" timeout 5000
		      data "" data-type "text"
		      complete (lambda())
		      success (lambda()) 
		      error (lambda()))
	      
	      
	      ajax-setup
	      (lambda(options)
		(setf (@ ,$ ajax-options)
		      (chain ,$ (union options (@ ,$ ajax-options)))))
	      
	      setup-options
	      (lambda(options)
		(setf options (or options (create)))
		(setf options (chain ,$ (union options (@ ,$ ajax-options)))))
	      
	      ;;
	      serilize
	      (lambda(obj)
		(print obj)
		(var ret (array))
		(var len (length obj))
		(var elem)
		;;如果是数组， 假设是只包含表单元素的数组
		(cond((chain ,$ (is-array obj)) 
		      (dotimes(i len)
			(setf elem (elt obj i))
			(chain ret (push (+ (encode-u-r-i-component (@ elem name))
					    "="
					    (encode-u-r-i-component (@ elem value)))))))
		     ;;如果是对象，序列化对象，不管嵌套
		     ((chain ,$ (is-plain-object obj))
		      (for-in(i obj)
			     (chain ret (push (+ (encode-u-r-i-component i)
						 "="
						 (encode-u-r-i-component (elt obj i)))))))		     
		     (t obj));; string or null
		     
		(chain ret (join "&")))
		     
	      http-success
	      (lambda(r)
		(var status (@ r status)) 
		(if(or
		    ;;正常情况
		    (and (>= status 200) (< status 300))
		    ;;not modified
		    (= status 304)
		    ;;safari not modified
		    (and (>= (chain (@ navigator user-agent)(to-lower-case)(index-of "safari")) 0)
			 (undefined status)) 
		    ;;本地文件
		    (and (not status) (= (@ location protocol) "file:")))
		   true 
		   false))
		
	      http-data
	      (lambda(r data-type)
		(when data-type (setf data-type (chain data-type (to-lower-case))))
		(var ct (chain r (get-response-header "content-type")))
		;;如果没提供type则默认返回xml
		(var data (if (or (null data-type) (= data-type "xml"))
			      (@ r response-x-m-l)
			      (@ r response-text)))
		(when (= data-type "json") (setf data (chain *j-s-o-n (parse data))))
		(when (= data-type "script") (chain eval (call window data)))
		data)


	      ajax
	      (lambda(options)
		(chain ,$ (setup-options options))
		(var xhr (chain ,$ (create-x-h-r)))
		(var url (@ options url))
		(var type (chain options type (to-upper-case)))
		(var data (@ options data))
		(when(and (= type "GET") data)
		  (setf url (+ url "?" (chain ,$ (serilize data)))))
		(clear-timeout (@ xhr timer))
		(var request-done false)
		(setf (@ xhr timer) (set-timeout (lambda() (setf request-done true)) 
						 (@ options timeout)))
		;;初始化请求
		(chain xhr (open type url true))
		(when(= type "POST")
		  (chain xhr (set-request-header (create "Content-type" "text/plain:charset=UTF-8"))))
		;;; 配置请求
		(setf (@ xhr onreadystatechange)
		      (lambda()
			(when(and (= (@ xhr ready-state) 4)
				  (not request-done))
			  (if(chain ,$ (http-success xhr))
			     (chain options (success (chain ,$ (http-data xhr (@ options data-type)))))
			     (chain options (error xhr)))
			  (chain options (complete xhr))
			  (setf xhr null))))	       
			       
		;;发起请求
		(if(= type "GET")
		   (chain xhr (send null))
		   (chain xhr (send data))))
	      
	      ;;使用GET方式来进行异步请求
	      get
	      (lambda(options)
		(chain ,$ (ajax (chain ,$ (union (create type "GET") options)))))


	      ;;使用POST方式来进行异步请求
	      post	      
	      (lambda(options)
		(chain ,$ (ajax (chain ,$ (union (create type "POST") options)))))
	      
	      )))))


