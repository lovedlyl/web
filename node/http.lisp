(in-package #:web)

(defpsmacro http(callback &key
			  (port 8080))
  (let((message (format nil  "server running at ~A" port)))
  `(progn 
     (chain http
	    (create-server ,callback)
	    (listen ,port))
     (print ,message))))
     
