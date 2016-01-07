;;;; web.asd

(asdf:defsystem #:web
  :description "Describe web here"
  :author "claudio<lovedlyl@163.com>"
  :license "Specify license here"
  :depends-on (#:parenscript
               #:sexml
               #:cl-css
	       #:quickproject)
  :serial t
  :components ((:file "package")
	       ;;;;;;;;;;;;
	       ;;for both
	       ;;;;;;;;;;;
	       (:file "util")
	       (:file "web")
	       (:file "js-api")
	       ;;css
	       (:file "front/css/util")
	       (:file "front/css/help")

	       ;;js
	       (:file "front/js/util")	     
	       (:file "front/js/dom")
	       (:file "front/js/style")
	       (:file "front/js/event")
	       (:file "front/js/ajax")
	       ;;插件
	       (:file "front/js/plugins/check")
	      ;; (:file "front/js/form")
	       ;;(:file "front/js/canvas")
;;
;;	       (:file "front/js/drag")
;;	       (:file "front/js/css")
;;	       (:file "front/js/jquery")
;;	       ;;------------------------------------------------	  
	       ;;;;;;;;;;;;;
	       ;;for node
	       ;;;;;;;;;;;;
	       (:file "node/util")
	       (:file "node/http")
;;	       ;;(:file "node/node-api")
;;	       (:file "node/url")
;;	       (:file "node/require")
;;	       (:file "node/querystring")
;;	       (:file "node/fs")
;;	       (:file "node/server")
;;	       (:file "node/path")
;;	       (:file "node/socket")
	       ;;-------------------------------------------------
               ))

