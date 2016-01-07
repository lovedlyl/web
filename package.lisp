;;;; package.lisp

(defpackage #:web
  (:use #:cl #:parenscript)
  (:export 
   :make-web-project
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   :js2file :js2string :json2string :json2file 
   :$$ :$$* :init-lib

   :html2file :html2string :html-frame

   :css2file :css2string

   ;;-----------------
   ;;;;;;js
   ;;;;macro names define by defpsmacro

   ;;css
   :opacity :box-sizing

   ;;;node
   :node-lib :http
))

;;;;;;;;;;;;;;
(sexml:with-compiletime-active-layers
    (sexml:standard-sexml sexml:xml-doctype)
  (sexml:support-dtd
   (merge-pathnames "html5.dtd" (asdf:system-source-directory "sexml"))
   :<))
