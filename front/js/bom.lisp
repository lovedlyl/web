(in-package #:web)

(defpslibmacro(bom)
    `(create
   ;;;;return location query string as object
     query-string-args
     (lambda()
       (var search (@ location search))
       (var qs (if (> (length search) 0)
		   (chain search (slice 1))
		   ""))
       (cond((= qs "") null)
	    (t (var args (create))
	       (var items (chain qs (split "&")))
	       (when(> (length items) 0)
	       (do*((i 0 (incf i))
		    (len (length items)))
		   ((>= i len) args)
		 (var item (chain (elt items i) (split "=")))		
		 (var name (decode-u-r-i-component (elt item 0)))
		 (var value (decode-u-r-i-component (elt item 1)))
		 (when(length name)
		   (setf (elt args name) value)))))))
     ;;;check plugins
     has-plugin
     (lambda(normal-name *i-e-name)
       (cond((@ navigator plugins)
	     (cond((= (@ navigator plugins length) 0) false)
		  (t     
		   (setf normal-name (chain normal-name (to-lower-case)))
		   (var plugins (@ navigator plugins))
		   (var has false)
		   (do*((i 0 (incf i))
			(len (length plugins)))
		       ((>= i len))
		     (if(> (chain (elt plugins i) name (to-lower-case) (index-of normal-name)) -1)
			(setf has true)))
		   has)))
	    (t
	     (try (not (null (new (*active-x-object *i-e-name))))
		  (:catch(ex) false)))))
     ;;;Screen Object
     ;;availWidth(avialHeight):The pixel width(height) of the screen minus system elements such as Windows (read only).
     ;;width(height): The pixel width(height) of the screen.
     avail-screen-dimension
     (lambda()
       (create width (@ screen avail-width) height (@ screen avail-height)))
     screen-dimension
     (lambda()
       (create width (@ screen width) height (@ screen height)))

     ))

     
     
	       

;;;;;--------------------------------------------
(defpsmacro query-string-args()
  `(chain ,*lib-name* bom (query-string-args)))

(defpsmacro has-plugin(normal-name *i-e-name)
  `(chain ,*lib-name* bom (has-plugin ,normal-name ,*i-e-name)))

;;;The screen object
(defpsmacro avail-screen-dimension()
  `(chain ,*lib-name* bom (avail-screen-dimension)))
(defpsmacro screen-dimension()
  `(chain ,*lib-name* bom (screen-dimension)))

