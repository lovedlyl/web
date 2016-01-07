(in-package #:web)

;;;quirk bugs check
`(defpsmacro (quirk-check)
    `(create
      ;;<= IE8
      has-dont-enum-quirk 
      ((lambda()
	 (var o (create to-string (lambda())))
	 (var result true)
	 (for-in 
	  (prop o)
	  (when(= prop "toString")
	    (setf result false)))
	 result))
	    ;;;Safari 3
      has-enum-shadows-quirk
      ((lambda()
	 (var o (create to-string (lambda())))
	 (var count 0)
	 (for-in
	  (prop o)
	  (when(= prop "toString")
	    (incf count)))
	 (> count 2)))))

	    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;userAgent check
(extend-lib
    (browser-check :plugin t)
  `(chain ,$ 
	  (extend
	   (create
	    browser
	    ((lambda()
	       ;;render engin
	       (var engine (create ie 0 gecko 0 webkit 0 khtml 0 opera 0
				   ver null))
	       ;;browsers
	       (var browser (create ie 0 firefox 0 safari 0 konq 0 opera 0
				    Chrome 0 ver null))
	       (var system (create win false mac false x11 false
				   ;;mobile
				   iphone false ipad false ipod false
				   ios false android false nokia-n false
				   win-mobile false
				   ;;game systems
				   wii false ps false))
	      ;;;;;;detect redering engine/browswers
	       (var ua (@ navigator user-agent))
	       (cond((@ window opera)
		     (setf (@ engine ver) (chain window opera (version))
			   (@ browser ver) (chain window opera (version))
			   (@ engine opera) (parse-float (@ engine ver))
			   (@ browser opera) (parse-float (@ engine ver))))
		    ;;1--------------
		    ((chain (regex "AppleWebKit\\/(\\S+)") (test ua))
		     (setf (@ engine ver) (elt *reg-exp "$1")
			   (@ engine webkit) (parse-float (@ engine ver)))
	     ;;;figure out if it's chrome or safari
		     (cond((chain (regex "Chrome\\/(\\S+)") (test ua))
			   (setf (@ browser ver) (elt *reg-exp "$1")
				 (@ browser chrome) (parse-float (@ browser ver))))
			  ((chain (regex "Version\\/(\\S+)") (test ua))
			   (setf (@ browser ver) (elt *reg-exp "$1")
				 (@ browser safari) (parse-float (@ browser ver))))
			  (t ;;approximate version
			   (var safari-version 1)
			   (cond((< (@ engine webkit) 100) (setf safari-version 1))
				((< (@ engine webkit) 312) (setf safari-version 1.2))
				((< (@ engine webkit) 412) (setf safari-version 1.3))
				(t (setf safari-version 2)))
			   (setf (@ browser safari) safari-version
				 (@ browser ver) safari-version))))
		    ;;2--------------------------
		    ((or (chain (regex "KHTML\\/(\\S+)") (test ua))
			 (chain (regex "Konqueror\\/([^;]+)") (test ua)))
		     (setf (@ engine ver) (elt *reg-exp "$1")
			   (@ browser ver) (elt *reg-exp "$1")
			   (@ engine khtml) (parse-float (@ engine ver))
			   (@ browser konq) (parse-float (@ engine ver))))
	     ;;;3------------------------
		    ((chain (regex "rv:([^\\)]+)\\) Gecko\\/\\d{8}") (test ua))
		     (setf (@ engine ver) (elt *reg-exp "$1")
			   (@ engine gecko) (parse-float (@ engine ver)))
	      ;;;determine if it is firefox
		     (when(chain (regex "Firefox\\/(\\S+)") (test ua))
		       (setf (@ browser ver) (elt *reg-exp "$1")
			     (@ browser firefox) (parse-float (@ browser ver)))))
		    ;;4--------------------------------
		    ((chain (regex "MSIE ([^;]+)") (test ua))
		     (setf (@ engine ver) (elt *reg-exp "$1")
			   (@ browser ver) (elt *reg-exp "$1")
			   (@ engine ie) (parse-float (@ engine ver))
			   (@ browser ie) (parse-float (@ engine ver)))))
	;;;;;;;;detect browswers
	       (setf (@ browser ie) (@ engine ie)
		     (@ browser opera) (@ engine opera))
	;;;;;;;;;;detect platform
	       (var p (@ navigator platform))
	       (setf (@ system win) (= (chain p (index-of "Win")) 0)
		     (@ system mac) (= (chain p (index-of "Mac")) 0)
		     (@ system x11) (or (= p "X11") (= (chain p (index-of "Linux")) 0)))
	;;;;;;detect windows operations systems
	       (when(@ system win)
		 (when(chain (regex "Win(?:dows )?([^do]{2})\\s?(\\d+\\.\\d+)?") (test ua))
		   (cond((= (elt *reg-exp "$1") "NT")
			 (switch (elt *reg-exp "$2")
			   ("5.0" (setf (@ system win) "2000") break)
			   ("6.0" (setf (@ system win) "Vista") break)
			   ("6.1" (setf (@ system win) "7") break)
			   (default    (setf (@ system win) "NT") break)))
			((= (elt *reg-exp "$1") "9x")
			 (setf (@ system win) "ME"))
			(t (setf (@ system win) (elt *reg-exp "$1"))))))
	;;;;mobile device
	       (setf (@ system iphone) (> (chain ua (index-of "iPhone")) -1) 
		     (@ system ipod) (> (chain ua (index-of "iPod")) -1)
		     (@ system ipad) (> (chain ua (index-of "iPad")) -1)
		     (@ system nokia-n) (> (chain ua (index-of "NokiaN")) -1))
	;;;windows mobile
	       (cond((= (@ system win) "CE")
		     (setf (@ system win-mobile) (@ system win)))
		    ((= (@ system win) "Ph")
		     (when (chain (regex "Windows Phone OS (\\d+.\\d+)") (test ua))
		       (setf (@ system win) "Phone"
			     (@ system win-mobile) (parse-float (elt *reg-exp "$1"))))))
	;;;;determine iOS version
	       (when(and (@ system mac) (> (chain ua (index-of "Mobile")) -1))
		 (if(chain (regex "CPU (?:iPhone )?OS (\\d+_\\d+)") (test ua))
		    (setf (@ system ios) (parse-float (chain *reg-exp $1 (replace "_" "."))))
		    (setf (@ system ios) 2)));; can not really detect - so guess
	;;;determine Android version
	       (when (chain (regex "Android (\\d+\\.\\d+)") (test ua))
		 (setf (@ system android) (parse-float (elt *reg-exp "$1"))))
	;;;gaming systems
	       (setf (@ system wii) (> (chain ua (index-of "Wii")) -1)
		     (@ system ps)  (chain (regex "/playstation/i") (test ua)))
	;;;return it
	       (create engine engine browser browser system system)
	       
	       ))))))

