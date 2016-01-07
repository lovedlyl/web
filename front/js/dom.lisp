(in-package #:web)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(extend-lib
    (dom-core-proto)
  `(progn
     ;;;;查找元素
     (chain ,$ (each (create    
		      parent 
		      (lambda(elem)
			(var parent (@ elem parent-node))
			(if (and parent (not (= (@ parent node-type) 11)));;11-> documentFragment
			    parent
			    null))

		      parents
		      (lambda(elem)
			(chain ,$ dom (dir elem "parentNode")))

		      prev
		      (lambda(elem)
			(chain ,$ dom (sibling elem "previousSibling")))

		      next
		      (lambda(elem)
			(chain ,$ dom (sibling elem "nextSibling")))

		      prev-all
		      (lambda(elem)
			(chain ,$ dom (dir elem "previousSibling")))

		      next-all
		      (lambda(elem)
			(chain ,$ dom (dir elem "nextSibling")))

		      siblings
		      (lambda(elem)
			(chain ,$ dom (next-siblings (@ (or (@ elem parent-node) (create)) first-child) elem)))
		      
		      childern
		      (lambda(elem)
			(chain ,$ dom (next-siblings (@ elem first-child)))))
		     ;;end methods
		     (lambda(fn name)
		       (setf (elt (@ ,$ fn) name)
			     (lambda(selector)
			       (var ret (chain ,$ (map this fn)))
			       (when (= (typeof selector) "string")
				 (setf ret (chain ,$ (filter selector ret))))
			       (when (> (length this) 1)
				 (setf ret (chain ,$ (unique ret))))
			       (chain this (push-stack ret)))))))
     ;;;;;;;;;;;;;;;;;属性和类
     (chain ,$ fn (extend 
		   (create 
                   ;;;属性
		    attr 
		    (lambda(name value)
		      (var attr (@ ,$ dom attr));;缓存多用
		      (if (not (undefined value))
			  (chain this (each (lambda(elem) (attr elem name value))))
			  (cond((chain ,$ (is-plain-object name)) (chain this (each (lambda(elem) (attr elem name)))))
			       ((stringp name) (chain ,$ (map-one this (lambda(elem) (attr elem name)))))
			       (t this))))
		    
		    remove-attr
		    (lambda(name)
		      (chain this (each (lambda(elem) (chain ,$ dom (remove-attr elem name)))))))))
     
     
     ;;类相关
     (chain ,$ (each (chain "hasClass removeClass addClass toggleClass" (split " ")) 
		     (lambda(fn-name i)
		       (setf (elt (@ ,$ fn) fn-name)
			     (if (= fn-name "hasClass")
				 (lambda(class-name)	      		   
				   (var ret (chain ,$ (map this (lambda(elem) (chain ,$ dom  (has-class elem class-name))))))
				   (if (chain ,$ (some ret (lambda(e) (= e true))));;一些为真就真
				       true false))
				 (lambda(class-name)
				   (chain this (each (lambda(elem) ((elt (@ ,$ dom) fn-name) elem class-name))))))))))

     ;;;;;;DOM操作          
     ;;插入 html 选择
     (chain ,$ (each (create before "before" prepend "atStart"
			     append "atEnd" after "after")
		     (lambda(location name)
		       (setf (elt (@ ,$ fn) name)
			     (lambda(html)
			       (chain this (each (lambda(elem) (chain ,$ dom (locate-to elem html location))))))))))
     ;;插入 selector 选择
     (chain ,$ (each (create 	   
		      append-to
		      (lambda(elem target)
			(chain target (append-child elem)))

		      prepend-to
		      (lambda(elem target)
			(chain target (insert-before elem (@ target first-child))))

		      insert-before
		      (lambda(elem target)
			(chain (@ target parent-node) (insert-before elem target)))

		      insert-after
		      (lambda(elem target)
			(chain (@ target parent-node) (insert-before elem (chain ,$ dom (sibling target "nextSibling")))))
		      ;;;替换
		      replace-all
		      (lambda(elem target)
			(chain (@ target parent-node) (replace-child elem target)))
		      )
			   ;;;end methods
		     (lambda(fn name)
		       (setf (elt (@ ,$ fn) name)
			     (lambda(selector)
			       (chain this (push-stack (chain ,$ dom (insert-to this selector fn)))))))))

     ;;逆反
     (setf (@ ,$ fn reverse)
	   (lambda()
	     (chain this (each (lambda(elem) 
				 (var fragment (chain document (create-document-fragment)))
				 (while(@ elem last-child)
				   (chain fragment (append-child (@ elem last-child))))
				 (chain elem (append-child fragment)))))))
     ;;移除
     (chain ,$ fn (extend 
		   (create remove 
			   (lambda()
			     (chain this (each (lambda(elem) (chain elem parent-node (remove-child elem))))))
			   empty
			   (lambda()
			     (chain this (each (lambda(elem)
						 ;;(setf (@ elem inner-h-t-m-l) "")))));;;????
						 (while (@ elem first-child)
						   (chain elem (remove-child (@ elem first-child))))))))
			   )))

     
     ;;包裹
     (chain ,$ (each (chain "wrap wrapInner" (split " "))
		     (lambda(name)
		       (setf (elt (@ ,$ fn) name)
			     (lambda(html)
			       (chain this (each (lambda(elem) ((elt (@ ,$ dom) name) elem html)))))))))
     (setf (@ ,$ fn wrap-all)
	   (lambda(html)
	     (chain ,$ dom (wrap-all this html))))
     
     ;;替换
     (setf (@ ,$ fn replace-with)
	   (lambda(html)
	     (chain ,$ dom (replace-with this html))))
     ;;克隆
     (setf (@ ,$ fn clone)
	   (lambda()
	     (chain this (map (lambda(elem) (chain elem (clone-node true)))))))

     ;;html && text
     (chain ,$ fn (extend
		   (create		    
		    html
		    (lambda(new-html)
		      (if new-html
			  (chain this (each (lambda(elem) (setf (@ elem inner-h-t-m-l) new-html))))
			  (@ (elt this 0) inner-h-t-m-l)))
		    text
		    (lambda(new-text)
		      (var text (@ ,$ dom text));;缓存多用
		      (if new-text
			  (chain this (each (lambda(elem) (text elem new-text))))
			  (chain ,$ (map this (lambda(elem) (text elem))) (join "")))))))
     
     
     (chain ,$ (each (chain "outerHtml outerText" (split " "))
		     (lambda(name)
		       (setf (elt (@ ,$ fn) name)
			     (lambda(value)
			       (if value 
				   (chain this (each (lambda(elem) ((elt (@ ,$ dom) name) elem value))))
				   (chain ,$ (map this (lambda(elem) ((elt (@ ,$ dom) name) elem ))) (join ""))))))))
     ;;dataset
     (setf (@ ,$ fn dataset)
	   (lambda(name)
	     (elt (chain ,$ (map-one this  (lambda(elem) (chain ,$ dom (dataset elem))))) name)))
     
     ;;;;;;;;;;;;;DOM中样式相关
     ;;(print support)
     ))


(extend-lib
    (dom-core)
  `(chain ,$
	  (extend
	   (create 
	    dom 
	    (create	    
	     dir;;根据dir(如 “parentNode”) 找到满足条件的所有元素
	     (lambda(elem dir)
	       (var matched (array))
	       (var cur (elt elem dir))
	       (while(and cur 
			  (not (= (@ cur node-type) 9))) ;;;nodeType9-> document
		 (when(= (@ cur node-type) 1) ;;保证是element类型	
		   (chain matched (push cur)))
		 (setf cur (elt cur dir)))
	       matched)
	     
	     sibling;;根据dir（“previousSibling” 或 “nextSibling”)找到相邻元素
	     (lambda(elem dir)
	       (setf elem (elt elem dir))
	       (while(and elem (not (= (@ elem node-type) 1)))
		 (setf elem (elt elem dir)))
	       elem)

	     next-siblings;;找到后面相邻的所有元素, 不包含自身(实用于childern siblings方法)
	     (lambda(n elem)
	       (var ret (array))
	       (do*((i n (@ i next-sibling)))
		   ((not i))
		 (when (and (= (@ i node-type) 1) (not (= i elem)))
		   (chain ret (push i))))
	       ret)
	     
	   ;;;;属性
	     set-attr
	     (lambda(elem name value)
	       (var to-normal (create for "htmlFor" class "className"))
	       (var un-to-normal (create html-for "html" class-name "class"))
	       (setf name (or (elt to-normal name) name))
	       (setf (elt elem name) value)
	       (when (@ elem set-attribute)
		 (setf name (or (elt un-to-normal name) name))
		 (chain elem (set-attribute name value)))
	       elem)
	     
	     attr
	     (lambda(elem name value)
	       (if name ;;;;name value or values provided 
		   (if (not value)
		       (cond((stringp name)
			     (or (elt elem name) (chain elem (get-attribute name)) ""))
			    ((chain ,$ (is-plain-object name)) 
			     (for-in (i name) (chain ,$ (set-attr elem i (elt name i)))) elem))
		       (chain ,$ dom (set-attr elem name value)) elem)))
	     
	     
	     remove-attr
	     (lambda(elem name)	       
	       (chain elem (remove-attribute name))
	       elem)
	     
	     
	     ;;类
	     ;;does element has a specifc class
	     
	     _is-element-with-class-list	   
	     (lambda()
	       (= (chain *object prototype to-string (call (@ document body class-list)))
		  "[object DOMTokenList]"))

	     has-class
	     (lambda(elem name)
	       ;;(print "not lazy") ;;test for if this function lazy evaled
	       (setf (@ ,$ dom  has-class)
		     (if (chain ,$ dom  (_is-element-with-class-list))
			 (lambda(elem name)
			   (chain elem class-list (contains name)))	      
			 (lambda(elem name)
			   (var pattern (new (*reg-exp (+ "(^|\\s)" name "(\\s|$)"))));;		    
			   (var classes) 
			   (cond((or (= (length name) 0) (> (chain name (index-of " ")) -1)) ;;using multi class names forbidden
				 (throw (new (*error (+ "ivalide class name '" name "'")))))
				(t (setf classes (@ elem class-name))
				   (cond ((= classes "") false)
					 ((= classes name) true)
					 (t (chain pattern (test classes)))))))))
	       (chain ,$ dom (has-class elem name)))

	     ;;add a class to an element, if not already has
	     add-class
	     (lambda(elem name)
	       (setf (@ ,$ dom add-class)
		     (if (chain ,$ (_is-element-with-class-list))
			 (lambda(elem name) 
			   (chain elem class-list (add name))
			   elem);; return the elem
			 (lambda(elem name)
			   (var classes)
			   (unless (chain ,$ dom (has-class elem name))
			     (setf classes (@ elem class-name))
			     (if (= classes "") ;;no class yet
				 (setf (@ elem class-name) name)
				 (incf (@ elem class-name)
				       ;; for class mybe remove by remove-class below
				       (if (= (chain classes (last-indexof " ")) (- (length classes) 1))
					   name 
					   (+ " " name)))))
			   elem)));; return the elem
	       (chain ,$ dom (add-class elem name)))

	     ;;remove a class from an element, if already has
	     remove-class
	     (lambda(elem name)
	       (setf (@ ,$ dom remove-class)
		     (if (chain ,$ dom (_is-element-with-class-list))
			 (lambda(elem name)
			   (chain elem class-list (remove name))
			   elem);;return the elem
			 (lambda(elem name)
			   (var classes)
			   (var pattern (new (*reg-exp (+ "\\s*\\b" name "\\b\\s*") "g")))
			   (when (chain ,$ dom  (has-class elem name))
			     (setf classes (@ elem class-name))
			     (setf (@ elem class-name) (chain classes (replace pattern " "))))
			   elem)))
	       (chain ,$ dom (remove-class elem name)))
	     
	     ;;toggle class from element
	     toggle-class
	     (lambda(elem name)
	       (setf (@ ,$ dom toggle-class)
		     (if (chain ,$ dom (_is-element-with-class-list))
			 (lambda(elem name)
			   (chain elem class-list (toggle name))
			   elem);;return the elem
			 (lambda(elem name)
			   (if (chain ,$ dom (has-class elem name))
			       (chain ,$ dom (remove-class elem name))
			       (chain ,$ dom (add-class elem name)))
			   elem)));;return the elem
	       (chain ,$ dom (toggle-class elem name)))
	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     fragment
	     (lambda(html)
	       (var temp (chain document (create-element "div")))
	       (var frag (chain document (create-document-fragment)))
	       (when html 
		 (setf (@ temp inner-h-t-m-l) html))
	       (while(@ temp first-child)
		 (chain frag (append-child (@ temp first-child))))
	       frag)

	     ;;DOM操作
	     insert
	     (if (chain document (create-element "div") insert-adjacent-h-t-m-l)
		 (create before (lambda(e h) (chain e (insert-adjacent-h-t-m-l "beforebegin" h)))
			 after (lambda(e h) (chain e (insert-adjacent-h-t-m-l "afterend" h)))
			 at-start (lambda(e h) (chain e (insert-adjacent-h-t-m-l "afterbegin" h)))
			 at-end (lambda(e h) (chain e (insert-adjacent-h-t-m-l "beforeend" h))))
		 (create before (lambda(e h) (chain e parent-node (insert-before (chain ,$ dom (fragment h)) e)))
			 after (lambda(e h) (chain e parent (insert-before (chain ,$ dom (fragment h)) (@ e next-sibling))))
			 at-start (lambda(e h) (chain e (insert-before (chain ,$ dom (fragment h)) (@ e first-child))))
			 at-end (lambda(e h) (chain e (append-child (chain ,$ dom (fragment h)))))))

	     locate-to 
	     (lambda(elem html location)
	       ((elt (@ ,$ dom insert) location) elem html)
	       elem)
	     
	     insert-to
	     (lambda(src-elems selector insert-method)
	       (var src-len (length src-elems))
	       (var target-elems (chain ,$ (find selector)))
	       (var target-len (length target-elems))
	       (var ret (array))
	       (var temp)
	       (var fragment)
	       (dotimes(j target-len)
		 (setf fragment (chain document (create-document-fragment)))
		 (dotimes(i src-len)
		   (setf temp (if (< j (- target-len 1)) (chain (elt src-elems i) (clone-node true)) (elt src-elems i)))
		   (chain fragment (append-child temp))
		   (chain ret (push temp)))
		 (insert-method fragment (elt target-elems j)))
	       ;;(insert-method temp (elt target-elems j))));;;generic style here 
	       ret)
	     

	     ;;包裹
	     wrap
	     (lambda(elem html)
	       (var target (elt (chain ,$ (find html)) 0))
	       (var p (@ elem parent-node))
	       (chain p (replace-child target elem))
	       (chain target (append-child elem))
	       elem)

	     wrap-inner
	     (lambda(elem html)
	       (var target (elt (chain ,$ (find html)) 0))
	       (while(@ elem first-child)
		 (chain target (append-child (@ elem first-child))))
	       (chain elem (append-child target))
	       elem)
	     
	     wrap-all
	     (lambda(elems html)
	       (var target (elt (chain ,$ (find html)) 0))
	       (var elem0 (elt elems 0))
	       (var parent (@ elem0 parent-node))
	       (setf (elt elems 0) (chain elem0 (clone-node true)))
	       (var len (length elems))
	       (dotimes(i len)
		 (chain target (append-child (elt elems i))))
	       (chain parent (replace-child target elem0))
	       elems)
	     
	     ;;替换
	     replace-with
	     (lambda(elems html)
	       (var src (chain ,$ dom (fragment html)))
	       (var len (length elems))
	       (var parent)
	       (var elem)
	       (dotimes(i len)
		 (setf elem (elt elems i)
		       parent (@ elem parent-node))
		 (chain parent (replace-child (if (< i (- len 1))
						  (chain src (clone-node true))
						  src)
					      elem)))
	       elems)
	     
	     
	     text
	     (lambda(elem new-text)	     
	       (var _is-support-text-content (@ document body text-content))
	       (setf (@ ,$ dom text)
		     (if _is-support-text-content
			 (lambda(elem new-text)
			   (if new-text
			       (progn (setf (@ elem text-content) new-text) elem)
			       (@ elem text-content)))
			 (lambda(elem new-text)
			   (if new-text
			       (progn (setf (@ elem inner-text) new-text) elem)
			       (@ elem inner-text)))))
	       (chain ,$ dom (text elem new-text)))
	     


	     outer-html
	     (lambda(elem value)
	       (var _is-support-outer-html (chain document (create-element "div") outer-h-t-m-l))
	       (setf (@ ,$ dom outer-html)
		     (if _is-support-outer-html
			 (lambda(elem value)
			   (if value
			       (progn (setf (@ elem outer-h-t-m-l) value) elem)
			       (@ elem outer-h-t-m-l)))
			 (lambda(elem value)
			   (var temp (chain document (create-element "div")))
			   (var temp-html)
			   (var parent)
			   (if value
			       (progn ;;(print "set outer")
				 (setf (@ temp inner-h-t-m-l) value)
				 (setf parent (@ elem parent-node))
				 (while(@ temp first-child)
				   (chain parent (insert-before (@ temp first-child) elem)))
				 (chain parent (remove-child elem))
				 elem)
			       (progn ;;(print "get outer")
				 (chain temp (append-child (chain elem (clone-node true))))
				 (setf temp-html (@ temp inner-h-t-m-l)
				       temp null)
				 elem)))))	     
	       (chain ,$ dom (outer-html elem value)))


	     outer-text
	     (lambda(elem value)
	       (var _is-support-outer-text (@ document body outer-text))
	       (setf (@ ,$ dom outer-text)
		     (if _is-support-outer-text
			 (lambda(elem value)
			   (if value
			       (progn (setf (@ elem outer-text) value) elem)
			       (@ elem outer-text)))
			 (lambda(elem value)
			   ;;(print "firefox")
			   (var temp (chain document (create-element "div")))
			   (var temp-text)
			   (var parent)
			   (if value
			       (progn 
				 (setf parent (@ elem parent-node))
				 (setf value (chain document (create-text-node value)))
				 (chain parent (insert-before value elem))
				 (chain parent (remove-child elem))
				 elem)
			       (progn 
				 (chain temp (append-child (chain elem (clone-node true))))
				 (setf temp-text (chain ,$ dom (text temp))
				       temp null)
				 temp-text)))))
	       (chain ,$ dom (outer-text elem value)))
	     

	     ;;--7 Custom data attributs
	     ;; IE10- supported partially, only can be get individual by getAttribut
	     ;; make it compatible by tranversal the attributes
	     dataset
	     (lambda(elem)
	       (setf (@ ,$ dom dataset)
		     (if (= (chain to-string (call (@ document body dataset)))
			    "[object DOMStringMap]")
			 (lambda(elem) (@ elem dataset))
			 (lambda(elem) ;;otherwise using low level Attributs properties to filter
			   (var attributes (@ elem attributes))
			   (var len (length attributes))
			   (var attr)
			   (var attr-name)
			   (var attr-value)
			   (var result (create))
			   (var prefix "data-")
			   (do*((i 0 (1+ i)))
			       ((>= i len))
			     (setf attr (elt attributes i)
				   attr-name (@ attr node-name)
				   attr-value (@ attr node-value))
			     (when (and (@ attr specified)
					(= (chain attr-name (to-lower-case) (index-of prefix)) 0)
					(> (length attr-name) 5)) ;;the length of prefix
			       (setf (elt result (chain attr-name (slice 5))) attr-value)))
			   result)))
	       (chain ,$ dom (dataset elem)))

	     )))))



