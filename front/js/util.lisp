(in-package #:web)
;;;the libarary util
(defvar $ 'claudio)

(defvar *js-lib-macros* nil)
(defvar *js-lib-plugins* nil)
(defvar *node-macros* nil)
(defmacro extend-lib((name &key plugin node ) &body body)
  `(progn
     (cond(,plugin (pushnew (list ',name) *js-lib-plugins* :test #'equal))
	  (,node (pushnew (list ',name) *node-macros* :test #'equal))
	  (t (pushnew (list ',name) *js-lib-macros* :test #'equal)))		  
     (defpsmacro ,name ()
	 ,@body)))


		 
(defpsmacro init-lib()
  `(progn 
     ((lambda()
      (var ,$ (lambda(selector)
		(new (chain ,$ fn (init selector)))))
      ;;array methods
      (var deleted-ids (array))
      ;;for IE8- bug
      (var index-of (or (@ deleted-ids index-of)
			(lambda(elem)
			  (var len (length this))
			  (var ret -1)
			  (dotimes(i len)
			    (when (= elem (elt this i))
			      (setf ret i)
			      (break)))
			  ret)))
      (var slice (@ deleted-ids slice))
      (var splice (@ deleted-ids splice))
      (var push (@ deleted-ids push))
      (var concat (@ deleted-ids concat))
      ;;object methods
      (var class2type (create))
      (var to-string (@ class2type to-string))
      (var has-own (@ class2type has-own-property))
      ;;;;;standard-mode + ***
      (var support (create))
      ;;;;;;;;;;;
      (var doc-elem (@ document document-element))
      (var doc-body (@ document body))
      ;;;
     ;;;;prototype properties
      (setf (@ ,$ fn)
	    (create
	     constructor ,$	     
	     length 0
	     
	     init
	     (lambda(selector context)
	       (cond((not selector) this)
		    (t (chain this (push-stack (chain ,$ (find selector context)))))))
	     
	     ;;;make HTMLCollection and NodeList results as the Object
	     ;;;depends on merge
	     push-stack
	     (lambda(elems)
	       (var ret (chain ,$ (merge (chain this (constructor)) elems)))
	       (setf (@ ret prev-object) this)
	       ret)
	     
	     ;;;;;通过CSS选择器过滤 
	     filter
	     (lambda(selector)
	       (chain this (push-stack (chain ,$ (filter selector this)))))
	     
	     find
	     (lambda(selector)
	       (chain this (map (lambda(elem i)
				  (chain ,$ (find selector elem))))))

	     ;;追加
	     add
	     (lambda(selector)
	       (chain this (push-stack (chain ,$ (add selector this)))))
	     
	     
	     add-self
	     (lambda()
	       (chain ,$ (unique (chain ,$ (merge (or (@ this prev-object) (chain this (constructor)))
				this)))))

	     ;;历遍
	     map
	     (lambda(callback unique);;the arg: for remove duplicates if true
	       (chain this (push-stack (chain ,$ (map this (lambda(elem i)
							       (chain callback (call elem elem i)))

						      unique))))) 	     
	     each
	     (lambda(callback)
	       (chain ,$ (each this callback)))

	     ;;;return nth element of this object as an new Object
	     ;;;support negtive index, if the index is not number, return an object with no element
	     eq
	     (lambda(i)
	       (var len (length this))
	       (var j (+ (+ i 0) (if (< i 0) len 0)));;;NaN may happens
	       (chain this (push-stack (if (and (>= j 0) (< j len)) (array (elt this j)) (array)))))
	     
	     first
	     (lambda() (chain this (eq 0)))
	     last
	     (lambda() (chain this (eq -1)))
	     
	     end
	     (lambda() (or (@ this prev-object) (chain this (constructor null))))

	     get
	     (lambda(num)
	       (if (not (null num))		   
		   (if (>= num 0) (elt this num) (elt this (+ (length this) num)))
		   (chain slice (call this))));;clean Array
	     
	     index
	     (lambda(elem)
	       (var len (length this))
	       (var ret -1)
	       (dotimes(i len)
		 (when (= elem (elt this i))
		   (setf ret i)
		   (break)))
	       ret)
	     
	     slice
	     (lambda()
	       (chain this (push-stack (chain slice (apply this arguments)))))

	     to-array
	     (lambda()
	       (chain slice (call this)))

	     ;;扩展, simple version than jQuery
	     extend
	     (lambda(options)
	       (for-in (i options)
		       (when (chain has-own (call options i));;   (chain options (has-own-property i))
			 (setf (elt this i) (elt options i))))
	       this)
	    

	     ))

      (setf (@ ,$ prototype) (@ ,$ fn));;** !important
      (setf (@ ,$ fn init prototype) (@ ,$ fn));;** !important
      (setf (@ ,$ extend) (@ ,$ fn extend));;for extend static properties
      ;;(var comment "-----------------------------------------------------------------")
	;;;;;;;;;;;;;;;;
      (chain ,$ (extend 
		 (create
			     
		  ;;;历遍元素
		  each 
		  (lambda(obj callback)
		    (var len (length obj))
		    (var is-array (chain ,$ (is-array-like obj)))
		    (var value)
		    (if is-array
			(dotimes(i len)
			  (setf value (chain callback (call (elt obj i) (elt obj i) i)))
			  (when (= value false)
			    (break)))
			(for-in (i obj)
				(setf value (chain callback (call (elt obj i) (elt obj i) i)))
				(when (= value false)
				  (break))))
		    obj)
		  
		  map
		  (lambda(obj callback &optional(unique false))
		    (var len (length obj))
		    (var is-array (chain ,$ (is-array-like obj)))
		    (var ret (array))
		    (var value)
		    (if is-array
			(dotimes(i len)			  
			  (setf value (callback (elt obj i) i))
			  (unless(null value) (chain ret (push value))))
			(for-in (i obj)				
				(setf value (callback (elt obj i) i))
				(unless(null value) (chain ret (push value)))))
		    (setf ret (chain concat (apply (array) ret)))
		    (when (= unique true) (setf ret (chain ,$ (unique ret))))
		    ret)

		  map-one ;;只返回第一个值
		  (lambda(obj callback)
		    (var elem0 (elt obj 0))
		    (when elem0
		      (chain callback (call elem0 elem0 0))))

		  ;;筛选
		  grep;;相当与remove-if 和 remove-if-not 的结合, 通过改变invert的真价值实现
		  (lambda(elems callback invert)
		    (var callback-invese)
		    (var len (length elems))
		    (var callback-expect (not invert))
		    (var elem)
		    (var matches (array))
		    (dotimes(i len)
		      (setf elem (elt elems i)
			    callback-invese (not (callback elem i)))
		      (unless(= callback-invese callback-expect)
			(chain matches (push elem))))
		    matches)
			

		  ;;;删除重复
		  unique ;;test--> ===, like remove-duplicates, also remove false value
		  (lambda(array) ;;supply second arguments as /true/ for remove duplicates
		    ;;remove duplicates and null in the array
		    (var elem)
		    (var former)
		    (var later)
		    (dotimes(i (length array))
		      (setf elem (elt array i))
		      (setf former (chain slice (call array 0 i)))
		      (setf later (chain slice (call array (+ i 1))))		    
		      (when(or ;;(not elem)
			       (> (chain index-of (call former elem)) -1)
			       (> (chain index-of (call later elem))  -1))
			(chain splice (call array i 1))
			(decf i)));;;for ensure check all elements
		    array)
		  
		  some
		  (or (@ *array some)
		      (lambda(arr callback)
			(var len (length arr))
			(var ret false)
			(dotimes(i len)
			  (when (callback (elt arr i) i arr)
			    (setf ret true)
			    (break)))
			ret))
		  every
		  (or (@ *array every)
		      (lambda(arr callback)
			(var ret true)
			(var len (length arr))
			(dotimes(i len)
			  (unless (callback (elt arr i) i arr)
			    (setf ret false)
			    (break)))
			ret))
		  ;;;;检查对象类型
		  type
		  (lambda(obj)
		    (var type (typeof obj))
		    (cond((null obj) (+ obj ""))
			 (t (if (or (= type "object") (= type "function"))
				(or (elt class2type (chain to-string (call obj))) "object")
				type))))
		  is-array
		  (or (@ *array is-array) ;;Array.isArray 只是个函数 不是prototype方法
		      (lambda(obj) (= (chain ,$ (type obj)) "array")))
		  
		  ;;;"1213" "0x221" 等包含纯数字的字符串也会返回true
		  is-numeric
		  (lambda(obj)
		    (and (not (chain ,$ (is-array obj)))
				    (>= (+ (- obj (parse-float obj)) 1) 0)));;???
		  
		  is-function
		  (lambda(obj) (= (chain ,$ (type obj)) "function"))

		  is-window
		  (lambda(obj)
		    (and (not (null obj))
			 (= obj (@ obj window))))
		  
		  is-empty-object
		  (lambda(obj)
		    (var ret true)
		    (for-in (name obj) (setf ret false) (break))
		    ret)
		  
		  is-array-like
		  (lambda(obj)
		    (var length (and (in "length" obj) (@ obj length)))
		    (var type (chain ,$ (type obj)))
		    (cond((or (= type "function") (chain ,$ (is-window obj))) false)
			 ((and (= (@ obj node-type) 1) length) true);;; form element
			 (t (or (= type "array")
				
				(= length 0)
				(and (= (typeof length) "number")
				     (> length 0)
				     (in (- length 1) obj))))))
		  
		  
		  is-plain-object
		  (lambda(obj)
		    (when (or (not obj) 
			      (not (= (chain to-string (call obj)) "[object Object]"))
			      (@ obj node-type)
			      (chain ,$ (is-window obj)))
		      (return false))
		    (try 
		     (when (and (@ obj constructor)
				(not (chain has-own (call obj "constructor")))
				(not (chain has-own (call (@ obj constructor prototype ) "isPrototypeOf"))))
		       (return false))
		     (:catch (e) (return false)))
		    (for-in (key obj))
		    (or (undefined key) (chain has-own (call obj key))))

		  is-h-t-m-l
		  (lambda(str)
		    (var pattern (regex "/<[a-z][\\s\\S]*>/i"))
		    ;;(var pattern (regex "((<\\w+>)\\w*\\1)\\2*"))
		    (and (stringp str) (chain pattern (test str))))

		  ;;创建
		  merge;;merge two array like object: with numeric index
		  (lambda(first second)
		    (var len (+ (length second)))
		    (var j 0)
		    (var i (length first))
		    (while(< j len);;always return false for NaN for NodeList in IE<9
		      (setf (elt first i) (elt second j))
		      (incf i) (incf j))
		    (unless (= len len);;NaN: for IE<9 the NodeList have no length property with the value NaN
		      (while(not (= undefined (elt second j)))
			(setf (elt first i) (elt second j))
			(incf i) (incf j)))
		    (setf (length first) i)
		    first)
		  
		  ;;;融合两个对象，如果obj2不存在返回obj1,如果两个对象存在相同属性，使用obj1的
		  ;;如果是数组 保证顺序 但obj2中重复的原子会被忽略
		  union
		  (lambda(obj1 obj2)
		    ;;对象
		    (when(and (chain ,$ (is-plain-object obj1))
			      (chain ,$ (is-plain-object obj2)))
		      (for-in(i obj2)
			     (unless (in i obj1)
			       (setf (elt obj1 i) (elt obj2 i)))))
		    ;;数组
		    (when(and (chain ,$ (is-array obj1))
			      (chain ,$ (is-array obj2)))
		      (var len (length obj2))
		      (var value)
		      (dotimes(i len)
			(setf value (elt obj2 i))
			(unless(chain ,$ (in-array value obj1))
			  (setf (elt obj1 (length obj1)) value))))

		    obj1)
			  
		    
		  make-array
		  (lambda(arr result)
		    (setf ret (or result (array)))
		    (unless(null arr)
		      (if (chain ,$ (is-array-like arr))
			  (chain ,$ (merge ret (if (stringp arr) (array arr) arr)))
			  (chain ret (push arr))))
		    ret)

		  ;;;NodeList HTMLCollection -> Array 专用
		  to-array;;for IE<9 the NodeList with length as NaN		  
		  (lambda(array-like)
		    (try (chain slice (call array-like))
			 (:catch (e)
			   (chain ,$ (merge (array) array-like)))))

		  in-array
		  (lambda(elem arr)
		    (var len (length arr))
		    (var ret false)
		    (cond((chain ,$ (is-function elem))
			  (dotimes(i len)
			    (when (elem (elt arr i))
			      (setf ret true)
			      (break)))
			  ret)
			 (t (if (> (chain index-of (call arr elem)) -1)
				true 
				false))))
		  ;;获得elements

		  find 
		  (lambda(selector context)
		    (cond((not selector) (array))
			 ((chain ,$ (is-h-t-m-l selector))
			  (chain ,$ (to-array (chain ,$ dom (fragment selector) child-nodes))))
			 ((stringp selector)
			   (chain ,$ (to-array (chain (or context document) (query-selector-all selector)))))
			 ((@ selector node-type) (array selector))
			 (t (array))))
		   
		  is-match ;;match css selector
		  (lambda(selector elem)
		    (setf parent (or (@ elem parent-node) document))		    
		    (var ret (chain ,$ (find selector parent)))
		    (chain ,$ (in-array elem ret)))

		  filter
		  (lambda(selector elems)
		    (chain ,$ (grep elems (lambda(elem i)
					    (chain ,$ (is-match selector elem))))))
		  add
		  (lambda(selector elems)
		    (chain ,$ (unique (chain ,$ (make-array (chain ,$ (find selector)) elems)))))
		  
		  support support
		    )))
            ;;simple version to to determine type of object
      (chain ,$ (each (chain "Boolean Number String Function Array Date RegExp Object Error" (split " ")) 
		      (lambda(name i)
			(setf (elt class2type (+ "[object " name "]")) (chain name (to-lower-case))))))
      
      ;;;
      ,@*js-lib-macros*
      ;;;export to window
      (setf (@ window ,$) ,$
	    (@ window $) (@ window ,$))
      ;;;return it
      ,$

      ))
     ;;;;;;;;;;;插件
     ,@*js-lib-plugins*))

;;;;;;;;;;;;;;;;;helper PS macros


(defpsmacro $$(selector &rest args)
  `(chain ($ ,selector) ,@args))


(defpsmacro $$*(&rest args)
  `(chain $ ,@args))



