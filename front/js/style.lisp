(in-package #:web)

(extend-lib
    (style-proto)
  `(progn
     
     ;;;获取opacity 和 float支持:support.opacity support.cssFloat
     ((lambda()
	(var div (chain document (create-element "div")))
	(setf (@ div inner-h-t-m-l) "  <link/><table></table><a href='/a'>a</a><input type='checkbox'/>")	
	(var a (elt (chain div (get-elements-by-tag-name "a")) 0))
	(var style (@ a style))
	(setf (@ style css-text) "float:left;opacity:.5")
	;;;;;透明度 和 浮动
	;;透明度获取结果是字符串，所以用equal而不是=
	;;IE<9 currentStyle 也有opacity属性但始终=1
	;;直接在cssText中加入opacity也会出现opacity属性，只有用.5才会判断失败，得到正确结果
	;;由于IE<9不支持opacity所以不会将.5转换为0.5，以此判断是否支持opacity属性
	(setf (@ support opacity) (equal (@ style opacity) "0.5")
	      ;;IE<9 也有opacity属性
	      (@ support css-float) (@ style css-float))
	;;;;;标准模式还是混乱模式
	(setf (@ support standard-mode) (= (@ document compat-mode) "CSS1Compat"))
	(setf div null)))
     ;;元素样式
     (setf (@ ,$ fn css)
	   (lambda(name value)
	     (if (undefined value)
		 (cond((stringp name)
		       (chain ,$ (map-one this (lambda(elem) (chain ,$ style (get-style elem name))))))
		      ((chain ,$ (is-plain-object name))
		       (chain this (each (lambda(elem)  
					   (for-in (i name) (chain ,$ style (set-style elem i (elt name i)))))))))
		 (chain this (each (lambda(elem) (chain ,$ style (set-style elem name value))))))))
     
     ;;页面可视高度和滚动偏移
     (chain ,$ (extend 
		(create
		 
	     ;;;;;;;;;;获取页面滚动坐标
		 ;;;;chrome中documentElement.scrollTop 始终为0
		 get-scroll-offsets
		 (lambda()
		   (setf (@ , $ style get-scroll-offsets)
			 (if false;;(not (null (@ window page-x-offset)))
			     (lambda() (create top (@ window page-y-offset) left (@ page-x-offset)))
			     (lambda()
			       (create top (max (@ doc-elem scroll-top) (@ doc-body scroll-top))
				       left (max (@ doc-elem scroll-left) (@ doc-body scroll-left))))))
		   (chain ,$ style (get-scroll-offsets)))
		 
	     ;;;;;;;;;;获取可视窗口尺寸:所有内容尺寸
		 get-view-port
		 (lambda()
		   (setf (@ ,$ style get-view-port)
			 (if(@ support standard-mode)
			    ;;标准模式
			    (lambda() (create width (@ doc-elem client-width) height (@ doc-elem client-height)))
			    ;;混乱模式
			    (lambda() (create width (@ doc-body client-width) height (@ doc-body client-height)))))
		   (chain ,$ style (get-view-port))))))




     ;;;获取元素内容高度和宽度
     (chain ,$ (each (chain "width height" (split " "))
		     (lambda(name)
		       (setf (elt (@ ,$ fn) name)
			     (lambda(value)
			       (chain this (css name value)))))))

     ;;获取元素内宽和外宽  
     (chain ,$ (each (chain "Width Height" (split " "))
		     (lambda(name)
		       (setf (elt (@ ,$ fn) (+ "inner" name))
			     (lambda()
			       (chain ,$ (map-one this (lambda(elem)
							 (elt (chain ,$ style (inner-dimension elem)) (chain name (to-lower-case))))))))
		       (setf (elt (@ ,$ fn) (+ "outer" name))
			     (lambda()
			       (chain ,$ (map-one this (lambda(elem)
							 (elt (chain ,$ style (outer-dimension elem)) (chain name (to-lower-case)))))))))))
     
     ;;;获取元素偏移量
     (setf (@ ,$ fn offset)
	   (lambda()
	     (chain ,$ (map-one this (lambda(elem)
				       (var cur-left 0)
				       (var cur-top 0)
				       (while elem
					 (incf cur-left (- (@ elem offset-left) (@ elem scroll-left)))
					 (incf cur-top  (- (@ elem offset-top) (@ elem scroll-top)))
					 (setf elem (@ elem offset-parent)))
				       (create left cur-left top cur-top))))))

     
     
     (chain ,$ fn (extend
		   (create 

		    

		    hide
		    (lambda()
		      (chain this (each (lambda(elem)
					  (var cur-display (chain ,$ style (get-style elem "display")))
					  (unless(= cur-display "none")
					    (unless(@ elem $old-display)
					      (setf (@ elem $old-display) cur-display)))
					  (setf (@ elem style display) "none")))))
		    
		    show
		    (lambda()
		      (chain this (each (lambda(elem)
					  (setf (@ elem style display) (or (@ elem $old-display) ""))
					  ))))

		    toggle
		    (lambda()
		      (chain this (each (lambda(elem)
					  (if (= (chain ,$ style (get-style elem "display")) "none")
					      (chain (,$ elem) (show))
					      (chain (,$ elem) (hide)))))))
		    
		   ;;;;;;;;;;;动画效果

		    animate
		    (lambda(target-json opts)
		      ;;;初始化动画参数
		      (var default-opts (create delay 10 duration 300  delta "linear"))
		      (var default-delta (@ ,$ style deltas linear))

		      (setf opts 
			    (if opts
				(chain ,$ (union opts default-opts))
				default-opts))
		      ;;;delta 可以是使用时自己提供的变化函数
		      (var delta (@ opts delta))
		      (when(stringp delta)
			(setf (@ opts delta)
			      (cond((> (chain delta (search (regex "/(circ|quad)/"))) -1)
				    (elt (@ ,$ style deltas) (elt *reg-exp "$1")))
				   ((> (chain delta (search (regex "/(back|elastic)/"))) -1)
				    ;;delta-x 默认为1.5
				    ((elt (@ ,$ style deltas) (elt *reg-exp "$1")) (or (@ opts delta-x) 1.5)))
				   (t default-delta))))
		      
		      (var that this)
		      ;;;所有元素共用一个启始时间
		      (var start (new (*date)))

		      (chain this
			     (each (lambda(elem)
				     (clear-interval (@ elem timer))
				     ;;获取目标值与原始值的差
				     ;;只记录相差不为0的属性
				     (var sub-json (create))
				     (var src-json (create))
				     (var i-sub)
				     (for-in(attr target-json)						 
					    (setf (elt src-json attr)
						  (chain ,$ style (get-style elem attr))))
				     (for-in(attr target-json)						
					    (setf i-sub (- (elt target-json attr) (elt src-json attr)))
					    (unless(= i-sub 0)
					      (setf (elt sub-json attr) i-sub)))
				     ;;;如果没有属性之间有差距，就不进行动画，反正重复点击开销
				     ;;;不必要的CPU
				     (unless(chain ,$ (is-empty-object sub-json))
				       (setf (@ elem timer)
					     (set-interval
					      (lambda()				       
						(var time-passed (- (new (*date)) start))
						(var progress (/ time-passed (@ opts duration)))
						(when(> progress 1) (setf progress 1))
						(var delta (chain opts (delta progress)))
						
		                              ;;;只在相差不为0的属性上进行动画
						(for-in(attr sub-json)
						       (chain ,$ style (set-style elem attr (+ (elt src-json attr)
											       (* (elt sub-json attr) delta)))))
						
						
						(when(= progress 1)					     
						  (clear-interval (@ elem timer))						  
						  (setf (@ that timer) null)
						  (when (@ opts callback)
						    (chain opts callback (call that)))))
					      (@ opts delay))))))))
		      

		    ;;;slideDown slideUp
		    slide-down
		    (lambda(opts)		      
		      (chain this (each (lambda(elem)
					  (var h (or (@ elem $old-height) (chain (,$ elem) (css "height"))))
					  (chain (,$ elem) (show))					 
					  (chain (,$ elem) (animate (create height h) opts))))))
		    
		    
		    slide-up
		    (lambda(opts)		      
		      (chain this (each (lambda(elem)
					  ;;记录原来高度
					  (unless (@ elem $old-height)
					    (setf (@ elem $old-height) (chain (,$ elem) (css "height")))))))
		      (chain this (animate (create height 0)
					   (chain ,$ (union 
						      (create callback
							      (lambda()
								(chain this (hide))))
						      opts)))))
			     
		    
		    slide-toggle
		    (lambda()
		      (chain this (each (lambda(elem)
					  (if (<= (chain (,$ elem) (css "height")) 0)
					      (chain (,$ elem) (slide-down))
					      (chain (,$ elem) (slide-up)))))))
		    fade-to
		    (lambda(opacity)
		      (chain this (animate (create opacity opacity))))
		    
		    fade-in
		    (lambda() (chain this (fade-to 100)))

		    fade-out
		    (lambda() (chain this (fade-to 0)))
		    
		    ;;(chain (,$ elem) (hide))))))

		    )))
     
     
     
     ))




(extend-lib 
    (style)
  `(chain ,$ 
	  (extend
	   (create
	    style
	    (create

	     _get-style
	     (lambda(elem name)
	       (cond((elt (@ elem style) name) (elt (@ elem style) name))
		    ((and (@ document default-view)
			  (@ document default-view get-computed-style))
		     (var s (chain document default-view (get-computed-style elem "")))		  
		     (setf name (chain name (replace (regex "/([A-Z])/g") "-$1"))
			   name (chain name (to-lower-case)))
		     (and s (chain s (get-property-value name))))
		    ((@ elem current-style) 
		     (elt (@ elem current-style) name))		     
		    (t "")))

	     
	     get-style
	     (lambda(elem name)
	       (var tmp-name (chain name (to-lower-case)))
	       ;;opacity相关
	       (when (and (= tmp-name "opacity")
			  (not (@ support opacity)))		 
		 (setf name "filter"))
	       ;;;float相关
	       (when (and (> (chain tmp-name (search "float")) -1)
			  (not (@ support css-float)))
		 ;;用_getStyle中的getPropertyValue时，只有float有效，而cssFloat无效
		 (setf name "styleFloat"))
	       ;;;获取样式	     
	       (var ret (chain ,$ style (_get-style elem name)))
	       ;;转换结果
	       ;;透明度
	       ;;
	       (when (= name "filter")  
		 ;;如果未设置filter IE<9中filter返回结果为""
		 (setf ret
		       (if (= ret "")
			   100
			   (parse-float (elt (chain ret (match (regex "/\\d+/g"))) 0)))))

	       (when (= name "opacity")
		 (setf ret (* ret 100)))

	       ;;;高度或宽度
	       (when (> (chain name (search (regex "/width|height|left|right/i"))) -1)
		 (setf ret (parse-float ret)))
	       ret)
	     
	     set-style
	     (lambda(elem name value)
	       (cond((= name "opacity")
		     (chain ,$ style (set-opacity elem value)))
		    ((> (chain name (search (regex "/float/i"))) -1)
		     (chain ,$ style (set-float elem value)))		    
		    (t (when(> (chain name (search (regex "/width|height|left|right|top|bottom/i"))) -1)			 
			 (when(numberp value)			   
			   (setf value (+ value "px"))))
		       ;;(alert value)
		       (setf (elt (@ elem style) name) value))))
	    
	     set-opacity
	     (lambda(elem value)
	       
	       (setf value (min 100 (max 0 value)))
	       (if (@ ,$ support opacity)
		   (setf (@ elem style opacity) (/ value 100))
		   (progn ;;(Alert value);;(alert "set filter")
		     (setf (@ elem style filter) (+ "alpha(opacity=" value ")")))))
	     
	     set-float
	     (lambda(elem value)
	       (if (@ ,$ support css-float)
		   (setf (elt (@ elem style) "float") value)
		   (setf (elt (@ elem style) "styleFloat") value)))
	     
	     

	     ;;;;;;;;
	     reset-c-s-s
	     (lambda(elem prop)
	       (var old (create))
	       (for-in (i prop) (setf (elt old i) (elt (@ elem style) i)
				      (elt (@ elem style) i) (elt prop i)))
	       old)

	     restore-c-s-s
	     (lambda(elem prop)
	       (for-in (i prop) (setf (elt (@ elem style) i) (elt prop i))))

	     
	     ;;;混乱模式下很不准确
	     ;;;;;;;;;;获取元素内部尺寸:content+padding
	     inner-dimension
	     (lambda(elem)
	       (var old (create))
	       (var ret)
	       (when(= (chain ,$ style (get-style elem "display")) "none")
		 (setf old (chain ,$ style (reset-c-s-s elem (create display "" visibility "hidden" position "absolute")))))
	       (setf ret (create width (@ elem client-width) 
				 height (@ elem client-height)))
	       (chain ,$ style (restore-c-s-s elem old))
	       ret)
	     ;;;;;;;;;;获取元素外部尺寸:content+padding+border
	     outer-dimension
	     (lambda(elem)
	       (var old (create))
	       (var ret)
	       (when(= (chain ,$ style (get-style elem "display")) "none")
		 (setf old (chain ,$ style (reset-c-s-s elem (create display "" visibility "hidden" position "absolute")))))
	       (setf ret (create width (@ elem offset-width) 
				 height (@ elem offset-height)))
	       (chain ,$ style (restore-c-s-s elem old))
	       ret)
	     ;;----------------------------
	     		    ;;;;动画

	     ;;;; animation deltas
	     ;;控制动画方式
	     deltas 
	     (create linear (lambda(progress) progress)
		     quad (lambda(progress) (expt progress 2))
		     circ (lambda(progress) (- 1 (sin (acos progress))))
		     ;;;如果计算过于复杂IE<9会崩溃
		     back (lambda(x) 
			    (lambda(progress)
			      (* (expt progress 2)
				 (- (* (+ x 1) progress) x))))
		    
		     elastic (lambda(x)
			       (lambda(progress)
				 (* (expt 2 (* 10 (- progress 1)))
				    (cos (/ (* 20 pi x progress) 3)))))
		     
		     )
	     
	     )))))

