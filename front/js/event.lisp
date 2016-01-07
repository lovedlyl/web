(in-package #:web)

;;1.-----------事件流
;;事件冒泡2：直到最外层body html, 不到document 和 window
;;  事件捕获：方向相反，html为最前端
(extend-lib
    (event-proto)
  `(progn
     (chain ,$ (each (create on "addEvent" off "removeEvent")
		     (lambda(event-name proto-name)
		       (setf (elt (@ ,$ fn) proto-name)
			     (lambda(type handler)
			       (chain this (each (lambda(elem) 						   
						   ((elt (@ ,$ event) event-name) elem type handler)))))))))
     (setf (@ ,$ fn once);;;还没解决 this 和 event的问题****
	   (lambda(type handler)
	     (var that this)
	     (defun callback(e)
	       (chain that (off type callback))
	       (handler))
	     (chain this (on type callback))))
    
      ;;;方便直接操作的事件 onclick...等属性
     (chain ,$ (each (chain "click mouseover" (split " "))
		     (lambda(type i)
		       (setf (elt (@ ,$ fn) (+ "on" type))
			    (lambda(handler)
			      (chain this (on type handler))))
		       (setf (elt (@ ,$ fn) (+ "off" type))
			     (lambda(handler)
			       (chain this (off type handler)))))))
        
     (var ready-list (array))
     (chain ,$ (extend 
		(create
		 				  
                 ;;;;;;domReady 
		 ready
		 (lambda(handler)        
		   ;;执行ReadyList中的所有函数
		   (defun execute-handlers()
		     (var len (length ready-list))
		     (dotimes(i len)
		       ((elt ready-list i))))
		   ;;第一次进入即注册事件
		   (unless(length ready-list)
		     (chain ,$ event (bind-ready execute-handlers)))
			  ;;;集中多个事件
		   (chain ready-list (push handler)))

				     
		 )))
		
     ;;事件代理
     (setf (@ ,$ fn delegate)
	   (lambda(selector type fn)
	     (chain this (each (lambda(elem) (chain ,$ event (delegate elem selector type fn)))))))
     ))

(extend-lib
    (event)
  `(chain ,$
	  (extend
	   (Create 
	    event
	    (create 
    ;;;2.---------事件添加和删除
	     ;;1.in Opera(v=12.16): which supports attachEvent(like IE), but the typeof document.body.attachEvent      --> "unbdefined"
	     ;;                                                                  typeof document.body.addEventListener --> "function"
	     ;;            Object.prototype.toString.call(document.body.attachEvent)--> "[object Function]"    
	     ;;2. For IE8-: wind using method below to add event to window, the srcElement is not provided as the "document"
	     ;;3.IE8-中，使用attachEvent时，handler函数中的this始终指向window对象。
	     ;;  当添加多个事件时，后添加的事件先执行，与addEventListener相反
	     

	     ;;Refernce: http://dean.edwards.name/my/events.js
	 
	     ;; 为了使所有浏览器效果一致，即使是Firefox也不使用addEventlistner
	     guid 0	     
	     add-event
	     (lambda(elem type handler)
	       (unless(@ handler $$guid)
		 (setf (@ handler $$guid) (incf (@ ,$ event guid))))
	       (unless (@ elem events)
		 (setf (@ elem events) (create)))
	       (setf handlers (elt (@ elem events) type))
	       (unless handlers
		 ;;handlers不采用Object，而是Array 使添加事件的顺行得到保证
		 (setf (elt (@ elem events) type) (array)) 
		 (setf handlers (elt (@ elem events) type))
		 (when (elt elem (+ "on" type))
		   (setf (elt handlers 0)  (elt elem (+ "on" type))))		 
		 (setf (elt elem (+ "on" type)) (@ ,$ event handle-event)))
	       ;;(print handlers)
	       (setf (elt handlers (@ handler $$guid)) handler))
	     
	     remove-event
	     (lambda(elem type handler)
	       (var len)
	       (when (@ elem events)
		 (setf handlers (elt (@ elem events) type)
		       len handlers)
		 (when handlers
		   (delete (elt handlers (@ handler $$guid)))
		   ;;当所有通过addEvent添加的事件都removed后，元素上不再具有events属性
		   ;;而且，如果html中没有添加任何事件, 元素的on[?type]属性被删除
		   ;;如果有，则回到原来的状态
		   (when (chain ,$ (every (chain slice (call handlers 1))
					(lambda(handler) (= handler undefined))))
		     (setf (@ elem events) null)
		     (if (= (elt handlers 0) undefined)		       
			 (setf (elt elem (+ "on" type)) null)
			 (setf (elt elem (+ "on" type)) (elt handlers 0)))))))

	     handle-event
	     (lambda(event)
	       (var return-value true)
	       (setf event (or event (chain ,$ event 
					    (fixed-event
					     (@ (or (@ (or (@ this owner-document) (@ this document) this) parent-window)
						    window)
						event)))))

	       (var handlers (elt (@ this events) (@ event type)))
	       (var handler)
	       (var len (length handlers))
	       (dotimes(i len)
		 (setf handler (elt handlers i))
		 (unless(= handler undefined)
		   ;;有多个handlers时，当某个handler返回false停止执行后面的handler（s）
		   ;;并且可通过handler返回true/false控制是否preventDefault
		   (when(= (chain handler (call this event)) false) 
		     ;;保证handler中的this指向元素本身
		     (setf return-value false)
		     (break))))
	       return-value)
	     

	     ;;;为MSIE中的Event对象增加W3C的方法
	     fixed-event
	     (lambda(event)
	       (setf (@ event prevent-default) (@ ,$ event prevent-default)
		     (@ event stop-propagation) (@ ,$ event stop-propagation)
		     (@ event target) (@ event src-element))
	       event)

	     prevent-default
	     (lambda()
	       (setf (@ this return-value) false)
	       true);;赋值表达式的返回值就是那个值，同lisp
	     
	     stop-propagation
	     (lambda()
	       (setf (@ this cancel-bubble) true))

	     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     ;;;;DOM ready
	     bind-ready
	     (lambda(handler)
	       (var called false)
	       (var is-frame false)
	       (defun unbind-ready()
		 "移除事件"
		 (cond((@ document add-event-listener)
		       (chain document (remove-event-listener "DOMContentLoaded" ready false)))
		      ((@ document attach-event)
		       (chain document (attach-event "onreadystatechange" frame-load-handler)))))
		     
	       (defun ready()
		 "执行各个事件前，移除已经注册的事件"
		 (unless called
		   (setf called true)		   
		   (unbind-ready)
		   (handler)))

	       (defun frame-load-handler()
		 "frame中的事件处理方式"
		 (when (chain (regex "loaded|complete") 
			      (test (@ document ready-state)))
		   (ready)))
	       
	       (defun try-scroll()
		 "如果不是在frame中，轮询得知是否DOMReady"
		 (unless called		   
		   (try (progn (chain doc-elem (do-scroll "left"))
			       (ready))
			(:catch(e)
			  (set-timeout try-scroll)))))
	       
	       ;;http://bugs.jquery.com/ticket/12282#comment:15
	       ;;防止DOM依靠服务端传入内容，还没开始传入，延迟
	       (cond((= (@ document ready-state) "complete") (set-timeout ready))
		    ((@ document add-event-listener);;;标准
		     (chain document (add-event-listener "DOMContentLoaded" ready false)))
		    ((@ document attach-event);;;IE<9
		     (try 
		      (setf is-frame (not (null (@ window frame-element))))
		      (:catch(e)))
		     ;;;;
		     (when(@ doc-elem do-scroll)
		       (if(not is-frame);;非frame
			  (try-scroll)
			  ;;frame
			  (chain document (attach-event "onreadystatechange" frame-load-handler)))))))
	     
		        
	     ;;;事件代理
	     delegate
	     (lambda(src-elem selector type fn)
	       (var target-elems (chain ,$ (find selector)))
	       (var len (length target-elems))
	       (var target-elem)
	       (chain ,$ event (add-event src-elem type
					  (lambda(e)
					    (dotimes(i len)
					      (setf target-elem (elt target-elems i))
					      (when(= (@ e target) target-elem)
						(chain fn (call target-elem e))))))))
	       
	     
  ;;;3.-------------事件对象
	     ;;   Except IE, Event Object may different from different event,
	     ;;   but all Event share properties and methods below(all read only)
	     ;;   ;;;properties
	     ;;   1.bubbles:(boolen) indicates if the event bubbles
	     ;;   2.cancelable:(boolen) indicates if the default behavior of the event can be cancled
	     ;;   3.currentTaget:(Element) The Element whose event handler is currently handling the event 
	     ;;   4.target:(Element) The target of the event
	     ;;   5.defaultPrevented(boolen): for true indicates the preventDefault method has be called (added in DOM 3)
	     ;;   6.detail:(integer): extra information related to the event
	     ;;   7.eventPhrase(integer)?: The phrase during which the event handler is being called:
	     ;;                          1 for capturing phrase, 2 for "at target", 3 for bubbling.
	     ;;   8.trusted(boolen)?: When true, indicates if the event was generated by the browser. 
	     ;;                      When false, indicates the event was created by the developer(added in DOM 3)
	     ;;   9.type(string): the type of the event was fired
	     ;;   10.view(AbstractView): The abstract view associated with the event.
	     ;;                          This is equal to the window in which the event was fired.
	     ;;   ;;;methods
	     ;;   1.preventDefualt(function): cancels the default behavior of the event, if cancelable is true, this mehtod can be used
	     ;;   2.stopImmdeiatePropagation(function): cancel any further event capturing and event bubbling 
	     ;;                                         and prevent any other event handlers from being called (added in DOM 3)
	     ;;   3.stopPropagation(function): cancel any event bubbling or capturing, if bubbles is true, this method can be used.
	     ;;   

	     ;;   for IE
	     ;;   1. If add event by DOM 0(button.onclick=function(e){var event=window.event}), the event exists on window.event
	     ;;      if add by attachEvent, the event behave normally as the the parameter passed in the handler, but also exists
	     ;;      on window.event
	     ;;   2.properties
	     ;;     a.cancleBubble(boolen, read/write): like stopPropagation()
	     ;;     b.returnValue(boolen, read/write): like preventDefault()
	     ;;     c.srcElement(element): like event.target
	     ;;     d.type: the same as none-IE event.type
	     ;;   3. the "this" in event handler may not equal to the target event, so using event.srcElement

	     ;;    get-event ;;get event
	     ;;    (lambda(e)
	     ;;      (or e (@ window e)))
	     ;;    
	     ;;    get-event-target ;;get event element
	     ;;    (lambda(e)
	     ;;      (or (@ e target) (@ e src-element)))
	     ;;    
	     ;;    prevent-default ;;prevent default event action
	     ;;    (lambda(e)
	     ;;      (setf (@ ,$ event prevent-default)
	     ;;	    (if (@ e prevent-default)
	     ;;		(lambda(e) (chain e (prevent-default)) e) ;;return the event object
	     ;;		(lambda(e) (setf (@ e return-value) false) e))) ;;return the event object
	     ;;      (chain ,$ event (prevent-default e)))
	     ;;    
	     ;;
	     ;;    stop-propagation ;;stop event propagation
	     ;;    (lambda(e)
	     ;;      (setf (@ ,$ event stop-propagation)
	     ;;	    (if (@ e stop-propagation)
	     ;;		(lambda(e) (chain e (stop-propagation)) e) ;;return the event object
	     ;;		(lambda(e) (setf (@ e cancel-bubble) true) e))) ;;return the event object
	     ;;      (chain ,$ event (stop-propagation e)))
	     


	     )))))



