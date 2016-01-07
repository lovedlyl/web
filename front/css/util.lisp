(in-package #:web)

(defun px(num)
  "convert num to px: 12->12px"
  (format nil "~Apx" num))

(defun opacity(value)
  (let((value (min 100 (max 0 value))))
  `(:opacity ,(/ value 100 1.0)
	     :filter ,(format nil "alpha(opacity=~A)" value))))

	    

		         

(defmacro box-sizing(value)
  "3种类型：1.content-box 默认行为，宽和高分别为元素内容宽和高，内边距和
便框另外绘制。
2：border-box 元素的宽和高决定了元素的边框盒子，即内边距和边框在已经设定
的宽和高中绘制，已设定的宽和高减去内边距和边框才是内容尺寸。
3：inherit 继承父元素的box-sizing属性"
  `(cross-broswer :box-sizing ',value))
 
(defun transform-origin(value)
  (cross-broswer :transform-origin value))

(defmacro border-radius(value)
  `(cross-broswer :border-radius ',value))

