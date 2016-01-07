(in-package #:web)

(defpslibmacro(canvas)
    `(create
      find-context
      (lambda(elem)
	(chain elem (get-context "2d")))
      
      polygon 
      ;;n:边数 x,y:中心 r:半径 angle:起始角度 conterclockwise:是否逆时针
      (lambda(c n x y r angle counterclockwise)
	(setf angle (or angle 0)
	      counterclockwise (or counterclockwise false))
	;;;begin a new path
	(chain c (move-to (+ x (* r (sin angle)))
			  (- y (* r (cos angle)))))
	
	;;两点之间的夹角
	(var delta (/ (* pi 2) n))
	;;循环剩余的每个顶点
	(dotimes(i n)
	  (incf angle (if counterclockwise (- delta) delta))
	  (chain c (line-to (+ x (* r (sin angle)))
			    (- y (* r (cos angle))))))
	;;将最后一个定点链接起来
	(chain c (close-path))
	c)
      ;;;通过对象o来设置图形属性，如果没有提供，返回当点属性
      cxt-attrs
      (lambda(c o)
	(cond(o (for-in (a o)
			(setf (elt c a) (elt o a)))
		c)
	     (t (create fill-style (@ c fill-style)
			font (@ c font)
			global-alpha (@ c global-alpha)
			global-composite-operation 
			(@ c global-composite-operation)
			line-cap (@ c line-cap)
			line-join (@ c line-join)
			line-width (@ c line-width)
			miter-limit (@ c miter-limit)
			text-align (@ c text-align)
			text-baseline (@ c text-baseline)
			shadow-blur (@ c shadow-blur)
			shadow-color (@ c shadow-color)
			shadow-offset-x (@ c shadow-offset-x)
			shadow-offset-y (@ c shadow-offset-y)
			stroke-style (@ c stroke-style)
			))))
      cxt-revert
      (lambda(c)
	(chain c (restore));; 恢复最后一次保存的图像状态
	(chain c (save));;再次保存以便后续使用
	c)

      ))

(defpsmacro find-context(elem)
  `(chain ,*lib-name* canvas (find-context ,elem)))

(defpsmacro polygon(c x y r &rest angle-and-counterclockwise)
  `(chain ,*lib-name* canvas (polygon ,c ,x ,y ,r ,@angle-and-counterclockwise)))

(defpsmacro cxt-attrs(c &rest o)
  `(chain ,*lib-name* canvas (cxt-attrs ,c ,@o)))

(defpsmacro cxt-revert(c)
  `(chain ,*lib-name* canvas (cxt-revert ,c)))

`(html2file
 "~/test.html"
 :title "fucking canvas test"
 :body (<:canvas :id "canvas" :width 1000 :height 500)
 :internal-css
 `(("#canvas" :border "1px solid black" 
	      :position absolute :top 0 :left 50% :margin-left -500px))
 :internal-script
 ((define-front-lib canvas)
  (var canvas (selector "#canvas"))
;;  (var c (find-context canvas))
;;  (chain c (begin-path))
;;  (polygon c 3 50 70 50)
;;  (polygon c 4 150 60 50 (/ pi 4))
;;  (polygon c 5 255 55 50)
;;  (polygon c 6 365 53 50 (/ pi 6))
;;  (polygon c 4 365 53 20 (/ pi 4) true)
;;  (polygon c 50 500 200 100 )
;;
;;  (cxt-attrs c (create fill-style "#ccc"
;;		       stroke-style "#007"
;;		       line-width 5))
;; (chain c (fill))
;; (chain c (stroke))
;; (chain c (translate  5 5))
;; (chain c (rotate (/ pi 6)))
;; (chain c (scale 1.5 0.8))
 (chain c (shear 0.5 0)
)))


