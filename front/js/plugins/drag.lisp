(in-package #:web)
;;The fully documented DOM-Drag Library
(defpsmacro define-drag-util()
  `(var
    drag-util
    (create
     obj;;The current element being dragged
     null
     ;;The initalization function for the drag object
     ;;o = The element to act as the drag handle
     ;;oRoot= The elemnt to be dragged, if not specified,
     ;;       the handle will be the element be dragged
     ;;minX, minY, maxX, maxY = the min and max coordinates allowed for the elemnt
     ;;bSwapHorzRef=Toggle the horizontal coordinate system
     ;;bSwapVertRef=Toggle the vertical coordinate system
     ;;fxMapper, fyMapper = functions for mapping x and y coordinates to others
     init
     (lambda(o &optional o-root min-x max-x min-y max-y
	     b-swap-horz-ref b-swap-vert-ref
	     f-x-mapper f-y-mapper)
       ;;watch for the drag event
       (setf (@ o onmousedown) (@ drag-util start))
       ;;Figure out which coordinate system is being used
       (setf (@ o hmode) (if b-swap-horz-ref false true)
	     (@ o vmode) (if b-swap-vert-ref false true)
	     ;;Figure out which element is actiing the draggable "handle"
	     (@ o root) (or o-root o));;????
       ;;;initalize the specified coordinate system
       (when (and (@ o hmode) (is-na-n (parse-int (@ o root style left))))
	 (setf (@ o root style left) "0px"))
       (when (and (@ o vmode) (is-na-n (parse-int (@ o root style top))))
	 (setf (@ o root style top) "0px"))
       (when (and (@ o hmode) (is-na-n (parse-int (@ o root style right))))
	 (setf (@ o root style right) "0px"))
       (when (and (@ o vmode) (is-na-n (parse-int (@ o root style bottom))))
	 (setf (@ o root style bottom) "0px"))
       ;;;loop to see if the user provided min/max x/y coordinates
       (setf (@ o min-x) (or min-x null)
	     (@ o min-y) (or min-y null)
	     (@ o max-x) (or max-x null)
	     (@ o max-y) (or max-y null)
	     ;;check for any specified x and y coordinate mappers
	     (@ o x-mapper) (or f-x-mapper null)
	     (@ o y-mapper) (or f-y-mapper null)
	     ;;add shells for all the user-defined functions
	     (@ o root on-drag-start) (new (*function))
	     (@ o root on-drag-end) (new (*function))
	     (@ o root on-drag) (new (*function))))
     start
     (lambda(e)
       ;;figure out the object that's being dragged
       (setf (@ drag-util obj) this
	     o (@ drag-util obj)
	     ;;;normalize the event object
	     e (chain drag-util (fixed-e e)))
       ;;get the current x and y coordinates
       (var y (parse-int (if (@ o vmode) 
			     (@ o root style top)
			     (@ o root style bottom))))
       (var x (parse-int (if (@ o hmode) 
			     (@ o root style left)
			     (@ o root style right))))
       ))))


       




	     
     
    
