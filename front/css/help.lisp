(in-package #:web)


(defun offset-height()
  "clientHeight的值由DIV内容的实际高度和CSS中的padding值决定，而offsetHeight的值由DIV内容的实际高度，CSS中的padding值，scrollbar的高度和DIV的border值决定；至于CSS中的margin值，则不会影响clientHeight和offsetHeight的值:offsetHeight 有border clientHeight 无border height 不含padding"
)

(defun how-to-hide-element()
  "visibility(visible(default) || hidden): 不会从文档流中删除元素；display(block || none ||..)：从文档流中删除元素")
