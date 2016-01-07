(in-package #:web)

(defpsmacro print(&rest strings)
  `(chain console (log (+ ,@strings))))

;;;;;;;;;;;;;;;;;new javascript API make javascript like CL

;;;;for both string and array

;;
;;(defpsmacro valueof (object)
;;  `(chain ,object (value-of)))
;;
;;(defpsmacro to-string(array)
;;  "throw array that not an array"
;;  `(chain ,array (to-string)))
;;
;;(defpsmacro to-local-string(array)
;;  "throw array that not an array"
;;  `(chain ,array (to-local-string)))
;;
;;(defpsmacro indexof(elt array &rest start)
;;  "not like LISP return the value if find, return the indexof"
;;  `(chain ,array (index-of ,elt ,@start)))
;;
;;(defpsmacro last-indexof(elt array &rest start)
;;  `(chain ,array (last-index-of ,elt ,@start)))
;;
;;(defpsmacro subseq(array start &rest end)
;;  `(chain ,array (slice ,start ,@end)))
;;
;;(defpsmacro nsubseq(array start delete-count &rest values)
;;  "specific delete-count to 0 add provide values for insert"
;;  `(chain ,array (splice ,start ,delete-count ,@values)))
;;
;;;;;;;;;;array
;;;;;append--> concat
;;;;???
;;(defpsmacro arrayp(value)
;;  `(= (chain *object prototype to-string (apply ,value))
;;      "[object Array]"))
;;
;;(defpsmacro car(array)
;;  `(elt ,array 0))
;;(defpsmacro cdr(array)
;;  `(chain ,array (slice 1)))
;; 
;;(defpsmacro every(fn array &rest o)
;;  `(chain ,array (every ,fn ,@o)))
;;
;;(defpsmacro remove-if-not(fn array &rest o)
;;  `(chain ,array (filter ,fn ,@o)))
;;
;;(defpsmacro mapc(fn array)
;;  `((lambda()
;;      (do*((i (length ,array))
;;	   (i 0 (1+ i)))
;;	  ((>= i len) ,array)
;;	(var item (elt ,array i))
;;	(,fn item)))))
;;  
;;(defpsmacro mapcar(fn array)
;;  `((lambda()
;;      (do*((result '())
;;	   (len (length ,array))
;;	   (i 0 (1+ i)))
;;	  ((>= i len) result)
;;	(var item (,fn (elt ,array i)))
;;	(chain result (push item))))))
;;(defpsmacro join(array &optional (separator " "))
;;  "the separator default to ',' in JS"
;;  `(chain ,array (join ,separator)))
;;
;;;;length --> length
;;
;;
;;(defpsmacro pop(array &optional (n 1))
;;  (if (<= n 1)
;;      `(chain ,array (shift))
;;      `(dotimes(i ,n)
;;	 (chain ,array (shift)))))
;;
;;
;;(defpsmacro push(array &rest values)
;;  `(chain ,array (unshift ,@values)))
;;
;;(defpsmacro pop-from-end(array &optional (n 1))
;;  (if (<= n 1)
;;      `(chain ,array (pop))
;;      `(dotimes(i ,n)
;;	 (chain ,array (pop)))))
;;
;;(defpsmacro push-to-end(array &rest values)
;;  `(chain ,array (push ,@values)))
;;
;;(defpsmacro reduce(fn array &rest init-value)
;;  `(chain ,array (reduce ,fn ,@init-value)))
;;
;;(defpsmacro reduce-from-end(fn array &rest init-value)
;;  `(chain ,array (reduce-right ,fn ,@init-value)))
;;
;;(defpsmacro nreverse(array)
;;  `(chain ,array (reverse)))
;;
;;(defpsmacro some(fn array &rest o)
;;  `(chain ,array (some ,fn ,@o)))
;;
;;(defpsmacro sort(array &rest orderfunc)
;;  `(chain ,array (sort ,@orderfunc)))
;; 
;;;;----------------------------------------
;;;;String
;;
;;(defpsmacro char-at(string &rest start)
;;  "if 'start' not provided return string.charAt(0);"
;;  `(chain ,string (char-at ,@start)))
;;
;;(defpsmacro char-code-at(string &rest start)
;;  `(chain ,string (char-code-at ,@start)))
;;
;;(defpsmacro code-string(&rest codes)
;;  "create string with unicode"
;;  `(chain *string (from-char-code ,@codes)))
;;
;;(defpsmacro string-lessp(target-string compare-string)
;;  `(chain ,target-string (local-compare ,compare-string)))
;;
;;(defpsmacro string-match(string pattern)
;;  `(chain ,string (match ,pattern)))
;;
;;(defpsmacro string-replace(string regexp replacement)
;;  `(chain ,string (replace ,regexp ,replacement)))
;;
;;(defpsmacro string-search(string regexp)
;;  `(chain ,string (search ,regexp)))
;;
;;(defpsmacro split(string delimiter &rest limit)
;;  `(chain ,string (split ,delimiter ,@limit)))
;;
;;(defpsmacro substr(string start &rest length)
;;  `(chain ,string (substr ,start ,@length)))
;;
;;(defpsmacro substring(string from &rest to)
;;  "if 'from' > 'to' they switch"
;;  `(chain ,string (substring ,from ,@to)))
;;
;;(defpsmacro to-lower-case(string)
;;  `(chain ,string (to-lower-case)))
;;(defpsmacro to-local-lower-case(string)
;;  `(chain ,string (to-local-lower-case)))
;;(defpsmacro to-upper-case(string)
;;  `(chain ,string (to-upper-case)))
;;(defpsmacro to-local-upper-case(string)
;;  `(chain ,string (to-local-upper-case)))
;;
;;(defpsmacro trim-whitespace(string)
;;  "with leading and trailing whitespace removed"
;;  `(chain ,string (trim)))
;;
;;(defpsmacro string<(a b)
;;  `(chain ,a (local-compare b)))
;;;;---------------------------------
;;;;function
;;;;(defpsmacro apply(fn thisobj &rest argument-array)
;;;;  `(chain ,fn (apply ,thisobj ,@argument-array)))
;;;;
;;;;(defpsmacro call(fn thisobj &rest args)
;;;;  `(chain ,fn (call ,thisobj ,@args)))
;;;;  
;;;;;json
;;(defpsmacro parsejson(json-string)
;;  `(chain *j-s-o-n (parse ,json-string)))
;;
;;(defpsmacro stringifyjson(object)
;;  `(chain *j-s-o-n (stringify ,object)))
;;
;;
;;
