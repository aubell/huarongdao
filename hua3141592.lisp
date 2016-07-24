;;19 18 17 16
;;15 14 13 12
;;11 10  9  8
;; 7  6  5  4 
;; 3  2  1  0


(defun != (x y)  (not (= x y)))

(defun sigma(L)
  (reduce #'(lambda(x y) (boole boole-ior x y)) L))

(defmacro select-locs-where ( &rest body )
  `(loop for $_ from 0 to 19
      when ,@body
      collect $_))

(setf locs-can-left  (select-locs-where  (!= (mod $_ 4) 3)))
(setf locs-can-down  (select-locs-where  (>= $_  4)))
(setf locs-can-up    (select-locs-where  (< $_ 16)))
(setf locs-can-right (select-locs-where  (!= (mod $_ 4) 0)))

(defun locs-of-piece(p)  
  (select-locs-where (= 1 (ldb (byte 1 $_) p))))

(defmacro gen-force(range dir)
  `(lambda(p)
     (let ((ps (locs-of-piece p)))
       (let ((new-pos (if (subsetp ps ,range)
			  (mapcar ,dir ps))))
	 (let ((re 0))
	   (progn 
	     (mapc 
	      #'(lambda(pos)
		  (setf (ldb (byte 1 pos) re) 1))	   
	      new-pos)
	     re))))))

(setf (symbol-function 'force-left) (gen-force locs-can-left  (lambda(i) (+ i 1))))
(setf (symbol-function 'force-right)(gen-force locs-can-right (lambda(i) (- i 1))))
(setf (symbol-function 'force-up)   (gen-force locs-can-up    (lambda(i) (+ i 4))))
(setf (symbol-function 'force-down) (gen-force locs-can-down  (lambda(i) (- i 4))))
;;all the force move is not related to a centen board
;;make sure it is w4h5 board

(defun count1 (x)
  (loop for $_ from 0 to 19
     when (= 1 (ldb (byte 1 $_) x))
     count 1))

(defmacro gen-can-move (force-dir)
  `(lambda(p board)(let ((new-board (cons (funcall ,force-dir p)(remove p board))))
		     (let ((n (sigma new-board)))
		       (= 18 (count1 n))))))

(setf (symbol-function 'can-up)    (gen-can-move #'force-up))
(setf (symbol-function 'can-down)  (gen-can-move #'force-down))
(setf (symbol-function 'can-left)  (gen-can-move #'force-left))
(setf (symbol-function 'can-right) (gen-can-move #'force-right))

(defun how-can-move(p board)
  (let ((re '()))
    (progn (if (can-up    p board)  (setf re (cons 'up re )))
	   (if (can-down  p board)  (setf re (cons 'down re )))
	   (if (can-left  p board)  (setf re (cons 'left re )))
	   (if (can-right p board)  (setf re (cons 'right re )))
	   re)))

(defun possible-moves(board)
  (mapcar #'(lambda(i)
	      (cons i (how-can-move i board)))
	  board))

(defun move-left(p board)
  (cons (force-left p) (remove p board)))

(defun move-right(p board)
  (cons (force-right p) (remove p board)))

(defun move-up(p board)
  (cons (force-up p) (remove p board)))

(defun move-down(p board)
  (cons (force-down p) (remove p board)))
;; missing order

(defun move(p dir board)
  (case dir
    (up (move-up p board))
    (down (move-down p board))
    (left (move-left p board))
    (right (move-right p board))))

(defun type-of-piece(p)
  (let ((locs (locs-of-piece p)))
    (if (= (length locs) 4)  
	#\A    ;cao
	(if (= (length locs) 2)
	    (if  (=  (-  ( second locs)(first locs)) 1)
		 #\B     ;=
		 #\C)    ; ||
	    #\D))))          ;single


(defun by-type(x y)
  (char< (type-of-piece x)(type-of-piece y)))


(defun sort-board(bd)
  (sort bd  #'by-type))

(defmacro select-pieces-type (tp bd)
  `(loop for $_ in ,bd
      when (char= (type-of-piece $_)  ,tp)
      collect $_))

(defun locs-of-type(tp bd)
  (sort (mapcar #'car
		(mapcar #'locs-of-piece 
			(select-pieces-type tp bd)))
	#'<))

(defun map-num-to-char(num)
  (code-char (+ num (char-code #\E))))

(defun string-locs-of-type(tp bd)
  (concatenate 'string 
	       ( cons tp (mapcar #'map-num-to-char
				 (locs-of-type tp bd)))))

(defun string-of-board (bd)
  (apply #'concatenate 'string
	 (mapcar #'(lambda(tp)
		     (string-locs-of-type tp bd))
		 '(#\A #\B #\C #\D))))

(defun child-boards-of-cb (cb bd)
  (let ((c (car cb))
	(dirs (cdr cb)))
    (if dirs
	(mapcar #'(lambda(dir)
		    (move c dir bd))
		dirs )
	nil)))

;;; important
(defun child-boards (bd)
  (mapcan #'(lambda(cube) (child-boards-of-cb cube bd))
	  (possible-moves bd)))


(defun child-string-boards (bd)
  (mapcar #'string-of-board 
	  (child-boards bd)))

;;X W V U
;;T S R Q
;;P O N M
;;L K J I
;;H G F E

(defun map-char-to-num(c)
  (- (char-code c) (char-code #\E)))

(defun string-to-nums(str)
  (loop for c across str
     collect (map-char-to-num c)))

(defun build-on-nums (nums test start)
  (let ((p1 (position-if test nums :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda(c) (not (funcall test c)))
			       nums :start p1)))
	  (cons (subseq nums p1 p2)
		(if p2
		    (build-on-nums nums test p2)
		    nil)))
	nil)))

(defun a-group(c)
  (>= c 0))

(defun group-it (nums)
  (build-on-nums  nums #'a-group  0))

(defun g-A(x) (list x (+ x 1)(+ x 4)(+ x 5)))
(defun g-B(x) (list x (+ x 1)))
(defun g-C(x) (list x (+ x 4)))
(defun g-D(x) (list x))

(defun g-A-L(L) (mapcar #'g-A L))
(defun g-B-L(L) (mapcar #'g-B L))
(defun g-C-L(L) (mapcar #'g-C L))
(defun g-D-L(L) (mapcar #'g-D L))

(defun g-ABCD(L)
  (append (g-A-L (first L))
	  (g-B-L (second L))
	  (g-C-L (third L))
	  (g-D-L (fourth L))))

(defun set-on(L)
  (let ((re 0))
    (loop for i in L
       do (setf (ldb (byte 1 i) re) 1))
    re))

(defun string-to-board (str)
  (mapcar #'set-on
	  (g-ABCD (group-it (string-to-nums str )))))

(defun board-to-string (bd)
  (string-of-board bd))

(defun childs (strBoard)
  (child-string-boards (string-to-board strBoard)))

(defun who-at (loc bd)
  (loop for p in bd
     when (= 1 (ldb (byte 1 loc) p))
     return p ))

(defun type-at (loc bd)
  (let ((w (who-at loc  bd)))
    (if w (type-of-piece w) #\Z)))  ;; space

(defun line(loc) (truncate (/ loc 4)))
(defun col (loc) (mod loc 4))

(defun sym-loc(loc)
  (let ((s (+ 3 (* 8 (line loc)))))
    (- s loc)))

(defun sym-locs(loc-list)
  (sort (mapcar #'sym-loc loc-list) #'<))

(defun board-to-locs(bd)
  (mapcar #'locs-of-piece bd))

(defun sym-locs-board(bd)
  (mapcar #'sym-locs (board-to-locs bd)))

(defun sym-board (bd)
  (mapcar #'set-on (sym-locs-board bd)))

(defun sym-string (str)
  (board-to-string
   (sym-board (string-to-board str))))

(defun remove-sym-i(x L)
  (if L
      (cons x  ;; to aviod remove it self
	    (remove (sym-string x) 
		    (remove x L :test #'equal ) :test #'equal))))

;(defun fold (closure seed sequence)
 ; (if sequence
  ;    (fold closure (funcall closure seed (car sequence)) (cdr sequence))
   ;   seed))

(defun remove-sym(L)
  (do ((x L (cdr x))
       (y (car L) (car x))
       (re L (remove-sym-i y re)))
      ((null x) re)))

(defmacro operate-on(fun L)  ;; O(n*n)
  `(do ((x-inside ,L (cdr x-inside))
	(y-inside (car ,L) (car x-inside))
	(re ,L (funcall ,fun y-inside re)))
       ((null x-inside) re)))

(defun rchilds (strBoard)
  (remove-sym (childs strBoard)))

(defun show-hash(k v)
  (format t "(~A ~A)~%" k v))

(defun add-new-node(k v hashTable)
  (if (not (gethash k hashTable))
      (setf (gethash k hashTable) v)))

(defun collector()
  (let ((saver '()))
    (lambda(&optional x)
      (if x 
	  (push  x saver)
	  saver))))

(defun collect-keys-by(hTb  test)
  (let ((save (collector)))
    (progn (maphash #'(lambda(k v)
			(if (funcall test k v)
			    (funcall save k)))
		    hTb)
	   (funcall save))))

(defun show-table(nodes)
  (maphash #'show-hash nodes))

(defun expand-node(strBoard nodes)
  (let ((cds (rchilds strBoard)))
    (if cds
	(let ((d (first (gethash strBoard nodes))))
	  (mapc 
	   #'(lambda(i)(add-new-node i (list (+ d 1) strBoard) nodes))
	   cds)))))

;;;needs change for speed
(defun get-generation-keys( nodes deep)
  (collect-keys-by nodes #'(lambda(k v)(= (first v) deep))))

(defun gen-next-g(nodes deep)
  (let ((ks (get-generation-keys nodes deep)))
    (mapc #'(lambda(n)(expand-node n nodes))
	  ks)))

(defun gen-ng(nodes from in-deep)
  (do ((g from (+ g 1)))
      ((= g (+ from in-deep)))
    (progn
      (format t "Gen ~A gap.~%" g)
      (gen-next-g nodes g))))

(defun deep(nodes)
  (let ((re 0))
    (progn (maphash #'(lambda(k v)
			(if (> (first v) re)
			    (setf re (first v))
			    ))
		    nodes)
	   re)))


(defun is-at (strBoard x)
  (char= (char strBoard 1) x))

(defun deep-of-node (node table)
  (first (gethash node table )))

(defun father-of-node(node table)
  (second (gethash node table)))

(defun select-nodes (nodes deep loc)
  (collect-keys-by nodes
		   #'(lambda(k v)
		       (= deep (deep-of-node k nodes))
		       (is-at k loc))))

(defun expand-nodes(L table)
  (mapc #'(lambda(nd)
	    (expand-node nd table))
	L))





(defun select-most(L test)
  (if L
      (let ((re (first L)))
	(loop for c in L
	     do(if (funcall test c re)
		   (setf re c)))
	re)))


(defun path (object nodes)      
  (do ((c object (father-of-node c nodes))
       (re nil (cons c re)))
      ((null c) re)))

(defun save-hash-to-file(h f)
  (with-open-file (stream f :direction :output
			  :if-exists :supersede)
    (maphash
     #'(lambda(k v)
	 (format stream "(~A ~A)~%" k v)
	 )
     h)))

(defun bits-add-to-board(num ch bd)
  (loop for i from 0 to 19
     when(= 1 (ldb (byte 1 i) num))
     do (setf (aref bd i) ch)))

(defun show-true-board(bd)
  (progn  (loop for i from 19 downto 0
	     do(progn 
		 (format t "~A" (aref bd i))
		 (if (=(mod i 4) 0) (format t "~%"))))
	  (format t "~%")))

(defun strBoard-to-trueBoard(str)
  (let (( bd (make-array 20 :initial-element #\SPACE)))
    (let ((bits-list (string-to-board str)))
      (mapc #'(lambda(i c) (bits-add-to-board i c bd))
	    bits-list '(#\N #\P #\s #\a #\i #\j #\+ #\- #\* #\= )))
    bd))

(defun show-result(object nodes)    
  (progn (mapc #'show-true-board (mapcar #'strBoard-to-trueBoard 
					 (path object nodes)))
	 nil))



(setf board 
      '(#b01100110000000000000
	#b10001000000000000000
	#b00010001000000000000
	#b00000000100010000000
	#b00000000000100010000
	#b00000000011000000000
	#b00000000000000001000
	#b00000000000001000000
	#b00000000000000100000
	#b00000000000000000001))

(setf board-little-path
      '(#b10000000000000000000
	#b01100110000000000000
	#b00010000000000000000
	#b00001000100000000000
	#b00000001000100000000
	#b00000000000010000000
	#b00000000000001100000
	#b00000000000000010000
	#b00000000000000000110
	#b00000000011000000000)
)

(defun solve(board deep)
  (let* (( str0  (string-of-board board) )
	 ( nodes (make-hash-table :test #'equal)))
    (progn
      (add-new-node str0 '(0 nil) nodes)
      (gen-ng nodes 0 deep) 
      (let* ((objects
	      (collect-keys-by nodes
			       #'(lambda(k v)
				   (char= (char k 1) #\F))))
	     (object (select-most objects #'(lambda(x y)
					      (< (deep-of-node x nodes)
						 (deep-of-node y nodes))))))
	(show-result object nodes)))))

(solve board-little-path 200)