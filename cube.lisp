;;;;;; operates define

(defparameter I
  '(NIL
    ((LUF LUF) (LBU LBU) (LDB LDB) (LFD LFD) (ULB ULB) (UBR UBR) (URF URF)
     (UFL UFL) (BLD BLD) (BUL BUL) (BRU BRU) (BDR BDR) (RUB RUB) (RBD RBD)
     (RDF RDF) (RFU RFU) (DLF DLF) (DBL DBL) (DRB DRB) (DFR DFR) (FLU FLU)
     (FUR FUR) (FRD FRD) (FDL FDL))
    ((BU BU) (BR BR) (BL BL) (BD BD) (UB UB) (UR UR) (UL UL) (UF UF) (RB RB)
     (RU RU) (RD RD) (RF RF) (LB LB) (LU LU) (LD LD) (LF LF) (DB DB) (DR DR)
     (DL DL) (DF DF) (FU FU) (FR FR) (FL FL) (FD FD))))
(defparameter *op18*
  '((R1
     ((R1)
      ((LUF LUF) (LBU LBU) (LDB LDB) (LFD LFD) (ULB ULB) (UBR BDR) (URF BRU)
       (UFL UFL) (BLD BLD) (BUL BUL) (BRU DRB) (BDR DFR) (RUB RBD) (RBD RDF)
       (RDF RFU) (RFU RUB) (DLF DLF) (DBL DBL) (DRB FRD) (DFR FUR) (FLU FLU)
       (FUR UBR) (FRD URF) (FDL FDL))
      ((BU BU) (BR DR) (BL BL) (BD BD) (UB UB) (UR BR) (UL UL) (UF UF) (RB RD)
       (RU RB) (RD RF) (RF RU) (LB LB) (LU LU) (LD LD) (LF LF) (DB DB) (DR FR)
       (DL DL) (DF DF) (FU FU) (FR UR) (FL FL) (FD FD))))
    (R2
     ((R2)
      ((LUF LUF) (LBU LBU) (LDB LDB) (LFD LFD) (ULB ULB) (UBR DFR) (URF DRB)
       (UFL UFL) (BLD BLD) (BUL BUL) (BRU FRD) (BDR FUR) (RUB RDF) (RBD RFU)
       (RDF RUB) (RFU RBD) (DLF DLF) (DBL DBL) (DRB URF) (DFR UBR) (FLU FLU)
       (FUR BDR) (FRD BRU) (FDL FDL))
      ((BU BU) (BR FR) (BL BL) (BD BD) (UB UB) (UR DR) (UL UL) (UF UF) (RB RF)
       (RU RD) (RD RU) (RF RB) (LB LB) (LU LU) (LD LD) (LF LF) (DB DB) (DR UR)
       (DL DL) (DF DF) (FU FU) (FR BR) (FL FL) (FD FD))))
    (R3
     ((R3)
      ((LUF LUF) (LBU LBU) (LDB LDB) (LFD LFD) (ULB ULB) (UBR FUR) (URF FRD)
       (UFL UFL) (BLD BLD) (BUL BUL) (BRU URF) (BDR UBR) (RUB RFU) (RBD RUB)
       (RDF RBD) (RFU RDF) (DLF DLF) (DBL DBL) (DRB BRU) (DFR BDR) (FLU FLU)
       (FUR DFR) (FRD DRB) (FDL FDL))
      ((BU BU) (BR UR) (BL BL) (BD BD) (UB UB) (UR FR) (UL UL) (UF UF) (RB RU)
       (RU RF) (RD RB) (RF RD) (LB LB) (LU LU) (LD LD) (LF LF) (DB DB) (DR BR)
       (DL DL) (DF DF) (FU FU) (FR DR) (FL FL) (FD FD))))
    (U1
     ((U1)
      ((LUF BUL) (LBU BRU) (LDB LDB) (LFD LFD) (ULB UBR) (UBR URF) (URF UFL)
       (UFL ULB) (BLD BLD) (BUL RUB) (BRU RFU) (BDR BDR) (RUB FUR) (RBD RBD)
       (RDF RDF) (RFU FLU) (DLF DLF) (DBL DBL) (DRB DRB) (DFR DFR) (FLU LBU)
       (FUR LUF) (FRD FRD) (FDL FDL))
      ((BU RU) (BR BR) (BL BL) (BD BD) (UB UR) (UR UF) (UL UB) (UF UL) (RB RB)
       (RU FU) (RD RD) (RF RF) (LB LB) (LU BU) (LD LD) (LF LF) (DB DB) (DR DR)
       (DL DL) (DF DF) (FU LU) (FR FR) (FL FL) (FD FD))))
    (U2
     ((U2)
      ((LUF RUB) (LBU RFU) (LDB LDB) (LFD LFD) (ULB URF) (UBR UFL) (URF ULB)
       (UFL UBR) (BLD BLD) (BUL FUR) (BRU FLU) (BDR BDR) (RUB LUF) (RBD RBD)
       (RDF RDF) (RFU LBU) (DLF DLF) (DBL DBL) (DRB DRB) (DFR DFR) (FLU BRU)
       (FUR BUL) (FRD FRD) (FDL FDL))
      ((BU FU) (BR BR) (BL BL) (BD BD) (UB UF) (UR UL) (UL UR) (UF UB) (RB RB)
       (RU LU) (RD RD) (RF RF) (LB LB) (LU RU) (LD LD) (LF LF) (DB DB) (DR DR)
       (DL DL) (DF DF) (FU BU) (FR FR) (FL FL) (FD FD))))
    (U3
     ((U3)
      ((LUF FUR) (LBU FLU) (LDB LDB) (LFD LFD) (ULB UFL) (UBR ULB) (URF UBR)
       (UFL URF) (BLD BLD) (BUL LUF) (BRU LBU) (BDR BDR) (RUB BUL) (RBD RBD)
       (RDF RDF) (RFU BRU) (DLF DLF) (DBL DBL) (DRB DRB) (DFR DFR) (FLU RFU)
       (FUR RUB) (FRD FRD) (FDL FDL))
      ((BU LU) (BR BR) (BL BL) (BD BD) (UB UL) (UR UB) (UL UF) (UF UR) (RB RB)
       (RU BU) (RD RD) (RF RF) (LB LB) (LU FU) (LD LD) (LF LF) (DB DB) (DR DR)
       (DL DL) (DF DF) (FU RU) (FR FR) (FL FL) (FD FD))))
    (F1
     ((F1)
      ((LUF URF) (LBU LBU) (LDB LDB) (LFD UFL) (ULB ULB) (UBR UBR) (URF RDF)
       (UFL RFU) (BLD BLD) (BUL BUL) (BRU BRU) (BDR BDR) (RUB RUB) (RBD RBD)
       (RDF DLF) (RFU DFR) (DLF LUF) (DBL DBL) (DRB DRB) (DFR LFD) (FLU FUR)
       (FUR FRD) (FRD FDL) (FDL FLU))
      ((BU BU) (BR BR) (BL BL) (BD BD) (UB UB) (UR UR) (UL UL) (UF RF) (RB RB)
       (RU RU) (RD RD) (RF DF) (LB LB) (LU LU) (LD LD) (LF UF) (DB DB) (DR DR)
       (DL DL) (DF LF) (FU FR) (FR FD) (FL FU) (FD FL))))
    (F2
     ((F2)
      ((LUF RDF) (LBU LBU) (LDB LDB) (LFD RFU) (ULB ULB) (UBR UBR) (URF DLF)
       (UFL DFR) (BLD BLD) (BUL BUL) (BRU BRU) (BDR BDR) (RUB RUB) (RBD RBD)
       (RDF LUF) (RFU LFD) (DLF URF) (DBL DBL) (DRB DRB) (DFR UFL) (FLU FRD)
       (FUR FDL) (FRD FLU) (FDL FUR))
      ((BU BU) (BR BR) (BL BL) (BD BD) (UB UB) (UR UR) (UL UL) (UF DF) (RB RB)
       (RU RU) (RD RD) (RF LF) (LB LB) (LU LU) (LD LD) (LF RF) (DB DB) (DR DR)
       (DL DL) (DF UF) (FU FD) (FR FL) (FL FR) (FD FU))))
    (F3
     ((F3)
      ((LUF DLF) (LBU LBU) (LDB LDB) (LFD DFR) (ULB ULB) (UBR UBR) (URF LUF)
       (UFL LFD) (BLD BLD) (BUL BUL) (BRU BRU) (BDR BDR) (RUB RUB) (RBD RBD)
       (RDF URF) (RFU UFL) (DLF RDF) (DBL DBL) (DRB DRB) (DFR RFU) (FLU FDL)
       (FUR FLU) (FRD FUR) (FDL FRD))
      ((BU BU) (BR BR) (BL BL) (BD BD) (UB UB) (UR UR) (UL UL) (UF LF) (RB RB)
       (RU RU) (RD RD) (RF UF) (LB LB) (LU LU) (LD LD) (LF DF) (DB DB) (DR DR)
       (DL DL) (DF RF) (FU FL) (FR FU) (FL FD) (FD FR))))
    (L1
     ((L1)
      ((LUF LFD) (LBU LUF) (LDB LBU) (LFD LDB) (ULB FLU) (UBR UBR) (URF URF)
       (UFL FDL) (BLD ULB) (BUL UFL) (BRU BRU) (BDR BDR) (RUB RUB) (RBD RBD)
       (RDF RDF) (RFU RFU) (DLF BLD) (DBL BUL) (DRB DRB) (DFR DFR) (FLU DLF)
       (FUR FUR) (FRD FRD) (FDL DBL))
      ((BU BU) (BR BR) (BL UL) (BD BD) (UB UB) (UR UR) (UL FL) (UF UF) (RB RB)
       (RU RU) (RD RD) (RF RF) (LB LU) (LU LF) (LD LB) (LF LD) (DB DB) (DR DR)
       (DL BL) (DF DF) (FU FU) (FR FR) (FL DL) (FD FD))))
    (L2
     ((L2)
      ((LUF LDB) (LBU LFD) (LDB LUF) (LFD LBU) (ULB DLF) (UBR UBR) (URF URF)
       (UFL DBL) (BLD FLU) (BUL FDL) (BRU BRU) (BDR BDR) (RUB RUB) (RBD RBD)
       (RDF RDF) (RFU RFU) (DLF ULB) (DBL UFL) (DRB DRB) (DFR DFR) (FLU BLD)
       (FUR FUR) (FRD FRD) (FDL BUL))
      ((BU BU) (BR BR) (BL FL) (BD BD) (UB UB) (UR UR) (UL DL) (UF UF) (RB RB)
       (RU RU) (RD RD) (RF RF) (LB LF) (LU LD) (LD LU) (LF LB) (DB DB) (DR DR)
       (DL UL) (DF DF) (FU FU) (FR FR) (FL BL) (FD FD))))
    (L3
     ((L3)
      ((LUF LBU) (LBU LDB) (LDB LFD) (LFD LUF) (ULB BLD) (UBR UBR) (URF URF)
       (UFL BUL) (BLD DLF) (BUL DBL) (BRU BRU) (BDR BDR) (RUB RUB) (RBD RBD)
       (RDF RDF) (RFU RFU) (DLF FLU) (DBL FDL) (DRB DRB) (DFR DFR) (FLU ULB)
       (FUR FUR) (FRD FRD) (FDL UFL))
      ((BU BU) (BR BR) (BL DL) (BD BD) (UB UB) (UR UR) (UL BL) (UF UF) (RB RB)
       (RU RU) (RD RD) (RF RF) (LB LD) (LU LB) (LD LF) (LF LU) (DB DB) (DR DR)
       (DL FL) (DF DF) (FU FU) (FR FR) (FL UL) (FD FD))))
    (D1
     ((D1)
      ((LUF LUF) (LBU LBU) (LDB FDL) (LFD FRD) (ULB ULB) (UBR UBR) (URF URF)
       (UFL UFL) (BLD LFD) (BUL BUL) (BRU BRU) (BDR LDB) (RUB RUB) (RBD BLD)
       (RDF BDR) (RFU RFU) (DLF DFR) (DBL DLF) (DRB DBL) (DFR DRB) (FLU FLU)
       (FUR FUR) (FRD RBD) (FDL RDF))
      ((BU BU) (BR BR) (BL BL) (BD LD) (UB UB) (UR UR) (UL UL) (UF UF) (RB RB)
       (RU RU) (RD BD) (RF RF) (LB LB) (LU LU) (LD FD) (LF LF) (DB DL) (DR DB)
       (DL DF) (DF DR) (FU FU) (FR FR) (FL FL) (FD RD))))
    (D2
     ((D2)
      ((LUF LUF) (LBU LBU) (LDB RDF) (LFD RBD) (ULB ULB) (UBR UBR) (URF URF)
       (UFL UFL) (BLD FRD) (BUL BUL) (BRU BRU) (BDR FDL) (RUB RUB) (RBD LFD)
       (RDF LDB) (RFU RFU) (DLF DRB) (DBL DFR) (DRB DLF) (DFR DBL) (FLU FLU)
       (FUR FUR) (FRD BLD) (FDL BDR))
      ((BU BU) (BR BR) (BL BL) (BD FD) (UB UB) (UR UR) (UL UL) (UF UF) (RB RB)
       (RU RU) (RD LD) (RF RF) (LB LB) (LU LU) (LD RD) (LF LF) (DB DF) (DR DL)
       (DL DR) (DF DB) (FU FU) (FR FR) (FL FL) (FD BD))))
    (D3
     ((D3)
      ((LUF LUF) (LBU LBU) (LDB BDR) (LFD BLD) (ULB ULB) (UBR UBR) (URF URF)
       (UFL UFL) (BLD RBD) (BUL BUL) (BRU BRU) (BDR RDF) (RUB RUB) (RBD FRD)
       (RDF FDL) (RFU RFU) (DLF DBL) (DBL DRB) (DRB DFR) (DFR DLF) (FLU FLU)
       (FUR FUR) (FRD LFD) (FDL LDB))
      ((BU BU) (BR BR) (BL BL) (BD RD) (UB UB) (UR UR) (UL UL) (UF UF) (RB RB)
       (RU RU) (RD FD) (RF RF) (LB LB) (LU LU) (LD BD) (LF LF) (DB DR) (DR DF)
       (DL DB) (DF DL) (FU FU) (FR FR) (FL FL) (FD LD))))
    (B1
     ((B1)
      ((LUF LUF) (LBU DBL) (LDB DRB) (LFD LFD) (ULB LDB) (UBR LBU) (URF URF)
       (UFL UFL) (BLD BDR) (BUL BLD) (BRU BUL) (BDR BRU) (RUB ULB) (RBD UBR)
       (RDF RDF) (RFU RFU) (DLF DLF) (DBL RBD) (DRB RUB) (DFR DFR) (FLU FLU)
       (FUR FUR) (FRD FRD) (FDL FDL))
      ((BU BL) (BR BU) (BL BD) (BD BR) (UB LB) (UR UR) (UL UL) (UF UF) (RB UB)
       (RU RU) (RD RD) (RF RF) (LB DB) (LU LU) (LD LD) (LF LF) (DB RB) (DR DR)
       (DL DL) (DF DF) (FU FU) (FR FR) (FL FL) (FD FD))))
    (B2
     ((B2)
      ((LUF LUF) (LBU RBD) (LDB RUB) (LFD LFD) (ULB DRB) (UBR DBL) (URF URF)
       (UFL UFL) (BLD BRU) (BUL BDR) (BRU BLD) (BDR BUL) (RUB LDB) (RBD LBU)
       (RDF RDF) (RFU RFU) (DLF DLF) (DBL UBR) (DRB ULB) (DFR DFR) (FLU FLU)
       (FUR FUR) (FRD FRD) (FDL FDL))
      ((BU BD) (BR BL) (BL BR) (BD BU) (UB DB) (UR UR) (UL UL) (UF UF) (RB LB)
       (RU RU) (RD RD) (RF RF) (LB RB) (LU LU) (LD LD) (LF LF) (DB UB) (DR DR)
       (DL DL) (DF DF) (FU FU) (FR FR) (FL FL) (FD FD))))
    (B3
     ((B3)
      ((LUF LUF) (LBU UBR) (LDB ULB) (LFD LFD) (ULB RUB) (UBR RBD) (URF URF)
       (UFL UFL) (BLD BUL) (BUL BRU) (BRU BDR) (BDR BLD) (RUB DRB) (RBD DBL)
       (RDF RDF) (RFU RFU) (DLF DLF) (DBL LBU) (DRB LDB) (DFR DFR) (FLU FLU)
       (FUR FUR) (FRD FRD) (FDL FDL))
      ((BU BR) (BR BD) (BL BU) (BD BL) (UB RB) (UR UR) (UL UL) (UF UF) (RB DB)
       (RU RU) (RD RD) (RF RF) (LB UB) (LU LU) (LD LD) (LF LF) (DB LB) (DR DR)
       (DL DL) (DF DF) (FU FU) (FR FR) (FL FL) (FD FD))))))
(defparameter op-i I)
(defparameter *corners8* '(ULB UFL DBL DLF UBR URF DRB DFR))
(defparameter *yang-corners* '(ULB URF DRB DLF))
(defparameter *e-edges* '(FR BL FL BR))
(defparameter *m-edges* '(DB DF UF UB))
(defparameter *edges12*
  '(FR UF DL FL UL DB BL UB DR BR UR DF))
(defparameter *ops-phase1* 
  '(U1 U2 U3 D1 D2 D3 L1 L2 L3 R1 R2 R3 F1 F2 F3 B1 B2 B3))
(defparameter *ops-phase2*
  '(U1 U2 U3 D1 D2 D3 L1 L2 L3 R1 R2 R3 F2 B2))
(defparameter *ops-phase3*
  '(U1 U2 U3 D1 D2 D3 L2 R2 F2 B2))
(defparameter *ops-phase4*
  '(U2 D2 L2 R2 F2 B2))

;;;;;;  math base and lisp base
(defun comb(n k)
  (cond ((> k n) 0)
	((> k (/ n 2)) (comb n (- n k)))
	('t
	 (do ((x 1)
	      (y 1)
	      (i n (- i 1))
	      (j k (- j 1)))	     
	     ((= j 0) (/ x y))
	   (progn (setf x (* x i))
		  (setf y (* y j)))))))      

(defun pack-comb(a k)
  (do((i 0 (+ i 1))
      (j k)
      (x 0)
      (n (array-dimension a 0)))
     ((= i (- n j)) x)       
    (progn
      (when (> (aref a i) 0)
	(incf x (comb (- n i 1) j))
	(decf j)))))       

(defun symbol-add(sym1 sym2)
  (intern (format nil "~A~A" sym1 sym2)))

(defun sigma(L)(reduce #'symbol-add L))

(defun string-to-chars(s)
  (loop for c across s collect c))

(defun chars-to-symbols(s)
  (mapcar (lambda(c)
	    (if (and (char>= c #\0)
		     (char<= c #\9))
		(- (char-code c)(char-code #\0))
		(intern (format nil "~A" c))))
	  s))

(defun split-symbol(s)
  (chars-to-symbols (string-to-chars (symbol-name s))))

;;;;;; operates on formula

(defun my-combine(x y)
  (if (null x)
      y
      (if (null y)
	  x
	  (mapcar (lambda(item)
		    (list (first item)
			  (second (assoc (second item) y))))
		  x))))

(defun mul-op(op1 op2)
  (list 
   (append  (first op1)(first op2))
   (my-combine (second op1)(second op2))
   (my-combine (third op1)(third op2))))

;(defun new-op-seq(&rest ops)
;  (reduce #'mul-op ops))

(defun new-formula(ops-list)
  (if ops-list
      (reduce #'mul-op 
	      (mapcar #'(lambda(sym)(second(assoc sym *op18*)))
		      ops-list))
      op-i))

(defun hy-mul(seq op)
  (mul-op seq (second (assoc op *op18*))))

;;;;;; analyze the cube's staus
(defun corner-to-number(c)
  (let ((re 0)
	(c (split-symbol c)))
    (progn 
      (if (member 'L c)(setf re (+ re 4)))
      (if (member 'U c)(setf re (+ re 2)))
      (if (member 'B c)(setf re (+ re 1)))
      re)))

(defun number-to-corner(n)
  (nth n '(DFR DRB URF UBR DLF DBL UFL ULB)))

(defun edge-to-number(e)
  (second (assoc e 
		 '((FR 0) (UF 1) (DL 2) (FL 3) 
		   (UL 4) (DB 5) (BL 6) (UB 7)
		   (DR 8) (BR 9) (UR 10) (DF 11)))))

(defun op-corner(c)
  (number-to-corner ( - 7 (corner-to-number c))))

(defun corners-in(seq set)
  (let* ((es (second seq)))
    (mapcar #'reverse
	    (loop for e in set
	       collect (assoc e (mapcar #'reverse es))))))

(defun edges-in(seq set)
  (let* ((es (third seq)))
    (mapcar #'reverse
	    (loop for e in set
	       collect (assoc e (mapcar #'reverse es))))))

(defun is-std(e)
  (member e *edges12*))  

(defun pair-status(pair)
  (let ((cubie (first pair))
	(loc (second pair)))
    (or (and (is-std cubie)
	     (is-std loc))
	(and (not (is-std cubie))
	     (not (is-std loc))))))	    

(defun edges-o(seq)
  (mapcar #'pair-status (edges-in seq *edges12*)))

(defun pack-edges-choice(seq set)
  (let ((es (edges-in seq *edges12*))
	(ar (make-array 12)))    
    (loop for e in es when (member (first e) set)
       do(setf (aref ar (edge-to-number (second e))) 1)
       finally(return (pack-comb ar (length set))))))

(defun status-of-corner-pair(pair)
  (let* ((cubie (first pair))
	 (loc (second pair))
	 (c-l (split-symbol cubie))
	 (l-l (split-symbol loc)))
    (loop for ic from 0 to 2
       when (or (eql (nth ic c-l) 'U)
		(eql (nth ic c-l) 'D))
       return (nth ic l-l))))	 

(defun reduce-locs(ns)
  (let* ((rn1 (mapcar #'(lambda(i)(boole boole-xor i (first ns))) ns))
	 (rn2 (cdr rn1))
	 (rn3 (list (first rn2)(< (second rn2)(third rn2)))))
    (sigma rn3)))

(defun pack-yang(seq0)
  (let* ((corners (corners-in seq0
			      '(ULB UFL DBL DLF DFR DRB URF UBR)))
	 (cs (mapcar #'first corners))
	 (i 0) (j 0)(k 0)(yangs nil)(yins nil)
	 (a-comb (make-array 12)))
    (progn
      (loop for c in cs
	 do(progn
	     (if (member c *yang-corners*)
		 (progn (setf (aref a-comb k) 1)
			(setf yangs (cons (list i c) yangs))
			(incf i))
		 (progn (setf yins (cons (list c j) yins))
			(incf j)))
	     (incf k)))
      (setf yangs (reverse yangs))
      (setf yins (reverse yins))
      (sigma
       (list
	(reduce-locs
	 (loop for i-c in yangs
	    collect (second (assoc (op-corner (second i-c)) yins))))
	(pack-comb a-comb 4))))))

(defun pack-oe(seq)
  (let ((oe(edges-o seq)))
    (symbol-add 'OE
		(loop for i from 0 to 10
		   sum (if (null(nth i oe))(expt 2 i) 0)))))

(defun pack-ee(seq)
  (symbol-add 'EE
	      (pack-edges-choice seq *e-edges*)))

(defun pack-oc(seq)
  (sigma
   (mapcar #'status-of-corner-pair 
	   (corners-in seq *corners8*))))

(defun pack-me(seq)
  (symbol-add 'ME
	      (pack-edges-choice seq *m-edges*)))

(defun pack-corners-perm(seq)
  (let ((cs (corners-in seq *corners8*)))
    (sigma (mapcar #'first cs))))

(defun pack-edges-perm(seq)
  (let ((es (edges-in seq *edges12*)))
    (sigma (mapcar #'first es))))

;;;; search

(defparameter *cur-phase* nil)
(defparameter *the-cube* nil)
(defparameter *depth-to-go* nil)
(defparameter *hash-table* nil)
(defparameter *search-mode* 'nil)

(defun hashf(&optional (cube *the-cube*))
  (case *cur-phase*
    (1 (pack-oe cube))
    (2 (sigma (list (pack-ee cube) (pack-oc cube))))
    (3 (sigma (list (pack-yang cube)(pack-me cube))))
    (4 (sigma (list (pack-edges-perm cube)(pack-corners-perm cube))))))

(defun solved()
  (equal (hashf) (hashf op-i)))

(defun easy-get(x)
  (multiple-value-bind(v has)
      (gethash x *hash-table*)
    (if has
	v
	6))) ;;;;6 is default

(defun easy-set(k v)
  (setf (gethash k *hash-table*)
	v))

(defun op-table()
  (case *cur-phase*
    (1 *ops-phase1*)
    (2 *ops-phase2*)
    (3 *ops-phase3*)
    (4 *ops-phase4*)))

(defun escape(st)(multiple-value-bind(v has)
		     (gethash st  *depth-to-go*)
		   (if has v 0)))
(defun my-xor (a b)
  (if a (not b) b))


(defun do-search(dpt)
  (let((st0 (hashf)))
    (if (my-xor *search-mode* 
	     (< dpt (easy-get  st0)))	
	(progn
	  (if (and *search-mode* (<= dpt (escape st0)))
	      (solved)
	      (progn		
		(when *search-mode*
		  (setf (gethash st0 *depth-to-go*)
			dpt))
		(when (not *search-mode*);;;???
		  (easy-set st0
			    (min dpt (easy-get st0))))

		(loop for op in (op-table)
		   do(let* ((old *the-cube*))
		       (progn
			 (setf *the-cube*
			       (hy-mul old op))
			 ;;(format t "~a~%" (first *the-cube*))
			 (if (do-search (+ dpt 1 
					   (if *search-mode* -2 0)))
			     (return 't))
			 (setf *the-cube* old))))))))))
;;;;;; for data
(defun load-hash(file-name)
  (let((re (make-hash-table)))
    (with-open-file (str file-name)
      (do ((kv (read str nil :eof) (read str nil :eof)))
	  ((eql kv :eof) re)
	(setf (gethash (first kv) re) (second kv))))))

(defun save-hash(h file)
  (with-open-file (str file :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (maphash (lambda(k v)
	       (format str "~a~%" (list k v))) h)))

;;;;;; for test
(defun gen-rander()
  (let ((record -1))
    (lambda(n)
      (do ((c (random n) (random n)))
	  ((not (= c record)) (setf record c))))))

(defvar my-rand (gen-rander)) 

(defun randop()
  (let* ((face (nth (funcall my-rand 6) '(R U F L D B)))
	 (times (1+ (random 3)))
	 (op (sigma (list face times))))
    op))

(defun rand-formula(&optional (len 26))
  (loop for i from 1 to len collect (randop)))

;;;;;; main
(defparameter my-data "onekey.tb")
(defun create-table()
  (progn
    (setf *hash-table* (make-hash-table))
    (setf *the-cube* op-i)
    (setf *search-mode* nil)
    (loop for i from 1 to 4
       do(progn (setf *cur-phase* i)
		(do-search 0)))
    (save-hash *hash-table* my-data)))

(defun solve-phase(x)
  (progn
    (setf *cur-phase* x)
    (loop for i from 0 to 19
       do(if (do-search i) (return (first *the-cube*))))))


(defun solve-formula(fm)
  (progn
    (setf *hash-table* (load-hash my-data))
    (setf *depth-to-go* (make-hash-table))
    (setf *the-cube* (new-formula fm))   
    (setf *search-mode* 't)
    (and (solve-phase 1)
	 (solve-phase 2)
	 (solve-phase 3)
	 (solve-phase 4)
	 (first *the-cube*))))	 

;;;;;; test

(defun test()
  (progn
    (when (not (probe-file  my-data))
      (create-table))
    (let ((s (rand-formula)))
      (progn
	(format t "Scramble:~a~%" s)
	(time (solve-formula s))))))