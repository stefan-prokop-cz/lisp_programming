; x^n - koncovou rekurzi
(defun binarnimockonc (x n)
	(if (zerop n)
		1
		(binmock x n 1)
	)
)

(defun binmock (x n vysl)
	(if (= n 1)
		(* vysl x)
		(if (evenp n)
			(binmock (* x x) (/ n 2) vysl)
			(binmock (* x x) (/ (- n 1) 2) (* vysl x))
		)
	)		
)
; ------------------------------------

; Fibonacci
(defun fib (n)
	(if (zerop n)
		0
		(if (= n 1)
			1
			(+ (fib (- n 1)) (fib (- n 2)))
		)
	)
)

; Fibonacci koncovou rekurzi
(defun fibopt (n)
	(if (zerop n)
		0
		(if (= n 1)
			1
			(fibkonc n 0 1)
		)
	)
)

; akumulator
(defun fibkonc (n f1 f2)
	(if (zerop n)
		f1
		(fibkonc (- n 1) f2 (+ f1 f2))
	)
)
; ------------------------------------

; testovani predikatu shodnosti
; ruzne chovani v ruznych odnozich lispu
	; = (aritmeticka rovnost - rovnost dvou cisel (= 1 1) -> T (= 1 1.0) -> T)
	; eq (znamena identitu - naprosto ve vsem (eq 1 1.0) -> NIL, (eq 1 1) -> T)
	; eql, equal - identita v nejake forme, podobne jako eq
	; equalp (nejobecnejsi shoda - porovna rovnost vcetne struktur (equalp 1 1) -> T (equalp 1 1.0) -> T)
	; vse, krome = vraci T -> (eq 'a 'A)

; Seznamy
	; - neresi jake prvky v seznamu jsou
	; prazdny seznam: 	() nebo NIL
	; 1-prvkovy: 		(1), (a)
	; viceprvkovy: 		(1 'ahoj 4/4)
	; l = (1 'ahoj 4/4) -> (car l) -> 1, car - vraci prvni polozku seznamu, 1 = hlava, zbytek = ocas / telo
	; (cdr l) -> ('ahoj 4/4) - vraci zbytek bez prvniho prvku

; Vratit 2. polozku seznamu
(defun druhy (l)
	; osetri prazdny seznam
	(if (equalp l NIL)
		NIL
		; osetri jednoprvkovy seznam
		(if (equalp (cdr l) NIL)
			NIL
			; jinak vraci druhy prvek seznamu
			(car (cdr l))
		)
	)
)

; Delka seznamu
(defun listLength (l)
	(if (equalp l NIL)
		0
		(+ 1 (listLength (cdr l)))
	)
)

; Delka vcetne polozek v podseznamu
; (delkaPod '(1 2 (3 4) 5)) -> 5
; (delkaPod '(1 (2 ((3) 4)) (5) ())) -> 5
; - vcetne jakekoliv urovne podseznamu
(defun delkaPod (l)
	(if (equalp l NIL)
		0
		(if (listp (car l)) ; listp - je zadany parametr seznam?
			(+ (delkaPod (car l)) (delkaPod (cdr l)))
			(+ 1 (delkaPod (cdr l)))
		)
	)
)

; Je x v seznamu
; (findp 5 '(1 2 3 4 5)) -> T
; (findp 5 '(1 2 (3 5)) -> NIL
; (findp 5 '(1 2 (3 4) 5) -> T
; - nehleda v podseznamech
(defun findp (x l)
	(if (equalp l NIL)
		NIL
		(if (equalp (car l) x)
			T
			(findp x (cdr l))
		)
	)
)

; - hleda v podseznamech
; (findp 5 '(1 2 (3 5)) -> T
; TODO - dokoncit
(defun findp2 (x l)
	(if (equalp l NIL)
		NIL
		(if (listp (car l))
			(or (findp2 x (car l)) (findp2 x (cdr l)))
			(if (equalp (car l) x
				T
				(findp2 x (cdr l))
			)
		)
	)
)
