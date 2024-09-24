#lang play

;; NOMBRE Y APELLIDO: Andrés Calderón
;; RUT: 21.273.734-8
;; Hizo Ud uso de la whiteboard policy: NO

;;----- ;;
;; P1.a ;;
;;----- ;;

;; <prop> ::= (tt)
;; 			| (ff)
;; 			| (p-not <prop>)
;; 			| (p-and List<prop>)
;; 			| (p-or List<prop>)
;; 			| (p-id <sym>)
;; 			| (p-where <expr> [<sym> <expr>])
;; Constructor de proposiciones booleanas
(deftype Prop
	(tt)
	(ff)
	(p-not p)
	(p-and ps)
	(p-or ps)
	(p-id x)
	(p-where where x expr)
)


;;----- ;;
;; P1.b ;;
;;----- ;;

;; Concrete syntax of propositions:
;; <s-prop> ::= true
;; 			  | false
;; 			  | (list 'p-not <s-expr>)
;; 			  | (list 'p-and (list <s-expr>))
;; 			  | (list 'p-or (list <s-expr>))
;; 			  | (list 'p-where <s-expr> [<sym> <s-expr>])
;; 			  | <sym>
;; parse-prop : <s-prop> -> Prop
(define (parse-prop s-expr)
	(match s-expr
		['true (tt)]
		['false (ff)]
		[(? symbol? x) (p-id x)]
		[(list 'not elems ...)
			(if (equal? (length elems) 1)
				(p-not (parse-prop (car elems)))
				(error "parse-prop: not expects only one operand")
			)
		]
		[(list 'and elems ...)
			(if (> (length elems) 1)
				(p-and (map parse-prop elems))
				(error "parse-prop: and expects at least two operands")
			)
		]
		[(list 'or elems ...)
			(if (> (length elems) 1)
				(p-or (map parse-prop elems))
				(error "parse-prop: or expects at least two operands")
			)
		]
		[(list x 'where [list y expr]) (p-where (parse-prop x) y (parse-prop expr))]
	)
)


;;----- ;;
;; P1.c ;;
;;----- ;;


#|
<value> ::= ...
|#

;; (deftype PValue ...)

;; from-Pvalue : PValue -> Prop
(define (from-Pvalue p-value) '???)


;;----- ;;
;; P1.d ;;
;;----- ;;


;; p-subst : Prop Symbol Prop -> Prop
(define (p-subst target name substitution) '???)


;;----- ;;
;; P1.e ;;
;;----- ;;


;; eval-or : (Listof Prop) -> PValue
(define (eval-or ps) '???)

;; eval-and : (Listof Prop) -> PValue
(define (eval-and ps) '???)

;; p-eval : Prop -> PValue
(define (p-eval p) '???)

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

;;----- ;;
;; P2.a ;;
;;----- ;;


#|
<expr> ::= ...
        | (add <expr> <expr>)
        | (sub <expr> <expr>)
        | (if0 <expr> <expr> <expr>)
        | ...
|#
(deftype Expr
  ; ...
  (add l r)
  (sub l r)
  (if0 c t f)
  ; ...
  )

;;----- ;;
;; P2.b ;;
;;----- ;;

#|
Concrete syntax of expressions:

<s-expr> ::= ...
        | (+ <s-expr> <s-expr>)
        | (- <s-expr> <s-expr>)
        | (if0 <s-expr> <s-expr> <s-expr>)
        | ...
|#

;; parse : <s-expr> -> Expr

(define (parse s-expr) '???)

;;----- ;;
;; P2.c ;;
;;----- ;;

;; subst :: Expr Symbol Expr -> Expr
(define (subst in what for) '???)

;;----- ;;
;; P2.d ;;
;;----- ;;

#|
<cvalue> ::= (compV <num> <num>)
|#

(deftype CValue (compV r i))

;; from-CValue :: CValue -> Expr
(define (from-CValue v) '???)

;; cmplx+ :: CValue CValue -> CValue
(define (cmplx+ v1 v2) '???)

;; cmplx- :: CValue CValue -> CValue
(define (cmplx- v1 v2) '???)

;; cmplx0? :: CValue -> Boolean
(define (cmplx0? v) '???)


;;----- ;;
;; P2.e ;;
;;----- ;;

;; interp : Expr -> CValue
(define (interp expr) '???)
