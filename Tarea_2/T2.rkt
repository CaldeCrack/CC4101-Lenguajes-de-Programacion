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
;; 			| (p-where <expr> <sym> <expr>)
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

;; <s-prop> ::= 'true
;; 			  | 'false
;; 			  | (list 'not <s-expr>)
;; 			  | (list 'and (list <s-expr>))
;; 			  | (list 'or (list <s-expr>))
;; 			  | (list 'where <s-expr> [<sym> <s-expr>])
;; 			  | <sym>
;; parse-prop : <s-prop> -> Prop
;; Parsea el lenguaje de proposiciones lógicas
(define (parse-prop s-expr)
	(match s-expr
		['true (tt)]
		['false (ff)]
		[(? symbol? x) (p-id x)]
		[(list 'not elems ...)
			(if (equal? (length elems) 1)
				(p-not (parse-prop (car elems)))
				(error "parse-prop: 'not' expects only one operand")
			)
		]
		[(list 'and elems ...)
			(if (> (length elems) 1)
				(p-and (map parse-prop elems))
				(error "parse-prop: 'and' expects at least two operands")
			)
		]
		[(list 'or elems ...)
			(if (> (length elems) 1)
				(p-or (map parse-prop elems))
				(error "parse-prop: 'or' expects at least two operands")
			)
		]
		[(list x 'where [list y expr]) (p-where (parse-prop x) y (parse-prop expr))]
	)
)


;;----- ;;
;; P1.c ;;
;;----- ;;

;; <value> ::= (ttV)
;; 			 | (ffV)
;; Constructor que captura la noción de valores del lenguaje
(deftype PValue
	(ttV)
	(ffV)
)

;; from-Pvalue : PValue -> Prop
;; Convierte un PValue en elemento Prop
(define (from-Pvalue p-value)
	(match p-value
		[(? ttV?) (tt)]
		[(? ffV?) (ff)]
	)
)


;;----- ;;
;; P1.d ;;
;;----- ;;

;; p-subst : Prop Symbol Prop -> Prop
;; Realiza la substitución de una proposición por un identificador
(define (p-subst target name substitution)
	(match target
		[(tt) (tt)]
		[(ff) (ff)]
		[(p-not x) (p-not (p-subst x name substitution))]
		[(p-and elems) (p-and (map (λ (elem) (p-subst elem name substitution)) elems))]
		[(p-or elems) (p-or (map (λ (elem) (p-subst elem name substitution)) elems))]
		[(p-id x)
			(if (symbol=? x name)
				substitution
				(p-id x)
			)
		]
		[(p-where where x expr)
			(p-where
				(if (symbol=? x name)
					where
					(p-subst where name substitution)
				)
				x
				(p-subst expr name substitution)
			)
		]
	)
)


;;----- ;;
;; P1.e ;;
;;----- ;;

;; eval-or : (Listof Prop) -> PValue
;; Reduce recursivamente la operación o lógica
(define (eval-or ps)
	(match ps
		['() (ffV)]
		[(list x elems ...)
			(if (ttV? (p-eval x))
				(ttV)
				(eval-or elems)
			)
		]
	)
)

;; eval-and : (Listof Prop) -> PValue
;; Reduce recursivamente la operación y lógica
(define (eval-and ps)
	(match ps
		['() (ttV)]
		[(list x elems ...)
			(if (ffV? (p-eval x))
				(ffV)
				(eval-and elems)
			)
		]
	)
)

;; p-eval : Prop -> PValue
;; Reduce una proposición a un valor del lenguaje
(define (p-eval p)
	(match p
		[(? tt?) (ttV)]
		[(? ff?) (ffV)]
		[(p-not x)
			(if (ttV? (p-eval x))
				(ffV)
				(ttV)
			)
		]
		[(p-and elems) (eval-and elems)]
		[(p-or elems) (eval-or elems)]
		[(p-id x) (error "p-eval: open expression (free occurrence of ~a)" x)]
		[(p-where where x expr) (p-eval (p-subst where x (from-Pvalue (p-eval expr))))]
	)
)

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

;;----- ;;
;; P2.a ;;
;;----- ;;

;; <expr> ::= (real)
;; 			| (imaginary)
;;          | (add <expr> <expr>)
;;          | (sub <expr> <expr>)
;;          | (if0 <expr> <expr> <expr>)
;;          | (id <sym>)
;;          | (with [(<sym> <expr>)*] <expr>)
;; Constructor de números reales e imaginarios
(deftype Expr
	(real r)
	(imaginary i)
	(add l r)
	(sub l r)
	(if0 c t f)
	(id x)
	(with elems expr)
)

;;----- ;;
;; P2.b ;;
;;----- ;;

;; <s-expr> ::= <number>
;; 			  | (list <number> 'i)
;; 			  | (list '+ <s-expr> <s-expr>)
;; 			  | (list '- <s-expr> <s-expr>)
;; 			  | (list 'if0 <s-expr> <s-expr> <s-expr>)
;; 			  | (list 'with (list <s-expr>) body)
;; 			  | <sym>
;; parse : <s-expr> -> Expr
;; Parsea el lenguaje de números complejos
(define (parse s-expr)
	(match s-expr
		[(? number? n) (real n)]
		[(list (? number? n) 'i) (imaginary n)]
		[(? symbol? x) (id x)]
		[(list '+ l-sexpr r-sexpr) (add (parse l-sexpr) (parse r-sexpr))]
		[(list '- l-sexpr r-sexpr) (sub (parse l-sexpr) (parse r-sexpr))]
		[(list 'if0 c-sexpr t-sexpr f-sexpr)
			(if0 (parse c-sexpr) (parse t-sexpr) (parse f-sexpr))
		]
		[(list 'with elems body)
			(if (> (length elems) 0)
				(with (map (λ (e) (cons (car e) (parse (car (cdr e))))) elems) (parse body))
				(error "parse: 'with' expects at least one definition")
			)
		]
	)
)

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
