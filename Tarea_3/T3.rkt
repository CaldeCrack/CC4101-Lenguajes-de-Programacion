#lang play

;; NOMBRE Y APELLIDO: Andrés Calderón
;; RUT: 21.273.734-8
;; Hizo Ud uso de la whiteboard policy: NO


;;------------------;;
;; P1.a, P1.e, P2.a ;;
;;------------------;;

;; <expr> ::= (num <Integer>)
;; 			| (add <expr> <expr>)
;; 			| (nil)
;; 			| (conz <expr> <expr>)
;; 			| (fun <pattern> <expr>)
;; 			| (app <expr> <expr>)
;; 			| (id <sym>)
;; 			| (pmatch <expr> List<<pattern> <expr>>)
;; Constructor de expresiones del lenguaje
(deftype Expr
	(num n)
	(add l r)
	(nil)
	(conz l r)
	(fun p e)
	(app f x)
	(id x)
	(pmatch p cases)
)


;;------------------;;
;; P1.b, P1.e, P2.b ;;
;;------------------;;

;; <s-expr> ::= <Integer>
;; 				| <sym>
;; 				| '(nil)
;; 				| (list '+ <s-expr> <s-expr>)
;; 				| (list 'cons <s-expr> <s-expr>)
;; 				| (list 'list (list <s-expr>))
;; 				| (list 'fun <pattern> <s-expr>)
;; 				| (list <s-expr> <s-expr>)
;; 				| (list 'match <s-expr> (list <<pattern> <s-expr>>))
;; parse : <s-expr> -> <Expr>
;; Parsea el lenguaje funcional
(define (parse s-expr)
	(match s-expr
		[(? number? n) (num n)]
		[(? symbol? x) (id x)]
		['(nil) (nil)]
		[(list '+ l r) (add (parse l) (parse r))]
		[(list 'cons l r) (conz (parse l) (parse r))]
		[(list 'list elems ...) (foldr (λ (elem lista) (conz (parse elem) lista)) (nil) elems)]
		[(list 'fun p e) (fun (parse-pattern p) (parse e))]
		[(list 'match p cases ...) (if (empty? cases)
			(error "SyntaxError: match expression must have at least one case")
			(pmatch (parse p) (map (λ (e) (cons (parse-pattern (car e)) (parse (car (cdr e))))) cases))
		)]
		[(list f x) (app (parse f) (parse x))]
	)
)


;;----- ;;
;; P1.c ;;
;;----- ;;

;; <pattern> ::= (numP <Integer>)
;; 				| (nilP)
;; 				| (varP <sym>)
;; 				| (conzP <pattern> <pattern>)
;; Constructor de patrones del lenguaje
(deftype Pattern
	(numP n)
	(nilP)
	(varP x)
	(conzP l r)
)


;;----- ;;
;; P1.d ;;
;;----- ;;

;; <s-expr> ::= <Integer>
;; 				| '(nil)
;; 				| <sym>
;; 				| (list 'cons <s-expr> <s-expr>)
;; 				| (list 'list (list <s-expr>))
;; parse-pattern : s-expr -> Pattern
;; Parsea los patrones del lenguaje
(define (parse-pattern s-expr)
	(match s-expr
		[(? number? n) (numP n)]
		['(nil) (nilP)]
		[(? symbol? x) (varP x)]
		[(list 'cons l r) (conzP (parse-pattern l) (parse-pattern r))]
		[(list 'list elems ...) (foldr (λ (elem lista) (conzP (parse-pattern elem) lista)) (nilP) elems)]
	)
)


;;----- ;;
;; P1.f ;;
;;----- ;;

;; <value> ::= (numV <Integer>)
;; 				| (conzV <value> <value>)
;; 				| (closureV <sym> <expr> <env>)
;; Constructor para los valores del lenguaje
(deftype Value
	(numV n)
	(nilV)
	(conzV l r)
	(closureV id body env)
)

#|
BEGIN utility definitions
|#

;; <env> ::= (mtEnv)
;; 			| (extEnv <sym> <value> <env>)
;; Constructor de los ambientes del lenguaje
(deftype Env
	(mtEnv)
	(extEnv x v env))

;; extend-env : Symbol Value Env -> Env
(define (extend-env x v env)
	(extEnv x v env))

;; empty-env : Env
(define empty-env (mtEnv))

;; extend-env* : (Listof (Symbol * Value)) Env -> Env
(define (extend-env* bindings env)
	(foldr
		(lambda (binding env) (match binding [(cons x v) (extend-env x v env)]))
		env
		bindings))

;; lookup-env : Symbol Env -> Value
(define (lookup-env x env)
	(match env
		[(mtEnv) (error "LookupError: variable ~a not found" x)]
		[(extEnv y v env) (if (equal? x y) v (lookup-env x env))]))

;; num+ : Value Value -> Value
(define (num+ v1 v2)
	(match v1
		[(numV n) (match v2
				[(numV m) (numV (+ n m))]
				[_ (error "TypeError: expected a number")])]
		[_ (error "TypeError: expected a number")]))

#|
END utility definitions
|#


;;----- ;;
;; P1.g ;;
;;----- ;;

;; <result> e v ::= (failure e)
;; 				| (success v)
;; Constructor de resultados del lenguaje
(deftype Result
	(failure e)
	(success v))

;; generate-substs : Pattern Value -> (Result String (Listof (Symbol * Value)))
;; A partir de un patrón y un valor, retorna una lista de substituciones o un mensaje de error
(define (generate-substs p v)
	;; Función auxiliar de generate-substs
	(define (aux-generate-substs p v)
		(match p
			[(numP x) (match v
				[(numV y) (if (= x y)
					'()
					(failure "MatchError: given number does not match pattern"))]
				[else (failure "MatchError: expected a number")]
			)]
			[(nilP) (match v
				[(nilV) '()]
				[else (failure "MatchError: expected nil")]
			)]
			[(varP x) (list (cons x v))]
			[(conzP x y) (match v
				[(conzV l r)
					(define leftResult (aux-generate-substs x l))
					(define rightResult (aux-generate-substs y r))
					(if (failure? leftResult)
						leftResult
						(if (failure? rightResult)
							rightResult
							(append leftResult rightResult)
						)
					)
				]
				[else (failure "MatchError: expected a cons constructor")]
			)]
		)
	)

	(define lista (aux-generate-substs p v))
	(if (failure? lista)
		lista
		(success lista)
	)
)


;;------------;;
;; P1.h, P2.c ;;
;;------------;;

;; interp : Expr Env -> Value
;; Intérprete del lenguaje
(define (interp expr env)
	(match expr
		[(num n) (numV n)]
		[(add l r) (num+ (interp l env) (interp r env))]
		[(nil) (nilV)]
		[(conz l r) (conzV (interp l env) (interp r env))]
		[(fun id body) (closureV id body env)]
		[(id x) (lookup-env x env)]
		[(pmatch p cases)
			(define substs (generate-substs (car (car cases)) (interp p env)))
			(match substs
				[(success s) (interp (cdr (car cases)) (extend-env* s env))]
				[(failure _) (if (empty? (cdr cases))
					(error "MatchError: expression does not match any pattern")
					(interp (pmatch p (cdr cases)) env)
				)]
			)
		]
		[(app f e)
			(define substs (generate-substs (match f
				[(num n) (parse-pattern n)]
				[(nil) (parse-pattern '(nil))]
				[(id x) (parse-pattern x)]
				[(conz l r) (parse-pattern (list 'cons l r))]
				[(fun p _) p]
			) (interp e env)))
			(match substs
				[(failure fail) (error fail)]
				[(success s)
					(def (closureV the-arg the-body closed-env) (interp f env))
					(def new-env (extend-env* s closed-env))
					(interp the-body new-env)
				]
			)
		]
	)
)


;;----- ;;
;; P2.d ;;
;;----- ;;

#|
En función de lo implementado en la pregunta anterior, argumente porqué es útil que la función
generate-substs no lance un error (cuando el valor no calza con el patrón) y, en cambio, retorne
un mensaje.

R:
En este caso, es útil puesto que no detiene la ejecución del código y permite verificar todos los
patrones que se quieran dentro del match, además de que puede servir para obtener una
contextualización de porque para cierto caso no se pudo realizar el match.

|#
