#lang play

#|

Hizo Ud uso de la whiteboard policy: (Indique SI/NO)
En caso que afirmativo, indique con quién y sobre qué ejercicio:
-
-

|#

;;------------------;;
;; P1.a, P1.e, P2.a ;;
;;------------------;;


#|
<expr> ::= ...
|#
(deftype Expr
  (num n)
  (add l r)
  ; ...
  )


;;------------------;;
;; P1.b, P1.e, P2.b ;;
;;------------------;;


;; parse : s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    [(? number? n) (num n)]
    [(list '+ l r) (add (parse l) (parse r))]
    ; ...
    ))


;;----- ;;
;; P1.c ;;
;;----- ;;

#|
<pattern> ::= ...
|#
(deftype Pattern
  ; ...
  )

;;----- ;;
;; P1.d ;;
;;----- ;;

;; parse-pattern : s-expr -> Pattern
(define (parse-pattern s-expr) '???)

;;----- ;;
;; P1.f ;;
;;----- ;;

#|
<value> ::= ...
|#
(deftype Value
  (numV n)
  ; ...
  )

#|
BEGIN utility definitions
|#

#|
<env> ::= (mtEnv)
       | (extEnv <sym> <value> <env>)
|#
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

#|
<result> e v ::= (failure e)
               | (success v)
|#
(deftype Result
  (failure e)
  (success v))

;; generate-substs : Pattern Value -> (Result String (Listof (Symbol * Value)))
(define (generate-substs p v) '???)

;;------------;;
;; P1.h, P2.c ;;
;;------------;;

;; interp : Expr Env -> Value
(define (interp expr env) '???)


;;----- ;;
;; P2.d ;;
;;----- ;;

#|
En función de lo implementado en la pregunta anterior, argumente porqué es útil que la función
generate-subst no lance un error (cuando el valor no calza con el patrón) y, en cambio, retorne
un mensaje.

R: ...

|#
