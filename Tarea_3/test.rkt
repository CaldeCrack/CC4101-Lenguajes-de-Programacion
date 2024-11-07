#lang play
(require "T3.rkt")

(print-only-errors #t)

;; P1.b
(test (parse '1) (num 1))
(test (parse '(+ 1 -2)) (add (num 1) (num -2)))
(test (parse '(nil)) (nil))
(test (parse '(cons 1 2)) (conz (num 1) (num 2)))
(test (parse '(list 1 2 3)) (conz (num 1) (conz (num 2) (conz (num 3) (nil)))))

;; P1.d
(test (parse-pattern '1) (numP 1))
(test (parse-pattern '(nil)) (nilP))
(test (parse-pattern 'x) (varP 'x))
(test (parse-pattern '(cons -1 y)) (conzP (numP -1) (varP 'y)))
(test (parse-pattern '(list 1 x 3)) (conzP (numP 1) (conzP (varP 'x) (conzP (numP 3) (nilP)))))

;; P1.e
(test (parse '(fun x x)) (fun (varP 'x) (id 'x)))
(test (parse '(fun (cons x xs) x)) (fun (conzP (varP 'x) (varP 'xs)) (id 'x)))
(test (parse '(f x)) (app (id 'f) (id 'x)))
(test (parse '(list 3 x 1)) (conz (num 3) (conz (id 'x) (conz (num 1) (nil)))))
(test (parse '((y 0) 2)) (app (app (id 'y) (num 0)) (num 2)))

;; P1.g
(test (generate-substs (numP 3) (numV 3)) (success '()))
(test (generate-substs (numP 3) (numV 4)) (failure "MatchError: given number does not match pattern"))
(test (generate-substs (numP -2) (nil)) (failure "MatchError: expected a number"))
(test (generate-substs (nilP) (nilV)) (success '()))
(test (generate-substs (nilP) (numV 2)) (failure "MatchError: expected nil"))
(test (generate-substs (varP 'x) (numV 3)) (success (list (cons 'x (numV 3)))))
(test(generate-substs (conzP (varP 'x) (varP 'y)) (conzV (numV 3) (numV 4)))
	(success (list (cons 'x (numV 3)) (cons 'y (numV 4)))))
(test (generate-substs (conzP (numP 1) (varP 'y)) (conzV (numV 3) (numV 4)))
	(failure "MatchError: given number does not match pattern"))
(test (generate-substs (conzP (varP 'x) (varP 'y)) (nil))
	(failure "MatchError: expected a cons constructor"))

;; P1.h
(test (interp (parse '1) empty-env) (numV 1))
(test (interp (parse '(+ 4 -3)) empty-env) (numV 1))
(test (interp (parse '(nil)) empty-env) (nilV))
(test (interp (parse '(cons 7 2)) empty-env) (conzV (numV 7) (numV 2)))
(test (interp (parse '(cons 0 (nil))) empty-env) (conzV (numV 0) (nilV)))

(test (interp (parse 'x) (extend-env 'x (numV 4) empty-env)) (numV 4))
(test (interp (app (parse '(fun 5 4)) (num 5)) empty-env) (numV 4))
(test (interp (app (parse '(fun x (+ x 3))) (num 4)) empty-env) (numV 7))
(test (interp (parse '(fun x (+ x 2))) empty-env) (closureV (varP 'x) (add (id 'x) (num 2)) empty-env))

(define func (parse '(fun (cons 1 y) y)))
(test (interp func empty-env) (closureV (conzP (numP 1) (varP 'y)) (id 'y) empty-env))
(test (interp (app func (conz (num 1) (num 2))) empty-env) (numV 2))

(test/exn (interp (app (parse '(fun 3 1)) (num 1)) empty-env)
	"MatchError: given number does not match pattern")
(test/exn (interp (parse '(id x)) empty-env) "LookupError: variable ~a not found")
(test/exn (interp (add (num -2) (nil)) empty-env) "TypeError: expected a number")
(test/exn (interp (parse '(0 1)) empty-env) "MatchError: given number does not match pattern")
(test/exn (interp (app (num 4) (nil)) empty-env) "MatchError: expected a number")
(test/exn (interp (app (nil) (num 2)) empty-env) "MatchError: expected nil")
(test/exn (interp (app func (num 3)) empty-env) "MatchError: expected a cons constructor")
