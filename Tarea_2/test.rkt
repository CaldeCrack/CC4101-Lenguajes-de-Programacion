#lang play
(require "T2.rkt")
(print-only-errors #t)

;;----- ;;
;;  P1  ;;
;;----- ;;

;; P1.b
(test (parse-prop 'true) (tt))
(test (parse-prop 'false) (ff))

(test (parse-prop '(not true)) (p-not (tt)))
(test/exn (parse-prop '(not true false)) "parse-prop: 'not' expects only one operand")
(test (parse-prop '(and true false true)) (p-and (list (tt) (ff) (tt))))
(test/exn (parse-prop '(and true)) "parse-prop: 'and' expects at least two operands")
(test (parse-prop '(or true false)) (p-or (list (tt) (ff))))
(test/exn (parse-prop '(or)) "parse-prop: 'or' expects at least two operands")

(test (parse-prop 'x) (p-id 'x))
(test (parse-prop '(x where [x true])) (p-where (p-id 'x) 'x (tt)))
(test (parse-prop '(x where [x (and true true (not true))]))
	(p-where (p-id 'x) 'x (p-and (list (tt) (tt) (p-not (tt))))))

;; P1.c
(test (from-Pvalue (ttV)) (tt))
(test (from-Pvalue (ffV)) (ff))

;; P1.d
(test (p-subst (p-id 'x) 'x (tt)) (tt))
(test (p-subst (p-id 'x) 'y (tt)) (p-id 'x))
(test (p-subst (p-where (p-id 'x) 'x (tt)) 'x (ff)) (p-where (p-id 'x) 'x (tt)))
(test (p-subst (p-where (p-id 'x) 'y (tt)) 'x (ff)) (p-where (ff) 'y (tt)))
(test (p-subst (p-where (p-id 'x) 'x (p-and (list (tt) (p-id 'x) (p-not (tt))))) 'x (p-or (list (tt) (ff))))
	(p-where (p-id 'x) 'x (p-and (list (tt) (p-or (list (tt) (ff))) (p-not (tt))))))

;; P1.e
(test (eval-or (list (ff) (ff) (tt) (ff))) (ttV))
(test (eval-or (list (ff) (p-or (list (ff) (ff))) (ff))) (ffV))
(test (eval-or (list (tt) (ttV))) (ttV)) ;; No lanza error de match por ttV así que hace corto circuito

(test (eval-and (list (tt) (tt) (ff) (tt))) (ffV))
(test (eval-and (list (tt) (p-not (ff)))) (ttV))
(test (eval-and (list (ff) (ffV))) (ffV)) ;; No lanza error de match por ffV así que hace corto circuito

(test (p-eval (tt)) (ttV))
(test (p-eval (ff)) (ffV))
(test (p-eval (p-where (p-id 'x) 'x (p-and (list (tt) (p-or (list (tt) (ff))) (p-not (tt)))))) (ffV))
(test (p-eval (p-or (list (ff) (p-where (p-id 'x) 'x (p-and (list (tt) (p-not (ff)))))))) (ttV))
(test/exn (p-eval (parse-prop 'x)) "p-eval: open expression (free occurrence of ~a) 'x")

;;----- ;;
;;  P2  ;;
;;----- ;;

;; P2.b
(test (parse '1) (real 1))
(test (parse '(2 i)) (imaginary 2))
(test (parse '(+ 1 (2 i))) (add (real 1) (imaginary 2)))
(test (parse '(with [(x 1) (y 1)] (- x y)))
	(with (list (cons 'x (real 1)) (cons 'y (real 1))) (sub (id 'x) (id 'y))))
(test/exn (parse '(with [] 1)) "parse: 'with' expects at least one definition")

;; P2.c
(test (from-CValue (compV 0 0)) (real 0))
(test (from-CValue (compV 0 3)) (imaginary 3))
(test (from-CValue (compV 1 0)) (real 1))
(test (from-CValue (compV 2 4)) (add (real 2) (imaginary 4)))

(test (cmplx+ (compV 1 -3) (compV 3 3)) (compV 4 0))
(test (cmplx+ (compV 0 2) (compV 3 0)) (compV 3 2))

(test (cmplx- (compV 6 3) (compV 2 3)) (compV 4 0))
(test (cmplx- (compV 5 2) (compV 0 3)) (compV 5 -1))

(test (cmplx0? (compV 0 0)) #t)
(test (cmplx0? (compV 5 2)) #f)
(test (cmplx0? (compV -3 0)) #f)

;; P2.d
(test (subst (parse '(with [(x 2) (y z)] (+ x z))) 'z (real 1))
	(with (list (cons 'x (real 2)) (cons 'y (real 1))) (add (id 'x) (real 1))))
(test (subst (parse '(with [(x 2) (y x)] (+ x x))) 'x (imaginary 2))
	(with (list (cons 'x (real 2)) (cons 'y (id 'x))) (add (id 'x) (id 'x))))
(test (subst (parse '(with [(x 2) (y (+ x 1))] (+ x y))) 'z (imaginary 1))
	(with (list (cons 'x (real 2)) (cons 'y (add (id 'x) (real 1)))) (add (id 'x) (id 'y))))
(test (subst (parse '(with [(x y) (y 2) (z y)] (+ x y))) 'y (real 1))
	(with (list (cons 'x (real 1)) (cons 'y (real 2)) (cons 'z (id 'y))) (add (id 'x) (id 'y))))

;; P2.e
(test (interp (real -1)) (compV -1 0))
(test (interp (imaginary 5)) (compV 0 5))
(test (interp (add (real -2) (imaginary 4))) (compV -2 4))
(test (interp (sub (sub (real 4) (imaginary 3)) (add (real 4) (imaginary -2)))) (compV 0 -1))
(test (interp (if0 (imaginary 0) (real -1) (imaginary 2))) (compV -1 0))
(test (interp (if0 (add (real 1) (imaginary 0)) (imaginary 7) (real 5))) (compV 5 0))
(test (interp (with (list (cons 'x (imaginary -15)) (cons 'y (real 10))) (add (id 'x) (real 2)))) (compV 2 -15))
(test (interp (parse '(with ((z 2) (z (+ 3 (5 i)))) z))) (compV 3 5))
(test/exn (interp (id 'x)) "interp: open expression (free occurrence of ~a) 'x")
