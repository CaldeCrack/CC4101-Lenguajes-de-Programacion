#lang play
(require "T2.rkt")

(print-only-errors #t)

;; P1.b
(test (parse-prop 'true) (tt))
(test (parse-prop 'false) (ff))

(test (parse-prop '(not true)) (p-not (tt)))
(test/exn (parse-prop '(not true false)) "parse-prop: not expects only one operand")
(test (parse-prop '(and true false true)) (p-and (list (tt) (ff) (tt))))
(test/exn (parse-prop '(and true)) "parse-prop: and expects at least two operands")
(test (parse-prop '(or true false)) (p-or (list (tt) (ff))))
(test/exn (parse-prop '(or)) "parse-prop: or expects at least two operands")

(test (parse-prop 'x) (p-id 'x))
(test (parse-prop '(x where [x true])) (p-where (p-id 'x) 'x (tt)))
(test (parse-prop '(x where [x (and true true (not true))]))
	(p-where (p-id 'x) 'x (p-and (list (tt) (tt) (p-not (tt)))))
)

;; P1.c
(test (from-Pvalue (ttV)) (tt))
(test (from-Pvalue (ffV)) (ff))

;; P1.d
(test (p-subst (p-id 'x) 'x (tt)) (tt))
(test (p-subst (p-id 'x) 'y (tt)) (p-id 'x))
(test (p-subst (p-where (p-id 'x) 'x (tt)) 'x (ff)) (p-where (p-id 'x) 'x (tt)))
(test (p-subst (p-where (p-id 'x) 'y (tt)) 'x (ff)) (p-where (ff) 'y (tt)))
(test (p-subst (p-where (p-id 'x) 'x (p-and (list (tt) (p-id 'x) (p-not (tt))))) 'x (p-or (list (tt) (ff))))
	(p-where (p-id 'x) 'x (list 'p-and (tt) (p-or (list (tt) (ff))) (p-not (tt))))
)
