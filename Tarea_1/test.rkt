#lang play
(require "T1.rkt")

(print-only-errors #t)

;; b)
(test (eval frac1) 1)
(test (eval frac2) 29/19)

;; c)
(test (degree frac1) 0)
(test (degree frac2) 2)

;; d)
;; Se testea con la Parte e)

;; e)
(test (eval2 frac1) 1)
(test (eval2 frac2) 29/19)

(test (degree2 frac1) 0)
(test (degree2 frac2) 2)

;; f)
(test (mysterious-cf 0) (simple 6))
(test (mysterious-cf 2) (compound 6 (sqr 1) (compound 6 (sqr 3) (simple 6))))
(test/exn (mysterious-cf -1) "Error: argumento negativo")

;; g)
(test (from-to 3 0) '())
(test (from-to 0 5) '(0 1 2 3 4 5))

(test (mysterious-list 3) (list (fl 3) (fl 19/6) (fl 47/15) (fl 1321/420)))

;; h)
(test (rac-to-cf (+ 3 49/200)) (compound 3 1 (compound 4 1 (compound 12 1 (simple 4)))))
