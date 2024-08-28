#lang play
(require math/flonum)

;; NOMBRE Y APELLIDO: Andrés Calderón
;; RUT: 21.273.734-8

;; Parte a)
;; <CFraction> ::= (simple <Integer>)
;; 				|   (compound <Integer> <Integer> <CFraction>)
;; Constructor de fracciones continuas
(deftype CFraction
	(simple value)
	(compound value num den)
)

(define frac1 (simple 1))
(define frac2 (compound 1 2 (compound 3 4 (simple 5))))

;; Parte b)
;; eval :: CFraction -> Rational
;; Devuelve el número racional que representa una fracción continua
(define (eval cf)
	(match cf
		[(simple value) value]
		[(compound value num den) (+ value (/ num (eval den)))]
	)
)
(test (eval frac1) 1)
(test (eval frac2) 29/19)

;; Parte c)
;; degree ::  CFraction -> Integer
;; Devuelve el grado de una fracción continua
(define (degree cf)
	(match cf
		[(simple value) 0]
		[(compound value num den) (+ 1 (degree den))]
	)
)
(test (degree frac1) 0)
(test (degree frac2) 2)

;; Parte d)
;; fold-cfraction :: (Integer -> A) (Integer Integer A -> A) -> (CFraction -> A)
;; Captura el esquema de recursión asociado a CFraction
(define (fold-cfraction f g)
	(λ (cf)
		(match cf
			[(simple value) (f value)]
			[(compound value num den) (g value num ((fold-cfraction f g) den))]
		)
	)
)
;; Se testea con la Parte e)

;; Parte e)
;; eval2 :: CFraction -> Rational
;; Devuelve el número racional que representa una fracción continua usando fold
(define eval2
	(fold-cfraction identity (λ (value num den) (+ value (/ num den))))
)
(test (eval2 frac1) 1)
(test (eval2 frac2) 29/19)

;; degree2 ::  CFraction -> Integer
;; Devuelve el grado de una fracción continua usando fold
(define degree2
	(fold-cfraction (λ (value) 0) (λ (value num den) (+ 1 den)))
)
(test (degree2 frac1) 0)
(test (degree2 frac2) 2)

;; Parte f)
;; mysterious-cf :: Integer -> CFraction
;; Genera una secuencia de fracciones continuas
(define (mysterious-cf value)
	;; mysterious-cf-aux :: Integer Integer -> CFraction
	;; Función auxiliar para generar fracciones continuas
	(define (mysterious-cf-aux value max-value)
		(cond   [(< value 0) (error "Error: argumento negativo")]
				[(zero? value) (simple 6)]
				[else (compound 6 (sqr (+ (* (- max-value value) 2) 1)) (mysterious-cf-aux (- value 1) max-value))]
		)
	)
	(mysterious-cf-aux value value)
)
(test (mysterious-cf 0) (simple 6))
(test (mysterious-cf 2) (compound 6 (sqr 1) (compound 6 (sqr 3) (simple 6))))
(test/exn (mysterious-cf -1) "Error: argumento negativo")

;; Parte g)
;; from-to :: Integer Integer -> ListOf Integer
;; Construye una lista de enteros comprendidos entre dos enteros dados
(define (from-to start end)
	(if (> start end)
		'()
		(append (list start) (apply append (list (from-to (+ start 1) end))))
	)
)
(test (from-to 3 0) '())
(test (from-to 0 5) '(0 1 2 3 4 5))

;; mysterious-list :: Integer -> ListOf Float
;; Devuelve una lista tal que el i-ésimo elemento es calculado como la resta
;; de la evaluación de (mysterious-cf i) menos 3.
(define (mysterious-list value)
	(map (λ (v) (fl (- (eval v) 3))) (map mysterious-cf (from-to 0 value)))
)
(test (mysterious-list 3) (list (fl 3) (fl 19/6) (fl 47/15) (fl 1321/420)))

;; A que numero tiende (mysterious-cf k) cuando k tiende a infinito?
;; Cuando k tiende a infinito el valor de (mysterious-cf k) se aproxima a pi

;; Parte h)
;; rac-to-cf :: Rational -> CFraction
;; Transforma un número racional no-negativo en su representación en forma de fracción continua
(define (rac-to-cf value)
	(let* ([int (floor value)] [diff (- value int)])
		(if (equal? diff 0)
			(simple int)
			(compound int 1 (rac-to-cf (/ 1 diff)))
		)
	)
)
(test (rac-to-cf (+ 3 49/200)) (compound 3 1 (compound 4 1 (compound 12 1 (simple 4)))))
