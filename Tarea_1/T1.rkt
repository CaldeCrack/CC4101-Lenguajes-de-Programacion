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

;; Parte c)
;; degree ::  CFraction -> Integer
;; Devuelve el grado de una fracción continua
(define (degree cf)
	(match cf
		[(simple value) 0]
		[(compound value num den) (+ 1 (degree den))]
	)
)

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

;; Parte e)
;; eval2 :: CFraction -> Rational
;; Devuelve el número racional que representa una fracción continua usando fold
(define eval2
	(fold-cfraction identity (λ (value num den) (+ value (/ num den))))
)

;; degree2 ::  CFraction -> Integer
;; Devuelve el grado de una fracción continua usando fold
(define degree2
	(fold-cfraction (λ (value) 0) (λ (value num den) (+ 1 den)))
)

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

;; Parte g)
;; from-to :: Integer Integer -> ListOf Integer
;; Construye una lista de enteros comprendidos entre dos enteros dados
(define (from-to start end)
	(if (> start end)
		'()
		(append (list start) (apply append (list (from-to (+ start 1) end))))
	)
)

;; mysterious-list :: Integer -> ListOf Float
;; Devuelve una lista tal que el i-ésimo elemento es calculado como la resta
;; de la evaluación de (mysterious-cf i) menos 3.
(define (mysterious-list value)
	(map (λ (v) (fl (- (eval v) 3))) (map mysterious-cf (from-to 0 value)))
)

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
