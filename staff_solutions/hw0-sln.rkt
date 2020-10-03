#lang plait

(print-only-errors #t)

;; Compute n^3
(define (3rd-power [n : Number]) : Number
  (* n (* n n)))

(test (3rd-power 0) 0)
(test (3rd-power 17) 4913)

;; Compute n^42
(define (42nd-power [n : Number]) : Number
  (local [(define (square n)
            (* n n))
          (define (6th-power n)
            (3rd-power (square n)))
          (define (7th-power n)
            (* n (6th-power n)))]
    (6th-power (7th-power n))))

(test (42nd-power 0) 0)
(test (42nd-power 1) 1)
(test (42nd-power 17) 4773695331839566234818968439734627784374274207965089)

;; Converts a trailing "y" to "ies", otherwise
;; adds an "s"
(define (plural [s : String]) : String
  (local [(define l (reverse (string->list s)))]
    (list->string
     (reverse 
      (cond
       [(empty? l) (list #\s)]
       [(equal? #\y (first l))
        (append (list #\s #\e #\i) (rest l))]
       [else (cons #\s l)])))))

(test (plural "cat") "cats")
(test (plural "pony") "ponies")


(define-type Light
  (bulb [watts : Number]
        [technology : Symbol])
  (candle [inches : Number]))

;; Computes the electrical-energy usage per hour
(define (energy-usage [l : Light]) : Number
  (type-case Light l
    [(bulb w t) (/ (* w 24) 1000)]
    [(candle l) 0]))

(test (energy-usage (bulb 100.0 'halogen)) 2.4)
(test (energy-usage (candle 10.0)) 0)
(test (energy-usage (bulb 1000 'led)) 24)

;; Takes a light and returns a representation of the light
;; after it has been used for one hour
(define (use-for-one-hour [l : Light]) : Light
  (type-case Light l
    [(bulb w t) l]
    [(candle l) (candle (max 0 (- l 1)))]))

(test (use-for-one-hour (bulb 100.0 'halogen)) (bulb 100.0 'halogen))
(test (use-for-one-hour (candle 10.0)) (candle 9.0))
(test (use-for-one-hour (candle 0.5)) (candle 0.0))
