#lang plait

; name: Pranav Rajan
; uID: u1136324
; date: 08/30/2020

; 3rd Power
(define (3rd-power [base : Number]) : Number
  (* base (* base base)))

; 42nd Power
(define (42nd-power [base : Number]) : Number
  ; break exponent into sums of 6 => 6 + 6 + 6 + 6 + 6 + 6 + 6 = 42
  ; using 3rd-power function
  (local [(define 6-power (* (3rd-power base) (3rd-power base)))]
    (* (* (* (* 6-power 6-power) (* 6-power 6-power))
          (* 6-power 6-power))
       6-power)))

; plural
(define (plural [s : String]) : String
  ; check if the string is empty
  ; if yes, then return a string with just s
  ; if no, check the last character and add either s or ies to the end
  (cond
    [(equal? s "")
     (string-append s "s")]
    [else
     ; get the index of the last character
     (local [(define last-char-index (sub1 (string-length s)))]
       (cond
         [(eq? (string-ref s last-char-index) #\y)
          (string-append (substring s 0 last-char-index) "ies")]
         [else (string-append s "s")]))]))

; Light definition from Professor Flatt
(define-type Light
  (bulb [watts : Number]
        [technology : Symbol])
  (candle [inches : Number]))

; energy-use
(define (energy-usage [l : Light]) : Number
  (type-case Light l
    [(bulb w t) (/ (* w 24) 1000)]
    [(candle i) 0.0]))

; use-for-one-hour
; this method assumes users enter a height >= 0.0
(define (use-for-one-hour [l : Light]) : Light
  (type-case Light l
    [(bulb w t) l]
    [(candle i)
     (cond
       [(eq? i 0.0) (candle 0.0)]
       [else
        (local [(define new-height (sub1 i))]
          (candle new-height))])]))
          

; 3rd Power Tests
(test (3rd-power 17) 4913)
(test (3rd-power 2) 8)
(test (3rd-power 1) 1)
(test (3rd-power 4) 64)

; 42nd Power Tests
(test (42nd-power 17) 4773695331839566234818968439734627784374274207965089)
(test (42nd-power 1) 1)
(test (42nd-power 2) 4398046511104)
(test (42nd-power 3) 109418989131512359209)

; plural Tests
(test (plural "baby") "babies")
(test (plural "fish") "fishs")
(test (plural "") "s")
(test (plural "programming") "programmings")
(test (plural "racket") "rackets")
(test (plural "py") "pies")

; energy-usage Tests
(test (energy-usage (bulb 100.0 'halogen)) 2.4)
(test (energy-usage (candle 10.0)) 0.0)
(test (energy-usage (bulb 50.0 'krypton)) 1.2)
(test (energy-usage (bulb 200.0 'neon)) 4.8)
(test (energy-usage (candle 150.0)) 0.0)

; use-for-one-hour Tests
(test (use-for-one-hour (bulb 100.0 'halogen)) (bulb 100.0 'halogen))
(test (use-for-one-hour (candle 10.0)) (candle 9.0))
(test (use-for-one-hour (candle 0.0)) (candle 0.0))
(test (use-for-one-hour (bulb 300.0 'neon)) (bulb 300.0 'neon))
