#lang plait

; name: Pranav Rajan
; uID: u1136324
; date: 08/27/2020

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
(define (plural [input : String]) : String
  ; find the index of the last character
  (local[(define end (sub1 (string-length input)))]
    (if eq ? (string-ref input end) #\y)
        (string-append (substring input 0 end) "ies")
        (string-append (substring input 0 end) "s"))))
        
        
  
; energy-use
; use-for-one-hour

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

; plural test
(plural "fish")
