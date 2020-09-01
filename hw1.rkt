#lang plait

; name: Pranav Rajan
; uID: u1136324
; date: 08/31/20

; Tree definition from Professor Flatt
(define-type Tree
  (leaf [val : Number])
  (node [val : Number]
        [left : Tree]
        [right : Tree]))

; How to Design Programs Strategy
; representation
; examples
; template
; body;
; run tests

; Sum

; Representation
; Number
; sum : (Tree -> Number)

; examples
(module+ test
  (test (sum (node 5 (leaf 6) (leaf 7))) 18)
  )

; template
#;(define (sum [t : Tree])
    ...t...)

; body
(define (sum [t : Tree]) : Number
  (type-case Tree t
    [(leaf v) v]
    [(node v l r) (+ v (+ (sum l) (sum r)))]))
