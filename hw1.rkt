#lang plait

; name: Pranav Rajan
; uID: u1136324
; date: 09/06/20


; HTDP Steps
; 1) Representation
; 2) Examples (Tests)
; 3) Template
; 4) Body
; 5) Run Tests


; Tree definition from Professor Flatt
(define-type Tree
  (leaf [val : Number])
  (node [val : Number]
        [left : Tree]
        [right : Tree]))

; Sum


