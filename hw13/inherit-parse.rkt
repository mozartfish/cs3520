#lang plait
(require "class.rkt"
         "inherit.rkt")

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (parse-class [s : S-Exp]) : (Symbol * ClassI)
  (cond
    [(s-exp-match? `{class SYMBOL extends SYMBOL {ANY ...} ANY ...} s)
     (values (s-exp->symbol (second (s-exp->list s)))
             (classI
              (s-exp->symbol (fourth (s-exp->list s)))
              (map parse-field
                   (s-exp->list (fourth (rest (s-exp->list s)))))
              (map parse-method 
                   (rest (rest (rest (rest (rest (s-exp->list s)))))))))]
   [else (error 'parse-class "invalid input")]))

(define (parse-field [s : S-Exp]) : Symbol
  (cond
   [(s-exp-match? `SYMBOL s)
    (s-exp->symbol s)]
   [else (error 'parse-field "invalid input")]))

(define (parse-method [s : S-Exp]) : (Symbol * ExpI)
  (cond
   [(s-exp-match? `[SYMBOL {arg} ANY] s)
    (values (s-exp->symbol (first (s-exp->list s)))
            (parse (third (s-exp->list s))))]
   [else (error 'parse-method "invalid input")]))

(define (parse [s : S-Exp]) : ExpI
  (cond
   [(s-exp-match? `NUMBER s) (numI (s-exp->number s))]
   [(s-exp-match? `null s)
    (nullI)]
   [(s-exp-match? `arg s) (argI)]
   [(s-exp-match? `this s) (thisI)]
   [(s-exp-match? `{+ ANY ANY} s)
    (plusI (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
   [(s-exp-match? `{* ANY ANY} s)
    (multI (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
   [(s-exp-match? `{new SYMBOL ANY ...} s)
    (newI (s-exp->symbol (second (s-exp->list s)))
          (map parse (rest (rest (s-exp->list s)))))]
   [(s-exp-match? `{get ANY SYMBOL} s)
    (getI (parse (second (s-exp->list s)))
          (s-exp->symbol (third (s-exp->list s))))]
   ; setI parse
   [(s-exp-match? `{set ANY SYMBOL ANY} s)
    (setI (parse (second (s-exp->list s)))
          (s-exp->symbol (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
   ; castI parse
   [(s-exp-match? `{cast SYMBOL ANY} s)
    (castI (s-exp->symbol (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
   ; if0I parse
   [(s-exp-match? `{if0 ANY ANY ANY} s)
    (if0I (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
   [(s-exp-match? `{send ANY SYMBOL ANY} s)
    (sendI (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
   [(s-exp-match? `{super SYMBOL ANY} s)
    (superI (s-exp->symbol (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
   [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `0)
        (numI 0))
  (test (parse `arg)
        (argI))
  (test (parse `this)
        (thisI))
  (test (parse `{+ 1 2})
        (plusI (numI 1) (numI 2)))
    (test (parse `{+ 1 null})
        (plusI (numI 1) (nullI)))
  (test (parse `{* 1 2})
        (multI (numI 1) (numI 2)))
  (test (parse `{new Posn 1 2})
        (newI 'Posn (list (numI 1) (numI 2))))
  ; castI test case
  (test (parse `{cast Posn {new Posn 1 2}})
        (castI 'Posn (newI 'Posn (list (numI 1) (numI 2)))))
  ;if0I test case
  (test (parse `{if0 1 3 5})
        (if0I (numI 1) (numI 3) (numI 5)))
  ; nullI test case
  (test (parse `null)
        (nullI))
  ; castI test case
  (test (parse `{cast Posn3D {new Posn3D 1 2 3}})
        (castI 'Posn3D (newI 'Posn3D (list (numI 1) (numI 2) (numI 3)))))
  (test (parse `{get 1 x})
        (getI (numI 1) 'x))
  ; setI test case
  (test (parse `{set {new Posn3D 1 2 3} x 10})
        (setI (newI 'Posn3D (list (numI 1) (numI 2) (numI 3))) 'x (numI 10)))
  (test (parse `{send 1 m 2})
        (sendI (numI 1) 'm (numI 2)))
  (test (parse `{super m 1})
        (superI 'm (numI 1)))
  (test/exn (parse `x)
            "invalid input")

  (test (parse-field `x)
        'x)
  (test/exn (parse-field `{x 1})
            "invalid input")

  (test (parse-method `[m {arg} this])
        (values 'm (thisI)))
  (test/exn (parse-method `[m {arg} 1 2])
            "invalid input")
  
  (test (parse-class `{class Posn3D extends Posn
                        {x y z}
                        [m1 {arg} arg]
                        [m2 {arg} this]})
        (values 'Posn3D
                (classI 'Posn
                        (list 'x 'y 'z)
                        (list (values 'm1 (argI))
                              (values 'm2 (thisI))))))
  (test/exn (parse-class `{class})
            "invalid input"))

;; ----------------------------------------

(define (interp-prog [classes : (Listof S-Exp)] [a : S-Exp]) : S-Exp
  (let ([v (interp-i (parse a)
                     (map parse-class classes))])
    (type-case Value v
      [(numV n) (number->s-exp n)]
      [(objV class-name field-vals) `object]
      [(nullV) `null])))

(module+ test
  (test (interp-prog
         (list
          `{class Empty extends Object
             {}})
         `{new Empty})
        `object)

 (test (interp-prog 
        (list
         `{class Posn extends Object
            {x y}
            [mdist {arg} {+ {get this x} {get this y}}]
            [addDist {arg} {+ {send arg mdist 0}
                              {send this mdist 0}}]}
         
         `{class Posn3D extends Posn
            {z}
            [mdist {arg} {+ {get this z} 
                            {super mdist arg}}]})
        
        `{send {new Posn3D 5 3 1} addDist {new Posn 2 7}})
       `18))

