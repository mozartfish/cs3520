#lang plait

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (maxE [l : Exp]
        [r : Exp])
  (appE [s : Symbol]
        [args : (Listof Exp)]))

(define-type Func-Defn
  (fd [name : Symbol]
      [args : (Listof Symbol)]
      [body : Exp]))

(module+ test
  (print-only-errors #t))

;; An EXP is either
;; - `NUMBER
;; - `SYMBOL
;; - `{+ EXP EXP}
;; - `{* EXP EXP}
;; - `{SYMBOL EXP)

;; A FUNC-DEFN is
;; - `{define {SYMBOL SYMBOL} EXP}

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{max ANY ANY} s)
     (maxE (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ...} s)
     (appE (s-exp->symbol (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-fundef [s : S-Exp]) : Func-Defn
  (cond
    [(s-exp-match? `{define {SYMBOL SYMBOL ...} ANY} s)
     (fd (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
         (check-unique
          (map s-exp->symbol (rest (s-exp->list (second (s-exp->list s))))))
         (parse (third (s-exp->list s))))]
    [else (error 'parse-fundef "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{max 3 4})
        (maxE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{double 9})
        (appE 'double (list (numE 9))))
  (test (parse `{area 3 4})
        (appE 'area (list (numE 3) (numE 4))))
  (test (parse `{five})
        (appE 'five (list)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")

  (test (parse-fundef `{define {double x} {+ x x}})
        (fd 'double (list 'x) (plusE (idE 'x) (idE 'x))))
  (test (parse-fundef `{define {area w h} {* w h}})
        (fd 'area (list 'w 'h) (multE (idE 'w) (idE 'h))))
  (test (parse-fundef `{define {five} 5})
        (fd 'five (list) (numE 5)))
  (test/exn (parse-fundef `{define {f x x} x})
            "bad syntax")
  (test/exn (parse-fundef `{def {f x} x})
            "invalid input")

  (define double-def
    (parse-fundef `{define {double x} {+ x x}}))
  (define quadruple-def
    (parse-fundef `{define {quadruple x} {double {double x}}})))

;; check-unique ----------------------------------------

(define (check-unique [syms : (Listof Symbol)])
  (type-case (Listof Symbol) syms
    [empty empty]
    [(cons sym rst-syms)
     (if (member sym rst-syms)
         (error 'parse "bad syntax")
         (cons sym (check-unique rst-syms)))]))

(module+ test
  (test (check-unique '())
        '())
  (test (check-unique '(a b c))
        '(a b c))
  (test/exn (check-unique '(a b b))
            "bad syntax")
  (test/exn (check-unique '(b a b))
            "bad syntax"))

;; interp ----------------------------------------
(define (interp [a : Exp] [defs : (Listof Func-Defn)]) : Number
  (type-case Exp a
    [(numE n) n]
    [(idE s) (error 'interp "free variable")]
    [(plusE l r) (+ (interp l defs) (interp r defs))]
    [(multE l r) (* (interp l defs) (interp r defs))]
    [(maxE l r) (max (interp l defs) (interp r defs))]
    [(appE s args) (local [(define fd (get-fundef s defs))]
                     (cond
                       [(= (length args) (length (fd-args fd)))
                        (interp (subst* (map (lambda (arg)
                                               (numE (interp arg defs)))
                                             args)
                                        (fd-args fd)
                                        (fd-body fd))
                                defs)]
                       [else (error 'interp "wrong arity")]))]))

(module+ test
  (test (interp (parse `2) empty)
        2)
  (test/exn (interp (parse `x) empty)
            "free variable")
  (test (interp (parse `{+ 2 1}) empty)
        3)
  (test (interp (parse `{* 2 1}) empty)
        2)
  (test (interp (parse `{max 1 2}) empty)
        2)
  (test (interp (parse `{+ {* 2 3}
                           {+ 5 8}})
                empty)
        19)
  (test (interp (parse `{max {+ 4 5} {+ 2 3}})
                empty)
        9)
  (test (interp (parse `{double 8})
                (list double-def))
        16)
  (test (interp (parse `{quadruple 8})
                (list double-def quadruple-def))
        32)

  (test/exn (interp (parse `{double})
                    (list double-def))
            "wrong arity"))

;; get-fundef ----------------------------------------
(define (get-fundef [s : Symbol] [defs : (Listof Func-Defn)]) : Func-Defn
  (type-case (Listof Func-Defn) defs
    [empty (error 'get-fundef "undefined function")]
    [(cons def rst-defs) (if (eq? s (fd-name def))
                             def
                             (get-fundef s rst-defs))]))

(module+ test
  (test (get-fundef 'double (list double-def))
        double-def)
  (test (get-fundef 'double (list double-def quadruple-def))
        double-def)
  (test (get-fundef 'double (list quadruple-def double-def))
        double-def)
  (test (get-fundef 'quadruple (list quadruple-def double-def))
        quadruple-def)
  (test/exn (get-fundef 'double empty)
            "undefined function"))

;; subst ----------------------------------------
(define (subst [what : Exp] [for : Symbol] [in : Exp])
  (type-case Exp in
    [(numE n) in]
    [(idE s) (if (eq? for s)
                 what
                 in)]
    [(plusE l r) (plusE (subst what for l)
                        (subst what for r))]
    [(multE l r) (multE (subst what for l)
                        (subst what for r))]
    [(maxE l r) (maxE (subst what for l)
                      (subst what for r))]
    [(appE s args) (appE s (map (lambda (arg)
                                  (subst what for arg))
                                args))]))

(module+ test
  (test (subst (parse `8) 'x (parse `9))
        (numE 9))
  (test (subst (parse `8) 'x (parse `x))
        (numE 8))
  (test (subst (parse `8) 'x (parse `y))
        (idE 'y))
  (test (subst (parse `8) 'x (parse `{+ x y}))
        (parse `{+ 8 y}))
  (test (subst (parse `8) 'x (parse `{* y x}))
        (parse `{* y 8}))
  (test (subst (parse `8) 'x (parse `{max y x}))
        (parse `{max y 8}))
  (test (subst (parse `8) 'x (parse `{double x}))
        (parse `{double 8}))
  (test (subst (parse `8) 'x (parse `{area x y}))
        (parse `{area 8 y})))

;; subst* ----------------------------------------
(define (subst* [whats : (Listof Exp)] [fors : (Listof Symbol)] [in : Exp])
  (cond
    [(empty? whats) in]
    [else (subst* (rest whats)
                  (rest fors)
                  (subst (first whats)
                         (first fors)
                         in))]))

(module+ test
  (test (subst* empty empty (parse `{+ x y}))
        (parse `{+ x y}))
  (test (subst* (list (numE 1) (numE 2)) (list 'x 'y) (parse `{+ x y}))
        (parse `{+ 1 2})))