#lang plait

(define-type Value
  (numV [n : Number])
  (closV [args : (Listof Symbol)]
         [body : Exp]
         [env : Env])
  (contV [k : Cont]))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (lamE [ns : (Listof Symbol)]
        [body : Exp])
  (appE [fun : Exp]
        [args : (Listof Exp)])
  (let/ccE [n : Symbol]
           [body : Exp])
  (negE [a : Exp])
  (avgE [l : Exp]
        [m : Exp]
        [r : Exp])
  (if0E [tst : Exp]
        [thn : Exp]
        [els : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)
(define extend-env* append)

(define-type Cont
  (doneK)
  (plusSecondK [r : Exp]
               [e : Env]
               [k : Cont])
  (doPlusK [v : Value]
           [k : Cont])
  (multSecondK [r : Exp]
               [e : Env]
               [k : Cont])
  (doMultK [v : Value]
           [k : Cont])
  (appArgsK [as : (Listof Exp)]
            [env : Env]
            [k : Cont])
  (appK [f : Value]
        [vs : (Listof Value)] ; reverse order
        [as : (Listof Exp)]
        [env : Env]
        [k : Cont])
  (doNegK [k : Cont])
  (avgSecondK [m : Exp]
              [r : Exp]
              [env : Env]
              [k : Cont])
  (avgThirdK [l : Value]
             [r : Exp]
             [env : Env]
             [k : Cont])
  (doAvgK [l : Value]
          [m : Value]
          [k : Cont])
  (doIf0K [thn : Exp]
          [els : Exp]
          [env : Env]
          [k : Cont]))

(module+ test
  (print-only-errors #t))

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
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (list (s-exp->symbol (first bs)))
                   (parse (third (s-exp->list s))))
             (list (parse (second bs)))))]
    [(s-exp-match? `{lambda {SYMBOL ...} ANY} s)
     (lamE (map s-exp->symbol 
                (s-exp->list 
                 (second (s-exp->list s))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let/cc SYMBOL ANY} s)
     (let/ccE (s-exp->symbol (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? `{neg ANY} s)
     (negE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{avg ANY ANY ANY} s)
     (avgE (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{if0 ANY ANY ANY} s)
     (if0E (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY ...} s)
     (appE (parse (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x) ; note: backquote instead of normal quote
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (appE (lamE (list 'x) (idE 'y))
              (list (plusE (numE 1) (numE 2)))))
  (test (parse `{lambda {x} 9})
        (lamE (list 'x) (numE 9)))
  (test (parse `{lambda {x y} 9})
        (lamE (list 'x 'y) (numE 9)))
  (test (parse `{let/cc k 0})
        (let/ccE 'k (numE 0)))
  (test (parse `{double 9})
        (appE (idE 'double) (list (numE 9))))
  (test (parse `{area 8 9})
        (appE (idE 'area) (list (numE 8) (numE 9))))
  (test (parse `{nothing})
        (appE (idE 'nothing) (list)))
  (test (parse `{neg 9})
        (negE (numE 9)))
  (test (parse `{avg 0 1 9})
        (avgE (numE 0) (numE 1) (numE 9)))
  (test (parse `{if0 0 1 9})
        (if0E (numE 0) (numE 1) (numE 9)))
  (test/exn (parse `{})
            "invalid input"))

;; interp & continue ----------------------------------------
(define (interp [a : Exp] [env : Env] [k : Cont]) : Value
  (type-case Exp a
    [(numE n) (continue k (numV n))]
    [(idE s) (continue k (lookup s env))]
    [(plusE l r) (interp l env
                         (plusSecondK r env k))]
    [(multE l r) (interp l env
                         (multSecondK r env k))]
    [(lamE ns body)
     (continue k (closV ns body env))]
    [(appE fun args) (interp fun env
                             (appArgsK args env k))]
    [(let/ccE n body)
     (interp body
             (extend-env (bind n (contV k))
                         env)
             k)]
    [(negE e) (interp e env (doNegK k))]
    [(avgE l m r) (interp l env
                          (avgSecondK m r env k))]
    [(if0E tst thn els) (interp tst env
                                (doIf0K thn els env k))]))

(define (continue [k : Cont] [v : Value]) : Value
  (type-case Cont k
    [(doneK) v]
    [(plusSecondK r env next-k)
     (interp r env
             (doPlusK v next-k))]
    [(doPlusK v-l next-k)
     (continue next-k (num+ v-l v))]
    [(multSecondK r env next-k)
     (interp r env
             (doMultK v next-k))]
    [(doMultK v-l next-k)
     (continue next-k (num* v-l v))]
    [(appArgsK as env next-k)
     (continue-apply v empty as env next-k)]
    [(appK v-f vs as env next-k)
     (continue-apply v-f (cons v vs) as env next-k)]
    [(doNegK next-k) (continue next-k (num-neg v))]
    [(avgSecondK m r env next-k)
     (interp m env
             (avgThirdK v r env next-k))]
    [(avgThirdK v-l r env next-k)
     (interp r env
             (doAvgK v-l v next-k))]
    [(doAvgK v-l v-m k)
     (continue k (num-avg v-l v-m v))]
    [(doIf0K thn els env next-k)
     (type-case Value v
       [(numV n) (if (zero? n)
                     (interp thn env next-k)
                     (interp els env next-k))]
       [else (error 'interp "not a number")])]))

(define (continue-apply v-f vs as env next-k)
  (cond
    [(empty? as)
     (type-case Value v-f
       [(closV ns body c-env)
        (if (= (length vs) (length ns))
            (interp body
                    (extend-env*
                     (map2 bind ns (reverse vs))
                     c-env)
                    next-k)
            (error 'interp "wrong arity"))]
       [(contV k-v) (if (= 1 (length vs))
                        (continue k-v (first vs))
                        (error 'interp "wrong arity"))]
       [else (error 'interp "not a function")])]
    [else
     (interp (first as) env
             (appK v-f
                   vs
                   (rest as)
                   env
                   next-k))]))

(module+ test
  (test (interp (parse `2) mt-env (doneK))
        (numV 2))
  (test/exn (interp (parse `x) mt-env (doneK))
            "free variable")
  (test (interp (parse `x)
                (extend-env (bind 'x (numV 9)) mt-env)
                (doneK))
        (numV 9))
  (test (interp (parse `{+ 2 1}) mt-env (doneK))
        (numV 3))
  (test (interp (parse `{* 2 1}) mt-env (doneK))
        (numV 2))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env
                (doneK))
        (numV 19))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env
                (doneK))
        (closV (list 'x) (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env
                (doneK))
        (numV 10))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                (doneK))
        (numV 12))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                (doneK))
        (numV 5))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env
                (doneK))
        (numV 16))

  (test (interp (parse `{let/cc k {+ 1 {k 0}}})
                mt-env
                (doneK))
        (numV 0))
  (test (interp (parse `{let {[f {let/cc k k}]}
                          {f {lambda {x} 10}}})
                mt-env
                (doneK))
        (numV 10))
  (test (interp (parse `{neg 5})
                mt-env
                (doneK))
        (numV -5))
  (test (interp (parse `{avg 2 5 11})
                mt-env
                (doneK))
        (numV 6))
  (test (interp (parse `{if0 1 2 3})
                mt-env
                (doneK))
        (numV 3))
  (test (interp (parse `{if0 0 2 3})
                mt-env
                (doneK))
        (numV 2))

  (test/exn (interp (parse `{1 2}) mt-env (doneK))
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env (doneK))
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    (doneK))
            "free variable")
  (test/exn (interp (parse `{neg {lambda {x} x}}) mt-env (doneK))
            "not a number")
  (test/exn (interp (parse `{avg 1 2 {lambda {x} x}}) mt-env (doneK))
            "not a number")
  (test/exn (interp (parse `{if0 {lambda {x} x} 1 2}) mt-env (doneK))
            "not a number")
  (test/exn (interp (parse `{{lambda {x} x} 1 2}) mt-env (doneK))
            "wrong arity")
  (test/exn (interp (parse `{{let/cc k k} 1 2}) mt-env (doneK))
            "wrong arity")
  
  ;; Eager:
  (test/exn (interp (parse `{{lambda {x} 0} {1 2}}) mt-env (doneK))
            "not a function")

  (test (continue (doneK) (numV 5))
        (numV 5))
  (test (continue (plusSecondK (numE 6) mt-env (doneK)) (numV 5))
        (numV 11))
  (test (continue (doPlusK (numV 7) (doneK)) (numV 5))
        (numV 12))
  (test (continue (multSecondK (numE 6) mt-env (doneK)) (numV 5))
        (numV 30))
  (test (continue (doMultK (numV 7) (doneK)) (numV 5))
        (numV 35))
  (test (continue (appArgsK (list (numE 5)) mt-env (doneK)) (closV (list 'x) (idE 'x) mt-env))
        (numV 5))
  (test (continue (appK (closV (list 'x) (idE 'x) mt-env) empty empty mt-env (doneK)) (numV 8))
        (numV 8))
  (test (continue (doNegK (doneK)) (numV 9))
        (numV -9))
  (test (continue (avgSecondK (numE 2) (numE 3) mt-env (doneK)) (numV 1))
        (numV 2))
  (test (continue (avgThirdK (numV 2) (numE 3) mt-env (doneK)) (numV 1))
        (numV 2))
  (test (continue (doAvgK (numV 2) (numV 3) (doneK)) (numV 1))
        (numV 2))
  (test (continue (doIf0K (numE 2) (numE 3) mt-env (doneK)) (numV 0))
        (numV 2)))

;; num+ and num* ----------------------------------------
(define (num-op [op : (Number Number -> Number)] [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else
     (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))

(define (num-neg [v : Value]) : Value
  (type-case Value v
    [(numV n) (numV (- 0 n))]
    [else (error 'interp "not a number")]))

(define (num-avg [l : Value] [m : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? m) (numV? r))
     (numV (/ (+ (+ (numV-n l) (numV-n m)) (numV-n r)) 3))]
    [else
     (error 'interp "not a number")]))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

;; lookup ----------------------------------------
(define (lookup [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
    [empty (error 'lookup "free variable")]
    [(cons b rst-env) (cond
                        [(symbol=? n (bind-name b))
                         (bind-val b)]
                        [else (lookup n rst-env)])]))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)))
        (numV 8)))

;; interp-expr ----------------------------------------
(define (interp-expr [a : Exp]) : S-Exp
  (type-case Value (interp a mt-env (doneK))
    [(numV n) (number->s-exp n)]
    [(closV n body env) `function]
    [(contV k) `function]))

(module+ test
  (test (interp-expr (parse `{neg 2}))
        `-2)
  (test (interp-expr (parse `{avg 0 6 6}))
        `4)
  (test (interp-expr (parse `{let/cc k {neg {k 3}}}))
        `3)
  (test (interp-expr (parse `{let/cc k {avg 0 {k 3} 0}}))
        `3)
  (test (interp-expr (parse `{let/cc k {avg {k 2} {k 3} 0}}))
        `2)
  (test (interp-expr (parse `{if0 1 2 3}))
        `3)
  (test (interp-expr (parse `{if0 0 2 3}))
        `2)
  (test (interp-expr (parse `{let/cc k {if0 {k 9} 2 3}}))
        `9)

  (test (interp-expr (parse `{{lambda {x y} {+ y {neg x}}} 10 12}))
        `2)
  (test (interp-expr (parse `{lambda {} 12}))
        `function)
  (test (interp-expr (parse `{lambda {x} {lambda {} x}}))
        `function)
  (test (interp-expr (parse `{{{lambda {x} {lambda {} x}} 13}}))
        `13)

  (test (interp-expr (parse `{let/cc k k}))
        `function)
  (test (interp-expr (parse `{let/cc esc {{lambda {x y} x} 1 {esc 3}}}))
        `3)
  (test (interp-expr (parse `{{let/cc esc {{lambda {x y} {lambda {z} {+ z y}}}
                                           1 
                                           {let/cc k {esc k}}}}
                              10}))
        `20))