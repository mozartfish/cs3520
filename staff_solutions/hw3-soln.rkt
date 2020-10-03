#lang plait

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env])
  (thunkV [body : Exp]
          [env : Env]))

(define-type Exp
  (numE [n : Number])
  (trueE)
  (falseE)
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (eqlE [l : Exp]
        [r : Exp])
  (letE [n : Symbol] 
        [rhs : Exp]
        [body : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (ifE [tst : Exp]
       [thn : Exp]
       [els : Exp])
  (unletE [n : Symbol]
          [body : Exp])
  (delayE [body : Exp])
  (forceE [t : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `true s) (trueE)]
    [(s-exp-match? `false s) (falseE)]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{= ANY ANY} s)
     (eqlE (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{unlet SYMBOL ANY} s)
     (unletE (s-exp->symbol (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
    [(s-exp-match? `{delay ANY} s)
     (delayE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{force ANY} s)
     (forceE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `true)
        (trueE))
  (test (parse `false)
        (falseE))
  (test (parse `x) ; note: backquote instead of normal quote
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{= 3 4})
        (eqlE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (letE 'x (plusE (numE 1) (numE 2))
              (idE 'y)))
  (test (parse `{lambda {x} 9})
        (lamE 'x (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test (parse `{if 1 2 3})
        (ifE (numE 1) (numE 2) (numE 3)))
  (test (parse `{unlet x x})
        (unletE 'x (idE 'x)))
  (test (parse `{delay 1})
        (delayE (numE 1)))
  (test (parse `{force 2})
        (forceE (numE 2)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(trueE ) (boolV #t)]
    [(falseE ) (boolV #f)]
    [(idE s) (lookup s env)]
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(multE l r) (num* (interp l env) (interp r env))]
    [(eqlE l r) (num= (interp l env) (interp r env))]
    [(letE n rhs body)
     (interp body
             (extend-env
              (bind n (interp rhs env))
              env))]
    [(lamE n body)
     (closV n body env)]
    [(appE fun arg) (type-case Value (interp fun env)
                      [(closV n body c-env)
                       (interp body
                               (extend-env
                                (bind n
                                      (interp arg env))
                                c-env))]
                      [else (error 'interp "not a function")])]
    [(ifE tst thn els) (type-case Value (interp tst env)
                         [(boolV b) (interp (if b thn els)
                                            env)]
                         [else (error 'interp "not a boolean")])]
    [(unletE n body) (interp body (unextend n env))]
    [(delayE body) (thunkV body env)]
    [(forceE t) (type-case Value (interp t env)
                  [(thunkV body t-env) (interp body t-env)]
                  [else (error 'interp "not a thunk")])]))

(module+ test
  (test (interp (parse `2) mt-env)
        (numV 2))
  (test (interp (parse `true) mt-env)
        (boolV #t))
  (test (interp (parse `false) mt-env)
        (boolV #f))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env))
        (numV 9))
  (test (interp (parse `{+ 2 1}) mt-env)
        (numV 3))
  (test (interp (parse `{* 2 1}) mt-env)
        (numV 2))
  (test (interp (parse `{= 2 1}) mt-env)
        (boolV #f))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env)
        (numV 19))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env)
        (closV 'x (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env)
        (numV 16))

  (test/exn (interp (parse `{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env)
            "free variable")

  (test (interp (parse `{if {= 2 {+ 1 1}} 7 8})
                mt-env)
        (numV 7))
  (test (interp (parse `{if {= 2 {+ 1 2}} 7 8})
                mt-env)
        (numV 8))
  (test (interp (parse `{if true 10 {+ 1 {lambda {x} x}}})
                mt-env)
        (numV 10))
  (test/exn (interp (parse `{if 1 2 3})
                    mt-env)
            "not a boolean")

  (test/exn (interp (parse `{let {[x 1]}
                              {unlet x
                                     x}})
                    mt-env)
            "free variable")
  (test (interp (parse `{let {[x 1]}
                          {+ x {unlet x 1}}})
                mt-env)
        (numV 2))
  (test (interp (parse `{let {[x 1]}
                          {let {[x 2]}
                            {+ x {unlet x x}}}})
                mt-env)
        (numV 3))
  (test (interp (parse `{let {[x 1]}
                          {let {[x 2]}
                            {let {[z 3]}
                              {+ x {unlet x {+ x z}}}}}})
                mt-env)
        (numV 6))
  (test (interp (parse `{let {[f {lambda {z}
                                   {let {[z 8]}
                                     {unlet z
                                       z}}}]}
                          {f 2}})
                mt-env)
        (numV 2))

  (test/exn (interp (parse `{force 1})
                    mt-env)
            "not a thunk")
  (test (interp (parse `{force {if {= 8 8} {delay 7} {delay 9}}})
                mt-env)
        (interp (parse `7)
                mt-env))
  (test (interp (parse `{let {[d {let {[y 8]}
                                   {delay {+ y 7}}}]}
                          {let {[y 9]}
                            {force d}}})
                mt-env)
        (interp (parse `15)
                mt-env)))

;; num+ and num* ----------------------------------------
(define (num-op [op : (Number Number -> 'a)] [l : Value] [r : Value]) : 'a
  (cond
   [(and (numV? l) (numV? r))
    (op (numV-n l) (numV-n r))]
   [else
    (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (numV (num-op + l r)))
(define (num* [l : Value] [r : Value]) : Value
  (numV (num-op * l r)))
(define (num= [l : Value] [r : Value]) : Value
  (boolV (num-op = l r)))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6))
  (test (num= (numV 2) (numV 2))
        (boolV #t))
  (test (num= (numV 2) (numV 3))
        (boolV #f)))

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

;; unextend ----------------------------------------
(define (unextend [n : Symbol] [env : Env]) : Env
  (type-case (Listof Binding) env
   [empty empty]
   [(cons b rst-env) (cond
                       [(symbol=? n (bind-name b))
                        rst-env]
                       [else
                        (cons b (unextend n rst-env))])]))

(module+ test
  (test (unextend 'x mt-env)
        mt-env)
  (test (unextend 'x (extend-env (bind 'x (numV 8)) mt-env))
        mt-env)
  (test (unextend 'x (extend-env (bind 'x (numV 8))
                                 (extend-env (bind 'x (numV 7))
                                             mt-env)))
        (extend-env (bind 'x (numV 7))
                    mt-env))
  (test (unextend 'y (extend-env
                      (bind 'x (numV 9))
                      (extend-env (bind 'y (numV 8)) mt-env)))
        (extend-env (bind 'x (numV 9))
                    mt-env)))
