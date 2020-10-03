#lang plait

(define-type-alias Location Number)

(define-type Value
  (numV [n : Number])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env])
  (boxV [l : Location])
  (recV [ns : (Listof Symbol)]
        [ls : (Listof Location)]))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (letE [n : Symbol] 
        [rhs : Exp]
        [body : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (boxE [arg : Exp])
  (unboxE [arg : Exp])
  (setboxE [bx : Exp]
           [val : Exp])
  (beginE [l : (Listof Exp)])
  (recordE [ns : (Listof Symbol)]
           [args : (Listof Exp)])
  (rgetE [rec : Exp]
         [n : Symbol])
  (rsetE [rec : Exp]
         [n : Symbol]
         [val : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(define-type Storage
  (cell [location : Location] 
        [val : Value]))

(define-type-alias Store (Listof Storage))
(define mt-store empty)
(define override-store cons)

(define-type Result
  (v*s [v : Value] [s : Store]))

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
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{box ANY} s)
     (boxE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{unbox ANY} s)
     (unboxE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{set-box! ANY ANY} s)
     (setboxE (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? `{begin ANY ANY ...} s)
     (beginE (map parse (rest (s-exp->list s))))]
    [(s-exp-match? `{record {SYMBOL ANY} ...} s)
     (recordE (map (lambda (l) (s-exp->symbol (first (s-exp->list l))))
                   (rest (s-exp->list s)))
              (map (lambda (l) (parse (second (s-exp->list l))))
                   (rest (s-exp->list s))))]
    [(s-exp-match? `{get ANY SYMBOL} s)
     (rgetE (parse (second (s-exp->list s)))
            (s-exp->symbol (third (s-exp->list s))))]
    [(s-exp-match? `{set! ANY SYMBOL ANY} s)
     (rsetE (parse (second (s-exp->list s)))
            (s-exp->symbol (third (s-exp->list s)))
            (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
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
        (letE 'x (plusE (numE 1) (numE 2))
              (idE 'y)))
  (test (parse `{lambda {x} 9})
        (lamE 'x (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test (parse `{box 0})
        (boxE (numE 0)))
  (test (parse `{unbox b})
        (unboxE (idE 'b)))
  (test (parse `{set-box! b 0})
        (setboxE (idE 'b) (numE 0)))
  (test (parse `{begin 1 2})
        (beginE (list (numE 1) (numE 2))))
  (test (parse `{record {x 2} {y 3}})
        (recordE (list 'x 'y)
                 (list (numE 2) (numE 3))))
  (test (parse `{get {+ 1 2} a})
        (rgetE (plusE (numE 1) (numE 2)) 'a))
  (test (parse `{set! {+ 1 2} a 7})
        (rsetE (plusE (numE 1) (numE 2)) 'a (numE 7)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input"))

;; with form ----------------------------------------
(define-syntax-rule
  (with [(v-id sto-id) call]
    body)
  (type-case Result call
    [(v*s v-id sto-id) body]))
                                
;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env] [sto : Store]) : Result
  (type-case Exp a
    [(numE n) (v*s (numV n) sto)]
    [(idE s) (v*s (lookup s env) sto)]
    [(plusE l r)
           (with [(v-l sto-l) (interp l env sto)]
             (with [(v-r sto-r) (interp r env sto-l)]
               (v*s (num+ v-l v-r) sto-r)))]
    [(multE l r)
           (with [(v-l sto-l) (interp l env sto)]
             (with [(v-r sto-r) (interp r env sto-l)]
               (v*s (num* v-l v-r) sto-r)))]
    [(letE n rhs body)
          (with [(v-rhs sto-rhs) (interp rhs env sto)]
            (interp body
                    (extend-env
                     (bind n v-rhs)
                     env)
                    sto-rhs))]
    [(lamE n body)
          (v*s (closV n body env) sto)]
    [(appE fun arg)
          (with [(v-f sto-f) (interp fun env sto)]
            (with [(v-a sto-a) (interp arg env sto-f)]
              (type-case Value v-f
                [(closV n body c-env)
                       (interp body
                               (extend-env
                                (bind n v-a)
                                c-env)
                               sto-a)]
                [else (error 'interp "not a function")])))]
    [(boxE a)
          (with [(v sto-v) (interp a env sto)]
            (let ([l (new-loc sto-v)])
              (v*s (boxV l) 
                   (override-store (cell l v) 
                                   sto-v))))]
    [(unboxE a)
            (with [(v sto-v) (interp a env sto)]
              (type-case Value v
                [(boxV l) (v*s (fetch l sto-v) 
                               sto-v)]
                [else (error 'interp "not a box")]))]
    [(setboxE bx val)
             (with [(v-b sto-b) (interp bx env sto)]
               (with [(v-v sto-v) (interp val env sto-b)]
                 (type-case Value v-b
                   [(boxV l)
                         (v*s v-v
                              (update-store (cell l v-v)
                                            sto-v))]
                   [else (error 'interp "not a box")])))]
    [(beginE as)
            (interp-seq as env sto)]
    [(recordE ns as)
             (interp-record ns as env sto)]
    [(rgetE a n)
           (with [(v-r sto-r) (interp a env sto)]
                 (type-case Value v-r
                            [(recV ns ls)
                                  (v*s (fetch (find n ns ls) sto-r)
                                       sto-r)]
                            [else (error 'interp "not a record")]))]
    [(rsetE a n v)
           (with [(v-r sto-r) (interp a env sto)]
                 (with [(v-v sto-v) (interp v env sto-r)]
                       (type-case Value v-r
                                  [(recV ns ls)
                                        (v*s v-v
                                             (override-store (cell (find n ns ls) v-v)
                                                             sto-v))]
                                  [else (error 'interp "not a record")])))]))

(module+ test
  (test (interp (parse `2) mt-env mt-store)
        (v*s (numV 2) 
             mt-store))
  (test/exn (interp (parse `x) mt-env mt-store)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env)
                mt-store)
        (v*s (numV 9)
             mt-store))
  (test (interp (parse `{+ 2 1}) mt-env mt-store)
        (v*s (numV 3)
             mt-store))
  (test (interp (parse `{* 2 1}) mt-env mt-store)
        (v*s (numV 2)
             mt-store))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env
                mt-store)
        (v*s (numV 19)
             mt-store))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env
                mt-store)
        (v*s (closV 'x (plusE (idE 'x) (idE 'x)) mt-env)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env
                mt-store)
        (v*s (numV 10)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                mt-store)
        (v*s (numV 12)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                mt-store)
        (v*s (numV 5)
             mt-store))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env
                mt-store)
        (v*s (numV 16)
             mt-store))
  (test (interp (parse `{box 5})
                mt-env
                mt-store)
        (v*s (boxV 1)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  (test (interp (parse `{unbox {box 5}})
                mt-env
                mt-store)
        (v*s (numV 5)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  (test (interp (parse `{set-box! {box 5} 6})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             mt-store)))
  (test (interp (parse `{begin 1 2})
                mt-env
                mt-store)
        (v*s (numV 2)
             mt-store))
  (test (interp (parse `{begin 1 2 3})
                mt-env
                mt-store)
        (v*s (numV 3)
             mt-store))
  (test (interp (parse `{unbox {begin {box 5} {box 6}}})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 2 (numV 6))
                             (override-store (cell 1 (numV 5))
                                             mt-store))))
  (test (interp (parse `{let {[b (box 5)]}
                          {begin
                            {set-box! b 6}
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             mt-store)))
  
  (test/exn (interp (parse `{1 2}) mt-env mt-store)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env mt-store)
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    mt-store)
            "free variable")
  (test/exn (interp (parse `{unbox 1}) mt-env mt-store)
            "not a box")
  (test/exn (interp (parse `{set-box! 1 1}) mt-env mt-store)
            "not a box")
  (test/exn (interp (parse `{get 1 x}) mt-env mt-store)
            "not a record")
  (test/exn (interp (parse `{set! 1 x 2}) mt-env mt-store)
            "not a record"))

;; interp-expr ----------------------------------------

(define (interp-expr e)
  (with [(v sto) (interp e mt-env mt-store)]
        (type-case Value v
                   [(numV n) (number->s-exp n)]
                   [(closV n body env) `function]
                   [(boxV v) `box]
                   [(recV ns vs) `record])))

(module+ test
  (test (interp-expr (parse `{+ 1 4}))
        `5)
  (test (interp-expr (parse `{lambda {x} x}))
        `function)
  (test (interp-expr (parse `{box 4}))
        `box)
  (test (interp-expr (parse `{record {a 10} {b {+ 1 2}}}))
        `record)
  (test (interp-expr (parse `{get {record {a 10} {b {+ 1 0}}} b}))
        `1)
  (test/exn (interp-expr (parse `{get {record {a 10}} b}))
            "no such field")
  (test (interp-expr (parse `{get {record {r {record {z 0}}}} r}))
        `record)
  (test (interp-expr (parse `{get {get {record {r {record {z 0}}}} r} z}))
        `0)

  (test (interp-expr (parse `{let {[z {box 1}]}
                              {begin
                               {record {x {set-box! z 2}}}
                               {unbox z}}}))
        `2)
  (test (interp-expr (parse `{let {[z {box 0}]}
                              {begin
                               {record
                                {x {set-box! z {+ {unbox z} 1}}}
                                {y {set-box! z {+ {unbox z} 2}}}}
                               {unbox z}}}))
        `3)

  (test (interp-expr (parse `{let {[r {record {x 1}}]}
                               {get r x}}))
        `1)

  (test (interp-expr (parse `{let {[z {box 1}]}
                              {let {[r {record {x 1}}]}
                               {begin
                                {get {begin {set-box! z 2} r} x}
                                {unbox z}}}}))
        `2)

  (test (interp-expr (parse `{let {[r {record {x 1}}]}
                               {begin
                                 {set! r x 5}
                                 {get r x}}}))
        `5)
  
  (test (interp-expr (parse `{let {[r {record {x 1}}]}
                               {let {[get-r {lambda {d} r}]}
                                 {begin
                                   {set! {get-r 0} x 6}
                                   {get {get-r 0} x}}}}))
        `6)

  (test (interp-expr (parse `{let {[g {lambda {r} {get r a}}]}
                               {let {[s {lambda {r} {lambda {v} {set! r b v}}}]}
                                 {let {[r1 {record {a 0} {b 2}}]}
                                   {let {[r2 {record {a 3} {b 4}}]}
                                     {+ {get r1 b}
                                        {begin
                                          {{s r1} {g r2}}
                                          {+ {begin
                                               {{s r2} {g r1}}
                                               {get r1 b}}
                                             {get r2 b}}}}}}}}))
        `5)

  (test (interp-expr (parse `{let {[r1 {record {x 1}}]}
                               {let {[r2 r1]}
                                 {begin
                                   {set! r1 x 2}
                                   {get r2 x}}}}))
        `2))

;; interp list helpers ----------------------------------------

;; Expects a non-empty list, returns the result of the last expression:
(define (interp-seq [as : (Listof Exp)] [env : Env] [sto : Store]) : Result
  (cond
   [(empty? (rest as)) (interp (first as) env sto)]
   [else (with [(v-l sto-l) (interp (first as) env sto)]
           (interp-seq (rest as) env sto-l))]))

(module+ test
  (test (interp-seq (list (numE 1)) mt-env mt-store)
        (v*s (numV 1) mt-store))
  (test (interp-seq (list (boxE (numE 1)) (numE 2)) 
                    mt-env 
                    mt-store)
        (v*s (numV 2) (override-store (cell 1 (numV 1))
                                      mt-store))))

;; interp-record ----------------------------------------

(define (interp-record ns as env sto) : Result
  (cond
   [(empty? as) (v*s (recV empty empty) sto)]
   [else
    (with [(v-v sto-v) (interp (first as) env sto)]
          (let ([l (new-loc sto-v)])
            (with [(r sto-r) (interp-record (rest ns)
                                            (rest as)
                                            env
                                            (override-store (cell l v-v)
                                                            sto-v))]
                  (v*s (recV (cons (first ns) (recV-ns r))
                             (cons l (recV-ls r)))
                       sto-r))))]))

(module+ test
  (test (interp-record empty empty mt-env mt-store)
        (v*s (recV empty empty) mt-store))
  (test (interp-record (list 'x 'y) (list (numE 7) (numE 8)) mt-env mt-store)
        (v*s (recV (list 'x 'y) (list 1 2))
             (override-store (cell 2 (numV 8))
                             (override-store (cell 1 (numV 7))
                                             mt-store)))))

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

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

;; lookup ----------------------------------------
(define (lookup [n : Symbol] [env : Env]) : Value
  (cond
   [(empty? env) (error 'lookup "free variable")]
   [else (cond
          [(symbol=? n (bind-name (first env)))
           (bind-val (first env))]
          [else (lookup n (rest env))])]))

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
  
;; store operations ----------------------------------------

(define (new-loc [sto : Store]) : Location
  (+ 1 (max-address sto)))

(define (max-address [sto : Store]) : Location
  (cond
   [(empty? sto) 0]
   [else (max (cell-location (first sto))
              (max-address (rest sto)))]))

(define (fetch [l : Location] [sto : Store]) : Value
  (cond
   [(empty? sto) (error 'interp "unallocated location")]
   [else (if (equal? l (cell-location (first sto)))
             (cell-val (first sto))
             (fetch l (rest sto)))]))

;; If `sto` has a cell whose location matches `c`,
;; replace the cell with `c`, otherwise add `c` to the
;; end.
(define (update-store [c : Storage] [sto : Store])
  (cond
   [(empty? sto) (list c)]
   [else (if (equal? (cell-location c)
                     (cell-location (first sto)))
             (cons c (rest sto))
             (cons (first sto) (update-store c (rest sto))))]))

(module+ test
  (test (max-address mt-store)
        0)
  (test (max-address (override-store (cell 2 (numV 9))
                                     mt-store))
        2)
  
  (test (fetch 2 (override-store (cell 2 (numV 9))
                                 mt-store))
        (numV 9))
  (test (fetch 2 (override-store (cell 2 (numV 10))
                                 (override-store (cell 2 (numV 9))
                                                 mt-store)))
        (numV 10))
  (test (fetch 3 (override-store (cell 2 (numV 10))
                                 (override-store (cell 3 (numV 9))
                                                 mt-store)))
        (numV 9))
  (test/exn (fetch 2 mt-store)
            "unallocated location")

  (test (update-store (cell 1 (numV 2)) mt-store)
        (override-store (cell 1 (numV 2)) mt-store))
  (test (update-store (cell 1 (numV 3))
                      (override-store (cell 1 (numV 2)) 
                                      mt-store))
        (override-store (cell 1 (numV 3)) mt-store))
  (test (update-store (cell 1 (numV 3))
                      (override-store (cell 2 (numV 2)) 
                                      mt-store))
        (override-store (cell 2 (numV 2))
                        (override-store (cell 1 (numV 3)) 
                                        mt-store))))

;; find ----------------------------------------

;; Takes a name and two parallel lists, returning an item from the
;; second list where the name matches the item from the first list.
(define (find [n : Symbol] [ns : (Listof Symbol)] [vs : (Listof Location)])
  : Location
  (cond
   [(empty? ns) (error 'interp "no such field")]
   [else (if (symbol=? n (first ns))
             (first vs)
             (find n (rest ns) (rest vs)))]))

(module+ test
  (test (find 'a (list 'a 'b) (list 1 2))
        1)
  (test (find 'b (list 'a 'b) (list 1 2))
        2)
  (test/exn (find 'a empty empty)
            "no such field"))