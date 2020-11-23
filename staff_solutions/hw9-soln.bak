#lang plait

;; For solution changes, look for "Part 1", "Part 2", and "Part 3"

;; ============================================================
;; classes without inheritance

(define-type Exp
  (numE [n : Number])
  (plusE [lhs : Exp]
         [rhs : Exp])
  (multE [lhs : Exp]
         [rhs : Exp])
  (argE)
  (thisE)
  (newE [class-name : Symbol]
        [args : (Listof Exp)])
  (getE [obj-expr : Exp]
        [field-name : Symbol])
  (sendE [obj-expr : Exp]
         [method-name : Symbol]
         [arg-expr : Exp])
  (ssendE [obj-expr : Exp]
          [class-name : Symbol]
          [method-name : Symbol]
          [arg-expr : Exp])
  ;; Part 2
  (selectE [num-expr : Exp]
           [obj-expr : Exp])
  ;; Part 3
  (instanceofE [obj-expr : Exp]
               [class-name : Symbol]))

(define-type Class
  (classC [super-name : Symbol] ; Part 3
          [field-names : (Listof Symbol)]
          [methods : (Listof (Symbol * Exp))]))

(define-type Value
  (numV [n : Number])
  (objV [class-name : Symbol]
        [field-values : (Listof Value)]))

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (find [l : (Listof (Symbol * 'a))] [name : Symbol]) : 'a
  (type-case (Listof (Symbol * 'a)) l
    [empty
     (error 'find (string-append "not found: " (symbol->string name)))]
    [(cons p rst-l)
     (if (symbol=? (fst p) name)
         (snd p)
         (find rst-l name))]))

(module+ test
  (test (find (list (values 'a 1)) 'a)
        1)
  (test (find (list (values 'a 1) (values 'b 2)) 'b)
        2)
  (test/exn (find empty 'a)
            "not found: a")
  (test/exn (find (list (values 'a 1)) 'x)
            "not found: x"))

;; ----------------------------------------
;; Part 3

(define (subclass? obj-class-name class-name classes)
  (cond
    [(equal? obj-class-name class-name) #t]
    [(equal? obj-class-name 'Object) #f]
    [else
     (type-case Class (find classes obj-class-name)
       [(classC super-name field-names methods)
        (subclass? super-name class-name classes)])]))

(module+ test
  (test (subclass? 'Object 'Object empty)
        #t)
  (test (subclass? 'A 'Object (list (values 'A (classC 'Object empty empty))))
        #t)
  (test (subclass? 'Object 'A (list (values 'A (classC 'Object empty empty))))
        #f)
  (test (subclass? 'B 'A (list (values 'A (classC 'Object empty empty))
                               (values 'B (classC 'A empty empty))))
        #t)
  (test (subclass? 'B 'Object (list (values 'A (classC 'Object empty empty))
                                    (values 'B (classC 'A empty empty))))
        #t))

;; ----------------------------------------

(define interp : (Exp (Listof (Symbol * Class)) Value Value -> Value)
  (lambda (a classes this-val arg-val)
    (local [(define (recur expr)
              (interp expr classes this-val arg-val))]
      (type-case Exp a
        [(numE n) (numV n)]
        [(plusE l r) (num+ (recur l) (recur r))]
        [(multE l r) (num* (recur l) (recur r))]
        [(thisE ) this-val]
        [(argE ) arg-val]
        [(newE class-name field-exprs)
         (local [(define c (if (symbol=? class-name 'Object) ;; Part 1
                               (classC 'Object empty empty) ;; Part 3 (adjust)
                               (find classes class-name)))
                 (define vals (map recur field-exprs))]
           (if (= (length vals) (length (classC-field-names c)))
               (objV class-name vals)
               (error 'interp "wrong field count")))]
        [(getE obj-expr field-name)
         (type-case Value (recur obj-expr)
           [(objV class-name field-vals)
            (type-case Class (find classes class-name)
              [(classC super-name field-names methods) ;; Part 3 (adjust)
               (find (map2 (lambda (n v) (values n v))
                           field-names
                           field-vals)
                     field-name)])]
           [else (error 'interp "not an object")])]
        [(sendE obj-expr method-name arg-expr)
         (local [(define obj (recur obj-expr))
                 (define arg-val (recur arg-expr))]
           (type-case Value obj
             [(objV class-name field-vals)
              (call-method class-name method-name classes
                           obj arg-val)]
             [else (error 'interp "not an object")]))]
        [(ssendE obj-expr class-name method-name arg-expr)
         (local [(define obj (recur obj-expr))
                 (define arg-val (recur arg-expr))]
           (call-method class-name method-name classes
                        obj arg-val))]
        ;; Part 2
        [(selectE num-expr obj-expr)
                 (local [(define num (recur num-expr))
                         (define obj (recur obj-expr))]
                   (type-case Value num
                     [(numV n)
                           (type-case Value obj
                             [(objV class-name field-values)
                                   (call-method class-name
                                                (if (zero? n) 'zero 'nonzero)
                                                classes
                                                obj (numV 0))]
                             [else (error 'interp "not an object")])]
                     [else (error 'interp "not a number")]))]
        ;; Part 3
        [(instanceofE obj-expr class-name)
                     (type-case Value (recur obj-expr)
                       [(objV obj-class-name field-values)
                             (if (subclass? obj-class-name class-name classes)
                                 (numV 0)
                                 (begin
                                   ;; check that `class-name` is defined:
                                   (find classes class-name)
                                   (numV 1)))]
                       [else (error 'interp "not an object")])]))))

(define (call-method class-name method-name classes
                     obj arg-val)
  (type-case Class (find classes class-name)
    [(classC super-name field-names methods) ;; Part 3 (adjust)
     (let ([body-expr (find methods method-name)])
       (interp body-expr
               classes
               obj
               arg-val))]))

(define (num-op [op : (Number Number -> Number)]
                [op-name : Symbol] 
                [x : Value]
                [y : Value]) : Value
  (cond
    [(and (numV? x) (numV? y))
     (numV (op (numV-n x) (numV-n y)))]
    [else (error 'interp "not a number")]))

(define (num+ x y) (num-op + '+ x y))
(define (num* x y) (num-op * '* x y))

;; ----------------------------------------
;; Examples

(module+ test
  (define posn-class
    (values 'Posn
            (classC
             'Object ; Part 3
             (list 'x 'y)
             (list (values 'mdist
                           (plusE (getE (thisE) 'x) (getE (thisE) 'y)))
                   (values 'addDist
                           (plusE (sendE (thisE) 'mdist (numE 0))
                                  (sendE (argE) 'mdist (numE 0))))
                   (values 'addX
                           (plusE (getE (thisE) 'x) (argE)))
                   (values 'multY (multE (argE) (getE (thisE) 'y)))
                   (values 'factory12 (newE 'Posn (list (numE 1) (numE 2))))))))
    
  (define posn3D-class
    (values 'Posn3D
            (classC
             'Posn ; Part 3
             (list 'x 'y 'z)
             (list (values 'mdist (plusE (getE (thisE) 'z)
                                         (ssendE (thisE) 'Posn 'mdist (argE))))
                   (values 'addDist (ssendE (thisE) 'Posn 'addDist (argE)))))))

  (define new-posn27 (newE 'Posn (list (numE 2) (numE 7))))
  (define new-posn531 (newE 'Posn3D (list (numE 5) (numE 3) (numE 1))))

  (define (interp-posn a)
    (interp a (list posn-class posn3D-class) (numV -1) (numV -1)))
  
  ;; Part 2
  (define snowball-class
    (values 'Snowball
            (classC 'Object ; Part 3
                    (list 'size)
                    (list (values 'zero (thisE))
                          (values 'nonzero (newE 'Snowball 
                                                 (list (plusE (numE 1) (getE (thisE) 'size))))))))))

;; ----------------------------------------

(module+ test
  (test (interp (numE 10) 
                empty (objV 'Object empty) (numV 0))
        (numV 10))
  (test (interp (plusE (numE 10) (numE 17))
                empty (objV 'Object empty) (numV 0))
        (numV 27))
  (test (interp (multE (numE 10) (numE 7))
                empty (objV 'Object empty) (numV 0))
        (numV 70))

  (test (interp-posn (newE 'Posn (list (numE 2) (numE 7))))
        (objV 'Posn (list (numV 2) (numV 7))))

  (test (interp-posn (sendE new-posn27 'mdist (numE 0)))
        (numV 9))
  
  (test (interp-posn (sendE new-posn27 'addX (numE 10)))
        (numV 12))

  (test (interp-posn (sendE (ssendE new-posn27 'Posn 'factory12 (numE 0))
                            'multY
                            (numE 15)))
        (numV 30))

  (test (interp-posn (sendE new-posn531 'addDist new-posn27))
        (numV 18))

  ;; Part 2
  (test (interp (selectE (numE 0) (newE 'Snowball (list (numE 1))))
                (list snowball-class)
                (objV 'Object empty) (numV 0))
        (objV 'Snowball (list (numV 1))))
  (test (interp (selectE (numE 1) (newE 'Snowball (list (numE 1))))
                (list snowball-class)
                (objV 'Object empty) (numV 0))
        (objV 'Snowball (list (numV 2))))
  (test/exn (interp (selectE (newE 'Object empty) (numE 1))
                    empty (objV 'Object empty) (numV 0))
            "not a number")
  (test/exn (interp (selectE (numE 0) (numE 1))
                    empty (objV 'Object empty) (numV 0))
            "not an object")
  
  ;; Part 3
  (test (interp-posn (instanceofE new-posn27 'Posn))
        (numV 0))
  (test (interp-posn (instanceofE new-posn531 'Posn))
        (numV 0))
  (test (interp-posn (instanceofE new-posn27 'Object))
        (numV 0))
  (test (interp-posn (instanceofE new-posn27 'Posn3D))
        (numV 1))
  (test/exn (interp-posn (instanceofE (numE 1) 'Posn))
            "not an object")
  (test/exn (interp-posn (instanceofE new-posn27 'Fish))
            "not found")
  
  (test/exn (interp-posn (plusE (numE 1) new-posn27))
            "not a number")
  (test/exn (interp-posn (getE (numE 1) 'x))
            "not an object")
  (test/exn (interp-posn (sendE (numE 1) 'mdist (numE 0)))
            "not an object")
  (test/exn (interp-posn (ssendE (numE 1) 'Posn 'mdist (numE 0)))
            "not an object")
  (test/exn (interp-posn (newE 'Posn (list (numE 0))))
            "wrong field count"))

;; ============================================================
;; inherit

(define-type ExpI
  (numI [n : Number])
  (plusI [lhs : ExpI]
         [rhs : ExpI])
  (multI [lhs : ExpI]
         [rhs : ExpI])
  (argI)
  (thisI)
  (newI [class-name : Symbol]
        [args : (Listof ExpI)])
  (getI [obj-expr : ExpI]
        [field-name : Symbol])
  (sendI [obj-expr : ExpI]
         [method-name : Symbol]
         [arg-expr : ExpI])
  (superI [method-name : Symbol]
          [arg-expr : ExpI])
  ;; Part 2
  (selectI [num-expr : ExpI]
           [obj-expr : ExpI])
  ;; Part 3
  (instanceofI [obj-expr : ExpI]
               [class-name : Symbol]))

(define-type ClassI
  (classI [super-name : Symbol]
          [field-names : (Listof Symbol)]
          [methods : (Listof (Symbol * ExpI))]))

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (exp-i->c [a : ExpI] [super-name : Symbol]) : Exp
  (local [(define (recur expr)
            (exp-i->c expr super-name))]
    (type-case ExpI a
      [(numI n) (numE n)]
      [(plusI l r) (plusE (recur l) (recur r))]
      [(multI l r) (multE (recur l) (recur r))]
      [(argI) (argE)]
      [(thisI) (thisE)]
      [(newI class-name field-exprs)
       (newE class-name (map recur field-exprs))]
      [(getI expr field-name)
       (getE (recur expr) field-name)]
      [(sendI expr method-name arg-expr)
       (sendE (recur expr)
              method-name
              (recur arg-expr))]
      [(superI method-name arg-expr)
       (ssendE (thisE)
               super-name
               method-name
               (recur arg-expr))]
      ;; Part 2
      [(selectI num-expr obj-expr)
       (selectE (recur num-expr)
                (recur obj-expr))]
      ;; Part 3
      [(instanceofI obj-expr class-name)
       (instanceofE (recur obj-expr)
                    class-name)])))

(module+ test
  (test (exp-i->c (numI 10) 'Object)
        (numE 10))
  (test (exp-i->c (plusI (numI 10) (numI 2)) 'Object)
        (plusE (numE 10) (numE 2)))
  (test (exp-i->c (multI (numI 10) (numI 2)) 'Object)
        (multE (numE 10) (numE 2)))
  (test (exp-i->c (argI) 'Object)
        (argE))
  (test (exp-i->c (thisI) 'Object)
        (thisE))
  (test (exp-i->c (newI 'Object (list (numI 1))) 'Object)
        (newE 'Object (list (numE 1))))
  (test (exp-i->c (getI (numI 1) 'x) 'Object)
        (getE (numE 1) 'x))
  (test (exp-i->c (sendI (numI 1) 'mdist (numI 2)) 'Object)
        (sendE (numE 1) 'mdist (numE 2)))
  (test (exp-i->c (superI 'mdist (numI 2)) 'Posn)
        (ssendE (thisE) 'Posn 'mdist (numE 2)))
  ;; Part 2
  (test (exp-i->c (selectI (numI 1) (numI 2)) 'Posn)
        (selectE (numE 1) (numE 2)))
  ;; Part 3
  (test (exp-i->c (instanceofI (numI 1) 'Posn) 'Posn)
        (instanceofE (numE 1) 'Posn)))

;; ----------------------------------------

(define (class-i->c-not-flat [c : ClassI]) : Class
  (type-case ClassI c
    [(classI super-name field-names methods)
     (classC
      super-name ; Part 3
      field-names
      (map (lambda (m)
             (values (fst m)
                     (exp-i->c (snd m) super-name)))
           methods))]))

(module+ test
  (define posn3d-mdist-i-method
    (values 'mdist
            (plusI (getI (thisI) 'z)
                   (superI 'mdist (argI)))))
  (define posn3d-mdist-c-method
    (values 'mdist
            (plusE (getE (thisE) 'z)
                   (ssendE (thisE) 'Posn 'mdist (argE)))))

  (define posn3d-i-class 
    (values 'Posn3D
            (classI
             'Posn
             (list 'z)
             (list posn3d-mdist-i-method))))
  (define posn3d-c-class-not-flat
    (values 'Posn3D
            (classC 'Posn ; Part 3
                    (list 'z)
                    (list posn3d-mdist-c-method))))

  (test (class-i->c-not-flat (snd posn3d-i-class))
        (snd posn3d-c-class-not-flat)))

;; ----------------------------------------

(define (flatten-class [name : Symbol]
                       [classes-not-flat : (Listof (Symbol * Class))] 
                       [i-classes : (Listof (Symbol * ClassI))]) : Class
  (type-case Class (find classes-not-flat name)
    [(classC super-name field-names methods)  ; Part 3 (adjust)
     (type-case Class (flatten-super name classes-not-flat i-classes)
       [(classC super-super-name super-field-names super-methods) ; Part 3 (adjust)
        (classC
         super-name
         (add-fields super-field-names field-names)
         (add/replace-methods super-methods methods))])]))

(define (flatten-super [name : Symbol]
                       [classes-not-flat : (Listof (Symbol * Class))] 
                       [i-classes : (Listof (Symbol * ClassI))]) : Class
  (type-case ClassI (find i-classes name)
    [(classI super-name field-names i-methods)
     (if (equal? super-name 'Object)
         (classC 'Object empty empty) ; Part 3 (adjust)
         (flatten-class super-name
                        classes-not-flat
                        i-classes))]))

(module+ test
  (define posn-i-class
    (values
     'Posn
     (classI 'Object
             (list 'x 'y)
             (list (values 'mdist
                           (plusI (getI (thisI) 'x)
                                  (getI (thisI) 'y)))
                   (values 'addDist
                            (plusI (sendI (thisI) 'mdist (numI 0))
                                   (sendI (argI) 'mdist (numI 0))))))))
  (define addDist-c-method
    (values 'addDist
            (plusE (sendE (thisE) 'mdist (numE 0))
                   (sendE (argE) 'mdist (numE 0)))))
  (define posn-c-class-not-flat
    (values
     'Posn
     (classC 'Object ; Part 3 (adjust)
             (list 'x 'y)
             (list (values 'mdist
                           (plusE (getE (thisE) 'x)
                                  (getE (thisE) 'y)))
                   addDist-c-method))))
  (define posn3d-c-class
    (values 'Posn3D
            (classC 'Posn ; Part 3 (adjust)
                    (list 'x 'y 'z)
                    (list posn3d-mdist-c-method
                          addDist-c-method))))
  
  (test (flatten-class 'Posn3D
                       (list posn-c-class-not-flat
                             posn3d-c-class-not-flat)
                       (list posn-i-class
                             posn3d-i-class))
        (snd posn3d-c-class)))

;; ----------------------------------------

(define add-fields append)

(define (add/replace-methods [methods : (Listof (Symbol * Exp))]
                             [new-methods : (Listof (Symbol * Exp))])
  : (Listof (Symbol * Exp))
  (cond
    [(empty? new-methods) methods]
    [else (add/replace-methods
           (add/replace-method methods (first new-methods))
           (rest new-methods))]))

(define (add/replace-method [methods : (Listof (Symbol * Exp))] 
                            [new-method : (Symbol * Exp)])
  : (Listof (Symbol * Exp))
  (cond
    [(empty? methods) (list new-method)]
    [else
     (if (equal? (fst (first methods))
                 (fst new-method))
         (cons new-method (rest methods))
         (cons (first methods) 
               (add/replace-method (rest methods)
                                   new-method)))]))

(module+ test
  (test (add-fields (list 'x 'y) (list 'z))
        (list 'x 'y 'z))

  (test (add/replace-methods empty empty)
        empty)
  (test (add/replace-methods empty (list (values 'm (numE 0))))
        (list (values 'm (numE 0))))
  (test (add/replace-methods (list (values 'm (numE 0))) empty)
        (list (values 'm (numE 0))))
  (test (add/replace-methods (list (values 'm (numE 0)))
                             (list (values 'm (numE 1))))
        (list (values 'm (numE 1))))
  (test (add/replace-methods (list (values 'm (numE 0))
                                   (values 'n (numE 2)))
                             (list (values 'm (numE 1))))
        (list (values 'm (numE 1))
              (values 'n (numE 2))))
  (test (add/replace-methods (list (values 'm (numE 0)))
                             (list (values 'm (numE 1))
                                   (values 'n (numE 2))))
        (list (values 'm (numE 1))
              (values 'n (numE 2))))

  (test (add/replace-method (list (values 'm (numE 0)))
                            (values 'm (numE 1)))
        (list (values 'm (numE 1))))
  (test (add/replace-method (list (values 'm (numE 0)))
                            (values 'n (numE 2)))
        (list (values 'm (numE 0))
              (values 'n (numE 2)))))

;; ----------------------------------------

(define (interp-i [i-a : ExpI] [i-classes : (Listof (Symbol * ClassI))]) : Value
  (local [(define a (exp-i->c i-a 'Object))
          (define classes-not-flat
            (map (lambda (i)
                   (values (fst i)
                           (class-i->c-not-flat (snd i))))
                 i-classes))
          (define classes
            (map (lambda (c)
                   (let ([name (fst c)])
                     (values name
                             (flatten-class name classes-not-flat i-classes))))
                 classes-not-flat))]
    (interp a classes (objV 'Object empty) (numV 0))))

(module+ test
  (test (interp-i (numI 0) empty)
        (numV 0))

  (test (interp-i
         (sendI (newI 'Posn3D (list (numI 5) (numI 3) (numI 1)))
                'addDist
                (newI 'Posn (list (numI 2) (numI 7))))
         (list posn-i-class
               posn3d-i-class))
        (numV 18)))

;; ============================================================
;; parse & interp-prog

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (parse-class [s : S-Exp]) : (Symbol * ClassI)
  (cond
    [(s-exp-match? `{class SYMBOL extends SYMBOL {ANY ...} ANY ...} s)
     (values (s-exp->symbol (second (s-exp->list s)))
             (classI (s-exp->symbol (fourth (s-exp->list s)))
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
   [(s-exp-match? `{send ANY SYMBOL ANY} s)
    (sendI (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
   [(s-exp-match? `{super SYMBOL ANY} s)
    (superI (s-exp->symbol (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
   ;; Part 2
   [(s-exp-match? `{select ANY ANY} s)
    (selectI (parse (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
   ;; Part 3
   [(s-exp-match? `{instanceof ANY SYMBOL} s)
    (instanceofI (parse (second (s-exp->list s)))
                 (s-exp->symbol (third (s-exp->list s))))]
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
  (test (parse `{* 1 2})
        (multI (numI 1) (numI 2)))
  (test (parse `{new Posn 1 2})
        (newI 'Posn (list (numI 1) (numI 2))))
  (test (parse `{get 1 x})
        (getI (numI 1) 'x))
  (test (parse `{send 1 m 2})
        (sendI (numI 1) 'm (numI 2)))
  (test (parse `{super m 1})
        (superI 'm (numI 1)))
  (test/exn (parse `x)
            "invalid input")
  
  ;; Part 2:
  (test (parse `{select 1 2})
        (selectI (numI 1) (numI 2)))
  ;; Part 3:
  (test (parse `{instanceof 1 Posn})
        (instanceofI (numI 1) 'Posn))

  (test (parse-field `x)
        'x)
  (test/exn (parse-field `{x 1})
            "invalid input")

  (test (parse-method `[m {arg} this])
        (values 'm (thisI)))
  (test/exn (parse-method `{m 1 2})
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
      [(objV class-name field-vals) `object])))

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

;; ----------------------------------------

(module+ test
  ;; Part 1
  (test (interp-prog (list)
                     `{new Object})
        `object)
  
  ;; Part 2:
  (test (interp-prog (list `{class Snowball extends Object
                              {size}
                              [zero {arg} this]
                              [nonzero {arg}
                                       {new Snowball {+ 1 {get this size}}}]})
                     `{get {select 0 {new Snowball 1}} size})
        `1)
  (test (interp-prog (list `{class Snowball extends Object
                              {size}
                              [zero {arg} this]
                              [nonzero {arg}
                                       {new Snowball {+ 1 {get this size}}}]})
                     `{get {select {+ 1 2} {new Snowball 1}} size})
        `2)
  
  ;; Part 3:
  (test (interp-prog (list `{class Fish extends Object
                                   {size color}})
                     `{instanceof {new Fish 1 2} Fish})
        `0)
  
  (test (interp-prog (list `{class Fish extends Object
                              {size color}})
                     `{instanceof {new Fish 1 2} Fish})
        `0)
  (test (interp-prog (list `{class Fish extends Object
                              {size color}}
                           `{class Shark extends Fish
                              {teeth}})
                     `{instanceof {new Shark 1 2 3} Fish})
        `0))