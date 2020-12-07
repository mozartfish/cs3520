#lang plait

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
  (setE [obj-expr : Exp]
        [field-name : Symbol]
        [field-value : Exp])
        
  (sendE [obj-expr : Exp]
         [method-name : Symbol]
         [arg-expr : Exp])
  (ssendE [obj-expr : Exp]
          [class-name : Symbol]
          [method-name : Symbol]
          [arg-expr : Exp])
  (castE [class-name : Symbol]
         [obj-expr : Exp])
  (if0E [tst : Exp]
        [thn : Exp]
        [els : Exp])
  (nullE))
  
  

(define-type Class
  (classC
   [super-name : Symbol]
   [field-names : (Listof Symbol)]
   [methods : (Listof (Symbol * Exp))]))

(define-type Value
  (numV [n : Number])
  (boxV [b : (Boxof Value)])
  (objV [class-name : Symbol]
        [field-values : (Listof (Boxof Value))])
  (nullV))

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

(define interp : (Exp (Listof (Symbol * Class)) Value Value -> Value)
  (lambda (a classes this-val arg-val)
    (local [(define (recur expr)
              (interp expr classes this-val arg-val))]
      (type-case Exp a
        [(numE n) (numV n)]
        [(plusE l r) (num+ (recur l) (recur r))]
        [(multE l r) (num* (recur l) (recur r))]
        [(thisE) this-val]
        [(argE) arg-val]
        [(newE class-name field-exprs)
         (local [(define c (find classes class-name))
                 (define vals (map box (map recur field-exprs)))]
           (if (= (length vals) (length (classC-field-names c)))
               (objV class-name vals)
               (error 'interp "wrong field count")))]
        [(getE obj-expr field-name)
         (type-case Value (recur obj-expr)
           [(objV class-name field-vals)
            (type-case Class (find classes class-name)
              [(classC super-name field-names methods)
               (find (map2 (lambda (n v) (values n v))
                           field-names
                           (map unbox field-vals))
                     field-name)])]
           [else (error 'interp "not an object")])]
        ; setE interp
        [(setE obj-expr field-name new-field-value)
         (type-case Value (recur obj-expr)
           [(objV class-name field-vals)
            (type-case Class (find classes class-name)
              [(classC super-name field-names methods)
               ;; need a helper function of some sorts to set the field value
               (begin
                 (set-field field-names field-vals field-name (recur new-field-value))
                 (objV class-name field-vals))])]
           [else (error 'interp "not an object")])]
        ; castE interp
        [(castE class-name obj-expr)
         (local [(define obj (recur obj-expr))]
           (type-case Value obj
             [(objV obj-class-name obj-field-vals)
              (if (subclass? obj-class-name class-name classes)
                  obj
                  (error 'interp "invalid cast: not an instance of a class/one of its subclasses"))]
             [else (error 'interp "not an object")]))]
        ; ifE interp
        [(if0E tst thn els)
         (type-case Value (recur tst)
           [(numV n) (if (= n 0)
                         (recur thn)
                         (recur els))]
           [else (error 'interp "not a number")])]
        [(nullE) (nullV)]
         
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
                        obj arg-val))]))))

(define (call-method class-name method-name classes
                     obj arg-val)
  (type-case Class (find classes class-name)
    [(classC super-name field-names methods)
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

; sub-class helper function
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

; set-field helper function
; need a function that takes a list of field names
; list of field values that find a field and updates its corrsponding value
(define (set-field [field-names : (Listof Symbol)]
                   [field-values : (Listof (Boxof Value))]
                   [field-name : Symbol]
                   [new-field-value : Value])
  ;; check if the list of field names is empty
  (type-case (Listof Symbol) field-names
    [empty (error 'interp "field does not exist")]
    [(cons first-name rst-names)
     (if (symbol=? first-name field-name)
         (set-box! (first field-values) new-field-value)
         (set-field rst-names (rest field-values) field-name new-field-value))]))

;; test cases
(module+ test
  (test
   (let ([field-names (list 'x 'y 'z)])
     (let ([field-values (list (box (numV 1)) (box (numV 2)) (box (numV 3)))])
       (begin
         (set-field field-names field-values 'x (numV 5))
         (unbox (first field-values)))))
   (numV 5))

  (test/exn
   (let ([field-names (list 'x 'y)])
     (let ([field-values (list (box (numV 1)) (box (numV 2)) (box (numV 3)))])
         (set-field field-names field-values 'z (numV 5))))
   "interp: field does not exist")

  (test
   (let ([field-names (list 'x 'y 'z)])
     (let ([field-values (list (box (numV 1)) (box (numV 2)) (box (numV 3)))])
       (begin
         (set-field field-names field-values 'z (numV 10))
         (unbox (third field-values)))))
   (numV 10)))
  

  ;; ----------------------------------------
;; Examples

(module+ test
  (define posn-class
    (values 'Posn
            (classC
             'Object
             (list 'x 'y)
             (list (values 'mdist
                           (plusE (getE (thisE) 'x) (getE (thisE) 'y)))
                   (values 'addDist
                           (plusE (sendE (thisE) 'mdist (numE 0))
                                  (sendE (argE) 'mdist (numE 0))))
                   (values 'addX
                           (plusE (getE (thisE) 'x) (argE)))
                   (values 'multY (multE (argE) (getE (thisE) 'y)))
                   (values 'factory12 (newE 'Posn (list (numE 1) (numE 2))))
                   (values 'multSetY
                           (multE (argE) (getE (setE (thisE) 'y (numE 2)) 'y)))))))
    
  (define posn3D-class
    (values 'Posn3D
            (classC
             'Posn
             (list 'x 'y 'z)
             (list (values 'mdist (plusE (getE (thisE) 'z)
                                         (ssendE (thisE) 'Posn 'mdist (argE))))
                   (values 'addDist (ssendE (thisE) 'Posn 'addDist (argE)))))))

  (define posn27 (newE 'Posn (list (numE 2) (numE 7))))
  (define posn531 (newE 'Posn3D (list (numE 5) (numE 3) (numE 1))))

  (define (interp-posn a)
    (interp a (list posn-class posn3D-class) (numV -1) (numV -1))))

;; ----------------------------------------
; castE test cases
(module+ test
  (test/exn (interp-posn (castE 'Posn3D posn27))
            "interp: invalid cast: not an instance of a class/one of its subclasses")
  (test/exn (interp-posn (castE 'Posn (plusE (numE 1) (numE 2))))
            "not an object")
  (test (interp-posn (castE 'Posn posn27))
        (objV 'Posn (list (box (numV 2)) (box (numV 7)))))

  (test (interp-posn (castE 'Posn posn531))
        (objV 'Posn3D (list (box (numV 5)) (box (numV 3)) (box (numV 1))))))
;;--------------------------------------------
; if0E test cases
(module+ test
  (test (interp (if0E (numE 1) (numE 3) (numE 5))
                empty (objV 'Object empty) (numV 0))
        (numV 5))
  (test (interp (if0E (numE 0) (numE 3) (numE 5))
                empty (objV 'Object empty) (numV 0))
        (numV 3))
  (test/exn (interp-posn (if0E (castE 'Object (newE 'Posn (list (numE 2) (numE 7))))
                               (numE 1) (numE 5)))
            "not a number"))
                    
;; -------------------------------------------

;; null E test cases
(module+ test
  (test/exn (interp (plusE (nullE) (numE 17))
                empty (objV 'Object empty) (numV 0))
            "interp: not a number")
  (test/exn
   (interp-posn (sendE posn27 'addX (nullE)))
   "interp: not a number")
  (test/exn (interp-posn (getE (nullE) 'x))
            "not an object"))

;;----------------------------------------------
;; setE test cases 
(module+ test
  (test (interp-posn (setE posn27 'x (numE 0)))
        (objV 'Posn (list (box (numV 0)) (box (numV 7)))))
  (test/exn (interp-posn (setE posn27 'z (numE 0)))
            "interp: field does not exist")
  (test (interp-posn (setE posn531 'z (numE 8)))
        (objV 'Posn3D (list (box (numV 5)) (box (numV 3)) (box (numV 8))))))
;;-----------------------------------------------
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
        (objV 'Posn (list (box (numV 2)) (box (numV 7)))))

  (test (interp-posn (sendE posn27 'mdist (numE 0)))
        (numV 9))
  
  (test (interp-posn (sendE posn27 'addX (numE 10)))
        (numV 12))
  ; multSetY test
  (test (interp-posn (sendE posn27 'multSetY (numE 10)))
        (numV 20))

  (test (interp-posn (sendE (ssendE posn27 'Posn 'factory12 (numE 0))
                            'multY
                            (numE 15)))
        (numV 30))

  (test (interp-posn (sendE posn531 'addDist posn27))
        (numV 18))
  
  (test/exn (interp-posn (plusE (numE 1) posn27))
            "not a number")
  ; setE exception test
  (test/exn (interp-posn (setE (numE 1) 'x (numE 27)))
            "not an object")
  (test/exn (interp-posn (getE (numE 1) 'x))
            "not an object")
  (test/exn (interp-posn (sendE (numE 1) 'mdist (numE 0)))
            "not an object")
  (test/exn (interp-posn (ssendE (numE 1) 'Posn 'mdist (numE 0)))
            "not an object")
  (test/exn (interp-posn (newE 'Posn (list (numE 0))))
            "wrong field count"))
