#lang plait

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (letE [name : Symbol]
        [rhs : Exp]
        [body : Exp])
  (newArrayE [size : Exp]
             [init : Exp])
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
  (objV [class-name : Symbol]
        [field-values : (Listof (Boxof Value))])
  (arrV [size : Number]
        [init-val : Value]
        [vec : (Vectorof Value)])
  (nullV))

; implementation for let
(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

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

(define interp : (Exp (Listof (Symbol * Class)) Value Value Env -> Value)
  (lambda (a classes this-val arg-val env)
    (local [(define (recur expr)
              (interp expr classes this-val arg-val env))]
      (type-case Exp a
        [(numE n) (numV n)]
        [(plusE l r) (num+ (recur l) (recur r))]
        [(multE l r) (num* (recur l) (recur r))]
        ; newArray case
        [(newArrayE size init)
         (type-case Value (recur size)
           [(numV n)
            (local  [(define init-val (recur init))]
              (arrV n init-val (make-vector n init-val)))]
           [else (error 'interp "size not a number")])]
        ; idE
        [(idE s) (lookup s env)]
        ;;(let ([x : num 5]) (+ 5 x))
        ;; the let case
        [(letE name rhs body)
         (let ([x (recur rhs)])
           (interp body
                   classes
                   this-val
                   arg-val
                   (extend-env
                    (bind name x)
                    env)))]
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
               (begin
                 (set-box!
                  (find (map2 (lambda (n v) (values n v))
                              field-names
                              field-vals)
                        field-name) (recur new-field-value))
                 (numV 0))])]
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
               arg-val mt-env))]))

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
;; lookup ----------------------------------------
(define (make-lookup [name-of : ('a -> Symbol)] [val-of : ('a -> 'b)])
  (lambda ([name : Symbol] [vals : (Listof 'a)]) : 'b
          (cond
            [(empty? vals)
             (error 'find "free variable")]
            [else (if (equal? name (name-of (first vals)))
                      (val-of (first vals))
                      ((make-lookup name-of val-of) name (rest vals)))])))

(define lookup
  (make-lookup bind-name bind-val))

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
                   (values 'setAddX
                           (plusE (getE (setE (thisE) 'x (numE 10)) 'x) (argE)))
                   (values 'multY (multE (argE) (getE (thisE) 'y)))
                   (values 'factory12 (newE 'Posn (list (numE 1) (numE 2))))))))
    
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
    (interp a (list posn-class posn3D-class) (numV -1) (numV -1) mt-env)))

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
                empty (objV 'Object empty) (numV 0) mt-env)
        (numV 5))
  (test (interp (if0E (numE 0) (numE 3) (numE 5))
                empty (objV 'Object empty) (numV 0) mt-env)
        (numV 3))
  (test/exn (interp-posn (if0E (castE 'Object (newE 'Posn (list (numE 2) (numE 7))))
                               (numE 1) (numE 5)))
            "not a number"))
                    
;; -------------------------------------------

;; null E test cases
(module+ test
  (test/exn (interp (plusE (nullE) (numE 17))
                empty (objV 'Object empty) (numV 0) mt-env)
            "interp: not a number")
  (test/exn
   (interp-posn (sendE posn27 'addX (nullE)))
   "interp: not a number")
  (test/exn (interp-posn (getE (nullE) 'x))
            "not an object"))
;;----------------------------------------------
;; newArrayE test cases
(module+ test
  (test (interp (newArrayE (numE 5) (numE 0))
                empty (objV 'Object empty) (numV 0) mt-env)
        (arrV 5 (numV 0) (make-vector 5 (numV 0))))
    (test/exn (interp (newArrayE (nullE) (numE 0))
                empty (objV 'Object empty) (numV 0) mt-env)
              "interp: size not a number"))
  
;; -----------------------------------------------

;; letE test cases-----------------------------------------
(module+ test
  (test (interp (letE 'x (numE 5) (plusE (numE 5) (idE 'x)))
                empty (objV 'Object empty) (numV 0) mt-env)
        (numV 10))
    (test/exn (interp (letE 'x (numE 5) (plusE (numE 5) (idE 'y)))
                empty (objV 'Object empty) (numV 0) mt-env)
       "find: free variable"))
;; -----------------------------------------------------------
;; setE test cases 
(module+ test
  (test (interp-posn (setE posn27 'x (numE 0)))
        (numV 0))
  (test/exn (interp-posn (setE posn27 'z (numE 0)))
            "find: not found: z")
  (test (interp-posn (setE posn531 'z (numE 8)))
        (numV 0)))
;;-----------------------------------------------
(module+ test
  (test (interp (numE 10) 
                empty (objV 'Object empty) (numV 0) mt-env)
        (numV 10))
  (test (interp (plusE (numE 10) (numE 17))
                empty (objV 'Object empty) (numV 0) mt-env)
        (numV 27))
  (test (interp (multE (numE 10) (numE 7))
                empty (objV 'Object empty) (numV 0) mt-env)
        (numV 70))

  (test (interp-posn (newE 'Posn (list (numE 2) (numE 7))))
        (objV 'Posn (list (box (numV 2)) (box (numV 7)))))

  (test (interp-posn (sendE posn27 'mdist (numE 0)))
        (numV 9))
  
  (test (interp-posn (sendE posn27 'addX (numE 10)))
        (numV 12))
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
