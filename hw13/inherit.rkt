#lang plait

;; Make all "class.rkt" definitions available here, where
;; the "class.rkt" file must be in the same directory
;; as this one:
(require "class.rkt")

(define-type ExpI
  (numI [n : Number])
  (plusI [lhs : ExpI]
         [rhs : ExpI])
  (multI [lhs : ExpI]
         [rhs : ExpI])
  (argI)
  (idEI [name : Symbol])
  (letEI [name : Symbol]
         [type : Type]
         [rhs : ExpI]
         [body : ExpI])
  (newArrayI [type : Type]
             [size : ExpI]
             [init : ExpI])
  ;;(arrayRefI [arr : ExpI]
  ;;           [index : ExpI])
  ;;(arraySetI [arr : ExpI]
  ;;           [index : ExpI]
   ;;          [value : ExpI])
  (thisI)
  (newI [class-name : Symbol]
        [args : (Listof ExpI)])
  (getI [obj-expr : ExpI]
        [field-name : Symbol])
  (setI [obj-expr : ExpI]
        [field-name : Symbol]
        [field-value : ExpI])
  (sendI [obj-expr : ExpI]
         [method-name : Symbol]
         [arg-expr : ExpI])
  (superI [method-name : Symbol]
          [arg-expr : ExpI])
  (castI [class-name : Symbol]
         [obj-expr : ExpI])
  (if0I [tst : ExpI]
        [thn : ExpI]
        [els : ExpI])
  (nullI))

(define-type ClassI
  (classI [super-name : Symbol]
          [field-names : (Listof Symbol)]
          [methods : (Listof (Symbol * ExpI))]))

(define-type Type
  (numT)
  (objT [class-name : Symbol])
  (nullT))


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
      ; newArrayI
      [(newArrayI type size init)
       (newArrayE (recur size) (recur init))]
      ; letEI
      [(letEI name type rhs body) (letE name (recur rhs) (recur body))]
      [(idEI name) (idE name)]
      ; castI
      [(castI class-name obj-expr)
       (castE class-name (recur obj-expr))]
      ; if0I
      [(if0I tst thn els)
       (if0E (recur tst) (recur thn) (recur els))]
      ; nullI
      [(nullI) (nullE)]
      [(argI) (argE)]
      [(thisI) (thisE)]
      [(newI class-name field-exprs)
       (newE class-name (map recur field-exprs))]
      [(getI expr field-name)
       (getE (recur expr) field-name)]
      ; setI
      [(setI expr field-name field-value)
       (setE (recur expr) field-name (recur field-value))]
      [(sendI expr method-name arg-expr)
       (sendE (recur expr)
              method-name
              (recur arg-expr))]
      [(superI method-name arg-expr)
       (ssendE (thisE)
               super-name
               method-name
               (recur arg-expr))])))

(module+ test
  (test (exp-i->c (numI 10) 'Object)
        (numE 10))
  ; test cases for idEI
  (test (exp-i->c (idEI 'x) 'Object)
        (idE 'x))
  ; test cases for letI----------------------------------------------------------
  (test (exp-i->c (letEI 'x (numT) (numI 10) (plusI (numI 10) (idEI 'x))) 'Object)
        (letE 'x (numE 10) (plusE (numE 10) (idE 'x))))
(test (exp-i->c (letEI 'x (objT 'Posn) (numI 10) (plusI (numI 10) (idEI 'x))) 'Object)
        (letE 'x (numE 10) (plusE (numE 10) (idE 'x))))
  ;;---------------------------------------------------------------------------------
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
  ; test cases for setI
  (test (exp-i->c (setI (newI 'Object (list (numI 1))) 'x (numI 5)) 'Object)
        (setE (newE 'Object (list (numE 1))) 'x (numE 5)))
  
  (test (exp-i->c (sendI (numI 1) 'mdist (numI 2)) 'Object)
        (sendE (numE 1) 'mdist (numE 2)))
  (test (exp-i->c (superI 'mdist (numI 2)) 'Posn)
        (ssendE (thisE) 'Posn 'mdist (numE 2)))
  
  ; test cases for castI
  (test (exp-i->c (castI 'Posn (newI 'Posn (list (numI 2) (numI 7)))) 'Posn)
        (castE 'Posn (newE 'Posn (list (numE 2) (numE 7)))))
  ; test cases for if0I
  (test (exp-i->c (if0I (numI 0) (numI 5) (numI 7)) 'Object)
        (if0E (numE 0) (numE 5) (numE 7)))
  ; test cases for nullI
  (test (exp-i->c (nullI) 'Object)
        (nullE)))
  

;; ----------------------------------------

(define (class-i->c-not-flat [c : ClassI]) : Class
  (type-case ClassI c
    [(classI super-name field-names methods)
     (classC
      super-name
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
            (classC
             'Posn
             (list 'z)
                    (list posn3d-mdist-c-method))))

  (test (class-i->c-not-flat (snd posn3d-i-class))
        (snd posn3d-c-class-not-flat)))

;; ----------------------------------------

(define (flatten-class [name : Symbol]
                       [classes-not-flat : (Listof (Symbol * Class))] 
                       [i-classes : (Listof (Symbol * ClassI))]) : Class
  (type-case Class (find classes-not-flat name)
    [(classC super-name field-names methods)
     (type-case Class (flatten-super name classes-not-flat i-classes)
       [(classC super-super-name super-field-names super-methods)
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
         (classC 'Object empty empty)
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
    (classC
     'Posn
     (list 'x 'y)
            (list (values 'mdist
                          (plusE (getE (thisE) 'x)
                                 (getE (thisE) 'y)))
                  addDist-c-method))))
  (define posn3d-c-class
    (values 'Posn3D
            (classC
             'Posn
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
    (interp a classes (objV 'Object empty) (numV 0) mt-env)))

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
