#lang plait

(require "class.rkt"
         "inherit.rkt")

(define-type ClassT
  (classT [super-name : Symbol]
          [fields : (Listof (Symbol * Type))]
          [methods : (Listof (Symbol * MethodT))]))

(define-type MethodT
  (methodT [arg-type : Type]
           [result-type : Type]
           [body-expr : ExpI]))



; typechecking for env
(define-type Type-Binding
  (tbind [name : Symbol]
         [type : Type]))

(define-type-alias Type-Env (Listof Type-Binding))


(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define (get-all-field-types class-name t-classes)
  (if (equal? class-name 'Object)
      empty        
      (type-case ClassT (find t-classes class-name)
        [(classT super-name fields methods)
         (append 
          (get-all-field-types super-name t-classes)
          (map snd fields))])))

;; ----------------------------------------

(define (make-find-in-tree class-items)
  (lambda (name class-name t-classes)
    (local [(define t-class (find t-classes class-name))
            (define items (class-items t-class))
            (define super-name 
              (classT-super-name t-class))]
      (if (equal? super-name 'Object)
          (find items name)
          (try (find items name)
               (lambda ()
                 ((make-find-in-tree class-items)
                  name 
                  super-name
                  t-classes)))))))

(define find-field-in-tree
  (make-find-in-tree classT-fields))

(define find-method-in-tree
  (make-find-in-tree classT-methods))

;; ----------------------------------------

(define (is-subclass? name1 name2 t-classes)
  (cond
    [(equal? name1 name2) #t]
    [(equal? name1 'Object) #f]
    [else
     (type-case ClassT (find t-classes name1)
       [(classT super-name fields methods)
        (is-subclass? super-name name2 t-classes)])]))

(define (is-subtype? t1 t2 t-classes)
  (type-case Type t1
    [(objT name1)
     (type-case Type t2 
       [(objT name2)
        (is-subclass? name1 name2 t-classes)]
       [else #f])]
    [else (equal? t1 t2)]))

(module+ test
  (define a-t-class (values 'A (classT 'Object empty empty)))
  (define b-t-class (values 'B (classT 'A empty empty)))

  (test (is-subclass? 'Object 'Object empty)
        #t)
  (test (is-subclass? 'A 'B (list a-t-class b-t-class))
        #f)
  (test (is-subclass? 'B 'A (list a-t-class b-t-class))
        #t)

  (test (is-subtype? (numT) (numT) empty)
        #t)
  (test (is-subtype? (numT) (objT 'Object) empty)
        #f)
  (test (is-subtype? (objT 'Object) (numT) empty)
        #f)
  (test (is-subtype? (objT 'A) (objT 'B) (list a-t-class b-t-class))
        #f)
  (test (is-subtype? (objT 'B) (objT 'A) (list a-t-class b-t-class))
        #t))

;; ----------------------------------------

(define typecheck-expr : (ExpI (Listof (Symbol * ClassT)) Type Type Type-Env -> Type)
  (lambda (expr t-classes this-type arg-type tenv)
    (local [(define (recur expr)
              (typecheck-expr expr t-classes this-type arg-type tenv))
            (define (typecheck-nums l r)
              (type-case Type (recur l)
                [(numT)
                 (type-case Type (recur r)
                   [(numT) (numT)]
                   [else (type-error r "num")])]
                [else (type-error l "num")]))]
      (type-case ExpI expr
        [(numI n) (numT)]
        [(plusI l r) (typecheck-nums l r)]
        [(multI l r) (typecheck-nums l r)]
        [(argI) arg-type]
        [(idEI n) (type-lookup n tenv)]
        [(letEI n t rhs body)
         (local [(define rhs-type (recur rhs))]
           (if (is-subtype? t rhs-type t-classes)
               (typecheck-expr body
                               t-classes
                               this-type
                               arg-type
                               (extend-env
                                (tbind n rhs-type)
                                tenv))
             (type-error rhs (string-append "type" (to-string t)))))]
               ;;(type-error rhs "type")))]
                   
         
        [(thisI) this-type]
        [(newI class-name exprs)
         (local [(define arg-types (map recur exprs))
                 (define field-types
                   (get-all-field-types class-name t-classes))]
           (if (and (= (length arg-types) (length field-types))
                    (foldl (lambda (b r) (and r b))
                           #t
                           (map2 (lambda (t1 t2) 
                                   (is-subtype? t1 t2 t-classes))
                                 arg-types
                                 field-types)))
               (objT class-name)
               (type-error expr "field type mismatch")))]
        [(getI obj-expr field-name)
         (type-case Type (recur obj-expr)
           [(objT class-name)
            (find-field-in-tree field-name
                                class-name
                                t-classes)]
           [else (type-error obj-expr "object")])]
        ; setI
        [(setI obj-expr field-name field-value-expr)
         (local [(define obj-expr-type (recur obj-expr))
                 (define field-val-expr-type (recur field-value-expr))]
           (type-case Type obj-expr-type
             [(objT obj-expr-class-name)
              (local [(define field-type (find-field-in-tree
                                          field-name
                                          obj-expr-class-name
                                          t-classes))]
                (if (is-subtype? field-type field-val-expr-type t-classes)
                    (numT)
                    (type-error field-value-expr "field type mismatch")))]
             
             [else (type-error obj-expr "object")]))]          
        ; castI
        [(castI class-name obj-expr)
         (local [(define obj-expr-type (recur obj-expr))]
           (type-case Type obj-expr-type
             [(objT obj-expr-class-name)
              (if (or (is-subtype? obj-expr-type (objT class-name) t-classes)
                      (is-subtype? (objT class-name) obj-expr-type t-classes))
                  (objT class-name)
                  (type-error obj-expr "object"))]
             [else
              (type-error obj-expr "object")]))]
        ; if0I
        [(if0I tst thn els)
         (local [(define tst-type (recur tst))
                 (define thn-type (recur thn))
                 (define els-type (recur els))]
           ; check whether tst is a number
           (type-case Type tst-type
             [(numT)
              ; call the least upper bound function
              ; need a helper function of some sorts
              ; that takes two types and determines
              ; a third specific type
              (least-upper-bound thn-type els-type t-classes)]
             [else (type-error tst "num")]))]
        
        ; nullI
        [(nullI) (nullT)]    
        [(sendI obj-expr method-name arg-expr)
         (local [(define obj-type (recur obj-expr))
                 (define arg-type (recur arg-expr))]
           (type-case Type obj-type
             [(objT class-name)
              (typecheck-send class-name method-name
                              arg-expr arg-type
                              t-classes)]
             [else
              (type-error obj-expr "object")]))]
        [(superI method-name arg-expr)
         (local [(define arg-type (recur arg-expr))
                 (define this-class
                   (find t-classes (objT-class-name this-type)))]
           (typecheck-send (classT-super-name this-class)
                           method-name
                           arg-expr arg-type
                           t-classes))]))))

(define (typecheck-send [class-name : Symbol]
                        [method-name : Symbol]
                        [arg-expr : ExpI]
                        [arg-type : Type]
                        [t-classes : (Listof (Symbol * ClassT))])
  (type-case MethodT (find-method-in-tree
                      method-name
                      class-name
                      t-classes)
    [(methodT arg-type-m result-type body-expr)
     (if (is-subtype? arg-type arg-type-m t-classes)
         result-type
         (type-error arg-expr (to-string arg-type-m)))]))

; type look up helper function
(define type-lookup
  (make-lookup tbind-name tbind-type))
; least upper bound helper function
(define (least-upper-bound [t1 : Type]
                           [t2 : Type]
                           [t-classes : (Listof (Symbol * ClassT))])
  (type-case Type t1
    [(numT)
     (type-case Type t2
       [(numT) (numT)]
       [else (error 'Typecheck "type mismatch")])]
    [(objT t1-class-name)
     (type-case Type t2
       [(objT t2-class-name)
        (if
         (is-subclass? t1-class-name t2-class-name t-classes)
         t2
         (least-upper-bound t1 (objT (classT-super-name
                                      (find t-classes t2-class-name)))
                            t-classes))]
       [else (error 'Typecheck "type mismatch")])]
    [(nullT)
     (type-case Type t2
       [(nullT) (nullT)]
       [(objT t2-class-name) (nullT)]
       [else (error 'Typecheck "type mismatch")])]))


(define (typecheck-method [method : MethodT]
                          [this-type : Type]
                          [t-classes : (Listof (Symbol * ClassT))]) : ()
  (type-case MethodT method
    [(methodT arg-type result-type body-expr)
     (if (is-subtype? (typecheck-expr body-expr t-classes
                                      this-type arg-type mt-env)
                      result-type
                      t-classes)
         (values)
         (type-error body-expr (to-string result-type)))]))


  

(define (check-override [method-name : Symbol]
                        [method : MethodT]
                        [this-class : ClassT]
                        [t-classes : (Listof (Symbol * ClassT))])
  (local [(define super-name 
            (classT-super-name this-class))
          (define super-method
            (try
             ;; Look for method in superclass:
             (find-method-in-tree method-name
                                  super-name
                                  t-classes)
             ;; no such method in superclass:
             (lambda () method)))]
    (if (and (equal? (methodT-arg-type method)
                     (methodT-arg-type super-method))
             (equal? (methodT-result-type method)
                     (methodT-result-type super-method)))
        (values)
        (error 'typecheck (string-append
                           "bad override of "
                           (to-string method-name))))))

(define (typecheck-class [class-name : Symbol]
                         [t-class : ClassT]
                         [t-classes : (Listof (Symbol * ClassT))])
  (type-case ClassT t-class
    [(classT super-name fields methods)
     (map (lambda (m)
            (begin
              (typecheck-method (snd m) (objT class-name) t-classes)
              (check-override (fst m) (snd m) t-class t-classes)))
          methods)]))

(define (typecheck [a : ExpI]
                   [t-classes : (Listof (Symbol * ClassT))]) : Type
  (begin
    (map (lambda (tc)
           (typecheck-class (fst tc) (snd tc) t-classes))
         t-classes)
    (typecheck-expr a t-classes (objT 'Object) (numT) mt-env)))

;; ----------------------------------------

(module+ test
  (define posn-t-class
    (values 'Posn
            (classT 'Object
                    (list (values 'x (numT)) (values 'y (numT)))
                    (list (values 'mdist
                                  (methodT (numT) (numT) 
                                           (plusI (getI (thisI) 'x) (getI (thisI) 'y))))
                          (values 'addDist
                                  (methodT (objT 'Posn) (numT)
                                           (plusI (sendI (thisI) 'mdist (numI 0))
                                                  (sendI (argI) 'mdist (numI 0)))))))))

  (define posn3D-t-class 
    (values 'Posn3D
            (classT 'Posn
                    (list (values 'z (numT)))
                    (list (values 'mdist
                                  (methodT (numT) (numT)
                                           (plusI (getI (thisI) 'z) 
                                                  (superI 'mdist (argI)))))))))

  (define square-t-class 
    (values 'Square
            (classT 'Object
                    (list (values 'topleft (objT 'Posn)))
                    (list))))

  (define (typecheck-posn a)
    (typecheck a
               (list posn-t-class posn3D-t-class square-t-class)))
  
  (define new-posn27 (newI 'Posn (list (numI 2) (numI 7))))
  (define new-posn531 (newI 'Posn3D (list (numI 5) (numI 3) (numI 1))))

  ;; letI typecheck---------------------------------------------------
    (test (typecheck (letEI 'x (numT) (numI 3)
                            (plusI (numI 2) (idEI 'x)))
                     empty)
          (numT))
  (test/exn (typecheck (letEI 'x (numT) (newI 'Object empty)
                              (plusI (numI 2) (idEI 'x)))
                       empty)
            "no type")

  ;;------------------------------------------------------------------

  ; castI typecheck-----------------------------------------------------
  (test (typecheck-posn (castI 'Posn new-posn531))
        (objT 'Posn))
  (test (typecheck-posn (castI 'Posn3D new-posn27))
        (objT 'Posn3D))
  (test/exn (typecheck-posn (castI 'Posn (plusI (numI 2) (numI 7))))
            "no type")
  (test/exn (typecheck-posn (castI 'Square new-posn27))
            "no type")
  ;;--------------------------------------------------------------------
   ; least-upper-bound test-----------------------------------------------------------------------------
  (test (least-upper-bound (objT 'Posn) (objT 'Posn3D)(list posn3D-t-class posn-t-class square-t-class))
        (objT 'Posn))
  (test (least-upper-bound (objT 'Posn) (objT 'Square)(list posn3D-t-class posn-t-class square-t-class))
        (objT 'Object))
  (test/exn (least-upper-bound (objT 'Posn) (numT)(list posn3D-t-class posn-t-class square-t-class))
            "Typecheck: type mismatch")
  (test (least-upper-bound (numT) (numT)(list posn3D-t-class posn-t-class square-t-class))
        (numT))
  (test/exn (least-upper-bound (numT) (objT 'Posn)(list posn3D-t-class posn-t-class square-t-class))
            "Typecheck: type mismatch")
  (test (least-upper-bound (nullT) (nullT) (list posn3D-t-class posn-t-class square-t-class))
        (nullT))
  (test (least-upper-bound (nullT) (objT 'Posn) (list posn3D-t-class posn-t-class square-t-class))
        (nullT))
  (test/exn (least-upper-bound (nullT) (numT) (list posn3D-t-class posn-t-class square-t-class))
        "Typecheck: type mismatch")
  ;;---------------------------------------------------------------------------------------------------
  

  (test (typecheck-posn (sendI new-posn27 'mdist (numI 0)))
        (numT))
  (test (typecheck-posn (sendI new-posn531 'mdist (numI 0)))
        (numT))  
  (test (typecheck-posn (sendI new-posn531 'addDist new-posn27))
        (numT))  
  (test (typecheck-posn (sendI new-posn27 'addDist new-posn531))
        (numT))

  (test (typecheck-posn (newI 'Square (list (newI 'Posn (list (numI 0) (numI 1))))))
        (objT 'Square))
  (test (typecheck-posn (newI 'Square (list (newI 'Posn3D (list (numI 0) (numI 1) (numI 3))))))
        (objT 'Square))
  
  (test (typecheck (multI (numI 1) (numI 2))
                   empty)
        (numT))

  ; if0I------------------------------------------------------------------------------
  (test (typecheck (if0I (numI 0) (numI 1) (numI 2))
                   empty)
        (numT))
  (test/exn (typecheck-posn (if0I (numI 0) (numI 1) new-posn27))
            "Typecheck: type mismatch")
  
  (test/exn (typecheck (if0I (newI 'Object empty) (numI 1) (numI 2))
                       empty)
            "typecheck: no type: (newI 'Object '()) not num")
  
  (test/exn (typecheck-posn (if0I new-posn27 (numI 1) (numI 2)))
            "typecheck: no type: (newI 'Posn (list (numI 2) (numI 7))) not num")
  (test (typecheck-posn (if0I (numI 0) new-posn27 new-posn531))
        (objT 'Posn))
  (test (typecheck-posn (if0I (numI 1) new-posn27 new-posn531))
        (objT 'Posn))
  (test/exn (typecheck-posn (if0I (numI 0) new-posn27 (numI 1)))
            "Typecheck: type mismatch")
  ;;----------------------------------------------------------------------------------
  ;; setI tests ----------------------------------------------------------------------
  (test/exn (typecheck-posn (setI (numI 0) 'x (numI 27)))
            "typecheck: no type: (numI 0) not object")
  (test (typecheck-posn (setI new-posn27 'x (numI 3)))
        (numT))
  (test/exn (typecheck-posn (setI new-posn27 'x new-posn27))
            "field type mismatch")
        
  ;;-----------------------------------------------------------------------------------
  ;; nullI tests
  (test (typecheck (nullI)
                   empty)
        (nullT))
  (test/exn (typecheck-posn (sendI (numI 10) 'mdist (nullI)))
            "no type")
  (test/exn (typecheck (plusI (numI 1) (nullI))
                       empty)
            "no type")
  ;;----------------------------------------------------------------------------------
  

  (test/exn (typecheck-posn (sendI (numI 10) 'mdist (numI 0)))
            "no type")
  (test/exn (typecheck-posn (sendI new-posn27 'mdist new-posn27))
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'Object empty))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (newI 'Object empty) (numI 1))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'Object (list (numI 1))))
                       empty)
            "no type")
  (test/exn (typecheck (getI (numI 1) 'x)
                       empty)
            "no type")
  (test/exn (typecheck (numI 10)
                       (list posn-t-class
                             (values 'Other
                                     (classT 'Posn
                                             (list)
                                             (list (values 'mdist
                                                           (methodT (objT 'Object) (numT)
                                                                    (numI 10))))))))
            "bad override")
  (test/exn (typecheck-method (methodT (numT) (objT 'Object) (numI 0)) (objT 'Object) empty)
            "no type")
  (test/exn (typecheck (numI 0)
                       (list square-t-class
                             (values 'Cube
                                     (classT 'Square
                                             empty
                                             (list
                                              (values 'm
                                                      (methodT (numT) (numT)
                                                               ;; No such method in superclass:
                                                               (superI 'm (numI 0)))))))))
            "not found"))

;; ----------------------------------------

(define strip-types : (ClassT -> ClassI)
  (lambda (t-class)
    (type-case ClassT t-class
      [(classT super-name fields methods)
       (classI
        super-name
        (map fst fields)
        (map (lambda (m)
               (values (fst m)
                       (type-case MethodT (snd m)
                         [(methodT arg-type result-type body-expr)
                          body-expr])))
             methods))])))
  
(define interp-t : (ExpI (Listof (Symbol * ClassT)) -> Value)
  (lambda (a t-classes)
    (interp-i a
              (map (lambda (c)
                     (values (fst c) (strip-types (snd c))))
                   t-classes))))

(module+ test
  (define (interp-t-posn a)
    (interp-t a
              (list posn-t-class posn3D-t-class)))
  
  (test (interp-t-posn (sendI new-posn27 'mdist (numI 0)))
        (numV 9))  
  (test (interp-t-posn (sendI new-posn531 'mdist (numI 0)))
        (numV 9))
  (test (interp-t-posn (sendI new-posn531 'addDist new-posn27))
        (numV 18))
  (test (interp-t-posn (sendI new-posn27 'addDist new-posn531))
        (numV 18)))
