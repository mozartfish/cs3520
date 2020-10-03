#lang plait

; name: Pranav Rajan
; uID: u1136324
; date: 09/06/20


; HTDP Steps
; 1) Representation
; 2) Examples (Tests)
; 3) Template
; 4) Body
; 5) Run Tests


; Tree definition from Professor Flatt
(define-type Tree
  (leaf [val : Number])
  (node [val : Number]
        [left : Tree]
        [right : Tree]))

; Sum

; Representation
; sum : (Tree -> Number)

; Examples
(module+ test
  (test (sum (node 5 (leaf 6) (leaf 7))) 18)
  )

; Template
#;(define (sum [t : Tree]) : Number
    (type-case Tree t
      [(leaf v)...v]
      ; l and r are trees from the definition
      [(node v l r)...v...l...r]))

; Body
(define (sum [t : Tree]) : Number
  (type-case Tree t
    [(leaf v) v]
    [(node v l r) (+ v (+ (sum l) (sum r)))]))


; Negate

; Representation
; negate : (Tree -> Tree)

; Examples
(module+ test
  (test (negate (node 5 (leaf 6) (leaf 7))) (node -5 (leaf -6) (leaf -7))))

; Template
#;(define (negate [t : Tree]) : Tree
    (type-case Tree t
      [(leaf v)...v]
      ; l and r are trees from the definition
      [(node v l r)...v...l...r]))

; Body
(define (negate [t : Tree]) : Tree
  (type-case Tree t
    [(leaf v) (leaf (* -1 v))]
    [(node v l r) (node (* -1 v) (negate l) (negate r))]))

; Contains

; Representation
; contains? : (Tree -> Boolean)

; Examples
(module+ test
  (test (contains? (node 5 (leaf 6) (leaf 7)) 6) #t)
  (test (contains? (node 5 (leaf 6) (leaf 7)) 0) #f))

; Template
; the number n is a case of the along for the ride argument pattern
#;(define (contains? [t : Tree] [n : Number]) : Boolean
    (type-case Tree t
      [(leaf v)...v]
      [(node v l r)...v...l...r... n]))

; Body
(define (contains? [t : Tree] [n : Number]) : Boolean
  (type-case Tree t
    [(leaf v)
     (eq? n v)]
    [(node v l r)
     (or (eq? v n)
         (or (contains? l n) (contains? r n)))]))


; Bigger Leaves

; Representation
; bigger-leaves : (Tree, Number -> Boolean)

; Examples
(module+ test
  (test (bigger-leaves? (node 5 (leaf 6) (leaf 7)) 0) #t)
  (test (bigger-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7)) 0) #f))

; Template
#; (define (bigger-leaves? [t : Tree] [sum : Number]) : Boolean
     (type-case Tree t
       [(leaf v)...v...sum]
       [(node v l r)...l...v...sum....r...v...sum]))

; Body
(define (bigger-leaves? [t : Tree] [sum : Number]) : Boolean
  (type-case Tree t
    [(leaf v) (> v sum)]
    [(node v l r) (and
                   (bigger-leaves? l (+ v sum)) (bigger-leaves? r (+ v sum)))]))

; Big Leaves

; Representation
; big-leaves : (Tree -> Boolean)

; Examples
(module+ test
  (test (big-leaves? (node 5 (leaf 6) (leaf 7))) #t)
  (test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7))) #f)
  (test (big-leaves? (leaf 4)) #t))

; Template
#;(define (big-leaves? [t : Tree]) : Boolean
    (type-case Tree t
      [(leaf v)...(bigger-leaves?...v...0)]
      [(node v l r)...(bigger-leaves?...l...0)...(bigger-leaves?...r....0)]))

;Body
(define (big-leaves? [t : Tree]) : Boolean
  (type-case Tree t
    [(leaf v) (bigger-leaves? t 0)]
    [(node v l r) (and
                   (bigger-leaves? l v) (bigger-leaves? r v))]))

; Postive Trees

; Representation
; postive-trees : ((listof Trees) -> Boolean)

; Examples
(module+ test
  (test (positive-trees? (cons (leaf 6)
                               empty))
        #t)

  (test (positive-trees? (cons (leaf -6)
                               empty))
        #f)

  (test (positive-trees? (cons (node 1 (leaf 6) (leaf -6))
                               empty))
        #t)

  (test (positive-trees? (cons (node 1 (leaf 6) (leaf -6))
                               (cons (node 0 (leaf 0) (leaf 1))
                                     empty)))
        #t)

  (test (positive-trees? (cons (node -1 (leaf 6) (leaf -6))
                               (cons (node 0 (leaf 0) (leaf 1))
                                     empty)))
        #f)
  )

; Template
#;(define (positive-trees? [trees : Listof Trees]) : Boolean
    (type-case (Listof Trees) t
      [(empty)...]
      [(cons n rst-t)...]))

; Body
(define (positive-trees? [trees : (Listof Tree)]) : Boolean
  (type-case (Listof Tree) trees
    [empty #t]
    [(cons n rst-trees)
     (if (< (sum n) 0)
         #f
         (positive-trees? rst-trees))]))
