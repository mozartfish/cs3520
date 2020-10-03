#lang plait

(define-type Tree
  (leaf [val : Number])
  (node [val : Number]
        [left : Tree]
        [right : Tree]))

(define (sum [t : Tree]) : Number
  (type-case Tree t
    [(leaf v) v]
    [(node v l r)
     (+ v (+ (sum l) (sum r)))]))

(test (sum (leaf 8)) 8)
(test (sum (node 5 (leaf 6) (leaf 7))) 18)

(define (negate [t : Tree]) : Tree
  (type-case Tree t
    [(leaf v) (leaf (- 0 v))]
    [(node v l r)
     (node (- 0 v) 
           (negate l)
           (negate r))]))

(test (negate (leaf 10)) (leaf -10))
(test (negate (node 5 (leaf 6) (leaf 7)))
      (node -5 (leaf -6) (leaf -7)))

(define (contains? [t : Tree] [n : Number]) : Boolean
  (type-case Tree t
    [(leaf v) (= v n)]
    [(node v l r)
     (or (= v n)
         (contains? l n)
         (contains? r n))]))

(test (contains? (leaf 10) 10) #t)
(test (contains? (leaf 10) 11) #f)
(test (contains? (node 5 (leaf 6) (leaf 7)) 6) #t)
(test (contains? (node 5 (leaf 6) (leaf 7)) 16) #f)

;; Determines whether every leaf in `t' has
;; a value that is greater than `n' plus the
;; sum of nodes to the leaf.
(define (bigger-leaves? [t : Tree] [n : Number]) : Boolean
  (type-case Tree t
    [(leaf v) (> v n)]
    [(node v l r)
     (and (bigger-leaves? l (+ v n))
          (bigger-leaves? r (+ v n)))]))

(test (bigger-leaves? (leaf 7) 6) #t)
(test (bigger-leaves? (leaf 7) 8) #f)
(test (bigger-leaves? (node 5 (leaf 6) (leaf 7)) 0) #t)
(test (bigger-leaves? (node 5 (leaf 6) (leaf 7)) 1) #f)

(define (big-leaves? [t : Tree]) : Boolean
  (bigger-leaves? t 0))

(test (big-leaves? (node 5 (leaf 6) (leaf 7))) #t)
(test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7))) #f)


(define (positive-trees? lst)
  (type-case (Listof Tree) lst
    [empty #t]
    [(cons t lst)
     (and (> (sum t) 0)
          (positive-trees? lst))]))

(test (positive-trees? empty)
      #t)
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


(define (flatten-onto [t : Tree] [lst : (Listof Number)]) : (Listof Number)
  (type-case Tree t
    [(leaf v) (cons v lst)]
    [(node v l r)
     (flatten-onto l
                   (cons v
                         (flatten-onto r lst)))]))

(define (flatten [t : Tree]) : (Listof Number)
  (flatten-onto t empty))

(test (flatten (leaf 7)) '(7))
(test (flatten (node 5 (leaf 6) (leaf 7))) '(6 5 7))
(test (flatten (node 0
                     (node 5 (leaf 6) (leaf 7))
                     (node -5 (leaf -6) (leaf -7))))
      '(6 5 7 0 -6 -5 -7))
