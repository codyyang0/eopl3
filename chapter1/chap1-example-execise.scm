#lang eopl

(define in-S?
  (lambda (n)
    (if (zero? n) #t
        (if (>= (- n 3) 0)
            (in-S? (- n 3))
            #f))))

;list-length : List -> Int
;usage: (list-length l) = the length of l
(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (list-length (cdr lst))))))

;nth-element : List * Int -> SchemeVal
;usage: (nth-element lst n) = the n-th element of lst
(define nth-element
  (lambda (lst n)
    (nth-element-i lst n '() 0)))

(define nth-element-i
  (lambda (lst n r-lst i)
    (if (null? lst)
        (report-list-too-short r-lst i)
        (if (zero? n)
            (car lst)
            (nth-element-i (cdr lst) (- n 1) (cons (car lst) r-lst) (+ i 1))))))

(define report-list-too-short
  (lambda (lst n)
    (eopl:error 'nth-element
                "~s does not have ~s elements.~%" (reverse lst) (+ n 1))))


;remove-first : Sym * Listof(Sym) -> Listof(Sym)
;usage: (remove-first s los) returns a list with
;the same elements arranged the same order as los,
;except that the first occurence of the symbol s is removed.
(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))

;remove-elements-before : Sym * Listof(Sym) -> Listof(Sym)
(define remove-elements-before
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (remove-first s (cdr los))))))

;remove : Sym * Listof(sym) -> Listof(Sym)
;removes all occurrences of a given symbol from a list of symbols
(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? s (car los))
            (remove s (cdr los))
            (cons (car los) (remove s (cdr los)))))))

(define identifier?
  (lambda (var)
    (if (not (symbol? var))
        #f
        (if (eqv? 'lambda var)
            #f
            #t))))

;occur-free? Sym * LcExp -> Bool
;usage: returns #t if the symbol var occurs free
;in exp, otherwise returns #f.
(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      ((eqv? (car exp) 'lambda)
       (and
        (not (eqv? var (car (cadr exp))))
        (occurs-free? var (caddr exp))))
      (else
       (or
        (occurs-free? var (car exp))
        (occurs-free? var (cadr exp)))))))
       
;subst : Sym * Sym * S-list -> S-list
;(define subst
;  (lambda (new old slist)
;    (if (null? slist)
;        '()
;        (cons
;         ((lambda (new old sexp)
;           (if (symbol? sexp)
;               (if (eqv? old sexp) new sexp)
;               (subst new old sexp)))
;          new old (car slist))
;         (subst new old (cdr slist))))))

; Exercise 1.13
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (map
         (lambda (sexp)
           (if (symbol? sexp)
               (if (eqv? old sexp) new sexp)
               (subst new old sexp)))
         slist))))
            
;number-elements-from : Listof(schemeVal) * Int -> Listof(List(Int,SchemeVal))
;usage: (number-elements-from ’(v0 v1 v2 ...) n) = ((n v0) (n+1 v1) (n+2 v2) ...)
;(define number-elements-from
;  (lambda (lst n)
;    (if (null? lst) '()
;        (cons
;         (list n (car lst))
;         (number-elements-from (cdr lst) (+ n 1))))))

;number-elements : List -> Listof(List(Int, SchemeVal))
;(define number-elements
;  (lambda (lst)
;    (number-elements-from lst 0)))

;list-sum : Listof(Int) -> Int
(define list-sum
  (lambda (loi)
    (if (null? loi)
        0
        (+ (car loi)
           (list-sum (cdr loi))))))

;partial-vector-sum : Vectorof(Int) * Int -> Int
;usage: if 0 <= n < length(v)
(define partitial-vector-sum
  (lambda (v n)
    (if (zero? n)
        (vector-ref v 0)
        (+ (vector-ref v n)
           (partitial-vector-sum v (- n 1))))))

;vector-sum : Vectorof(Int) -> Int
(define vector-sum
  (lambda (v)
    (let ((n (vector-length v)))
      (if (zero? n)
          0
          (partitial-vector-sum v (- n 1))))))

; 1.4 Section Exercises

;Exercise 1.15 [*] (duple n x) returns a list containing n copies of x
;duple : Int * SchemeVal -> Listof(SchemeVal)
(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        (cons x (duple (- n 1) x)))))

;Exercise 1.16 [*] (invert lst), where lst is a list of 2-lists (lists of length two),
;returns a list with each 2-list reversed.
;invert : Listof(Listof(s1 s2)) -> Listof(Listof(s2 s1))
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons
         (list (car (cdar lst))
               (caar lst))
         (invert (cdr lst))))))

;Exercise 1.17 [*] (down lst) wraps parentheses around each top-level element of lst.
;down : Listof(schemeval) -> Listof(Listof(schemeval))
(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons
         (list (car lst))
         (down (cdr lst))))))
  

;Exercise 1.18 [*] (swapper s1 s2 slist) returns a list the same as slist, but
;with all occurrences of s1 replaced by s2 and all occurrences of s2 replaced by s1.
;swapper : Sym * Sym * slist -> slist
(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (cons
         (swapper-elements-in-sexp s1 s2 (car slist))
         (swapper s1 s2 (cdr slist))))))

(define swapper-elements-in-sexp
  (lambda (s1 s2 sexp)
    (if (symbol? sexp)
        (if (eqv? s1 sexp) s2
            (if (eqv? s2 sexp) s1 sexp))
        (swapper s1 s2 sexp))))


;Exercise 1.19 [**](list-set lst n x) returns a list like lst, except that the n-th
;element, using zero-based indexing, is x.
;list-set : Listof(Sym) * Int * SchemeVal -> Listof(SchemeVal)
(define list-set
  (lambda (lst n x)
    (cond
      ((null? lst) '())
      ((zero? n) (cons x (cdr lst)))
      (else
       (cons (car lst)
             (list-set (cdr lst) (- n 1) x))))))

;Exercise 1.20 [*] (count-occurrences s slist) returns the number of
;occurrences of s in slist
;count-occurrences : Sym * Slist -> Int
(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (+ (count-occurrences-s-in-sexp s (car slist))
           (count-occurrences s (cdr slist))))))

(define count-occurrences-s-in-sexp
  (lambda (s sexp)
    (if (symbol? sexp)
        (if (eqv? s sexp) 1 0)
        (count-occurrences s sexp))))

;Exercise 1.21 [**] (product sos1 sos2), where sos1 and sos2 are each a list
;of symbols without repetitions, returns a list of 2-lists that represents the
;Cartesian product of sos1 and sos2. The 2-lists may appear in any order.
;product : Listof(Sym) * Listof(Sym) -> Listof(Sym)
(define product
  (lambda (los1 los2)
    (if (null? los1)
        '()
        (append
         (product-s-los (car los1) los2)
         (product (cdr los1) los2)))))

(define product-s-los
  (lambda (s los)
    (if (null? los)
        '()
        (cons
         (list s (car los))
         (product-s-los s (cdr los))))))

;Exercise 1.22 [**] (filter-in pred lst) returns the list of thoese elements in
;lst that satisfy the predicate pred.
;filter-in : pred * Listof(schemeVal) -> Listof(schemeVal)
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
        '()
        (if (pred (car lst))
            (cons (car lst) (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst))))))
        
;Exercise 1.23 [**] (list-index pred lst) returns the 0-based position of the
;first element of lst that satisfies the predicate pred. If no element of lst satisfies
;the predicate, then list-index returns #f.
;list-index : pred * Listof(schemeVal) -> Int
(define list-index
  (lambda (pred lst)
    (if (null? lst)
        0
        (if (pred (car lst))
            0
            (+ 1 (list-index pred (cdr lst)))))))

;Exercise 1.24 [**] (every? pred lst) returns #f if any element of lst fails to
;satisfy pred, and returns #t otherwise.
;every? : pred * Listof(symbol) -> boolean
(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (if (pred (car lst))
            (every? pred (cdr lst))
            #f))))
            
;Exercise 1.25 [**] (exists? pred lst) returns #t if any element of lst satisfies
;pred, and returns #f otherwise.
;exists? : pred * Listof(symbol) -> boolean
(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (if (pred lst)
            #t
            (exists? pred (cdr lst))))))

;Exercise 1.26 [**] (up lst) removes a pair of parentheses from each top-level element
;of lst.If a top-level element is not a list, it is included in the result, as is.
;The value of (up (down lst)) is equivalent to lst, but (down (up lst) is not neccessarily
;lst.(See exercise 1.17.)
;up : ListOf(schemeVal) -> ListOf(schemeVal)
(define up
  (lambda (lst)
    (if (null? lst)
        '()
        (if (not (pair? (car lst)))
            (cons (car lst) (up (cdr lst)))
            (append (car lst) (up (cdr lst)))))))

;Exercise 1.27 [**] (flatten slist) returns a list of the symbols contained in
;slist in the order in which they occur when slist is printed. Intuitively, flatten
;removes all the inner parentheses from its argument.
;flatten : Slist -> Listof(schemeVal)
(define flatten
  (lambda (slist)
    (if (null? slist)
        '()
        (append
         (flatten-in-sexp (car slist))
         (flatten (cdr slist))))))

(define flatten-in-sexp
  (lambda (sexp)
    (if (symbol? sexp)
        (list sexp)
        (flatten sexp))))

;Exercise 1.28 [**] (merge loi1 loi2), where loi1 and loi2 are lists of integers
;that are sorted in ascending order, returns a sorted list of all the integers in
;loi1 and loi2
;merge : Listof(Int) * Listof(Int) -> Listof(Int)
(define merge
  (lambda (loi1 loi2)
    (if (null? loi1)
        loi2
        (merge (cdr loi1) (merge-i-in-loi (car loi1) loi2)))))

(define merge-i-in-loi
  (lambda (i loi)
    (if (null? loi)
        (list i)
        (if (> i (car loi))
            (cons (car loi) (merge-i-in-loi i (cdr loi)))
            (cons i loi)))))

;Exercise 1.29 [**] (sort loi) returns a list of the elements of loi in ascending order.
;sort : Listof(Int) -> Listof(Int)
(define sort
  (lambda (loi)
    (if (null? loi)
        '()
        (cons (min (car loi) (cdr loi))
              (sort (sort-remain (car loi) (cdr loi)))))))     

(define min
  (lambda (i loi)
    (if (null? loi)
        i
        (if (< i (car loi))
            (min i (cdr loi))
            (min (car loi) (cdr loi))))))

(define sort-remain
  (lambda (i loi)
    (if (null? loi)
        '()
        (if (< i (car loi))
            (cons (car loi) (sort-remain i (cdr loi)))
            (cons i (sort-remain (car loi) (cdr loi)))))))

;Exercise 1.30 [**] (sort/predicate pred loi) returns a list of elements sorted
;by the predicate
;sort : pred Listof(Int) -> Listof(Int)
(define sort/predicate
  (lambda (pred loi)
    (if (null? loi)
        '()
        (cons (pred-element-in-loi pred (car loi) (cdr loi))
              (sort/predicate pred (pred-remain-elements pred (car loi) (cdr loi)))))))

(define pred-element-in-loi
  (lambda (pred i loi)
    (if (null? loi)
        i
        (if (pred i (car loi))
            (pred-element-in-loi pred i (cdr loi))
            (pred-element-in-loi pred (car loi) (cdr loi))))))

(define pred-remain-elements
    (lambda (pred i loi)
    (if (null? loi)
        '()
        (if (pred i (car loi))
            (cons (car loi) (pred-remain-elements pred i (cdr loi)))
            (cons i (pred-remain-elements pred (car loi) (cdr loi)))))))

;Exercise 1.31 [*] Write the following procedures for calculating on a bintree
;(definition 1.17):leaf and interior-node, which build bintrees, leaf?, which tests
;whether a bintree is a leaf, and lson, rson, and contents-of, which extract the
;components of a node. contents-of should work on both leaves and interior nodes.
;leaf : Int -> leaf
(define leaf
  (lambda (i) i))

;interior-node : Sym * Bintree * Bintree -> Bintree
(define interior-node
  (lambda (s bt1 bt2)
    (list s bt1 bt2)))

;leaf? : bintree -> boolean
(define leaf?
  (lambda (bintree)
    (if (number? bintree) #t #f)))

;lson : interior-node -> bintree
(define lson
  (lambda (inode) (cadr inode)))

;rson : interior-node -> bintree
(define rson
  (lambda (inode) (caddr inode)))

;contents-of : bintree -> Int | Symbol
(define contents-of
  (lambda (bintree)
    (if (leaf? bintree)
        bintree
        (car bintree))))

;Exercise 1.32 [*] Write a procedure double-tree that takes a bintree, as represented
;in definition 1.17, and produres another bintree like the original, but with all the
;integers in the leaves doubled.
;double-tree : bintree -> bintree
(define double-tree
  (lambda (bintree)
    (if (leaf? bintree)
        (* 2 (contents-of bintree))
        (interior-node
         (contents-of bintree)
         (double-tree (lson bintree))
         (double-tree (rson bintree))))))

;Exercise 1.33 [**] Write a procedure mark-leaves-with-red-depth that takes a bintree
;(definition 1.1.7), and produces a bintree of the same shape as the original,
;except that in the new tree, each leaf contains the integer of nodes between it and the
;root that contain the symbol red. 
;mark-leaves-with-red-depth : bintree -> bintree
(define mark-leaves-with-red-depth
  (lambda (bintree)
    (mark-leaves-with-color-depth 'red bintree 0)))

;bintree-color-depth : Sym * bintree * Int -> Int
(define bintree-color-depth
  (lambda (color bintree depth)
    (if (leaf? (contents-of bintree))
        depth
        (if (eqv? (contents-of bintree) color)
            (+ depth 1)
            depth))))

;mark-leaves-with-color-depth: Sym * bintree * Int -> bintree
(define mark-leaves-with-color-depth
  (lambda (color bintree depth)
    (if (leaf? bintree)
        (leaf depth)
        (interior-node
         (contents-of bintree)
         (mark-leaves-with-color-depth
          color
          (lson bintree)
          (bintree-color-depth color bintree depth))
         (mark-leaves-with-color-depth
          color
          (rson bintree)
          (bintree-color-depth color bintree depth))))))
     
;Exercise 1.34 [***] Write a procedure path that takes an integer n and a binary
;search tree bst (page 10) that contains the integer n, and returns a list of leafs
;and rights showing how to find the node containing n. If n is found at the root, it returns
;the empty list.
;path : bst * Int -> ListOf(Sym)
(define path
  (lambda (i bst)
    (if (null? bst)
        '()
        (path-signal bst '() i))))


;path-signal : bst * ListOf(Sym) * Int -> ListOf(sym)
(define path-signal
  (lambda (bst lst i)
    (cond
      ((= i (car bst)) lst)
      ((> i (car bst)) (path-signal (caddr bst) (append lst (list 'right)) i))
      (else
       (path-signal (cadr bst) (append lst (list 'left)) i)))))
        
;Exercise 1.35 [***] Write a procedure number-leaves that takes a bintree, and produces
;a bintree like the original, except the contents of the leaves are numbered
;starting from 0.
; number-leaves : bintree -> bintree
(define number-leaves
  (lambda (bintree)
    (if (null? bintree)
        '()
        (leaf-with-number bintree 0))))

; leaf-number : bintree * Int -> Int
(define leaf-number
  (lambda (bintree i)
    (if (leaf? bintree)
        (+ i 1)
        (+ (leaf-number (lson bintree) i)
           (leaf-number (rson bintree) 0)))))
         
; leaf-number : bintree * Int -> bintree
(define leaf-with-number
  (lambda (bintree i)
    (if (leaf? bintree)
        (leaf i)
        (interior-node
         (contents-of bintree)
         (leaf-with-number (lson bintree) i)
         (leaf-with-number (rson bintree) (leaf-number (lson bintree) i))))))
         
;Exercise 1.36 [***] Write a procedure g such that number-elements from page 23
;could be defined as
(define number-elements
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))

; g : lst * lst -> lst
(define g
  (lambda (pre-lst lst)
    (if (null? lst)
        (list pre-lst)
        (cons
          pre-lst
          (map (lambda (p)
                 (cons (+ 1 (car p))
                       (cdr p)))
               lst)))))
                 
        



















