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
(define number-elements-from
  (lambda (lst n)
    (if (null? lst) '()
        (cons
         (list n (car lst))
         (number-elements-from (cdr lst) (+ n 1))))))

;number-elements : List -> Listof(List(Int, SchemeVal))
(define number-elements
  (lambda (lst)
    (number-elements-from lst 0)))

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


