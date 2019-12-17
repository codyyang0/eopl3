(module sec2.1 (lib "eopl.ss" "eopl")

  (require "utils.scm")

  (let ()
    ;; Unary Representation
    ;; page 33
    (define zero (lambda () '()))
    (define is-zero? (lambda (n) (null? n)))
    (define successor (lambda (n) (cons #t n)))
    (define predecessor (lambda (n) (cdr n)))

    ;; Need this style of definition to define a recursive function
    ;; inside a let, sorry.
    (define (plus x y)
      (if (is-zero? x)
        y
        (successor (plus (predecessor x) y))))

    (define (scheme-int->my-int n)
      (if (zero? n) (zero)
        (successor (scheme-int->my-int (- n 1)))))

    (define (my-int->scheme-int x)
      (if (is-zero? x) 0
        (+ 1 (my-int->scheme-int (predecessor x)))))

    (equal?? 
      (my-int->scheme-int
        (plus 
          (scheme-int->my-int 3)
          (scheme-int->my-int 7)))
      10)

    (report-unit-tests-completed 'unary-representation)
    )

  (let ()
    ;; Scheme number representation
    ;; page 33
    (define zero (lambda () 0))
    (define is-zero? (lambda (n) (zero? n)))
    (define successor (lambda (n) (+ n 1)))
    (define predecessor (lambda (n) (- n 1)))

    (define (plus x y)
      (if (is-zero? x)
        y
        (successor (plus (predecessor x) y))))

    (equal?? (plus 3 7) 10)

    (report-unit-tests-completed 'scheme-number-representation)

    )

  (let ()
    ;; Reverse-number representation
    ;; Represent n by the Scheme number 5-n
    (define zero (lambda () 5))
    (define is-zero? (lambda (n) (= n 5)))
    (define successor (lambda (n) (- n 5)))
    (define predecessor (lambda (n) (+ n 5)))

    ;; unchanged below here!

    (define plus
      (lambda (x y)
        (if (is-zero? x)
          y
          (successor (plus (predecessor x) y)))))

    (define (scheme-int->my-int n)
        (if (zero? n) (zero)
          (successor (scheme-int->my-int (- n 1)))))

    (define (my-int->scheme-int x)
        (if (is-zero? x) 0
          (+ 1 (my-int->scheme-int (predecessor x)))))

    (equal?? 
      (my-int->scheme-int
        (plus 
          (scheme-int->my-int 3)
          (scheme-int->my-int 7)))
      10)

    (report-unit-tests-completed 'reverse-number-representation)
    )

  (let ()
    ;; Bignum representation
    ;; Page 34 Exercise 2.1
    (define zero (lambda () '()))
    (define is-zero? null?)
    (define successor
      (lambda (n)
        (cond
          ((null? n) (list 1))
          ((equal? 99999999 (car n)) (cons 0 (successor (cdr n))))
          (else (cons (+ (car n) 1) (cdr n))))))
   
    (define predecessor
      (lambda (n)
        (cond
          ((null? n) (list -1))
          ((equal? '(1) n) '())
          ((equal? 0 (car n)) (cons 99999999 (predecessor (cdr n))))
          (else
           (cons (- (car n) 1) (cdr n))))))

        ;; unchanged below here!

    (define plus
      (lambda (x y)
        (if (is-zero? x)
          y
          (plus (predecessor x) (successor y)))))

    (define (scheme-int->my-int n)
        (if (zero? n) (zero)
          (successor (scheme-int->my-int (- n 1)))))

    (define (my-int->scheme-int x)
        (if (is-zero? x) 0
          (+ 1 (my-int->scheme-int (predecessor x)))))

    (equal?? 
      (my-int->scheme-int
        (plus 
          (scheme-int->my-int 3)
          (scheme-int->my-int 7)))
      10)

    (define multi
      (lambda (x y)
        (mult-aux (zero) x y)))

    (define mult-aux
      (lambda (a x y)
        (if (equal? y (successor (zero)))
            (plus a x)
            (mult-aux (plus a x) x (predecessor y)))))
    
    (equal??
     (my-int->scheme-int
      (multi
       (scheme-int->my-int 10)
       (scheme-int->my-int 10)))
     100)

    (define fact
      (lambda (x)
        (if (equal? x (successor (zero)))
            x
            (multi x (fact (predecessor x))))))
    
;    (equal??
;     (my-int->scheme-int
;      (fact
;       (scheme-int->my-int 10)))
;     3628800)
    
    (report-unit-tests-completed 'bignum-representation))

  )

