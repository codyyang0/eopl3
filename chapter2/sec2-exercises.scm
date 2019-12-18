(module sec2-exe (lib "eopl.ss" "eopl")
  
  (require "utils.scm")

;Exercise 2.4 [**] Consider the data type of stacks of values, with an interface consising
;of the procedures empty-stack, push, pop, top, and empty-stack?. Write a specification for
;these operations in the style of the example above. Which operations are contructors
;and which are observers?

;specification
;stack := (empty-stack) | (push-stack stack)
;  (define empty-stack
;    (lambda () '()))
;
;  (define push-stack
;    (lambda (v stack)
;      (cons v stack)))
;
;  (define pop
;    (lambda (stack)
;      (cdr stack)))
;
;  (define top
;    (lambda (stack)
;      (car stack)))
;
;  (define empty-stack? null?)


;Exercise 2.5 [*] We can use any data structure for representing environments, if we
;can distinguish empty environments from non-empty ones, and in which one can extract
;the pieces of a non-empty environment. Implement environments using a representation
;in which the empty environment is represented as the empty list, and in which extend-env
;builds an environment that looks like
;env := (empty-env) | (extend var val env)
; () | (cons (list saved-var saved-val) saved-env)
  (define report-no-binding-found
    (lambda (search-var)
      (eopl:error 'apply-env "No binding for ~s" search-var)))
  
  (define report-invalid-env
    (lambda (env)
      (eopl:error 'apply-env "Bad environment: ~s" env)))

  (let ()
    (define empty-env
      (lambda () '()))

    (define extend-env
      (lambda (var val env)
        (cons (list var val) env)))

    (define apply-env
      (lambda (env search-var)
        (cond
          ((null? env) (report-no-binding-found search-var))
          ((pair? (car env))
           (let ((saved-var (caar env))
                 (saved-val (cadar env))
                 (saved-env (cdr env)))
             (if (eqv? search-var saved-var) saved-val (apply-env saved-env search-var))))
          (else
           (report-invalid-env env)))))

    (define e
      (extend-env 'd 6
        (extend-env 'y 8
          (extend-env 'x 7
            (extend-env 'y 14
              (empty-env))))))

    (equal?? (apply-env e 'd) 6)
    (equal?? (apply-env e 'y) 8)
    (equal?? (apply-env e 'x) 7)
    (report-unit-tests-completed 'apply-env))

;Exercise 2.6 [*] Invent at least three differenct representations of the environment
;interface and implement them.
;env ::= () | (var val env)
  (let ()
    (define empty-env (lambda () '()))
    (define extend-env
      (lambda (var val env)
        (list var val env)))

;Exercise 2.8 [*] Add to the environment interface on observe called empty-env?
;and implementing it using the a-list representation.
    (define empty-env? null?)

    (define apply-env
      (lambda (env search-var)
        (cond
          ((null? env) (report-no-binding-found search-var))
          ((list? env)
           (let ((saved-var (car env))
                 (saved-val (cadr env))
                 (saved-env (caddr env)))
             (if (eqv? saved-var search-var)
                 saved-val
                 (apply-env saved-env search-var))))
          (else
           (report-invalid-env env)))))

;Exercise 2.9 [*] Add to the environment interface an observer called has-binding?
;that takes an environment env and a variable s and tests to see if s has an associated
;value in env. Implementing it using the a-list representation.
    (define has-binding?
      (lambda (env s)
        (cond
          ((null? env) #f)
          ((list? env)
           (let ((saved-var (car env))
                 (saved-val (cadr env))
                 (saved-env (caddr env)))
             (if (eqv? saved-var s)
                 #t
                 (has-binding? saved-env s))))
          (else #f))))
;Page 39
;Exercise 2.10 [*] Add to the environment interface a constructor extend-env*, and
;implement it using the a-list represent. This constructor takes a list of variables,
;a list of values of the same length, and an environment, and is specified by
    (define extend-env*
      (lambda (vars vals env)
        (if (null? vars)
            env
            (let ((var (car vars))
                  (val (car vals))
                  (saved-vars (cdr vars))
                  (saved-vals (cdr vals)))
              (extend-env* saved-vars
                           saved-vals
                           (extend-env var val env))))))
    
    (define e
     (extend-env 'd 6
       (extend-env 'y 8
         (extend-env 'x 7
           (extend-env 'y 14
             (empty-env))))))

    (equal?? (apply-env e 'd) 6)
    (equal?? (apply-env e 'y) 8)
    (equal?? (apply-env e 'x) 7)
    (equal?? (has-binding? e 'x) #t)
    (equal?? (has-binding? e 'a) #f)
    (equal?? (has-binding? (extend-env* '(a b c) '(1 2 3) e) 'a) #t)
    (equal?? (has-binding? (extend-env* '(a b c) '(1 2 3) e) 'h) #f)
    (equal?? (apply-env (extend-env* '(a b c) '(1 2 3) e) 'a) 1)
    (report-unit-tests-completed 'apply-env))

;Page 39
;Exercise 2.11 [**]
;env ::= () | (((vars) (vals)) env)
  (let ()
    (define empty-env (lambda () '()))
    (define empty-env? null?)
    (define extend-env
      (lambda (var val env)
        (cons (cons (list var)
                    (list val))
              env)))
    (define extend-env*
      (lambda (vars vals env)
        (cons (cons vars vals)
              env)))

    (define apply-env
      (lambda (env search-var)
        (cond
          ((empty-env? env) (report-no-binding-found search-var))
          ((list? env)
           (let ((saved-vars (caar env))
                 (saved-vals (cdar env))
                 (saved-env (cdr env)))
             (let ((search-pair (apply-env-aux saved-vars saved-vals search-var)))
               (if (null? search-pair)
                   (apply-env saved-env search-var)
                   (cdr search-pair)))))
          (else
           (report-invalid-env env)))))

    (define apply-env-aux
      (lambda (vars vals search-var)
        (if (null? vars)
            '()
            (let ((var (car vars))
                  (val (car vals))
                  (saved-vars (cdr vars))
                  (saved-vals (cdr vals)))
              (if (eqv? var search-var)
                  (cons var val)
                  (apply-env-aux saved-vars saved-vals search-var))))))

    (define has-binding?
      (lambda (env s)
        (cond
          ((empty-env? env) #f)
          ((list? env)
           (let ((saved-vars (caar env))
                 (saved-vals (cdar env))
                 (saved-env (cdr env)))
             (let ((search-pair (apply-env-aux saved-vars saved-vals s)))
               (if (null? search-pair)
                   (has-binding? saved-env s)
                   #t))))
          (else #f))))
    
    (define e
     (extend-env 'd 6
       (extend-env 'y 8
         (extend-env 'x 7
           (extend-env 'y 14
             (empty-env))))))
    
    (equal?? (apply-env e 'd) 6)
    (equal?? (apply-env e 'y) 8)
    (equal?? (apply-env e 'x) 7)
    (equal?? (has-binding? e 'x) #t)
    (equal?? (has-binding? e 'a) #f)
    (equal?? (has-binding? (extend-env* '(a b c) '(1 2 3) e) 'a) #t)
    (equal?? (has-binding? (extend-env* '(a b c) '(1 2 3) e) 'h) #f)
    (equal?? (apply-env (extend-env* '(a b c) '(1 2 3) e) 'a) 1)
    (report-unit-tests-completed 'backbone-env-represent))

;Exercise 2.12 [*] Implement the stack data type of exercise 2.4 using a procedural
;representation.
  (define empty-stack
    (lambda ()
      (lambda (oper)
        (cond
          ((eqv? oper 'pop) (report-err-pop-stack))
          ((eqv? oper 'top) '())
          ((eqv? oper 'empty?) #t)
          (else (report-err-oper-stack oper))))))
           
  (define push
    (lambda (v stack)
      (lambda (oper)
        (cond
          ((eqv? oper 'pop) stack)
          ((eqv? oper 'top) v)
          ((eqv? oper 'empty?) #f)
          (else
           (report-err-oper-stack oper))))))
           
  (define pop
    (lambda (stack)
      (stack 'pop)))

  (define top
    (lambda (stack)
      (stack 'top)))

  (define empty-stack?
    (lambda (stack)
      (stack 'empty?)))

  (define report-err-pop-stack
    (lambda ()
      (eopl:error 'pop "Can't pop empty stack")))

  (define report-err-oper-stack
    (lambda (p)
      (eopl:error 'stack "Wrong oper on stack: ~s" p)))

;Page 42
;Exercise 2.13 [**] Extend the procedural representation to implement empty-env?
;by representing the environment by a list of two procedure: one that returns the
;value associated with a variable, as before, and one that returns whether or not
;the environment is empty.

  (let ()
    (define empty-env
      (lambda ()
        (list
         (lambda (search-var)
           (report-no-binding-found search-var))
         (lambda () #t)
         (lambda (search-var) #f))))

    (define extend-env
      (lambda (saved-var saved-val saved-env)
        (list
         (lambda (search-var)
           (if (eqv? saved-var search-var)
               saved-val
               (apply-env saved-env search-var)))
         (lambda () #f)
         (lambda (search-var)
           (if (eqv? saved-var search-var)
               #t
               (has-binding? saved-env search-var))))))

    (define apply-env
      (lambda (env search-var)
        ((car env) search-var)))

    (define empty-env?
      (lambda (env)
        ((cadr env))))

;;Page 42
;;Exercise 2.14 [**] Extend the representation of the preceding exercise to include a
;;third procedure that implements has-binding? (see exercise 2.9).
    (define has-binding?
      (lambda (env search-var)
        ((caddr env) search-var)))

    (define e
     (extend-env 'd 6
       (extend-env 'y 8
         (extend-env 'x 7
           (extend-env 'y 14
             (empty-env))))))
    
    (equal?? (apply-env e 'd) 6)
    (equal?? (apply-env e 'y) 8)
    (equal?? (apply-env e 'x) 7)
    (equal?? (empty-env? (empty-env)) #t)
    (equal?? (empty-env? e) #f)
    (equal?? (has-binding? e 'x) #t)
    (equal?? (has-binding? e 'a) #f)
    (report-unit-tests-completed 'list-precedures-env-represent))

  )