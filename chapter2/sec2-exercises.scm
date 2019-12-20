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

;Page 43
;Exercise 2.15 [*] Implement the lambda-calculus expression interface for the
;representation specified by the grammar above.
  (let ()
    (define var-exp
      (lambda (var) var))

    (define lambda-exp
      (lambda (var lcExp)
        (list 'lambda (list var) lcExp)))

    (define app-exp
      (lambda (lcExp1 lcExp2)
        (list lcExp1 lcExp2)))

    (define var-exp?
      (lambda (lcExp)
        (identifier? lcExp)))

    (define identifier?
      (lambda (s)
        (and
         (symbol? s)
         (not (eqv? 'lambda s)))))
  
    (define lambda-exp?
      (lambda (lcExp)
        (if (not (eqv? (car lcExp) 'lambda))
            #f
            (let ((var (caadr lcExp))
                  (saved-lcExp (caddr lcExp)))
              (and
               (identifier? var)
               (or
                (var-exp? saved-lcExp)
                (lambda-exp? saved-lcExp)
                (app-exp? saved-lcExp)))))))

    (define app-exp?
      (lambda (lcExp)
        (let ((lcExp1 (car lcExp))
              (lcExp2 (cadr lcExp)))
          (and
           (or
            (var-exp? lcExp1)
            (lambda-exp? lcExp1)
            (app-exp? lcExp1))
           (or
            (var-exp? lcExp2)
            (lambda-exp? lcExp2)
            (app-exp? lcExp2))))))

    (define var-exp->var
      (lambda (lcExp) lcExp))

    (define lambda-exp->bound-var
      (lambda (lcExp)
        (caadr lcExp)))

    (define lambda-exp->body
      (lambda (lcExp)
        (caddr lcExp)))

    (define app-exp->rator
      (lambda (lcExp)
        (car lcExp)))

    (define app-exp->rand
      (lambda (lcExp)
        (cadr lcExp)))

    (define occurs-free?
      (lambda (search-var exp)
        (cond
          ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
          ((lambda-exp? exp)
           (and
            (not (eqv? search-var (lambda-exp->bound-var exp)))
            (occurs-free? search-var (lambda-exp->body exp))))
          (else
           (or
            (occurs-free? search-var (app-exp->rator exp))
            (occurs-free? search-var (app-exp->rand exp)))))))

    ;; a few small unit tests
    (equal??
     (occurs-free? 'a (lambda-exp 'a (app-exp (var-exp 'b) (var-exp 'a))))
     #f)

    (equal??
     (occurs-free? 'b (lambda-exp 'a (app-exp (var-exp 'b) (var-exp 'a))))
     #t)

    (report-unit-tests-completed 'occurs-free?))

;Page 43
;Exercise 2.16 [*] Modify the implementation to use a representation in which there
;are no parentheses around the bound variable in a lambda expression.
;Lc-exp ::= Identifier
;       ::= (lambda Identifier Lc-exp)
;       ::= (Lc-exp Lc-exp)
  (let ()
    (define var-exp
      (lambda (var) var))

    (define lambda-exp
      (lambda (var lcExp)
        (list 'lambda var lcExp)))

    (define app-exp
      (lambda (lcExp1 lcExp2)
        (list lcExp1 lcExp2)))

    (define var-exp?
      (lambda (lcExp)
        (identifier? lcExp)))

    (define identifier?
      (lambda (s)
        (and
         (symbol? s)
         (not (eqv? 'lambda s)))))
  
    (define lambda-exp?
      (lambda (lcExp)
        (if (not (eqv? (car lcExp) 'lambda))
            #f
            (let ((var (cadr lcExp))
                  (saved-lcExp (caddr lcExp)))
              (and
               (identifier? var)
               (or
                (var-exp? saved-lcExp)
                (lambda-exp? saved-lcExp)
                (app-exp? saved-lcExp)))))))

    (define app-exp?
      (lambda (lcExp)
        (let ((lcExp1 (car lcExp))
              (lcExp2 (cadr lcExp)))
          (and
           (or
            (var-exp? lcExp1)
            (lambda-exp? lcExp1)
            (app-exp? lcExp1))
           (or
            (var-exp? lcExp2)
            (lambda-exp? lcExp2)
            (app-exp? lcExp2))))))
    
    (define var-exp->var
      (lambda (lcExp) lcExp))

    (define lambda-exp->bound-var
      (lambda (lcExp)
        (cadr lcExp)))

    (define lambda-exp->body
      (lambda (lcExp)
        (caddr lcExp)))

    (define app-exp->rator
      (lambda (lcExp)
        (car lcExp)))

    (define app-exp->rand
      (lambda (lcExp)
        (cadr lcExp)))

    (define occurs-free?
      (lambda (search-var exp)
        (cond
          ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
          ((lambda-exp? exp)
           (and
            (not (eqv? search-var (lambda-exp->bound-var exp)))
            (occurs-free? search-var (lambda-exp->body exp))))
          (else
           (or
            (occurs-free? search-var (app-exp->rator exp))
            (occurs-free? search-var (app-exp->rand exp)))))))

    ;; a few small unit tests
    (equal??
     (occurs-free? 'a (lambda-exp 'a (app-exp (var-exp 'b) (var-exp 'a))))
     #f)

    (equal??
     (occurs-free? 'b (lambda-exp 'a (app-exp (var-exp 'b) (var-exp 'a))))
     #t)

    (report-unit-tests-completed 'occurs-free?))

;Page 44
;Exercise 2.18
  ;NodeInSequence ::= (Int Listof(Int) Listof(Int))
  (let ()
    (define number->sequence
      (lambda (i)
        (list i '() '())))

    (define current-element
      (lambda (seq)
        (car seq)))

    (define left-list
      (lambda (seq)
        (cadr seq)))

    (define right-list
      (lambda (seq)
        (caddr seq)))

    (define move-to-left
      (lambda (seq)
        (let ((cur-node (current-element seq))
              (left (left-list seq))
              (right (right-list seq)))
          (let ((new-cur-node (car left))
                (new-l (cdr left))
                (new-r (cons cur-node right)))
            (list new-cur-node new-l new-r)))))

    (define move-to-right
      (lambda (seq)
        (let ((cur-node (current-element seq))
              (left (left-list seq))
              (right (right-list seq)))
          (let ((new-cur-node (car right))
                (new-l (cons cur-node left))
                (new-r (cdr right)))
            (list new-cur-node new-l new-r)))))

    (define insert-to-left
      (lambda (i seq)
        (let ((cur-node (current-element seq))
              (left (left-list seq))
              (right (right-list seq)))
          (list cur-node
                (cons i left)
                right))))
    
    (define insert-to-right
      (lambda (i seq)
        (let ((cur-node (current-element seq))
              (left (left-list seq))
              (right (right-list seq)))
          (list cur-node
                left
                (cons i right)))))

    (equal?? (number->sequence 7) '(7 () ()))
    (equal?? (current-element '(6 (5 4 3 2 1) (7 8 9))) 6)
    (equal?? (move-to-left '(6 (5 4 3 2 1) (7 8 9))) '(5 (4 3 2 1) (6 7 8 9)))
    (equal?? (move-to-right '(6 (5 4 3 2 1) (7 8 9))) '(7 (6 5 4 3 2 1) (8 9)))
    (equal?? (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (13 5 4 3 2 1) (7 8 9)))
    (equal?? (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (5 4 3 2 1) (13 7 8 9)))
    (report-unit-tests-completed 'NodeInSequence))

;Page 44
;Exercise 2.19
  ;Bintree ::= () | (Int Bintree Bintree)
  (let ()
    (define number->bintree
      (lambda (i)
        (list i '() '())))

    (define at-leaf? null?)

    (define current-element
      (lambda (bt)
        (car bt)))

    (define move-to-left
      (lambda (bt)
        (cadr bt)))

    (define move-to-right
      (lambda (bt)
        (caddr bt)))

    (define insert-to-left
      (lambda (i bt)
        (let ((cur-e (current-element bt))
              (left (move-to-left bt))
              (right (move-to-right bt)))
          (if (at-leaf? left)
              (let ((new-left (number->bintree i)))
                (list cur-e new-left right))
              (list cur-e
                    (list i left '())
                    right)))))

    (define insert-to-right
      (lambda (i bt)
        (let ((cur-e (current-element bt))
              (left (move-to-left bt))
              (right (move-to-right bt)))
          (if (at-leaf? right)
              (let ((new-right (number->bintree i)))
                (list cur-e left new-right))
              (list cur-e
                    left
                    (list i '() right))))))

    (equal?? (number->bintree 13) '(13 () ()))
    (define t1 (insert-to-right 14 (insert-to-left 12 (number->bintree 13))))
    (equal?? t1 '(13 (12 () ()) (14 () ())))
    (equal?? (move-to-left t1) '(12 () ()))
    (equal?? (current-element (move-to-left t1)) 12)
    (equal?? (at-leaf? (move-to-right (move-to-left t1))) #t)
    (equal?? (insert-to-left 15 t1) '(13 (15 (12 () ()) ()) (14 () ())))
    (report-unit-tests-completed 'Bintree))

;Page 45
;Exercise 2.20 [***] In the representation of binary trees in exercise 2.19 it is easy to
;move from a parent node to one of its sons, but it is impossible to move from a son to
;its parent without the help of context arguments. Extend the representation of lists in
;exercise 2.18 to represent nodes in a binary tree. As a hint, consider representing the
;portion of the tree above the current node by a reversed list, as in exercise 2.18.
;In this representation, implement the procedures from exercise 2.19. Also implement
;move-up, at-root?, and at-leaf?
  
  ;Bintree ::= () | (Int Bintree Bintree)
  ;nodeInBintree ::= (Bintree Listof(Bintree))
  
  (let ()
    (define number->bintree
      (lambda (i) (list i '() '())))

    ;node
    (define leaf? null?)
    (define leaf (lambda () '()))
    (define branch (lambda (i bt1 bt2) (list i bt1 bt2)))
    (define current-element (lambda (node) (car node)))
    (define left (lambda (node) (cadr node)))
    (define right (lambda (node) (caddr node)))

    ;node In Bintree
    (define nodeInBt
      (lambda (node loBt) (list node loBt)))
    (define current-node
      (lambda (nodeInBt) (car nodeInBt)))
    (define parents
      (lambda (nodeInBt) (cadr nodeInBt)))
    (define at-leaf?
      (lambda (nodeInBt) (leaf? (current-node nodeInBt))))
    (define at-root?
      (lambda (nodeInBt) (null? (parents nodeInBt))))
      
    (define move-to-left
      (lambda (nodeInBt)
        (let ((node (current-node nodeInBt))
              (ps (parents nodeInBt)))
          (let ((new-node (left node))
                (new-ps (list node ps)))
            (list new-node new-ps)))))

    (define move-to-right
      (lambda (nodeInBt)
        (let ((node (current-node nodeInBt))
              (ps (parents nodeInBt)))
          (let ((new-node (right node))
                (new-ps (list node ps)))
            (list new-node new-ps)))))

    (define move-up
      (lambda (nodeInBt)
        (let ((new-node (car (parents nodeInBt)))
              (new-parents (cadr (parents nodeInBt))))
          (list new-node new-parents))))

    (define insert-to-right
      (lambda (i nodeInBt)
        (let ((node (current-node nodeInBt))
              (p (parents nodeInBt)))
          (let ((i-node (number->bintree i))
                (old-right (right node))
                (old-left (left node)))
            (let* ((new-right
                   (list (current-element i-node)
                         (left i-node)
                         old-right))
                   (new-node
                    (list (current-element node)
                          old-left
                          new-right)))
              (list new-node p))))))

    (define insert-to-left
      (lambda (i nodeInBt)
        (let ((node (current-node nodeInBt))
              (p (parents nodeInBt)))
          (let ((i-node (number->bintree i))
                (old-right (right node))
                (old-left (left node)))
            (let* ((new-left
                    (list (current-element i-node)
                          old-left
                          (right i-node)))
                   (new-node
                    (list (current-element node)
                          new-left
                          old-right)))
              (list new-node p))))))
    
    (equal?? (number->bintree 13) '(13 () ()))
    (define t (nodeInBt (number->bintree 13) '()))
    (define t1 (insert-to-right 14
                 (insert-to-left 12 t)))
    (equal?? t1 '((13 (12 () ()) (14 () ())) ()))
    (equal?? (move-to-left t1) '((12 () ()) ((13 (12 () ()) (14 () ())) ())))
    (equal?? (current-node (current-element (move-to-left t1))) 12)
    (equal?? (at-leaf? (move-to-right (move-to-left t1))) #t)
    (equal?? (insert-to-left 15 t1) '((13 (15 (12 () ()) ()) (14 () ())) ()))
    (equal?? (at-root? t1) #t)
    (equal?? (at-root? (move-to-left t1)) #f)
    (equal?? (move-up (move-to-left t1)) '((13 (12 () ()) (14 () ())) ()))
    (equal?? (move-up (move-to-right t1)) '((13 (12 () ()) (14 () ())) ()))
    (define t2 (insert-to-left 15 t1))
    (equal?? (move-to-left t2) '((15 (12 () ()) ()) ((13 (15 (12 () ()) ()) (14 () ())) ())))
    (report-unit-tests-completed 'nodeInBt))

;Page 50
;Exercise 2.21 [*] Implement the data type of environments, as in section 2.2.2, using
;define-datatype. Then include has-binding? of exercise 2.9.
  (let ()
    (define identifier?
      (lambda (s)
        (and
         (symbol? s)
         (not (eqv? 'lambda s)))))
    
    (define-datatype env env?
      (empty-env)
      (extend-env
       (var identifier?)
       (val (lambda (v) (not (null? v))))
       (saved-env env?)))

    (define apply-env
      (lambda (e search-var)
        (cases env e
          (empty-env () (report-no-binding-found search-var))
          (extend-env (var val saved-env)
            (if (eqv? search-var var) val (apply-env saved-env search-var)))
          (else
           (report-invalid-env e)))))

    (define empty-env?
      (lambda (e)
        (cases env e
          (empty-env () #t)
          (extend-env (var val saved-env) #f))))

    (define has-binding?
      (lambda (e search-var)
        (cases env e
          (empty-env () #f)
          (extend-env (var val saved-env)
            (if (eqv? search-var var) #t (has-binding? saved-env search-var)))
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
    (equal?? (empty-env? (empty-env)) #t)
    (equal?? (empty-env? e) #f)
    (equal?? (has-binding? e 'x) #t)
    (equal?? (has-binding? e 'a) #f)
    (report-unit-tests-completed 'datatype-env))

;Exercise 2.22 [*] Using define-datatype, implement the stack data type of
;Exercise 2.4
  (let ()
    (define report-pop-err
      (lambda (s)
        (eopl:error 'pop "Can't pop from empty stack")))
    
    (define-datatype stack stack?
      (empty-stack)
      (push
       (v (lambda (n) (not (null? n))))
       (saved-stack stack?)))

    (define pop
      (lambda (s)
        (cases stack s
          (empty-stack () (report-pop-err))
          (push (v saved-stack) saved-stack))))

    (define top
      (lambda (s)
        (cases stack s
          (empty-stack () '())
          (push (v saved-stack) v))))

    (define empty-stack?
      (lambda (s)
        (cases stack s
          (empty-stack () #t)
          (push (v saved-stack) #f))))

    (equal?? (empty-stack? (empty-stack)) #t)
    (equal?? (top (push 'a (empty-stack))) 'a)
    (equal?? (pop (push 'a (push 'b (empty-stack)))) (push 'b (empty-stack)))
    (report-unit-tests-completed 'datatype-stack))


;Page 50
;Exercise 2.23
  (let ()
    (define identifier?
      (lambda (v)
        (lambda (v)
          (and
           (symbol? v)
           (not (eqv? 'lambda v))))))
    
    (define-datatype lc-exp lc-exp?
      (var-exp
       (var identifier?))
      (lambda-exp
       (bound-var identifier?)
       (body lc-exp?))
      (app-exp
       (rator lc-exp?)
       (rand lc-exp?)))

    ;occurs-free? : Sym × LcExp → Bool
    (define occurs-free?
      (lambda (search-var exp)
        (cases lc-exp exp
          (var-exp (var) (eqv? var search-var))
          (lambda-exp (bound-var body)
            (and
             (not (eqv? search-var bound-var))
             (occurs-free? search-var body)))
          (app-exp (rator rand)
            (or
             (occurs-free? search-var rator)
             (occurs-free? search-var rand))))))

    ;; a few small unit tests
    (equal??
     (occurs-free? 'a (lambda-exp 'a (app-exp (var-exp 'b) (var-exp 'a))))
     #f)

    (equal??
     (occurs-free? 'b (lambda-exp 'a (app-exp (var-exp 'b) (var-exp 'a))))
     #t)

    (report-unit-tests-completed 'occurs-free?))

;Exercise 2.24 [*]
  (let ()
    (define-datatype bintree bintree?
      (leaf-node
       (num integer?))
      (interior-node
       (key symbol?)
       (left bintree?)
       (right bintree?)))

    ;bintree-to-list : bintree -> list
    (define bintree-to-list
      (lambda (bt)
        (cases bintree bt
          (leaf-node (num) (list 'leaf-node num))
          (interior-node (key left right)
            (list 'interior-node
                  key
                  (bintree-to-list left)
                  (bintree-to-list right))))))

    ;这个属于client端的，还是interface端的呢?
    ;max-interior : bintree -> symbol

    (equal?? (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))
             '(interior-node a (leaf-node 3) (leaf-node 4)))
    (report-unit-tests-completed 'bintree-to-list))

;Exercise 2.25 
           
  )