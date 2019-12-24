(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;Exercise 3.11
        (math-exp (oper exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
             (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                ((eval oper) num1 num2)))))

        ;\commentbox{\diffspec}
;        (diff-exp (exp1 exp2)
;          (let ((val1 (value-of exp1 env))
;                (val2 (value-of exp2 env)))
;            (let ((num1 (expval->num val1))
;                  (num2 (expval->num val2)))
;              (num-val
;                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;Exercise 3.12
        (cond-exp (exp1s exp2s)
          (if (null? exp1s)
              (cond-exp-error)
              (let ((pre-exp (car exp1s))
                    (body-exp (car exp2s)))
                (let ((val (value-of pre-exp env)))
                  (if (expval->bool val)
                      (value-of body-exp env)
                      (let ((pre-exps (cdr exp1s))
                            (body-exps (cdr exp2s)))
                        (value-of (cond-exp pre-exps body-exps) env)))))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))

        ;Exercise 3.6
        (minus-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (num-val (- (expval->num val1)))))

        ;Exercise 3.7
;        (add-exp (exp1 exp2)
;          (let ((val1 (value-of exp1 env))
;                (val2 (value-of exp2 env)))
;            (let ((num1 (expval->num val1))
;                  (num2 (expval->num val2)))
;              (num-val
;                (+ num1 num2)))))
;
;        (multi-exp (exp1 exp2)
;          (let ((val1 (value-of exp1 env))
;                (val2 (value-of exp2 env)))
;            (let ((num1 (expval->num val1))
;                  (num2 (expval->num val2)))
;              (num-val
;                (* num1 num2)))))
;        
;        (quot-exp (exp1 exp2)
;          (let ((val1 (value-of exp1 env))
;                (val2 (value-of exp2 env)))
;            (let ((num1 (expval->num val1))
;                  (num2 (expval->num val2)))
;              (num-val
;                (/ num1 num2)))))

        ;Exercise 3.8
        (equal?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (bool-val
                (= num1 num2)))))
        
        (greater?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (bool-val
                (> num1 num2)))))
        
        (less?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (bool-val
                (< num1 num2)))))

        (cons-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (list-val val1 val2)))

        (car-exp (exp1)
          (let ((val (value-of exp1 env)))
            (expval->car val)))

        (cdr-exp (exp1)
          (let ((val (value-of exp1 env)))
            (expval->cdr val)))

        (null?-exp (exp1)
          (let ((val (value-of exp1 env)))
            (bool-val (expval->null? val))))

        (emptylist-exp () (emptylist-val))

        ;Page 73
        ;Exercise 3.10
        (list-exp (exps)
          (if (null? exps)
              (emptylist-val)
              (let ((exp1 (car exps))
                    (saved-exps (cdr exps)))
                (let ((val1 (value-of exp1 env)))
                  (list-val val1 (value-of (list-exp saved-exps) env))))))
        )))

      (define cond-exp-error
        (lambda ()
          (eopl:error 'cond-exp "evaluate error")))
  )

