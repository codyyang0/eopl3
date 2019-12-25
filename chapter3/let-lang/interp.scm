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
                (num-val 1)
                (num-val 0)))))

        (bool-exp (logic exp1 exps)
          (value-of-bool-exp exp env))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (not (eqv? (expval->num val1) 0))
              (value-of exp2 env)
              (value-of exp3 env))))

        ;Exercise 3.12
;        (cond-exp (exp1s exp2s)
;          (if (null? exp1s)
;              (cond-exp-error)
;              (let ((pre-exp (car exp1s))
;                    (body-exp (car exp2s)))
;                (let ((val (value-of pre-exp env)))
;                  (if (expval->bool val)
;                      (value-of body-exp env)
;                      (let ((pre-exps (cdr exp1s))
;                            (body-exps (cdr exp2s)))
;                        (value-of (cond-exp pre-exps body-exps) env)))))))

        ;Exercise 3.16
        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (vars exps body)
          (let ((vals (map (lambda (e) (value-of e env)) exps)))
            (let ((new-env (extend-env* vars vals env)))
              (value-of body new-env))))

        ;Exercise 3.17
        (let*-exp (vars exps body)
          (if (null? vars)
              (value-of body env)
              (let ((var (car vars))
                    (exp (car exps))
                    (saved-vars (cdr vars))
                    (saved-exps (cdr exps)))
                (let ((val (value-of exp env)))
                  (let ((new-env (extend-env var val env)))
                    (value-of (let*-exp saved-vars saved-exps body) new-env))))))
           
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
        )))

  ;Page 73
  ;Exercise 3.14 [**]
  (define value-of-bool-exp
    (lambda (exp env)
      (cases expression exp
        (bool-exp (pred exp1 exps)
          (if (null? exps)
              (num-val 1) ;expval: true
              (let ((exp2 (car exps))
                    (saved-exps (cdr exps)))
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (let ((oper (if (eqv? pred 'greater?)
                                    >
                                    (if (eqv? pred 'less?) < =))))
                      (if (not (oper num1 num2))
                          (num-val 0)
                          (value-of-bool-exp (bool-exp pred exp1 saved-exps)))))))))
        (else
         (let ((val (value-of exp env)))
           (let ((num (expval->num val)))
             (if (= num 0) (num-val 0) (num-val 1))))))))

      (define cond-exp-error
        (lambda ()
          (eopl:error 'cond-exp "evaluate error")))
  )

