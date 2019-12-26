(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the PROC language, using the data structure
  ;; representation of procedures.

  ;; The \commentboxes are the latex code for inserting the rules into
  ;; the code in the book. These are too complicated to put here, see
  ;; the text, sorry. 

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

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

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (vars body)
          (proc-val (procedure vars body env)))

        (letproc-exp (func var body exps)
          (let ((val (proc-val (procedure var body env))))
            (let ((new-env (extend-env func val env)))
              (value-of exps new-env))))

;        (call-exp (rator rands)
;          (let ((proc (expval->proc (value-of rator env)))
;                (args (map (lambda (rand) (value-of rand env))
;                           rands)))
;            (apply-procedure proc args)))
        
        (call-exp (rator rands)
          (let ((val (value-of rator env))
                (args (map (lambda (rand) (value-of rand env)) rands)))
            (cases expval val
              (proc-val (proc) (apply-procedure proc args))
              (builtin-func-val (func) (apply-builtin-func func args))
              (else
               (eopl:error 'call-exp "No binding for ~s" rator)))))
        )))


  ;; Page: 80 -> Exercise 3.21
  (define apply-procedure
    (lambda (proc1 vals)
      (cases proc proc1
        (procedure (vars body saved-env)
          (if (null? vars)
              (value-of body saved-env)
              (let ((var (car vars))
                    (val (car vals))
                    (saved-vars (cdr vars))
                    (saved-vals (cdr vals)))
                (let ((new-env (extend-env var val saved-env)))
                  (apply-procedure (procedure saved-vars body new-env) saved-vals))))))))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
;  (define apply-procedure
;    (lambda (proc1 val)
;      (cases proc proc1
;        (procedure (var body saved-env)
;          (value-of body (extend-env var val saved-env))))))

  ;; apply-builtin-func : builtin-func * ExpVal -> ExpVal
  (define apply-builtin-func
    (lambda (func1 vals)
      (let ((exe (cons func1 (map (lambda (v) (expval->base v)) vals))))
        (let ((result (eval exe)))
          (if (number? result)
              (num-val result)
              (bool-val result))))))
  )
