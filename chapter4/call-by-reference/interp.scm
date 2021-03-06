(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the CALL-BY-REFERENCE language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  (require "pairvals.scm")
  (require "arrval.scm")

  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #t))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.


;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)             
      (cases program pgm
        (a-program (body)
          (value-of body (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 132
  (define value-of
    (lambda (exp env)
      (cases expression exp

        (const-exp (num) (num-val num))

        (var-exp (var) (deref (apply-env env var)))

        (diff-exp (exp1 exp2)
          (let ((val1
		  (expval->num
		    (value-of exp1 env)))
                (val2
		  (expval->num
		    (value-of exp2 env))))
            (num-val
	      (- val1 val2))))
        
        (zero?-exp (exp1)
	  (let ((val1 (expval->num (value-of exp1 env))))
	    (if (zero? val1)
	      (bool-val #t)
	      (bool-val #f))))

        (if-exp (exp0 exp1 exp2) 
          (if (expval->bool (value-of exp0 env))
            (value-of exp1 env)
            (value-of exp2 env)))


        ;; straightforward version of LET, without instrumentation
        ;; (let-exp (id rhs body)       
        ;;   (let ((val (value-of rhs env)))
        ;;     (value-of body
        ;;       (extend-env id (newref val) env))))

        ;; LET with instrumentation
       (let-exp (var exp1 body)       
	  (when (instrument-let)
	    (eopl:printf "entering let ~s~%" var))
          (let ((val (value-of exp1 env)))
	    (let ((new-env (extend-env var (newref val) env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf "entering body of let ~s with env =~%" var)
		  (pretty-print (env->list new-env))
		  (eopl:printf "store =~%")
		  (pretty-print (store->readable (get-store-as-list)))
		  (eopl:printf "~%")
		  ))
	      (value-of body new-env))))


        ;; Page 133 Exercise 4.34
        (letref-exp (var exp1 body)
          (when (instrument-let)
            (eopl:printf "entering let ~s~%" var))
          (cases expression exp1
            (var-exp (v)
              (let ((new-env (extend-env var (apply-env env v) env)))
                (when (instrument-let)
                  (begin
                    (eopl:printf "entering body of let ~s with env =~%" var)
                    (pretty-print (env->list new-env))
                    (eopl:printf "store =~%")
                    (pretty-print (store->readable (get-store-as-list)))
                    (eopl:printf "~%")
                    ))
                (value-of body new-env)))
            (else
             (value-of (let-exp var exp1 body) env))))

        ;; Page 133 Exercise 4.35
        (ref-exp (var)
          (apply-env env var))

        (deref-exp (var)
          (deref (apply-env env var)))

        (setref-exp (var expression)
          (setref!
           (apply-env env var)
           (value-of expression env)))
        
        (proc-exp (vars body)
	  (proc-val
	    (procedure vars body env)))

        (call-exp (rator rands)
          (let ((proc (expval->proc (value-of rator env)))
                (args (map (lambda (rand) (value-of-operand rand env)) rands)))
            (begin
              (apply-procedure proc args)
              (for-each
               (lambda (a-param n-param)
                 (cases expression a-param
                   (var-exp (var)
                     (setref! (apply-env env var)
                              (deref n-param)))
                   (else
                    (eopl:error 'call-exp "Actual params must be a vairable"))))
               rands args))))

;         (call-exp (rator rands)
;          (let ((proc (expval->proc (value-of rator env)))
;                (args (map (lambda (rand) (value-of-operand rand env)) rands)))
;            (apply-procedure proc args)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (assign-exp (x e)
          (begin
            (setref!
              (apply-env env x)
              (value-of e env))
            (num-val 27)))

        (newpair-exp (exp1 exp2)
          (let ((v1 (value-of exp1 env))
                (v2 (value-of exp2 env)))
            (mutpair-val (make-pair v1 v2))))

        (left-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((p1 (expval->mutpair v1)))
              (left p1))))

        (setleft-exp (exp1 exp2)
          (let ((v1 (value-of exp1 env))
                (v2 (value-of exp2 env)))
            (let ((p (expval->mutpair v1)))
              (begin
                (setleft p v2)
                (num-val 82)))))

        (right-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((p1 (expval->mutpair v1)))
              (right p1))))

        (setright-exp (exp1 exp2)
          (let ((v1 (value-of exp1 env))
                (v2 (value-of exp2 env)))
            (let ((p (expval->mutpair v1)))
              (begin
                (setright p v2)
                (num-val 83)))))

        (newarray-exp (exp1 exp2)
          (let ((size (expval->num (value-of exp1 env)))
                (val (value-of exp2 env)))
            (array-val (make-arr size val))))

        (arrayref-exp (exp1 exp2)
          (let ((arr (expval->array (value-of exp1 env)))
                (idx (expval->num (value-of exp2 env))))
            (arraryref arr idx)))

        (arrayset-exp (exp1 exp2 exp3)
          (let ((arr (expval->array (value-of exp1 env)))
                (idx (expval->num (value-of exp2 env)))
                (val (value-of exp3 env)))
            (arrayset arr idx val)))

        (arraylength-exp (exp1)
          (let ((arr (expval->array (value-of exp1 env))))
            (num-val (arraylength arr))))

        )))

  ;; apply-procedure : Proc * Ref -> ExpVal
  ;; uninstrumented version
  ;; Page: 132
  ;;   (define apply-procedure
  ;;     (lambda (proc1 val)
  ;;       (cases proc proc1
  ;;         (procedure (var body saved-env)
  ;;           (value-of body
  ;;             (extend-env var val saved-env))))))
  

  ;; apply-procedure : Proc * Refs -> ExpVal
  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 vals)
      (cases proc proc1
        (procedure (vars body saved-env)
          (let ((new-env (extend-env* vars vals saved-env)))
	    (when (instrument-let)
	      (begin
	        (eopl:printf
		  "entering body of proc ~s with env =~%"
		   vars)
		(pretty-print (env->list new-env))
                (eopl:printf "store =~%")
                (pretty-print (store->readable (get-store-as-list)))
		(eopl:printf "~%")))
	      (value-of body new-env))))))


  ;; value-of-rand : Exp * Env -> Ref
  ;; Page: 132
  ;; if the expression is a var-exp, then pass the reference.
  ;; otherwise, evaluate the expression and pass a reference to a new
  ;; cell. 

;  (define value-of-operand
;    (lambda (exp env)
;      (cases expression exp
;        (var-exp (var) (apply-env env var))
;        (else
;          (newref (value-of exp env))))))

  ; value-of-rand : Exp * Env -> Ref
  ; Exercise 4.35, if the expression is ref-exp, then pass it
;  (define value-of-operand
;    (lambda (exp env)
;      (cases expression exp
;        (var-exp (var) (apply-env env var))
;        (ref-exp (var) (apply-env env var))
;        (arrayref-exp (arr idx)
;          (+ (start-idx (expval->array (value-of arr env)))
;             (expval->num (value-of idx env))))
;        (else
;          (newref (value-of exp env))))))

  ; Exercise 4.37 Call-by-value-result
  (define value-of-operand
    (lambda (var-exp env)
      (newref (value-of var-exp env))))

  ;; value-of-rand : Exp * Env -> Ref
  ;; Page 133: Exercise 4.33
;  (define value-of-operand
;    (lambda (exp env)
;      (cases expression exp
;        (var-exp (var)
;          (let* ((ref (apply-env env var))
;                (val (deref ref)))
;            (cases expval val
;              (num-val (num) (newref val))
;              (bool-val (b) (newref val))
;              (else ref))))
;        (else
;         (newref (value-of exp env))))))

  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (list
            (car p)
            (expval->printable (cadr p))))
        l)))

  )
  


  
