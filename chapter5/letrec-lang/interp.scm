(module interp (lib "eopl.ss" "eopl")
  
  ;; cps interpreter for the LETREC language, using the data structure
  ;; representation of continuations (Figure 5.3).

  ;; exercise: rewrite this using the procedural representation of
  ;; continuations (Figure 5.2).

  ;; exercise: rewrite this using a trampoline (page 159).

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> FinalAnswer
  ;; Page: 143 and 154
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of/k exp1 (init-env) (end-cont))))))  

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 143--146, and 154
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp
        (const-exp (num) (apply-cont cont (num-val num)))
        (var-exp (var) (apply-cont cont (apply-env env var)))
        (proc-exp (var body)
          (apply-cont cont 
            (proc-val (procedure var body env))))
        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont))
        (zero?-exp (exp1)
          (value-of/k exp1 env
            (zero1-cont cont)))
        (let-exp (vars exps body)
          (if (null? vars)
              (value-of/k body env cont)
              (let ((var1 (car vars))
                    (exp1 (car exps))
                    (rest-vars (cdr vars))
                    (rest-exps (cdr exps)))
                (value-of/k exp1 env (let-vars-cont var1 rest-vars rest-exps body env cont)))))
          
;        (let-exp (var exp1 body)
;          (value-of/k exp1 env
;            (let-exp-cont var body env cont)))
;        (let2-exp (var1 exp1 var2 exp2 body)
;          (value-of/k exp1 env (let2-exp-cont var1 var2 exp2 body env cont)))
;        (let3-exp (var1 exp1 var2 exp2 var3 exp3 body)
;          (value-of/k exp1 env (let3-exp-cont var1 var2 var3 exp2 exp3 body env cont)))
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont)))
        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont)))        
        (call-exp (rator rand) 
          (value-of/k rator env
            (rator-cont rand env cont)))
        (emptylist-exp ()
          (apply-cont cont (emptylist-val)))
        (cons-exp (exp1 exp2)
          (value-of/k exp1 env (cons1-cont exp2 env cont)))
        (car-exp (exp)
          (value-of/k exp env (car-cont cont)))
        (cdr-exp (exp)
          (value-of/k exp env (cdr-cont cont)))
        (null?-exp (exp)
          (value-of/k exp env (null-cont cont)))
        (list-exp (exps)
          (if (null? exps)
              (apply-cont cont (emptylist-val))
              (let ((first-exp (car exps))
                    (rest-exps (cdr exps)))
                (value-of/k first-exp env (list-first-cont rest-exps env cont)))))
   )))

  ;; apply-cont : Cont * ExpVal -> FinalAnswer
  ;; Page: 148
;  (define apply-cont
;    (lambda (cont val)
;      (cases continuation cont
;        (end-cont () 
;          (begin
;            (eopl:printf
;              "End of computation.~%")
;            val))
;        ;; or (logged-print val)  ; if you use drscheme-init-cps.scm
;        (zero1-cont (saved-cont)
;          (apply-cont saved-cont
;            (bool-val
;              (zero? (expval->num val)))))
;        (let-exp-cont (var body saved-env saved-cont)
;          (value-of/k body
;            (extend-env var val saved-env) saved-cont))
;        (if-test-cont (exp2 exp3 saved-env saved-cont)
;          (if (expval->bool val)
;             (value-of/k exp2 saved-env saved-cont)
;             (value-of/k exp3 saved-env saved-cont)))
;        (diff1-cont (exp2 saved-env saved-cont)
;          (value-of/k exp2
;            saved-env (diff2-cont val saved-cont)))
;        (diff2-cont (val1 saved-cont)
;          (let ((num1 (expval->num val1))
;                (num2 (expval->num val)))
;            (apply-cont saved-cont
;              (num-val (- num1 num2)))))
;        (rator-cont (rand saved-env saved-cont)
;          (value-of/k rand saved-env
;            (rand-cont val saved-cont)))
;        (rand-cont (val1 saved-cont)
;          (let ((proc (expval->proc val1)))
;            (apply-procedure/k proc val saved-cont)))
;        )))

  ;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
  ;; Page 152 and 155
  (define apply-procedure/k
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var arg saved-env)
            cont)))))


  ;; Page 153 Exercise 5.1
  ;; Implement this data type of continuations using procedual representation.

  ;; Cont = Expval -> FinalAnswer

  (define continuation?
    (lambda (cont)
      (procedure? cont)))

  ;; Page 153 Exercise 5.1
  ;; Implement this data type of continuations using procedual representation.
  ;; apply-cont: Cont * Expval -> FinalAnswer
  (define apply-cont
    (lambda (cont val)
      (cont val)))

  ; end-cont: () -> Cont
  (define end-cont
    (lambda ()
      (lambda (val)
        (begin
          (eopl:printf "End of computation.~%")
          val))))

  ; zeor1-cont : Cont -> Cont
  (define zero1-cont
    (lambda (cont)
      (lambda (val)
        (apply-cont cont
          (bool-val
           (zero? (expval->num val)))))))

  (define let-cont
    (lambda (var rest-vars rest-exps body env cont)
      (lambda (val)
        (value-of/k
         (let-exp rest-vars rest-exps body)
         env
         (let-env-cont (extend-env var val env) cont)))))

                    
  ; let-exp-cont : Var * Exp * Env * Cont -> Cont
;  (define let-exp-cont
;    (lambda (var body env cont)
;      (lambda (val)
;        (value-of/k body (extend-env var val env) cont))))

  ; let2-exp-cont: Var * Var * Exp * Env * Cont -> Cont
;  (define let2-exp-cont
;    (lambda (var1 var2 exp2 body env cont)
;      (lambda (val1)
;        (value-of/k (let-exp var2 exp2 body) (extend-env var1 val1 env) cont))))

  ; let2-exp-cont: Var * Var * Var * Exp * Exp * Env * Cont -> Cont
;  (define let3-exp-cont
;    (lambda (var1 var2 var3 exp2 exp3 body env cont)
;      (lambda (val1)
;        (value-of/k (let2-exp var2 exp2 var3 exp3 body) (extend-env var1 val1 env) cont))))
  
  ; if-test-cont: Exp * Exp * Env * Cont -> Cont
  (define if-test-cont
    (lambda (exp2 exp3 env cont)
      (lambda (val)
        (if (expval->bool val)
            (value-of/k exp2 env cont)
            (value-of/k exp3 env cont)))))

  (define diff1-cont
    (lambda (exp2 env cont)
      (lambda (val1)
        (value-of/k exp2 env (diff2-cont val1 env cont)))))

  (define diff2-cont
    (lambda (val1 env cont)
      (lambda (val2)
        (apply-cont cont
                    (num-val (- (expval->num val1)
                                (expval->num val2)))))))
  (define rator-cont
    (lambda (rand env cont)
      (lambda (val)
        (value-of/k rand env (rand-cont val cont)))))

  (define rand-cont
    (lambda (rator cont)
      (lambda (val)
        (apply-procedure/k (expval->proc rator) val cont))))

  (define cons1-cont
    (lambda (exp2 env cont)
      (lambda (val)
        (value-of/k exp2 env (cons2-cont val env cont)))))

  (define cons2-cont
    (lambda (val1 env cont)
      (lambda (val2)
        (apply-cont cont (cons-val val1 val2)))))

  (define car-cont
    (lambda (cont)
      (lambda (val)
        (apply-cont cont (cons-car val)))))

  (define cdr-cont
    (lambda (cont)
      (lambda (val)
        (apply-cont cont (cons-cdr val)))))

  (define null-cont
    (lambda (cont)
      (lambda (val)
        (apply-cont cont (cons-null? val)))))

  (define list-first-cont
    (lambda (exps env cont)
      (lambda (val)
        (value-of/k (list-exp exps) env (list-rest-cont val cont)))))

  (define list-rest-cont
    (lambda (first-val cont)
      (lambda (rest-val)
        (apply-cont cont (cons-val first-val rest-val)))))
  )
  


  
