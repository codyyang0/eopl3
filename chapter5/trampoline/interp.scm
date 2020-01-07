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
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (let [[boun (value-of/k exp1 (init-env) (end-cont))]]
            (eopl:printf "~s~%" boun)
            (trampoline boun))))))
  
  ;; trampoline : Bounce -> FinalAnswer
  ;; 把递归调用的动作做了一个弹床，中转
  (define trampoline
    (lambda (boun)
      (cases bounce boun
        (expval-bounce (val) val)
        (value-of/k-bounce (exp env cont) (trampoline (value-of/k exp env cont)))
        (cont-bounce (cont val) (trampoline (apply-cont cont val)))
        (proc-bounce (proc val cont) (trampoline (apply-procedure/k proc val cont))))))

  ;; value-of/k : Exp * Env * Cont -> Bounce
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp
        (const-exp (num) (cont-bounce cont (num-val num)))
        (var-exp (var) (cont-bounce cont (apply-env env var)))
        (diff-exp (exp1 exp2)
          (value-of/k-bounce exp1 env (diff1-cont exp2 env cont)))
        (if-exp (exp1 exp2 exp3)
          (value-of/k-bounce exp1 env (if-test-cont exp2 exp3 env cont)))
        (proc-exp (var body)
          (let [[val [proc-val (procedure var body env)]]]
            (cont-bounce cont val)))
        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k-bounce letrec-body (extend-env-rec p-name b-var p-body env) cont))
        (zero?-exp (exp1) (value-of/k-bounce exp1 env (zero1-cont cont)))
        (let-exp (var exp1 body)
          (value-of/k-bounce exp1 env (let-exp-cont var body env cont)))
        (call-exp (rator rand)
          (value-of/k-bounce rator env (rator-cont rand env cont))))))
 
  ;; apply-cont : Cont * ExpVal -> Bounce
  (define apply-cont
    (lambda (cont val)
      (cases continuation cont
        (end-cont () 
          (begin
            (eopl:printf
              "End of computation.~%")
            (expval-bounce val)))
        ;; or (logged-print val)  ; if you use drscheme-init-cps.scm
        (zero1-cont (saved-cont)
          (cont-bounce saved-cont (bool-val (zero? (expval->num val)))))
        (let-exp-cont (var body saved-env saved-cont)
          (value-of/k-bounce body (extend-env var val saved-env) saved-cont))
        (if-test-cont (exp2 exp3 saved-env saved-cont)
          (if (expval->bool val)
             (value-of/k-bounce exp2 saved-env saved-cont)
             (value-of/k-bounce exp3 saved-env saved-cont)))
        (diff1-cont (exp2 saved-env saved-cont)
          (value-of/k-bounce exp2
            saved-env (diff2-cont val saved-cont)))
        (diff2-cont (val1 saved-cont)
          (let ((num1 (expval->num val1))
                (num2 (expval->num val)))
            (cont-bounce saved-cont (num-val (- num1 num2)))))
        (rator-cont (rand saved-env saved-cont)
          (value-of/k-bounce rand saved-env
            (rand-cont val saved-cont)))
        (rand-cont (val1 saved-cont)
          (let ((proc (expval->proc val1)))
            (proc-bounce proc val saved-cont)))
        )))

  ;; apply-procedure/k : Proc * ExpVal * Cont -> bounce
  (define apply-procedure/k
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k-bounce body
            (extend-env var arg saved-env)
            cont)))))
  )
  


  
