(module environments (lib "eopl.ss" "eopl") 
  
  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm. 

  (require "data-structures.scm")

  (provide init-env empty-env extend-env apply-env bind-vars-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69
  (define init-env 
    (lambda ()
      (extend-env 
       'i (num-val 1)
       (extend-env
        'v (num-val 5)
        (extend-env
         'x (num-val 10)
         (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  (define empty-env
    (lambda ()
      (empty-env-record)))
  
  (define empty-env? 
    (lambda (x)
      (empty-env-record? x)))

  (define extend-env
    (lambda (sym val old-env)
      (extended-env-record sym val old-env)))

  (define apply-env
    (lambda (env search-sym)
      (if (empty-env? env)
	;(eopl:error 'apply-env "No binding for ~s" search-sym)
        (builtin-func-val (eval search-sym))
	(let ((sym (extended-env-record->sym env))
	      (val (extended-env-record->val env))
	      (old-env (extended-env-record->old-env env)))
	  (if (eqv? search-sym sym)
	    val
	    (apply-env old-env search-sym))))))

  ;Exercise 3.26
  (define has-binding?
    (lambda (env search-sym)
      (if (empty-env? env)
          #f
          (let ((sym (extended-env-record->sym env))
                (old-env (extended-env-record->old-env env)))
            (if (eqv? sym search-sym)
                #t
                (has-binding? old-env search-sym))))))
  
  (define bind-vars-env
    (lambda (vars env free-env)
      (if (null? vars)
          free-env
          (let ((var (car vars))
                (saved-vars (cdr vars)))
            (if (has-binding? env var)
                (let ((new-env (extend-env var (apply-env env var) free-env)))
                  (bind-vars-env saved-vars env new-env))
                (bind-vars-env saved-vars env free-env))))))

  )