(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for letrec-lang.

  (require "lang.scm")                  ; for expression?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?)))

;;; extractors:

  ;; expval->num : ExpVal -> Int
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  ;; expval->proc : ExpVal -> Proc
  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ;; proc? : SchemeVal -> Bool
  ;; procedure : Var * Exp * Env -> Proc
  (define-datatype proc proc?
    (procedure
      (bvar (list-of symbol?))
      (body expression?)
      (env environment?)))

  ;; Page: 86
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env*
     (bvars (list-of symbol?))
     (bvals (list-of expval?))
     (saved-env environment?))
;    (extend-env-rec
;      (id symbol?)
;      (bvars (list-of symbol?))
;      (body expression?)
;      (saved-env environment?))
;    (extend-env-rec
;     (id symbol?)
;     (vec vector?)
;     (saved-env environment?))
    ;Exercise 3.32
    (extend-env-rec*
     (id (list-of symbol?))
     (vec vector?)
     (saved-env environment?)))

  ;Exercise 3.34 [***] Implement extend-env-rec in the procedural representation
  ;of environments from section 2.2.3.
;  (define environment? procedure?)

;  (define apply-env
;    (lambda (env search-sym)
;      (env search-sym)))
  
;  (define empty-env
;    (lambda ()
;      (lambda (search-sym)
;        (eopl:error 'apply-env "No binding for ~s" search-sym))))

;  (define extend-env
;    (lambda (bvar bval saved-env)
;      (lambda (search-sym)
;        (if (eqv? search-sym bvar)
;            bval
;            (apply-env saved-env search-sym)))))

;  (define extend-env*
;    (lambda (bvars bvals saved-env)
;      (lambda (search-sym)
;        (let ([ref (idx search-sym bvars)])
;          (if ref
;              (list-ref bvals ref)
;              (apply-env saved-env search-sym))))))

;  (define extend-env-rec
;    (lambda (id bvars body saved-env)
;      (lambda (search-sym)
;        (if (eqv? search-sym id)
;            (let ([env (extend-env-rec id bvars body saved-env)])
;              (proc-val (procedure bvars body env)))
;            (apply-env saved-env search-sym)))))

;  (define extend-env-rec*
;    (lambda (lso-id lso-bvars lso-body saved-env)
;      (lambda (search-sym)
;        (let ([ref (idx search-sym lso-id)])
;          (if ref
;              (let ([env (extend-env-rec* lso-id lso-bvars lso-body saved-env)]
;                    [bvars (list-ref lso-bvars ref)]
;                    [body (list-ref lso-body ref)])
;                (proc-val (procedure bvars body env)))
;              (apply-env saved-env search-sym))))))

;  (define idx-aux
;    (lambda (sym lst i)
;      (if (null? lst)
;          #f
;          (if (eqv? sym (car lst))
;              i
;              (idx-aux sym (cdr lst) (+ i 1))))))
;  (define idx
;    (lambda (sym lst)
;      (idx-aux sym lst 0)))

)
