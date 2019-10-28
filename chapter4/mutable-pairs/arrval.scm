(module arrval (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (require "store.scm")
  
  (provide (all-defined-out))

  ;;;;;; ArrVal ;;;;;;;;;;;;;;;;;
  (define array?
    (lambda (v)
      (reference? v)))

  (define make-arr
    (lambda (size val)
      (letrec
        ((extend-arr
          (lambda (n val ref)
            (if (eqv? n 0)
                ref
                (begin
                  (newref val)
                  (extend-arr (- n 1) val ref))))))
        (extend-arr (- size 1) val (newref val)))))

  (define arraryref
    (lambda (arr idx)
      (deref (+ arr idx))))

  (define arrayset
    (lambda (arr idx val)
      (setref! (+ arr idx) val)))
)

