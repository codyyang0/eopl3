(module arrval (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (require "store.scm")
  
  (provide (all-defined-out))

  ;;;;;; ArrVal ;;;;;;;;;;;;;;;;;

  (define arrval?
    (lambda (v)
      (reference? v)))

  (define make-arr
    (lambda (size val)
      (letrec
        ((extend-arr
          (lambda (n val)
            (if (= (n 0))
                ref1
                (newref val)))))
        (extend-arr (- n 1) val))))

  (define arraryref
    (lambda (arr idx)
      (deref (+ arr idx))))

  (define arrayset
    (lambda (arr idx val)
      (setref! (+ arr idx) val)))
)

