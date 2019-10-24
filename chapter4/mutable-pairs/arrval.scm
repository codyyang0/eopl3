(module arrval (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (require "store.scm")
  (require "data-structures.scm")
  
  (provide (all-defined-out))

  ;;;;;; ArrVal ;;;;;;;;;;;;;;;;;

  (define arrval?
    (lambda (v)
      (reference? v)))

  (define make-arr
    (lambda (val1 val2)
      (let ((n (expval->num val1)))
        (let ((ref1 (newref val2)))
          (letrec
              ((extend-arr
                (lambda (n val)
                  (if (= (n 0))
                      ref1
                      (newref val)))))
            (extend-arr (- n 1) val2))))))

  (define arraryref
    (lambda (arr l)
      (deref (+ arr l))))

  (define arrayset
    (lambda (arr l val)
      (setref! (+ arr l) val)))
)
      
      

  
  