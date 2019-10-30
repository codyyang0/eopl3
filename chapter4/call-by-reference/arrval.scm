(module arrval (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (require "store.scm")
  
  (provide (all-defined-out))

  ;;;;;; ArrVal ;;;;;;;;;;;;;;;;;
  (define array?
    (lambda (v)
      (pair? v)))

  ;; array is pair
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
        (cons size (extend-arr (- size 1) val (newref val))))))

  (define arraryref
    (lambda (arr idx)
      (let ((len (arraylength arr))
            (ref (start-idx arr)))
        (if (>= idx len)
            (array-bound-error) 
            (deref (+ ref idx))))))

  (define arrayset
    (lambda (arr idx val)
      (let ((len (arraylength arr))
            (ref (start-idx arr)))
        (if (>= idx len)
            (array-bound-error) 
            (setref! (+ ref idx) val)))))
        
  (define array-bound-error
    (lambda ()
      (eopl:error 'array-bound-error "Array index out of range")))

  (define arraylength
    (lambda (arr)
      (car arr)))

  (define start-idx
    (lambda (arr)
      (cdr arr)))
  
)

