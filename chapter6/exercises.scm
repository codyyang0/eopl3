#lang eopl

;(define remove-first
;  (lambda (s los cont)
;    (if (null? los)
;        (apply-cont cont los)
;        (let ([hd (car los)]
;              [rest (cdr los)])
;          (if (eqv? s hd)
;              (apply-cont cont rest)
;              (remove-first s rest (remove-cont hd cont)))))))

;; data-structure representation
(define-datatype continuation continuation?
  (end-cont)
  (remove-cont
   (sym symbol?)
   (cont continuation?)))

;(define apply-cont
;  (lambda (cont val)
;    (cases continuation cont
;      [end-cont ()
;        (begin
;          (eopl:printf "End of computation.~%")
;          (eopl:printf "This sentence should appear only once.~%")
;          val)]
;      [remove-cont (s cont)
;        (apply-cont cont (cons s val))])))

;; procedural representation
;(define apply-cont
;  (lambda (cont val)
;    (cont val)))
;
;(define remove-cont
;  (lambda (s cont)
;    (lambda (val)
;      (apply-cont cont (cons s val)))))
;
;(define end-cont
;  (lambda ()
;    (lambda (val)
;      (begin
;        (eopl:printf "End of computation.~%")
;        (eopl:printf "This sentence should appear only once.~%")
;        val))))

;; inlined procedural
;(define remove-first
;  (lambda (s los cont)
;    (if (null? los)
;        (cont '())
;        (let [[hd (car los)]
;              [rest (cdr los)]]
;          (if (eqv? s hd)
;              (cont rest)
;              (remove-first s rest
;                              (lambda (val) (cont (cons hd val)))))))))

;; registerized version
(define val 'uninitialized)
(define cont 'uninitialized)
(define search-sym 'uninitialized)
(define rest-los 'uninitialized)
(define pc 'uninitialized)

(define remove-first
  (lambda (s los)
    (set! cont (end-cont))
    (set! search-sym s)
    (set! rest-los los)
    (set! pc remove-first/k)
    (trampoline!)))

(define trampoline!
  (lambda ()
    (if pc
        (begin
          (pc)
          (trampoline!))
        val)))
        
; 把trampoline!要做的事情弄错了
;(define trampoline!
;  (lambda ()
;    (if (null? rest-los)
;        (begin
;          (set! val '())
;          (apply-cont))
;        (let ([hd (car rest-los)]
;              [rlos (cdr rest-los)])
;          (if (eqv? search-sym hd)
;              (begin
;                (set! val rlos)
;                (apply-cont))
;              (begin
;                (set! cont (remove-cont hd cont))
;                (set! rest-los rlos)
;                (remove-first/k)))))))

(define remove-first/k
  (lambda ()
    (if (null? rest-los)
        (begin
          (set! val '())
          (set! pc apply-cont))
        (let ([hd (car rest-los)]
              [rlos (cdr rest-los)])
          (if (eqv? search-sym hd)
              (begin
                (set! val rlos)
                (set! pc apply-cont))
              (begin
                (set! cont (remove-cont hd cont))
                (set! rest-los rlos)
                (set! pc remove-first/k)
                (trampoline!)))))))

(define apply-cont
  (lambda ()
    (cases continuation cont
      [end-cont ()
        (begin
          (eopl:printf "End of computation.~%")
          (eopl:printf "This sentence should appear only once.~%")
          (set! pc #f))]
      [remove-cont (s rcont)
        (set! val (cons s val))
        (set! cont rcont)
        (set! pc apply-cont)
        (trampoline!)])))
        

