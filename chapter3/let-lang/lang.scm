(module lang

  ;; grammar for the LET language

  (lib "eopl.ss" "eopl")                
  
  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (oper
       ((arbno (or "+" "-" "*" "/")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      (pred ((arbno (or "greater?" "less?" "equal?"))) symbol)
      ))
  
  (define the-grammar
    '((program (expression) a-program)
      
      (expression (number) const-exp)

      ;Page 73
      ;Exercise 3.11
      (expression
       (oper "(" expression "," expression ")")
       math-exp)
      
;      (expression
;        ("-" "(" expression "," expression ")")
;        diff-exp)
;
;      ;Exercise 3.7
;      (expression
;       ("+" "(" expression "," expression ")")
;       add-exp)
;       
;      (expression
;       ("*" "(" expression "," expression ")")
;       multi-exp)
;      
;      (expression
;       ("/" "(" expression "," expression ")")
;       quot-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)
       
      (expression
       (pred "(" expression (arbno expression) ")")
        bool-exp)
      
      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      ;Exercise 3.12
;      (expression
;       ("cond" (arbno expression "==>" expression) "end")
;       cond-exp)

      (expression (identifier) var-exp)
      
      ;Exercise 3.16
      (expression
       ("let" (arbno identifier "=" expression) "in" expression)
       let-exp)

      ;Exercise 3.17
      (expression
       ("let*" (arbno identifier "=" expression) "in" expression)
       let*-exp)
      
      ;Exercise 3.6 minus
      (expression
       ("minus" "(" expression ")")
       minus-exp)

      ))
  
  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
