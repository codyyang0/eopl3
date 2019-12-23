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
      (oper (arbno (or "+" "-" "*" "/")) string)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
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
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)

      ;Exercise 3.6 minus
      (expression
       ("minus" "(" expression ")")
       minus-exp)

      ;Page 73
      ;Exercise 3.8
      (expression
       ("equal?" "(" expression "," expression ")")
       equal?-exp)

      (expression
       ("greater?" "(" expression "," expression ")")
       greater?-exp)

      (expression
       ("less?" "(" expression "," expression ")")
       less?-exp)

      (expression
       ("cons" "(" expression "," expression ")")
       cons-exp)

      (expression
       ("car" "(" expression ")")
       car-exp)

      (expression
       ("cdr" "(" expression ")")
       cdr-exp)

      (expression
       ("null?" "(" expression ")")
       null?-exp)

      (expression
       ("emptylist")
       emptylist-exp)

      ;Page 73
      ;Exercise 3.10
      (expression
       ("list" "(" (separated-list expression ",") ")")
       list-exp)

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
