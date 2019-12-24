#lang eopl

;In SLLGEN, scanners are specified by regular expressions. Our example
;would be written in SLLGEN as follows:

(define scanner-spec-a
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

;;If the scanner is used with a parser that has keywords or punctuation, like
;;while or =, it is not necessary to put these in the scanner manually; the
;;parser-generator will add those automatically.

;;Specifying Grammars
(define grammer-a1
  '((statement
     ("{" statement ";" statement "}")
     compound-statement)
    (statement
     ("while" expression "do" statement)
     while-statement)
    (statement
     (identifier ":=" expression)
     assign-statement)
    (expression
     (identifier)
     var-exp)
    (expression
     ("(" expression "-" expression ")")
     diff-exp)))

