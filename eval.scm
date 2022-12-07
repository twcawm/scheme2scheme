#lang sicp
(define eval (lambda (exp env)
               (cond
                 ((number? exp) exp)
                 ((eq? (car exp) 'quote) (car (cdr exp))) ;we're implicitly expecting a pair here; need to improve this
               )
             )
)

(define sre5 (scheme-report-environment 5)) ; shorthand for test environment
(eval 4 sre5) ; should evaluate to 4
(eval '(quote 3) sre5) ; if we just use (quote 3), we're just eval'ing a number.  to eval (quote 3) literally, need '(quote 3)
(eval "test" sre5) ; should break