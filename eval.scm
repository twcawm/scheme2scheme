#lang sicp
(define eval (lambda (exp env)
               (cond
                 ((number? exp) exp)
               )
             )
)

(define sre5 (scheme-report-environment 5)) ; shorthand for test environment
(eval 4 sre5) ; should evaluate to 4
(eval "test" sre5) ; should break