#lang sicp
(define builtin-apply apply) ;save the native apply for now

(define (true? x) (not (eq? x false))) ;i suppose this is a more inclusive definition than (eq? x true)
(define (false? x) (eq? x false))

(define the-empty-environment '()) ;we will use the empty list for an empty environment
(define (first-frame env) (car env)) ;environments have a nested structure.  the innermost frame is the car of the environment
(define (enclosing-environment env) (cdr env)) ;likewise, the enclosing environment is the cdr


the-empty-environment
(first-frame the-empty-environment) ;apparently invalid to call frame selectors on empty environment
(enclosing-environment the-empty-environment) ;apparently invalid to call frame selectors on empty environment