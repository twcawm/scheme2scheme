#lang sicp

(define evlist 
  (lambda (l env)
    (display "in evlist")
    (cond ((eq? l '() ) '() )
          (else
           (cons
            (eval (car l) env)
            (evlist (cdr l) env)
           )
          )
     )
   )
)
;next, write evcond
;and probably need the primitive apply...?
(define eval (lambda (exp env)
               (cond
                 ((number? exp) exp)
                 ((string? exp) exp) ; for a number or string, evaluates to itself
                 ((and (pair? exp) (eq? (car exp) 'quote)) (car (cdr exp))) ;we're implicitly expecting a pair here; need to improve this
                 ;btw, for quote, it's cadr instead of cdr because (cdr '(quote abc)) evaluates to (abc) (list of one element) when we really just want the first element of that list.
                 ((and (pair? exp) (eq? (car exp) 'lambda)) (display "in lambda def") (list 'closure (cdr exp) env)) ; start lambda definition
                 ((and (pair? exp) (eq? (car exp) 'cond)) (display "in cond")
                  (eval (cdr exp) env)) ;we do not have evcond yet - this is incorrect for now
                 (else ;default: a combination
                  (display "in combination") (newline)
                  (apply
                   (eval (car exp) env)
                   (evlist (cdr exp) env)
                  )
                 )
               )
             )
)

(define sre5 (scheme-report-environment 5)) ; shorthand for test environment
(eval 4 sre5) ; should evaluate to 4
(eval '(quote abc) sre5) ; if we just use (quote 3), we're just eval'ing a number.  to eval (quote 3) literally, need '(quote 3)
(eval '(quote (quote (quote abc) ) ) sre5) ; not sure
(eval "test" sre5) ; string works now
(eval '(lambda (x) (+ x 5)) sre5) ; doesn't really do anything yet, but saves lambda body and env in a "closure"-tagged list , at least

(newline)
;(eval '(cond ((> 5 4) 6 ) )) ;broken for now
(eval '(+ 5 6 ) sre5) ;this currently breaks - I think it is because we do not lookup '+ ! so we need to implement lookup