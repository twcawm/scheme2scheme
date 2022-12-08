#lang sicp

(define builtin-apply apply) ;save the native apply for now

(define (true? x)
  (eq? x #t))
(define (false? x)
  (eq? x #f))

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

(define evcond
  (lambda (clauses env)
    (cond ((eq? clauses '()) '())
          ((eq? (caar clauses) 'else)
           (eval (cadar clauses) env))
          ((false? (eval (caar clauses) env)) ;sicp assumes "false?" predicate is available.  but lang sicp in racket does not appear to have this.
           (evcond (cdr clauses) env))
          (else
           (eval (cadar clauses) env)
          )
    )
  )
)
#|
(define apply
  (lambda (proc args)
    (cond ((eq? 'closure (car proc))
           (eval (cadadr proc)
                 (bind (caadr proc)
                       args
                       (caddr proc)
                       )
                 )
           )
          (else (builtin-apply proc args))
          )
    )
  )
  |#                     
;currently, our eval can do a few things, but cannot handle things like applying a user-defined lambda to an argument (because the built-in apply does not accept how we've stored the closure)
;the goal was to see how far we could take the sicp eval without defining our own apply (just using built-in apply)
;note: I suspect that a fundamental incompatibility between builtin-apply and sicp-apply is the incompatible representation of environments
;  not to mention that the representation of compound procedures (and any closure) is also different
;  note: the sicp assq function also returns a different value than the builtin: returns '() (sicp) instead of #f (builtin)
;  so to get a fully functional sicp-eval, we might just need to also have a full sicp-apply as well.
;  we should probably save this attempt to implement sicp-eval/builtin-apply as a separate branch and move on to the full sicp version.
(define eval (lambda (exp env)
               (cond
                 ((number? exp) exp)
                 ((string? exp) exp) ; for a number or string, evaluates to itself
                 ((symbol? exp) (display "in symbol") (environment-lookup env exp)) ; just look up the symbol in the environment provided
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
(eval '(+ 5 6 ) sre5) ;this currently works in mit-scheme
(eval '( (lambda (x) (+ x 5) ) 3) sre5) ;this fails in mit-scheme because ";The object (closure ((x) (+ x 5)) #[environment 40]) is not applicable."
;this might be a good place to transition to full sicp-apply/sicp-eval rather than try to mix builtin-apply with sicp-eval