#lang sicp
;(define builtin-apply apply) ;save the native apply for now
;(define builtin-eval eval) ;save the native eval for now

(define (true? x) (not (eq? x false))) ;i suppose this is a more inclusive definition than (eq? x true)
(define (false? x) (eq? x false))

(define the-empty-environment '()) ;we will use the empty list for an empty environment
(define (first-frame env) (car env)) ;environments have a nested structure.  the innermost frame is the car of the environment
(define (enclosing-environment env) (cdr env)) ;likewise, the enclosing environment is the cdr

(define (make-frame variables values)
  (cons variables values)) ;frame: a pair of lists.  car is a list of variable names.  cons is a list of their values. frame: ((variables) values)
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;extend-environment: make a new frame as innermost env, use base-env as enclosing env
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env) ;cons the new frame and old base-env
      (if (< (length vars) (length vals))
          (error "in extend-environment: len(args) > len(vals)" vars vals)
          (error "in extend-environment: len(args) < len(vals)" vars vals))))

;lookup-variable-value: self-explanatory basically
(define (lookup-variable-value var env)
  (define (env-loop env) ; internal definition of env-loop
    (define (scan vars vals) ; internal definition of scan
      (cond ((null? vars) ; if there are no vars left, go to env-loop in enclosing environment
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals)) ; if we found the variable, return the corresponding value
            (else
             (scan (cdr vars) (cdr vals)) ; "loop" through the variable list in search of var, if not found yet
             ))) ;end of 'scan definition
    
    (if (eq? env the-empty-environment) ; in env-loop definition
        (error "unbound variable - in lookup-variable-value" var)
        (let ((frame (first-frame env))) ; bind 'frame to the innermost frame in the environment, if the environment isn't empty
          (scan (frame-variables frame) ; call scan
                (frame-values frame))))
    );end of 'env-loop definition
  (env-loop env));lookup-variable-value is essentially one line after its definitions: it just calls the env-loop routine it defines

;note: set-variable-value and lookup share almost the entire structure.  the only difference is (set-car! vals val) instead of car vals when we find the variable (when (eq? var (car vars))
(define (set-variable-value! var val env)
  (define (env-loop env) ; internal definition of env-loop: literally exactly the same as that in lookup! possible to-do for later would be to clean up that repetition
    (define (scan vars vals) ; internal definition of scan
      (cond ((null? vars) ; if there are no vars left, go to env-loop in enclosing environment
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val)) ; if we found the variable, mutate the car of the list to point to the new value
            (else
             (scan (cdr vars) (cdr vals)) ; "loop" through the variable list in search of var, if not found yet
             ))) ;end of 'scan definition
    
    (if (eq? env the-empty-environment) ; in env-loop definition
        (error "unbound variable - in set-variable-value" var)
        (let ((frame (first-frame env))) ; bind 'frame to the innermost frame in the environment, if the environment isn't empty
          (scan (frame-variables frame) ; call scan
                (frame-values frame))))
    );end of 'env-loop definition
  (env-loop env));set-variable-value is essentially one line after its definitions: it just calls the env-loop routine it defines

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals) ; once again, scan here is quite similar to scan in previous definitions
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val)) ; we here allow re-definition; it appears to act the same as set!
            ;another valid choice for the above would actually be to throw an error, since we don't necessarily want to allow re-definition
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))



(define primitive-procedures ;we get primitive procedures from the underlying scheme implementation
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '/ /)
        (list '* *)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'eqv? eqv?)
        ))

; not 100% why we implement primitive names and procedures as (no-argument) procedures instead of just lists, but it would work either way
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))
;;;;;;;; done with all boring environment setup stuff above

(define bind extend-environment) ; the chalkboard eval uses "bind"

;;;;;;;; eval definition:
(define evlist ; evaluate list of expr
  (lambda (l env)
    (cond ((eq? l '() ) '() )
          (else
           (cons (eval (car l) env) (evlist (cdr l env)) ) ) ) ) )
(define evcond ; evaluate conditional 'cond expression
  (lambda (clauses env) ; recall: clauses is a list of lists.  so caar is stuff like "else" or otherwise expressions that eval to false or true
    (cond ((eq? clauses '()) '()) ; if we ran out of clauses, just arbitrarily return '() (there is choice here)
          ((eq? (caar clauses) 'else) ; else clause
           (eval (cadar clauses) env)) ; reached default (else), so always eval.  car of cdr of car of clauses... ELSE is car of car, so (else (__)...) is cdr of car, so we want to eval car of cdr of car
          ;to clarify: (cadar '( (else (sdf)) ) ) --> (sdf) which is what we want in general
          ((false? (eval (caar clauses) env)) ; if current clause is false, 
           (evcond (cdr clauses) env)) ; cdr down to the rest of the clauses
          (else ; if true clause, short-circuit (eval the current clause)
           (eval (cadar clauses) env))))) ; see cadar explanation above.  clauses is a list of lists: (list ... (predicate expr) ...).  we want first item of outer list, cadr of that item.  so cadar.

(define eval
  (lambda (exp env)
    (cond
      ((number? exp) exp) ; number evaluates to itself
      ((string? exp) exp) ; string evaluates to itself
      ((symbol? exp) (lookup-variable-value exp env)) ; symbol evaluates to its lookup value
      ((and (pair? exp) (eq? (car exp) 'quote)) (cadr exp)) ; quoted expression
      ((and (pair? exp) (eq? (car exp) 'lambda)) (list 'closure (cdr exp) env)) ; lambda (function definition)
      ((and (pair? exp) (eq? (car exp) 'cond)) (evcond (cdr exp) env)) ; cond; we defined evcond as a helper for this
      (else
       (apply (eval (car exp) env) (evlist (cdr exp) env)))
      )))



;;;tests
(define frame0 (make-frame (list 'x 'y) (list 5 6 ))) (add-binding-to-frame! 'z 7 frame0) (display frame0) (frame-variables frame0) (frame-values frame0) ;illustrates building a frame
(define env0 (cons frame0 the-empty-environment))
(define env1 (extend-environment (list 'a 'b 'c) (list "s" "d" "ff") env0)) ;test extend-environment
env1
(first-frame env1) ;the innermost frame
(enclosing-environment env1) ;the enclosing environment
(lookup-variable-value 'x env1) ; works correctly!
(set-variable-value! 'y "hello" env1) ; works correctly!
env1
(define-variable! 'e "newvalue" env1) ; works correctly!
env1
(lookup-variable-value 'e env1)
(define-variable! 'x "new-x-value" env1) ; interesting, so a new inner x actually shadows the outer x here
env1 ; both the inner x and the outer x are still available
(lookup-variable-value 'x env1)
(define-variable! 'a "new-a-value" env1) ; in contrast, when we redefine a variable in the innermost frame, we do not "shadow" its old value.  instead the old value is totally lost.  an alternative option would be to throw an error here.
env1
(lookup-variable-value 'a env1)
the-global-environment
;(first-frame the-empty-environment) ;apparently invalid to call frame selectors on empty environment
;(enclosing-environment the-empty-environment) ;apparently invalid to call frame selectors on empty environment

;tests for eval:
(eval 5 the-global-environment)
(eval '(lambda (x) (+ x 5 )) the-global-environment)