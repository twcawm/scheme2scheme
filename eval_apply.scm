#lang sicp ;this is required for drracket ide; comment out for mit-scheme

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
        (list '> >)
        (list '< <)
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

(define apply-primop apply) ;use the built-in apply for primitive ops

(define (primitive? proc) (eq? (car proc) 'primitive)) ; we type-tag the primitive procedures

;;;;;;;; eval/apply definition:
(define evlist ; evaluate list of expr
  (lambda (l env)
    (cond ((eq? l '() ) '() )
          (else
           (cons (eval (car l) env) (evlist (cdr l) env)) ) ) ) )
(define evcond ; evaluate conditional 'cond expression
  (lambda (clauses env) ; recall: clauses is a list of lists.  so caar is stuff like "else" or otherwise expressions that eval to false or true
    (cond ((eq? clauses '()) '()) ; if we ran out of clauses, just arbitrarily return '() (could choose something else, for error-checking purposes)
          ((eq? (caar clauses) 'else) ; else clause
           (evseq (cdar clauses) env)) ;note: updated from eval cadar to evseq cdar (instead of evaluating only 1 expr, evseq a sequence of expr) ; reached default (else), so always eval.  car of cdr of car of clauses... ELSE is car of car, so (else (__)...) is cdr of car, so we want to eval car of cdr of car
          ;to clarify: (cadar '( (else (sdf)) ) ) --> (sdf) which is what we want in general
          ((false? (eval (caar clauses) env)) ; if current clause is false, 
           (evcond (cdr clauses) env)) ; cdr down to the rest of the clauses (in other words, keep going)
          (else ; if true clause, short-circuit (eval the current clause & return)
           (evseq (cdar clauses) env))))) ;note: updated from eval cadar to evseq cdar (instead of evaluating only 1 expr, evseq a sequence of expr) ; see cadar explanation above.  clauses is a list of lists: (list ... (predicate expr) ...).  we want first item of outer list, cadr of that item.  so cadar.
(define evseq ; evaluate sequence of expr. we are implementing this for procedure bodies, but it could also be used for "begin" expressions.
  (lambda (l env)
    (cond ((eq? (cdr l) '() ) (eval (car l) env)) ; if we reached the last expr in the list, evaluate it (and return its value)
          (else
           (eval (car l) env) (evseq (cdr l) env))))) ; otherwise, use scheme's built in sequence-of-expr syntax to evaluate the sequence

; exp is car[(define <var> <value>)] so <value> is cadr & <var> is car
; we don't bother with the (define (proc arg1 arg2) <body>) syntax since it's just shorthand
; note: since eval-define and eval-assign! share the same body except for the dispatch to define-variable! or set-variable-value! we could probably do this in a more elegant way with message-passing or dispatch etc
(define eval-define
  (lambda (exp env)
    (define-variable! (car exp) (eval (cadr exp) env) env) (display "new define")(newline)))
(define eval-assign!
  (lambda (exp env)
    (set-variable-value! (car exp) (eval (cadr exp) env) env) (display "assign!")(newline)))

(define eval
  (lambda (exp env)
    (cond
      ((number? exp) exp) ; number evaluates to itself
      ((string? exp) exp) ; string evaluates to itself
      ((symbol? exp) (lookup-variable-value exp env)) ; symbol evaluates to its lookup value
      ((and (pair? exp) (eq? (car exp) 'quote)) (cadr exp)) ; quoted expression
      ((and (pair? exp) (eq? (car exp) 'begin)) (evseq (cdr exp) env)) ;when we have a begin expr, (begin exp1 exp2 ...) evaluate the following sequence of exp
      ((and (pair? exp) (eq? (car exp) 'define)) (eval-define (cdr exp) env)) ; definition
      ((and (pair? exp) (eq? (car exp) 'set!)) (eval-assign! (cdr exp) env)) ; assignment!
      ((and (pair? exp) (eq? (car exp) 'lambda)) (list 'closure (cdr exp) env)) ; lambda (function definition).
      ;if exp is '(lambda (x y) (+ x y)) , then (cdr exp) is '((x y) (+ x y)) , so has the form ((formal-params) (body))
      ;so this will evaluate to '(closure ((formals) (body)) <env>)
      ((and (pair? exp) (eq? (car exp) 'cond)) (evcond (cdr exp) env)) ; cond; we defined evcond as a helper for this
      (else ; combination (not a special form)
       (appli (eval (car exp) env) (evlist (cdr exp) env)))
      )))

(define (appli proc args)
  (cond ((primitive? proc) (apply-primop (cadr proc) args))
        ((eq? (car proc) 'closure) (evseq (cdadr proc) (bind (caadr proc) args (caddr proc))))
        ))
; bind: create new frame with names of formals bound to the values of the arguments passed, and extend the base environment with that frame as the new innermost

;explanation note for appli: our conventional structure for a closure (non-primitive procedure) is:
;  '(closure ((formal-params) (body)) env)
;  so: 'closure is car
;  the cdr is: (((formals) (body)) env)
;  the car of the cdr is: ((formals) (body))
;  the cdr of the car of the cdr is: ((body))
;  the car of the cdr of the car of the cdr is: (body)
;  so the body is the cadadr of the proc
;  likewise, the (formals) is the car of the car of the cdr (caadr) of proc
;  and the cdr of the cdr is (env) so the car of the cdr of the cdr (caddr) is the env of the proc

;  note: we named it "appli" because drracket (our IDE) does not allow us to save the built-in definition of apply otherwise

;try to implement "delay" and "force" as "standard library" in the sense that we will define them as procedures:

;;;tests for environment & lookups
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
; this sort of explcitly indicates that the scoping will be lexical, as intended.  no chance of over-writing our nonlocal value.  good.
env1 ; both the inner x and the outer x are still available
(lookup-variable-value 'x env1)
(define-variable! 'a "new-a-value" env1) ; in contrast, when we redefine a variable in the innermost frame, we do not "shadow" its old value.  instead the old value is totally lost.  an alternative option would be to throw an error here.
env1
(lookup-variable-value 'a env1)
the-global-environment
;(first-frame the-empty-environment) ;apparently invalid to call frame selectors on empty environment
;(enclosing-environment the-empty-environment) ;apparently invalid to call frame selectors on empty environment

;tests for eval:
(eval '(define delay (lambda (expr) (lambda () expr))) the-global-environment)
(eval '(define force (lambda (expr) (expr))) the-global-environment)

(eval 5 the-global-environment)
(eval '(lambda (x) (+ x 5 )) the-global-environment)
(eval '(+ 4 5) the-global-environment) ; works!
(eval '(((lambda (x) (lambda (y) (+ x y ) ) ) 3 ) 4) the-global-environment) ;this illustrates that both procedure evaluation and higher-order-procedures (& closures/capturing free vars) works
;(eval '((lambda (x) (lambda (y) (+ x y ) ) ) 3) the-global-environment) ;partially applied version of the previous example.  showed the representation of the closure with the environment that has bound x to 3 and is waiting to bind y.
(eval '(define newvar "hello world") the-global-environment) ; works
(eval '(define newfun (lambda (x y ) (- x y))) the-global-environment) ; works
(eval '(newfun 5 7) the-global-environment) ; works; applied our user-defined function to values
(eval '(newfun (+ 2 3 ) (/ 14 2)) the-global-environment) ; nested eval-apply works
(eval '(newfun (newfun 9 4) (newfun 11 2)) the-global-environment) ; nested compound eval-apply works
(eval '(define x 3) the-global-environment)
(eval 'x the-global-environment)
(eval '(set! x "new-value-for-x") the-global-environment) ; assignment to simple values works
(eval 'x the-global-environment) ; assignment to simple values works
(eval '(set! x (+ 4 9)) the-global-environment)
(eval 'x the-global-environment) ; there appears to be a bug when we set! a variable in this manner
(eval '(define make_plus (lambda (x) (lambda (y) (+ x y)))) the-global-environment) ;test higher-order function
(eval '(define plus_three (make_plus 3)) the-global-environment)
(eval '(plus_three 4) the-global-environment)  ;higher-order function appears to work
;native scheme gives us the result of the evaluation of the value
;our version just gives us '(+ 4 9)
;fixed: now we get the evaluated value, which matches native mit-scheme
(eval '(define n 7) the-global-environment)
(eval '(plus_three n) the-global-environment)
(eval '(define multi-expr (lambda (x) (set! x (+ x 7)) (set! x (+ x 5)) x ) ) the-global-environment)
(eval '(multi-expr 4) the-global-environment) ; found bug in multi-expr compound procedure
(eval '(define sum-up (lambda (x) (cond ((= x 0) 0) (else (+ x (sum-up (- x 1))))))) the-global-environment)
(eval '(sum-up 5) the-global-environment) ; nice, recursion works
(eval '(define f1 (lambda (x) (define a 5) (define b (+ a x)) (+ a b ))) the-global-environment)
(eval '(f1 10) the-global-environment) ;produces 20.  note: flipping the definitions of a and b results in unbound variable error, same as in mit-scheme.
;(eval '(car (cdr (car (cdr multi-expr)))) the-global-environment) ; cadadr proc
;(eval '(cdr (car (cdr multi-expr))) the-global-environment) ; cdadr proc: this is what we probably need: a sequence of expr as the body of compound proc.
(eval '(define res 0) the-global-environment)
(eval '(cond ((< 4 5) "4 < 5" (set! res 1) "4<5 second expr") (else "else" (set! res 2) "else second expr")) the-global-environment) ;noticed
(eval 'res the-global-environment)
(eval '(cond ((> 4 5) "4 > 5" (set! res 1) "4>5 second expr") (else "else" (set! res 2) "else second expr")) the-global-environment) ;noticed
(eval 'res the-global-environment) ;verified that all expressions in list-of-expr in cond clauses are evaluated
(eval '(begin (define bvar 5) (set! bvar 6)) the-global-environment) ; test 'begin' expression
(eval 'bvar the-global-environment)
(eval '(define test_force (delay (+ 4 5 ) ) ) the-global-environment)
;(eval 'test_force the-global-environment)
(eval '(force test_force) the-global-environment) ; force / delay appear to work
(eval '(define test_delayed_def (delay (lambda (x y) (+ x y)))) the-global-environment) ;test delaying the definition of a function!
(eval 'test_delayed_def the-global-environment)
(eval '(define delayed_def (force test_delayed_def) ) the-global-environment) ;force the definition to occur
(eval '(delayed_def 7 8) the-global-environment) ;test the function
(eval '(define compose-with-mult (lambda (f0 multNum) (lambda (x) (* multNum (f0 x))))) the-global-environment)
;a function of two params (f0 and multNum) that returns a function of 1 parameter which returns the result of multiplying the number multNum by the result of f0 x
(eval '(define f (lambda (x) (- x 32))) the-global-environment)
(eval '(define f2c (compose-with-mult f (/ 5. 9.))) the-global-environment) ;compose with mult both takes in a function and returns a function
(eval '(f2c 212) the-global-environment) ;we created a fahrenheit to celsius conversion function by composing an addition with mult
(eval '(f2c 32 ) the-global-environment) 
(eval '(f2c -40) the-global-environment) ;works on the tests tried
(eval '(define make-localvarc (lambda () (define x 5) (lambda () (set! x (+ x 1)) x ) ) ) the-global-environment)
(eval '(define f_a (make-localvarc)) the-global-environment)
(eval '(define f_b (make-localvarc)) the-global-environment)
(eval '(f_a) the-global-environment)
(eval '(f_a) the-global-environment)
(eval '(f_a) the-global-environment)
(eval '(f_b) the-global-environment)
(eval '(f_b) the-global-environment)
(eval '(f_b) the-global-environment) ;make-localvarc made two closures which each have their own copy of the local variable.  this is correct.