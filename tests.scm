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
(eval '(define nestDelay (delay (delay (+ 4 5)))) the-global-environment) ;test nested delay
(eval '(define oneDelay (delay (+ 4 5))) the-global-environment)
;(eval 'oneDelay the-global-environment) ;also verified that single delay also acts this way - if we evaluate it we can see that it looks like the body has already been evaluated to '9'
(eval '(force (force nestDelay)) the-global-environment) ;nested delay appears to work, although! when we only force once, it does appear that the (+ 4 5) has already been evaluated.  so I'm not sure if this could be a "true" force/delay mechanism.  it just acts like one?

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

(eval '(define memo-delay-add (mdelay (+ 8 9))) the-global-environment) ; test memoized delay
(eval '(force memo-delay-add) the-global-environment) ; run the delayed procedure
(eval '(force memo-delay-add) the-global-environment) ; direct memoized return