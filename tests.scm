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
;(first-frame the-empty-environment) ;apparently invalid to call frame selectors on empty environment
;(enclosing-environment the-empty-environment) ;apparently invalid to call frame selectors on empty environment
