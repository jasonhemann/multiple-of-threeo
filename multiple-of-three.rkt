#lang htdp/isl+
(require 2htdp/abstraction)
(require "void.rkt")

"Turn crashing functions into successful relations"

;; An attempted approach using the *SL languages so that the miniKanren "turn the crank" would
;; directly produce the 3 relations we are interested in rather than the style where you have an
;; extra boolean flag in the arguments list. Each n-ary function becomes an n+1-ary relation, so
;; to have an 1-ary relation seems to require a 0-arity function.
;;
;; Returning void is a way to reasonably treat your n-ary function like it wants to be an n-ary relation, b/c it has no "true return value".
;; This is intended to avoid turning that function into an n+1 ary relation, like you would do when you have a real return value.
;;
;; Right now these tests are broken
;;
;; Related to work on "turn crashing functions into successful relations" draft in ~/Documents
;;
(define (evens#-odds#%3=0 bbn)
  (match bbn
    ['(1 1) (void)]
    [(cons a (cons ad dd))
     (cond
       [(equal? a ad) (evens#-odds#%3=0 dd)]
       [(equal? (list a ad) '(1 0)) (evens#-odds#%3=2 dd)]
       [(equal? (list a ad) '(0 1)) (evens#-odds#%3=0 dd)])]))

(define (evens#-odds#%3=1 bbn)
  (match bbn
    ['(1) (void)]
    [(cons a (cons ad dd))
     (cond
       [(equal? a ad) (evens#-odds#%3=1 dd)]
       [(equal? (list a ad) '(1 0)) (evens#-odds#%3=0 dd)]
       [(equal? (list a ad) '(0 1)) (evens#-odds#%3=2 dd)])]))

(define (evens#-odds#%3=2 bbn)
  (match bbn
    ['(0 1) (void)]
    [(cons a (cons ad dd))
     (cond
       [(equal? a ad) (evens#-odds#%3=2 dd)]
       [(equal? (list a ad) '(1 0)) (evens#-odds#%3=1 dd)]
       [(equal? (list a ad) '(0 1)) (evens#-odds#%3=0 dd)])]))

(check-expect (evens#-odds#%3=0 '(1 1)) #t)

(check-expect (evens#-odds#%3=0 '(1 0 0 1)) #t)

(check-expect (evens#-odds#%3=0 '(0 1 1)) #t)

(check-expect (evens#-odds#%3=0 '(0 1 1 1 1)) #t)

(check-error (evens#-odds#%3=0 '(1 0 0 1 1 1 0 0 1)))

(check-error (evens#-odds#%3=0 '(1 1 0 1)))

(check-error (evens#-odds#%3=0 '(0 1 1 0 1)))
