#lang htdp/isl+
(require 2htdp/abstraction)

"Turn crashing functions into succesful relations"

;; An alternative would be to use #<void>, b/c you want some unit
;; value, rather than a boolean.

(define (evens#-odds#%3=1 bbn)
  (match bbn
    ['(1) #t]
    [(cons a (cons ad dd))
     (cond
       [(equal? a ad) (evens#-odds#%3=1 dd)]
       [(equal? (list a ad) '(1 0)) (evens#-odds#%3=0 dd)]
       [(equal? (list a ad) '(0 1)) (evens#-odds#%3=2 dd)])]))

(define (evens#-odds#%3=2 bbn)
  (match bbn
    ['(0 1) #t]
    [(cons a (cons ad dd))
     (cond
       [(equal? a ad) (evens#-odds#%3=2 dd)]
       [(equal? (list a ad) '(1 0)) (evens#-odds#%3=1 dd)]
       [(equal? (list a ad) '(0 1)) (evens#-odds#%3=0 dd)])]))

(define (evens#-odds#%3=0 bbn)
  (match bbn
    ['(1 1) #t]
    [(cons a (cons ad dd))
     (cond
       [(equal? a ad) (evens#-odds#%3=0 dd)]
       [(equal? (list a ad) '(1 0)) (evens#-odds#%3=2 dd)]
       [(equal? (list a ad) '(0 1)) (evens#-odds#%3=0 dd)])]))

(check-expect (evens#-odds#%3=0 '(1 1)) #t)

(check-expect (evens#-odds#%3=0 '(1 0 0 1)) #t)

(check-expect (evens#-odds#%3=0 '(0 1 1)) #t)

(check-error (λ () (evens#-odds#%3=0 '(1 1 0 1))) "match: no matching clause for")

(check-error (λ () (evens#-odds#%3=0 '(0 1 1 0 1))) "match: no matching clause for")
