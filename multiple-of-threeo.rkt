#lang racket
(require (except-in rackunit fail))
(require minikanren)

;;
;; Jason Hemann and Brysen Pfingsten
;;
;; Computes the non-zero backwards-binary numbers where the sum of odd
;; indices minus the sum of the even indices, mod 3 is zero.
;;
;; Relies on two help relations that track other remainders.
;;
;; Abstraction over the concrete relations via macro


(define (make-operator base-n self one-up one-down)
  (λ (bn)
	(conde
	  [(== bn base-n)]
	  [(fresh (a ad dd)
		 (== `(,a ,ad . ,dd) bn)
		 (conde
		   [(== a ad) (self dd)]
		   [(== `(,a ,ad) '(1 0)) (one-up dd)]
		   [(== `(,a ,ad) '(0 1)) (one-down dd)]))])))

(define mod+2o      (lambda (bn) ((make-operator '(1) mod+2o same-counto mod+1o) bn)))
(define mod+1o      (lambda (bn) ((make-operator '(0 1) mod+1o mod+2o same-counto) bn)))
(define same-counto (lambda (bn) ((make-operator '(1 1) same-counto mod+1o mod+2o) bn)))

#;(defrel (same-counto bn)
  (conde
    [(== bn `(1 1))]
    [(fresh (a ad dd)
       (== `(,a ,ad . ,dd) bn)
       (conde
         [(== a ad) (same-counto dd)]
         [(== `(,a ,ad) '(1 0)) (mod+1o dd)]
         [(== `(,a ,ad) '(0 1)) (mod+2o dd)]))]))

#;(defrel (mod+1o bn)
  (conde
    [(== bn `(0 1))]
    [(fresh (a ad dd)
       (== `(,a ,ad . ,dd) bn)
       (conde
         [(== a ad) (mod+1o dd)]
         [(== `(,a ,ad) '(1 0)) (mod+2o dd)]
         [(== `(,a ,ad) '(0 1)) (same-counto dd)]))]))

#;(defrel (mod+2o bn)
  (conde
    [(== bn '(1))]
    [(fresh (a ad dd)
       (== `(,a ,ad . ,dd) bn)
       (conde
         [(== a ad) (mod+2o dd)]
         [(== `(,a ,ad) '(1 0)) (same-counto dd)]
         [(== `(,a ,ad) '(0 1)) (mod+1o dd)]))]))

#|

(time (begin (run 10000 (q) (same-counto q)) #t))
cpu time: 86 real time: 88 gc time: 15
#t

|#

(defrel (multiple-of-threeo bn)
  (conde
    [(== bn '())]
    [(same-counto bn)]))

(check-equal?
  (run 10 (q) (same-counto q))
  '((1 1)                  ;; 3 
    (_.0 _.0 1 1)          ;; 12 v 15
    (1 0 0 1)              ;; 9
    (0 1 1)                ;; 6
    (_.0 _.0 _.1 _.1 1 1)  ;; 96 (+ 3 v 0) (+ 12 v 0)
    (_.0 _.0 1 0 0 1)      ;; 36 + (3 v 0)
    (_.0 _.0 0 1 1)        ;; 24 + (3 v 0)
    (1 0 _.0 _.0 0 1)      ;; 33 + (12 v 0)
    (0 1 _.0 _.0 1)        ;; 18 + (12 v 0)
    (_.0 _.0 _.1 _.1 _.2 _.2 1 1))) ;; 192 + (3 v 0) + (12 v 0) + (48 v 0)
