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
;; Abstraction over the concrete relations via function and letrec
;;
;; build-defrel could take 4 arguments, but I can't come up with a coherent name
;; or type for the operator that you get by doing that.


;; Because plain old miniKanren does the interleave at `conde`s,
;; in the real miniKnaren implementation the lambdag@ is superfluous--so just the same as a lambda
(define-syntax-rule (lambdag@ args g) (lambda args g))

;; This lambdag@ definition does not work for the actual miniKanren implementation
;; because it has a more sophisticated notion of "state" than s/c.
;;
;; The lambdag@s are necessary so that we preserve the same interleave behavior that
;; we got with the 3 distinct defrels---namely an interleave at each defrel---in microKanren

;; (define-syntax-rule (lambdag@ args g)
;;   (lambda args
;;     (lambda ()
;;       (lambda (s/c)
;;         (g s/c)))))

(define (build-body base-n self one-up one-down)
  (lambda (bn)
	(conde
	  [(== bn base-n)]
	  [(fresh (a ad dd)
		 (== `(,a ,ad . ,dd) bn)
		 (conde
		   [(== a ad) (self dd)]
		   [(== `(,a ,ad) '(1 0)) (one-up dd)]
		   [(== `(,a ,ad) '(0 1)) (one-down dd)]))])))

(define same-counto
  (letrec
	[(div3r2 (lambdag@ (bn) ((build-body '(1) div3r2 div3r0 div3r1) bn)))
	 (div3r1 (lambdag@ (bn) ((build-body '(0 1) div3r1 div3r2 div3r0) bn)))
	 (div3r0 (lambdag@ (bn) ((build-body '(1 1) div3r0 div3r1 div3r2) bn)))]
	div3r0))

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
