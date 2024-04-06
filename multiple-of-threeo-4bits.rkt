#lang racket
(require (except-in rackunit fail))
(require minikanren)
(require minikanren/numbers)

;;
;; Jason Hemann and Brysen Pfingsten
;;
;; Computes the non-zero backwards-binary numbers where the sum of odd
;; indices minus the sum of the even indices, mod 3 is zero.
;;
;; Relies on two help relations that track other remainders.
;;

(define (unbuild-num a-lon)
  (if (empty? a-lon)
      0
      (first (foldl (λ (a-num accum)
                      (if (= a-num 1)
                          (list (+ (expt 2 (second accum))
                                   (first accum))
                                (add1 (second accum)))
                          (list (first accum)
                                (add1 (second accum)))))
                    (list 0 0)
                    a-lon))))

(defrel (same-counto bn)
  (conde
   [(== bn `(1 1 1 1))]
   [(== bn `(0 0 1 1))]
   [(== bn `(1 0 0 1))]
   [(== bn `(0 1 1))]
   [(== bn `(1 1))]
   [(fresh (a ad add addd dddd)
           (== `(,a ,ad ,add ,addd . ,dddd) bn)
           (conde
            [(== `(,a ,ad ,add ,addd) '(0 0 0 0)) (same-counto dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 0 0 1)) (mod+1o dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 0 1 0)) (mod+2o dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 0 1 1)) (same-counto dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 1 0 0)) (mod+1o dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 1 0 1)) (mod+2o dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 1 1 0)) (same-counto dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 1 1 1)) (mod+1o dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 0 0 0)) (mod+2o dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 0 0 1)) (same-counto dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 0 1 0)) (mod+1o dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 0 1 1)) (mod+2o dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 1 0 0)) (same-counto dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 1 0 1)) (mod+1o dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 1 1 0)) (mod+2o dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 1 1 1)) (same-counto dddd)]))]))

(defrel (mod+1o bn)
  (conde
   [(== bn `(1 0 1 1))]
   [(== bn `(0 1 0 1))]
   [(== bn `(1 1 1))]
   [(== bn `(0 0 1))]
   [(== bn `(1))]
   [(fresh (a ad add addd dddd)
           (== `(,a ,ad ,add ,addd . ,dddd) bn)
           (conde
            [(== `(,a ,ad ,add ,addd) '(0 0 0 0)) (mod+1o dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 0 0 1)) (mod+2o dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 0 1 0)) (same-counto dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 0 1 1)) (mod+1o dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 1 0 0)) (mod+2o dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 1 0 1)) (same-counto dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 1 1 0)) (mod+1o dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 1 1 1)) (mod+2o dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 0 0 0)) (same-counto dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 0 0 1)) (mod+1o dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 0 1 0)) (mod+2o dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 0 1 1)) (same-counto dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 1 0 0)) (mod+1o dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 1 0 1)) (mod+2o dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 1 1 0)) (same-counto dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 1 1 1)) (mod+1o dddd)]))]))

(defrel (mod+2o bn)
  (conde
   [(== bn `(0 1))]
   [(== bn `(0 1 1 1))]
   [(== bn `(1 1 0 1))]
   [(== bn `(0 0 0 1))]
   [(== bn `(1 0 1))]
   [(fresh (a ad add addd dddd)
           (== `(,a ,ad ,add ,addd . ,dddd) bn)
           (conde
            [(== `(,a ,ad ,add ,addd) '(0 0 0 0)) (mod+2o dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 0 0 1)) (same-counto dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 0 1 0)) (mod+1o dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 0 1 1)) (mod+2o dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 1 0 0)) (same-counto dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 1 0 1)) (mod+1o dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 1 1 0)) (mod+2o dddd)]
            [(== `(,a ,ad ,add ,addd) '(0 1 1 1)) (same-counto dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 0 0 0)) (mod+1o dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 0 0 1)) (mod+2o dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 0 1 0)) (same-counto dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 0 1 1)) (mod+1o dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 1 0 0)) (mod+2o dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 1 0 1)) (same-counto dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 1 1 0)) (mod+1o dddd)]
            [(== `(,a ,ad ,add ,addd) '(1 1 1 1)) (mod+2o dddd)]))]))

(defrel (multiple-of-threeo-pm bn)
  (conde
   [(== bn '())]
   [(same-counto bn)]))

; (sort (map unbuild-num (run 10000 (q) (multiple-of-threeo-pm q))) <)
; (time (run 10000 (q) (multiple-of-threeo-pm q)) (void))
; (time (run 1 (q) (multiple-of-threeo-pm (build-num 99999999))))
