#lang racket

(require minikanren)

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
            [(== a ad) (== ad add) (== add addd) (same-counto dddd)]
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
            [(== `(,a ,ad ,add ,addd) '(1 1 1 0)) (mod+2o dddd)]))]))

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
            [(== a ad) (== ad add) (== add addd) (mod+1o dddd)]
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
            [(== `(,a ,ad ,add ,addd) '(1 1 1 0)) (same-counto dddd)]))]))

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
            [(== a ad) (== ad add) (== add addd) (mod+2o dddd)]
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
            [(== `(,a ,ad ,add ,addd) '(1 1 1 0)) (mod+1o dddd)]))]))

(defrel (multiple-of-threeo-pm bn)
  (conde
   [(== bn '())]
   [(same-counto bn)]))

; (sort (map unbuild-num (run 10000 (q) (multiple-of-threeo-pm q))) <)
; (time (run 10000 (q) (multiple-of-threeo-pm q)))
; (time (run 1 (q) (multiple-of-threeo-pm (build-num 99999999))))