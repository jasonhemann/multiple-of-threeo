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
            [(== a ad) (== ad add) (== add addd)  (same-counto dddd)] ;; (_.0 _.0 _.0 _.0)
            [(== a ad) (== add addd)              (same-counto dddd)] ;; (_.0 _.0 _.1 _.1)
            [(== a addd) (== ad add)              (same-counto dddd)] ;; (_.0 _.1 _.1 _.0)

            [(=/= a add) (== ad 1) (== addd 1)    (mod+1o dddd)] ;; (_.0 1 _.1 1) (=/= ._0 ._1)
            [(=/= ad addd) (== a 0) (== add 0)    (mod+1o dddd)] ;; (0 _.0 0 ._1) (=/= ._0 ._1)  
            [(== `(,a ,ad ,add ,addd) '(1 0 1 0)) (mod+1o dddd)]

            [(=/= a add) (== ad 0) (== addd 0)    (mod+2o dddd)] ;; (_.0 0 _.1 0) (=/= ._0 ._1)
            [(=/= ad addd) (== a 1) (== add 1)    (mod+2o dddd)] ;; (1 _.0 1 ._1) (=/= ._0 ._1)  
            [(== `(,a ,ad ,add ,addd) '(0 1 0 1)) (mod+2o dddd)]
            ))]))

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
            [(== a ad) (== ad add) (== add addd)  (mod+1o dddd)] ;; (_.0 _.0 _.0 _.0)
            [(== a ad) (== add addd)              (mod+1o dddd)] ;; (_.0 _.0 _.1 _.1)
            [(== a addd) (== ad add)              (mod+1o dddd)] ;; (_.0 _.1 _.1 _.0)

            [(=/= a add) (== ad 1) (== addd 1)    (mod+2o dddd)] ;; (_.0 1 _.1 1) (=/= ._0 ._1)
            [(=/= ad addd) (== a 0) (== add 0)    (mod+2o dddd)] ;; (0 _.0 0 ._1) (=/= ._0 ._1)  
            [(== `(,a ,ad ,add ,addd) '(1 0 1 0)) (mod+2o dddd)]

            [(=/= a add) (== ad 0) (== addd 0)    (same-counto dddd)] ;; (_.0 0 _.1 0) (=/= ._0 ._1)
            [(=/= ad addd) (== a 1) (== add 1)    (same-counto dddd)] ;; (1 _.0 1 ._1) (=/= ._0 ._1)  
            [(== `(,a ,ad ,add ,addd) '(0 1 0 1)) (same-counto dddd)]
            ))]))

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
            [(== a ad) (== ad add) (== add addd)  (mod+2o dddd)] ;; (_.0 _.0 _.0 _.0)
            [(== a ad) (== add addd)              (mod+2o dddd)] ;; (_.0 _.0 _.1 _.1)
            [(== a addd) (== ad add)              (mod+2o dddd)] ;; (_.0 _.1 _.1 _.0)

            [(=/= a add) (== ad 1) (== addd 1)    (same-counto dddd)] ;; (_.0 1 _.1 1) (=/= ._0 ._1)
            [(=/= ad addd) (== a 0) (== add 0)    (same-counto dddd)] ;; (0 _.0 0 ._1) (=/= ._0 ._1)  
            [(== `(,a ,ad ,add ,addd) '(1 0 1 0)) (same-counto dddd)]

            [(=/= a add) (== ad 0) (== addd 0)    (mod+1o dddd)] ;; (_.0 0 _.1 0) (=/= ._0 ._1)
            [(=/= ad addd) (== a 1) (== add 1)    (mod+1o dddd)] ;; (1 _.0 1 ._1) (=/= ._0 ._1)  
            [(== `(,a ,ad ,add ,addd) '(0 1 0 1)) (mod+1o dddd)]
            ))]))

(defrel (multiple-of-threeo-pm bn)
  (conde
   [(== bn '())]
   [(same-counto bn)]))

; (sort (map unbuild-num (run 10000 (q) (multiple-of-threeo-pm q))) <)
; (time (run 10000 (q) (multiple-of-threeo-pm q)))
; (time (run 1 (q) (multiple-of-threeo-pm (build-num 99999999))))

;; > (andmap (λ (s) (andmap (λ (n) (= (remainder n 3) 0)) s)) (map (λ (s) (map unbuild-num s)) (map all-solutions (run 100 (q) (multiple-of-threeo-pm q)))))
;; #t