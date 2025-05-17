#lang racket

(require minikanren)

(defrel (same-counto bn)
  (conde
   [(== bn `(1 0 0 1))]
   [(== bn `(0 1 1))]
   [(== bn `(1 1))]
   [(fresh (b) (== bn `(,b ,b 1 1)))]
   [(fresh (a ad add addd dddd)
           (== `(,a ,ad ,add ,addd . ,dddd) bn)
           (conde
            [(== a ad) (== ad add) (== add addd) (same-counto dddd)] ;; (_.0 _.0 _.0 _.0)
            [(== a ad) (== add addd)             (same-counto dddd)] ;; (_.0 _.0 _.1 _.1)
            [(== a addd) (== ad add)             (same-counto dddd)] ;; (_.0 _.1 _.1 _.0)
            [(== a ad)
             (conde
              [(== `(,add ,addd) '(0 1))         (mod+1o dddd)]      ;; (_.0 _.0 0 1)
              [(== `(,add ,addd) '(1 0))         (mod+2o dddd)])]    ;; (_.0 _.0 1 0)
            [(== `(,a ,ad) '(0 1))
             (conde
              [(== add addd)                     (mod+1o dddd)]      ;; (0 1 _.0 _.0)
              [(== `(,add ,addd) '(0 1))         (mod+2o dddd)])]    ;; (0 1 1 0)
            [(== `(,a ,ad) '(1 0))
             (conde
              [(== add addd)                     (mod+2o dddd)]      ;; (1 0 _.0 _.0)
              [(== `(,add ,addd) '(1 0))         (mod+1o dddd)])]    ;; (1 0 1 0)
            ))]))

(defrel (mod+1o bn)
  (conde
   [(== bn `(1 0 1 1))]
   [(== bn `(0 1 0 1))]
   [(== bn `(1))]
   [(fresh (b) (== bn `(,b ,b 1)))]
   [(fresh (a ad add addd dddd)
           (== `(,a ,ad ,add ,addd . ,dddd) bn)
           (conde
            [(== a ad) (== ad add) (== add addd) (mod+1o dddd)]
            [(== a ad) (== add addd)             (mod+1o dddd)]
            [(== a addd) (== ad add)             (mod+1o dddd)]
            [(== a ad)
             (conde
              [(== `(,add ,addd) '(0 1))         (mod+2o dddd)]
              [(== `(,add ,addd) '(1 0))         (same-counto dddd)])]
            [(== `(,a ,ad) '(0 1))
             (conde
              [(== add addd)                     (mod+2o dddd)]
              [(== `(,add ,addd) '(0 1))         (same-counto dddd)])]
            [(== `(,a ,ad) '(1 0))
             (conde
              [(== add addd)                     (same-counto dddd)] 
              [(== `(,add ,addd) '(1 0))         (mod+2o dddd)])]  
            ))]))

(defrel (mod+2o bn)
  (conde
   [(== bn `(0 1))]
   [(== bn `(0 1 1 1))]
   [(== bn `(1 0 1))]
   [(fresh (b) (== bn `(,b ,b 0 1)))]
   [(fresh (a ad add addd dddd)
           (== `(,a ,ad ,add ,addd . ,dddd) bn)
           (conde
            [(== a ad) (== ad add) (== add addd) (mod+2o dddd)]
            [(== a ad) (== add addd)             (mod+2o dddd)]
            [(== a addd) (== ad add)             (mod+2o dddd)]
            [(== a ad)
             (conde
              [(== `(,add ,addd) '(0 1))         (same-counto dddd)]
              [(== `(,add ,addd) '(1 0))         (mod+1o dddd)])]
            [(== `(,a ,ad) '(0 1))
             (conde
              [(== add addd)                     (same-counto dddd)]
              [(== `(,add ,addd) '(0 1))         (mod+1o dddd)])]
            [(== `(,a ,ad) '(1 0))
             (conde
              [(== add addd)                     (mod+1o dddd)] 
              [(== `(,add ,addd) '(1 0))         (same-counto dddd)])]  
            ))]))

(defrel (multiple-of-threeo-pm bn)
  (conde
   ; [(== bn '())]
   [(same-counto bn)]))

(define (unbuild-num b)
  (if (null? b)
      0
      (+ (car b) (* 2 (unbuild-num (cdr b))))))


(define (all-solutions b)
  (local [(define (find-first-reified num)
            (local [(define REIFIEDS (filter (λ (i) (not (or (equal? i 0)
                                                             (equal? i 1))))
                                             num))]
              (if (empty? REIFIEDS)
                  '()
                  (first REIFIEDS))))
          
          (define (helper queue)
            (local [(define FIRST-REIFIED (find-first-reified (first queue)))]
              (cond
                [(empty? FIRST-REIFIED)
                 queue]
                [else (helper (append
                               (rest queue)
                               (list (map (λ (i) (if (equal? i FIRST-REIFIED)
                                                     0
                                                     i))
                                          (first queue)))
                               (list (map (λ (i) (if (equal? i FIRST-REIFIED)
                                                     1
                                                     i))
                                          (first queue)))))])))]
    (helper (if (not (or (equal? (last b) 0)
                         (equal? (last b) 1)))
                (list (map (λ (i) (if (equal? (last b) i)
                                1
                                i))
                     b))
                (list b)))))
; (sort (map unbuild-num (run 10000 (q) (multiple-of-threeo-pm q))) <)
; (time (run 10000 (q) (multiple-of-threeo-pm q)))
; (time (run 1 (q) (multiple-of-threeo-pm (build-num 99999999))))

;; > (andmap (λ (s) (andmap (λ (n) (= (remainder n 3) 0)) s)) (map (λ (s) (map unbuild-num s)) (map all-solutions (run 1000 (q) (multiple-of-threeo-pm q)))))
;; #t
