#lang racket

(require plot plot/no-gui)

(define first-point `(0 0))

;; following the "Computer generation" section of
;; https://en.wikipedia.org/wiki/Barnsley_fern
(define (next-step p)
  (define x (first p))
  (define y (second p))
  (define rand (random))
  (cond
    [(< rand 0.01) (list 0 (* 0.16 y))] ;; chosen 1%
    [(< rand 0.08) (list (- (* 0.2 x) (* 0.26 y))
                         (+ (* 0.23 x) (* 0.22 y) 1.6))] ;; chosen 7%
    [(< rand 0.15) (list (+ (* -0.15 x) (* 0.28 y))
                         (+ (* 0.26 x) (* 0.24 y) 0.44))] ;; chosen 7%
    [else (list (+ (* 0.85 x) (* 0.04 y)) ;; chosen the remaining 85%
                (+ (* -0.04 x) (* 0.85 y) 1.6))]))

(define (barnsley-fern r)
  (points (for*/fold ([p first-point]
                      [acc `()]
                      #:result acc)
                     ([i (in-range r)])
            (values (next-step p)
                    (cons p acc)))
          #:color "ForestGreen"
          #:size 2))

(plot (barnsley-fern 40000) #:x-min -3 #:x-max 3)
