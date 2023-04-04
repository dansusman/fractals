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
    [(< rand 0.02) (list 0 (- (* 0.25 y) 0.4))] ;; chosen 2%
    [(< rand 0.09) (list (- (* 0.035 x) (* 0.2 y) 0.09)
                         (+ (* 0.16 x) (* 0.04 y) 0.02))] ;; chosen 7%
    [(< rand 0.16) (list (+ (* -0.04 x) (* 0.2 y) 0.083)
                         (+ (* 0.16 x) (* 0.04 y) 0.12))] ;; chosen 7%
    [else (list (+ (* 0.95 x) (* 0.005 y) -0.002) ;; chosen the remaining 84%
                (+ (* -0.005 x) (* 0.93 y) 0.5))]))

(define (thelypteridaceae-fern r)
  (points (for*/fold ([p first-point]
                      [acc `()]
                      #:result acc)
                     ([i (in-range r)])
            (values (next-step p)
                    (cons p acc)))
          #:color "ForestGreen"
          #:size 2))

(plot (thelypteridaceae-fern 40000) #:x-min -3 #:x-max 3)

