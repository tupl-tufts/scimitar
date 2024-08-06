#lang racket

(provide
  log-random
  total-memory-size
  bucket-count
  cycles-total cycle-start
  shrink-factor
  grow-factor
  convergence-window)

(define (log-random lo [hi #f] [base 2])
  (let ((lo (if hi lo 0)) (hi (if hi hi lo)))
    (let* ((delta (- hi lo))
           (delta-s (* 100000 delta)))
      (when (or (<= hi lo) (< lo 0))
        (error (format "log-random can't pick a value between ~v and ~v" lo hi)))
      (inexact->exact (floor (+ lo (* delta (- 1 (/ (log (random 1 delta-s)) (log delta-s))))))))))

;; parameters:
(define total-memory-size
  (make-parameter (expt 2 12) ;; 2^24 = 16MB
    (invariant-assertion
      (-> natural? natural?)
      identity)))
(define bucket-count
  (make-parameter 6
    (invariant-assertion
      (-> natural? natural?)
      identity)))
;; total rounds we're testing
(define cycles-total
  (make-parameter 10
    (invariant-assertion
      (-> natural? natural?)
      identity)))
;; initial cycle
(define cycle-start
  (make-parameter 1
    (invariant-assertion
      (-> natural? natural?)
      identity)))
(define shrink-factor
  (make-parameter 0.9
    (invariant-assertion
      (-> natural? natural?)
      identity)))
(define grow-factor
  (make-parameter (/ 1 (shrink-factor))
    (invariant-assertion
      (-> natural? natural?)
      identity)))
;; the upper and lower bounds for convergence
(define convergence-window
  (make-parameter 6
    (invariant-assertion
      (-> natural? natural?)
      identity)))
