#lang rosette

(require "common.rkt")
(require (except-in scimitar/profile #%app))

(random-seed 123456)

;; (2^1 = 2)  (2^2 = 4)  etc
(define bucket-sizes (build-list (bucket-count) (lambda (i) (expt 2 (+ 1 i)))))
;; equal number of slots per bucket
(define equal-slots (inexact->exact (floor (/ (total-memory-size) (apply + bucket-sizes)))))
;; each bucket starts with 524416 total slots
(define total (build-list (bucket-count) (lambda _ equal-slots)))
;; buckets start randomly between 29970 and 269727 slots full
(define used (build-list (bucket-count) (lambda _ (log-random (floor (/ equal-slots 10)) (+ 1 (floor (* 9 (/ equal-slots 10))))))))

(require (only-in scimitar/profile time-expr))

(define (exact-gravitate-down xs [epsilon 1e-6])
  (map (lambda (e) (exact-floor (+ e epsilon))) xs))

(define-syntax-rule (sum i n m b)
  (foldl (lambda (i acc) (+ b acc)) 0 (range n m)))

(define (reallocate-buckets)
  (define (go cycle total used)
    (displayln (format "In cycle ~v, the slot sizes, totals, and usages are:" cycle))
    (for ([s bucket-sizes]
          [t total]
          [u used])
      (displayln (format "                       ~v    ~v    ~v" s t u)))
    (when (<= cycle (cycles-total))
      (let* (;; the number of allocations that were waiting to be serviced at the beginning of this cycle
             (pending (map (curry max 0) (map - used total)))
             ;; allocations and deallocations are randomly generated; deallocations is at most allocation
             (incremental-deallocations (build-list (bucket-count) (lambda (i) (let ((u (list-ref used i))) (- u (log-random (+ 1 u)))))))
             (incremental-allocations (build-list (bucket-count) (lambda (i) (log-random (+ 1 (/ (total-memory-size) (* 4 (list-ref bucket-sizes i))))))))
             ;; net allocations this cycle
             (new-allocs (map - incremental-allocations incremental-deallocations))
             ;; used slots at the end of this cycle
             (new-used (map + used new-allocs))
             (desired-memory-usage (apply + (map * bucket-sizes new-used)))
             (new-total
               (if (> desired-memory-usage (total-memory-size))
                 ; this could be a compromise case where the total wait time is
                 ; minimized, with the constraint that all existing allocations
                 ; are preserved.  That way, allocations would go where they are
                 ; most needed.  This is justified by the fact that more rarely
                 ; allocated objects are unlikely to even be hit in a given
                 ; window, so we can steal from them.  But that's way hard to
                 ; figure out for my addled brain right now, so skip it. (because
                 ; when there are squeued allocs left after this cycle's
                 ; deallocs, the average wait times have to be decreased by the
                 ; amount that the oldest ones have waited but to do that you'd
                 ; have to have a vector of the wait times for each individual
                 ; pending message)
                 (error (format "Desired memory usage ~v exceeded available memory ~v" desired-memory-usage (total-memory-size)))
                 (begin
                   (define-symbolic* new-memory-usage real?)
                   (define-symbolic* new-total real? #:length (bucket-count))
                   (define-symbolic* load-balance integer? #:length (bucket-count))
                   (time-expr problem-definition
                     (begin
                       (for ([i (range (bucket-count))])
                         (begin
                           (assert (<= 0 (list-ref load-balance i)))
                           (assert (>= 1 (list-ref load-balance i)))
                           (assert (<= 0 (list-ref new-total i)))))
                       (assert (= new-memory-usage
                                  (sum i 0 (bucket-count)
                                    (* (list-ref bucket-sizes i)
                                       (list-ref new-total i)))))
                       (assert (= (floor (/ (bucket-count) 2))
                                  (sum i 0 (bucket-count)
                                    (list-ref load-balance i))))
                       (for ([i (range (bucket-count))])
                         (if (> (list-ref pending i) 0)
                           ;; the logic of this constraint is that
                           ;; we want to have a few slack slots
                           ;; because this might be a busy bucket
                           (assert (>= (list-ref new-total i)
                                       (+ (list-ref new-used i)
                                          (* (grow-factor) (list-ref pending i)))))
                           (begin
                             ;; prefer removing from buckets that have been unused
                             ;; overall as compared to the size the last time.
                             (assert (>= (list-ref new-total i)
                                         (* (shrink-factor) (list-ref total i))))
                             (assert (>= (list-ref new-total i)
                                         (if (> (list-ref new-allocs i) 0)
                                           (+ (list-ref new-used i)
                                              (* (list-ref load-balance i)
                                                 (list-ref new-allocs i)))
                                           (list-ref new-used i)))))))))
                   (let* ((s (time-expr
                               (optimize
                                 #:minimize `(,new-memory-usage)
                                 #:guarantee
                                 #t
                                 ))))
                     (exact-gravitate-down (evaluate new-total s)))))))
        (if (and (andmap > (map - total new-total) (build-list (bucket-count) (lambda _ -6)))
                 (andmap > (build-list (bucket-count) (lambda _ 6)) (map - total new-total)))
          new-total
          (go (+ 1 cycle) new-total new-used)))))
  (go (cycle-start) total used))

(reallocate-buckets)
