#lang racket

(require "common.rkt")
(require scimitar/ty)
(require scimitar/vec)
(require scimitar)

;; Malloc sizes and buckets

(random-seed 123456)
;(debug #t)

;; assume steady state (warm up will obviously be different)
;; assume next cycle to be the same as this cycle
;; assume all deallocations happen at the beginning of a cycle
;; assume allocations are evenly spaced through a cycle, and pending allocations are queued
;; assume there are no allocations in the remaining space because that's too difficult to model

;; (2^1 = 2)  (2^2 = 4)  etc
(define bucket-sizes (vec-dense (build-list (bucket-count) (lambda (i) (expt 2 (+ 1 i))))))
;; equal number of slots per bucket
(define equal-slots (inexact->exact (floor (/ (total-memory-size) (apply + (vec-vs bucket-sizes))))))
;; each bucket starts with 524416 total slots
(define total (vec-dense (build-list (bucket-count) (lambda _ equal-slots))))
;; buckets start randomly between 29970 and 269727 slots full
(define used (vec-dense (build-list (bucket-count) (lambda _ (log-random (floor (/ equal-slots 10)) (+ 1 (floor (* 9 (/ equal-slots 10)))))))))

(define (reallocate-buckets)
  (define (go cycle total used)
    (displayln (format "In cycle ~v, the slot sizes, totals, and usages are:" cycle))
    (for ([s (vec-unpack bucket-sizes)]
          [t (vec-unpack total)]
          [u (vec-unpack used)])
      (displayln (format "                       ~v    ~v    ~v" s t u)))
    (when (<= cycle (cycles-total))
      (let* (;; the number of allocations that were waiting to be serviced at the beginning of this cycle
             (pending (vec-max (vec-0 `(,(bucket-count))) (vec- used total)))
             ;; allocations and deallocations are randomly generated; deallocations is at most allocation
             (incremental-deallocations (vec-dense (build-list (bucket-count) (lambda (i) (let ((u (vec! used `(,i)))) (- u (log-random (+ 1 u))))))))
             (incremental-allocations (vec-dense (build-list (bucket-count) (lambda (i) (log-random (+ 1 (/ (total-memory-size) (* 4 (vec! bucket-sizes `(,i))))))))))
             ;; net allocations this cycle
             (new-allocs (vec- incremental-allocations incremental-deallocations))
             ;; used slots at the end of this cycle
             (new-used (vec+ used new-allocs))
             (desired-memory-usage (vec-dot (vec-1 `(,(bucket-count))) (vec-hadamard bucket-sizes new-used)))
             (new-total (vec-exact-gravitate-down
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
                 (optimum-ref new-total
                   (minimize new-memory-usage (new-total new-memory-usage load-balance)
                     (assert
                       (= new-memory-usage
                          (sum ([i (range (racket (bucket-count)))])
                            (* (vec-ref (racket bucket-sizes) i)
                               (vec-ref (: new-total (nnegty (bucket-count))) i)))))
                     (assert
                       (= (racket (floor (/ (bucket-count) 2)))
                          (sum ([i (range (racket (bucket-count)))])
                            (vec-ref (: load-balance (bitty (bucket-count))) i))))
                     (for ([i (range (racket (bucket-count)))])
                       (if (> (vec-ref (racket pending) i) 0)
                         ;; the logic of this constraint is that we want to have a few slack slots because this might be a busy bucket
                         (assert
                           (>= (vec-ref (: new-total (nnegty (bucket-count))) i)
                               (+ (vec-ref (racket new-used) i)
                                  (* (racket (grow-factor))
                                     (vec-ref (racket pending) i)))))
                         (begin
                           ;; prefer removing from buckets that have been unused
                           ;; overall as compared to the size the last time.
                           (assert
                             (>= (vec-ref (: new-total (nnegty (bucket-count))) i)
                                 (* (racket (shrink-factor)) (vec-ref (racket total) i))))
                           (assert
                             (>= (vec-ref (: new-total (nnegty (bucket-count))) i)
                                 (if (> (vec-ref (racket new-allocs) i) 0)
                                   (+ (vec-ref (racket new-used) i)
                                      (* (vec-ref (: load-balance (bitty (bucket-count))) i)
                                         (vec-ref (racket new-allocs) i)))
                                   (vec-ref (racket new-used) i)))))))))))))
        (if (and (vec> (vec- total new-total) (vec.* (- (convergence-window)) (vec-1 `(,(bucket-count)))))
                 (vec> (vec.* (convergence-window) (vec-1 `(,(bucket-count)))) (vec- total new-total)))
          new-total
          (go (+ 1 cycle) new-total new-used)))))
  (go (cycle-start) total used))

(reallocate-buckets)
