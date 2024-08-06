#lang racket

(require "grammar.rkt")
(require "graph.rkt")
(require scimitar)
(require scimitar/ty)
(require scimitar/util)
(require scimitar/vec)

(provide
  compile-module)

(inline-amount 8)

(define (compile-module p)
  (define ps (pipes p))
  (define pixs (enumerate-with (compose swap list) (map car ps)))
  (define count-ps (length ps))
  (define lookup (compose second (curryr assoc pixs) car))
  (define (dopad v)
    (vec-pad v (map (curry max (inline-amount)) (vec-shape v))))
  (define (cast ty v #:pad [pad #t])
    (let ((v (if pad (dopad v) v)))
      (vec-dynamic-cast (apply  ty (vec-shape v)) v)))
  (define (get mode js)
    (let* ((valss (map (lambda (j) (map lookup (mode p (car j)))) js))
           (count-js (length js))
           (count-vals  (dopad (vec-dense (map length valss))))
           (pipe-valss  (dopad (vec-dense valss))))
      (values count-js count-vals pipe-valss)))
  (let*-values ([(js) (junctions p)]
                [(srcs) (filter (compose source? cadr) js)]
                [(snks) (filter (compose sink?   cadr) js)]
                [(js) (filter-cadr (lambda (j) (and (not (sink? j)) (not (source? j)))) js)]
                [(count-srcs count-srcouts pipe-srcoutss) (get out srcs)]
                [(count-snks count-snkins  pipe-snkinss)  (get in  snks)]
                [(count-js   count-ins   pipe-inss)  (get in  js)]
                [(count-js   count-outs  pipe-outss) (get out js)]
                [(src-flows)  (cast nnegty (vec-dense (map (compose source-flow cadr) srcs)))]
                [(pipe-flows) (cast nnegty (vec-dense (map (compose pipe-flow   cadr) ps)) #:pad #f)])
    (scimitar
      (optimum-ref sink-outs
        (maximize sink-outs (sink-outs pipes)
          ;; all source output flows sums equal the source flow
          (for ([j (range (racket count-srcs))])
            (assert (>=
              (vec-ref (racket src-flows) j)
              (sum ([i (range (: (vec-ref (racket count-srcouts) j) (vecty (interval 0 (inline-amount)) '())))])
                (vec-ref (: pipes (nnegty count-ps))
                         (vec-ref (racket pipe-srcoutss) `(,i ,j)))))))
          ;; all junction input and output flows sums are equal
          (for ([j (range (racket count-js))])
            (assert (=
              (sum ([i (range (: (vec-ref (racket count-ins) j) (vecty (interval 0 (inline-amount)) '())))])
                (vec-ref (: pipes (nnegty count-ps))
                                   (vec-ref (racket pipe-inss)  `(,i ,j))))
              (sum ([i (range (: (vec-ref (racket count-outs) j) (vecty (interval 0 (inline-amount)) '())))])
                (vec-ref (: pipes (nnegty count-ps))
                                   (vec-ref (racket pipe-outss) `(,i ,j)))))))
          ;; all sink input flows sums are equal to the sink outputs
          (for ([j (range (racket count-snks))])
            (assert (=
              (sum ([i (range (: (vec-ref (racket count-snkins) j) (vecty (interval 0 (inline-amount)) '())))])
                (vec-ref (: pipes (nnegty count-ps))
                                   (vec-ref (racket pipe-snkinss) `(,i ,j))))
              (vec-ref (: sink-outs (nnegty count-snks)) j))))
          ;; all pipe flows must be greater than their pipe inputs
          (for ([i (range (racket count-ps))])
            (assert (<= ; there seems to be an IR(?) bug here with using (- pipes) on this line instead of addition below
              0 ; the bug seems to be using this (vec-ref ...) passed to a poly that (= result (.* -1 input))
              (+ (vec-ref (: pipes (nnegty count-ps)) i)
                           (vec-ref (racket (vec-project pipe-flows '(0) `(,count-ps))) i)))))
          (for ([i (range (racket count-ps))])
            (assert (<=
              (vec-ref (: pipes (nnegty count-ps)) i)
              (vec-ref (racket (vec-project pipe-flows '(0) `(,count-ps))) i)))))))))
