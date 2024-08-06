#lang rosette

(require (except-in scimitar/profile #%app))

(define count-srcs 1)
(define count-ins (vector 1 1 1))
(define count-js 3)
(define count-outs (vector 2 1 1))
(define count-ps 5)
(define count-snkins (vector 2))
(define count-snks 1)
(define count-srcouts (vector 1))
(define pipe-flows (vector 10 5 5 4 6))
(define pipe-inss (vector (vector 0 1 2)))
(define pipe-outss (vector (vector 1 3 4) (vector 2 0 0)))
(define pipe-snkinss (vector (vector 3) (vector 4)))
(define pipe-srcoutss  (vector (vector 0)))
(define src-flows (vector 12))

(define-symbolic* sink-outs real? #:length count-snks)
(define-symbolic* pipes real? #:length count-ps)

(define-syntax-rule (sum i n m b)
  (foldl (lambda (i acc) (+ b acc)) 0 (range n m)))

(time-expr problem-definition (let ()
(for ([j (range count-srcs)])
  (assert (>= (vector-ref src-flows j)
              (sum i 0 (vector-ref count-srcouts j)
                   (list-ref pipes (vector-ref (vector-ref pipe-srcoutss i) j))))))
(for ([j (range count-js)])
  (assert (= (sum i 0 (vector-ref count-ins j)
                   (list-ref pipes (vector-ref (vector-ref pipe-inss i) j)))
             (sum i 0 (vector-ref count-outs j)
                  (list-ref pipes (vector-ref (vector-ref pipe-outss i) j))))))
(for ([j (range count-snks)])
  (assert (= (sum i 0 (vector-ref count-snkins j)
                   (list-ref pipes (vector-ref (vector-ref pipe-snkinss i) j)))
             (list-ref sink-outs j))))
(for ([i (range count-ps)])
  (assert (<= 0
              (+ (list-ref pipes i)
                 (vector-ref pipe-flows i)))))
(for ([i (range count-ps)])
  (assert (<= (list-ref pipes i)
              (vector-ref pipe-flows i))))
))

(let ((s (time-expr (optimize
             #:maximize sink-outs
             #:guarantee
             #t
             ))))
  (println (evaluate sink-outs s)))
