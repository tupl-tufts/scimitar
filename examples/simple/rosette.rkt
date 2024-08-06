#lang rosette

(require (only-in scimitar/profile time-expr))


  (letrec
    ((go (lambda (prev upper)
           (if (= prev upper)
             upper
             (go upper
                 (let ()
                   (define-symbolic* in integer?)
                   (define-symbolic* result real?)
                   (time-expr problem-definition
                     (letrec
                       ((foo (lambda (x)
                               (if (> x 3)
                                 (if (< x upper)
                                   (+ x 2)
                                   3)
                                 (* 2 x)))))
                       (assert
                         (= result
                            (foo in)))))
                   (let ((s (time-expr (optimize
                                #:maximize `(,result)
                                #:guarantee
                                #t
                                ))))
                     (println (list (evaluate in s)
                                    (evaluate result s)))
                     (evaluate in s))))))))
    (go 8 7))
