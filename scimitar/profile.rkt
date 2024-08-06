#lang racket

(require (for-syntax syntax/parse))

(provide time-expr time-fun)
(provide (rename-out (app #%app)))

(define-syntax (time-expr-base stx)
  (syntax-parse stx
    [(_ x f e)
     #'(let* ((name (syntax->datum #'x))
              (start (* (current-inexact-milliseconds) 1000))
              (finalize
                (lambda ()
                  (let* ((stop (* (current-inexact-milliseconds) 1000))
                         (delta (- stop start)))
                    (display (format "~a: elapsed time: ~v~n" name delta))
                    (f delta))))
              (result
                (call-with-values
                  (lambda ()
                    (with-handlers
                      ([exn:fail?
                         (lambda (err)
                           (finalize)
                           (raise err))])
                      e))
                  list)))
         (finalize)
         (apply values result))]))

(define-syntax (time-expr stx)
  (syntax-parse stx
    [(_ e)
     #'(time-expr-base e (lambda _ (void)) e)]
    [(_ x e)
     #'(time-expr-base x (lambda _ (void)) e)]
    [(_ x f e)
     #'(time-expr-base x f e)]))

(define-syntax (time-fun stx)
  (syntax-parse stx
    [(_ f) #'(make-keyword-procedure
               (lambda (kws kw-args . rest)
                 (time-expr-base f
                   (lambda _ (void))
                   (keyword-apply f kws kw-args rest))))]))

(define-syntax (app stx)
  (syntax-parse stx
    [(_ f es ...) #'(#%app (time-fun f) es ...)]))
