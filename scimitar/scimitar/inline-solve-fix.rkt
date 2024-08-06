#lang racket

(require "../contract-utils.rkt")
(require (only-in "../params.rkt" inline-amount))
(require "grammar.rkt")
(require "util.rkt")
(require "../util.rkt")

(provide
  (prefix-out scimitar- inline-solve-fix))

(define (with-default default val)
  (or val default))

(define/contract (inline-solve-fix e)
  (-> scimitar-expr? scimitar-expr?)
  (define (go-map es [cadr? #f])
    (define project-elem (if cadr? cadar car))
    (define embed-elem (if cadr? (lambda (v) (list (caar es) v)) identity))
    (and (pair? es)
         (match* ((go (project-elem es)) (go-map (cdr es) cadr?))
           [(#f #f) #f]
           [( v vs)
            (cons (with-default (car es) (embed-elem v))
                  (with-default (cdr es) vs))])))
  (define (go-map-cadr es)
    (go-map es #t))
  (define/match (go e)
    [((? scimitar-solve?)) (error "solve within solve not supported")]
    [((? scimitar-var?)) #f]
    [((scimitar-typed e ty))
     (let ((e (go e)))
       (and e (scimitar-typed e ty)))]
    [((? scimitar-val?)) #f]
    [((? scimitar-poly?)) #f]
    [((scimitar-tuple es))
     (let ((es (go-map es)))
       (and es (scimitar-tuple es)))]
    [((scimitar-switch op e es))
     (match* ((go e) (go-map es))
       [(#f #f) #f]
       [( v vs)
        (scimitar-switch op
          (with-default e v)
          (with-default es vs))])]
    [((scimitar-app f args))
     (match* ((go f) (go args))
       [(#f #f) #f]
       [(g ps)
        (scimitar-app
          (with-default f g)
          (with-default args ps))])]
    [((scimitar-lambda ps body))
     (let ((body (go body)))
       (and body (scimitar-lambda ps body)))]
    [((scimitar-fix fs (scimitar-typed e _)))
     (let* ((gs (map (match-lambda
                       [`(,f ,(scimitar-typed (scimitar-lambda ps (scimitar-typed _ bty)) ty))
                        ;; This looks convoluted but is needed to be type correct.
                        `(,f ,(scimitar-typed (scimitar-lambda ps (scimitar-typed (scimitar-infeasible) bty)) ty))])
                     fs))
            (e (subst gs (foldr (lambda (_ e) (subst fs e)) e (range (inline-amount))))))
       (or (go e) e))]
    [((scimitar-let bs e))
     (match* ((go-map-cadr bs) (go e))
       [(#f #f) #f]
       [(bs2 v)
        (scimitar-let
          (with-default bs bs2)
          (with-default e v))])]
    [((scimitar-let* bs e))
     (match* ((go-map-cadr bs) (go e))
       [(#f #f) #f]
       [(bs2 v)
        (scimitar-let*
          (with-default bs bs2)
          (with-default e v))])]
    [((scimitar-begin es))
     (let ((es (go-map es)))
       (and es (scimitar-begin es)))]
    [((scimitar-infeasible)) #f])
  (scimitar-fmap
    scimitar-solve?
    (match-lambda
      [(and e (scimitar-solve dir res obj locals ps))
       (let ((ps (go-map ps)))
         (if ps
           (scimitar-solve dir res obj locals ps)
           e))])
    e))
