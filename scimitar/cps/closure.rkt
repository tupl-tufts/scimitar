#lang racket

(require "../contract-utils.rkt")
(require "../util.rkt")
(require "../val.rkt")
(require "grammar.rkt")
(require "util.rkt")

(provide
  (prefix-out cps- closure))

(define/match (extend-app g ys e)
  [(_ _ (cps-app f args))
   (cps-app f (if (equal? f g) (sym-val-append args ys) args))]
  [(_ _ (cps-fix fs cont))
   (cps-fix fs (extend-app g ys cont))]
  [(_ _ (cps-primop op args out cont))
   (cps-primop op args out (extend-app g ys cont))]
  [(_ _ (cps-switch op v conts))
   (cps-switch op v (map (curry extend-app g ys) conts))])

(define call-graph? (listof (list/c symbol? symbol?)))

(define/contract (call-graph e)
  (-> cps-cexp? call-graph?)
  (define/match (calls e)
    [((cps-app f _))
     `(,f)]
    [((cps-fix _ cont))
     (calls cont)]
    [((cps-primop _ _ _ cont))
     (calls cont)]
    [((cps-switch _ _ conts))
     (concat (map calls conts))])
  (define/match (go e)
    [((? cps-app?))
     '()]
    [((cps-fix fs cont))
     (apply append
            (go cont)
            (map (match-lambda
                   [`(,f ,_ ,b)
                    (let ((cs (calls b)))
                      (append
                        (map (curry list f) cs)
                        (go b)))])
                 fs))]
    [((cps-primop _ _ _ cont))
     (go cont)]
    [((cps-switch _ _ conts))
     (concat (map go conts))])
  (go e))

(define/contract (callers f cg)
  (-> symbol? call-graph? call-graph?)
  (filter (compose (curry equal? f) cadr) cg))

(define/contract (closure e)
  (-> cps-cexp? cps-cexp?)
  (define cg (call-graph e))
  (define fns (fix-names e))
  (define/match (go fgys e)
    [(_ (? cps-app?))
     (values '() e)]
    [(`(,f ,g ,ys) (cps-fix fs cont))
     (match (assoc f fs)
       [`(,_ ,xs ,b)
        (let* ((b (extend-app g ys b))
               (zs (sym-val-list->sym-val
                     (remove* `(,@fns ,@(flatten xs)) (free-in b))))
               (xs (sym-val-append xs zs)))
          (values (if (null? zs)
                    '()
                    (map (curryr snoc zs) (callers f cg)))
                  (cps-fix (map (lambda (fxsb)
                                  (if (equal? f (car fxsb))
                                    `(,f ,xs ,b)
                                    fxsb))
                                fs)
                           cont)))]
       [#f (let-values ([(qs fs) (for/lists (qs fs) ([f fs])
                                   (match f
                                     [`(,g ,xs ,b)
                                      (let-values ([(q b) (go fgys b)])
                                        (values q `(,g ,xs ,b)))]))]
                        [(q cont) (go fgys cont)])
             (values (apply append q qs)
                     (cps-fix fs cont)))])]
    [(_ (cps-primop op args out cont))
     (let-values ([(q cont) (go fgys cont)])
       (values q (cps-primop op args out cont)))]
    [(_ (cps-switch op v conts))
     (let-values ([(qs conts) (map-values (curry go fgys) conts)])
       (values (concat qs) (cps-switch op v conts)))])
  (work go (map (curryr snoc '()) cg) e))
