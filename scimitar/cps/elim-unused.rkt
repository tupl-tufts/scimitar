#lang racket

(require "../contract-utils.rkt")
(require "../util.rkt")
(require "grammar.rkt")
(require "util.rkt")

(provide
  (prefix-out cps- elim-unused))

(define (enclosing f e)
  (define/match (go fs e)
    [(_ (cps-app g _))
     (if (equal? g f)
       fs
       '())]
    [(_ (cps-fix gs cont))
     (apply set-union
            (go fs cont)
            (map (match-lambda
                   [`(,g ,_ ,b)
                    (go (cons g fs) b)])
                 gs))]
    [(_ (cps-primop _ _ _ cont))
     (go fs cont)]
    [(_ (cps-switch _ v conts))
     (apply set-union (map (curry go fs) conts))])
  (go '() e))

(define (sym-val-prune ix v)
  (if (null? ix) '() (tree-prune v ix)))

(struct
  elim-params
  (f)
  #:transparent)

(struct elim-args
  (f ixs)
  #:transparent)

(define/contract (elim-unused e)
  (-> cps-cexp? cps-cexp?)
  (define enclosing-e (curryr enclosing e))
  (define/match (sym-val-fixup vs)
    [(`(,v)) (sym-val-fixup v)]
    [((? list?)) (map sym-val-fixup vs)]
    [(_) vs])
  (define/match (go w e)
    [((elim-args f ixs) (cps-app g args))
     #:when (equal? g f)
     (values '() (cps-app g (sym-val-fixup (foldr sym-val-prune args ixs))))]
    [(_ (? cps-app?))
     (values '() e)]
    [((elim-params f) (cps-fix fs cont))
     #:when (assoc f fs)
     (match (assoc f fs)
       [`(,_ ,xs ,b)
        (let* ((fvs (free-in b))
               (ys (remove* fvs (flatten xs)))
               (ixs (map (curry tree-index-of xs) ys)))
          (if (null? ixs)
            (values '() e)
            (values (cons (elim-args f ixs) (map elim-params (enclosing-e f)))
                    (let ((fs (map (match-lambda
                                     [`(,g ,xs ,b)
                                      `(,g ,(if (equal? g f) (sym-val-fixup (foldr sym-val-prune xs ixs)) xs) ,b)])
                                   fs)))
                      (cps-fix fs cont)))))])]
    [(_ (cps-fix fs cont))
     (let-values ([(qs fs) (for/lists (qs fs) ([f fs])
                             (match f
                               [`(,g ,xs ,b)
                                (let-values ([(q b) (go w b)])
                                  (values q `(,g ,xs ,b)))]))]
                  [(q cont) (go w cont)])
       (values (concat (snoc qs q)) (cps-fix fs cont)))]
    [(_ (cps-primop op args out cont))
     (let-values ([(q cont) (go w cont)])
       (values q (cps-primop op args out cont)))]
    [(_ (cps-switch op v conts))
     (let-values ([(qs conts) (map-values (curry go w) conts)])
       (values (concat qs)
               (cps-switch op v conts)))])
  (work go (map elim-params (fix-names e)) e))
