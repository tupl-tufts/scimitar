#lang racket

(require "../contract-utils.rkt")
(require "../util.rkt")
(require "../val.rkt")
(require "grammar.rkt")
(require "util.rkt")

(provide
  (prefix-out cps- flatten))

(define/contract (eta-abstract fs c)
  (-> (listof symbol?) cps-cexp? (list/c (listof cps-fun-entry?) cps-app?))
  (match c
    [(? cps-app?)
     (list '() c)]
    [(? (or/c cps-primop? cps-switch?))
     (let* ((f (gensym (if (cps-primop? c) 'primop 'switch)))
            (xs (sym-val-list->sym-val (remove* fs (free-in c)))))
       (list `((,f ,xs ,c)) (cps-app f xs)))]))

(define (fmap-fix f e)
  (if (cps-fix? e)
    (f e)
    e))

(define (map-fix f es)
  (map (curry fmap-fix f) es))

(define/contract (flatten e)
  (-> cps-cexp? cps-cexp?)
  (define fxs (box (fix-names e)))
  (define (box-cons x xs)
    (set-box! xs (cons x (unbox xs))))
  (define (add-fix-name f)
    (box-cons f fxs))
  (define/match (go e)
    [((? cps-app?))
     e]
    [((cps-fix fs cont))
     (let* ((cont (go cont))
            (conts (map (compose go caddr) fs))
            (conts (cons cont conts))
            (fixes (filter cps-fix? conts))
            (gs (concat (map cps-fix-fs fixes)))
            (conts (map-fix cps-fix-cont conts))
            (cont (car conts))
            (conts (cdr conts))
            (fs (map (match-lambda*
                       [`((,f ,xs ,_) ,c) `(,f ,xs ,c)])
                     fs
                     conts)))
       (cps-fix (append fs gs) cont))]
    [((cps-primop op args out cont))
     (match-let* ((conts `(,(go cont)))
                  (fixes (filter cps-fix? conts))
                  (fs (concat (map cps-fix-fs fixes)))
                  (conts (map-fix cps-fix-cont conts))
                  (cont (car conts))
                  [`(,gs ,cont) (eta-abstract (unbox fxs) cont)]
                  (_ (for-each (compose add-fix-name car) gs))
                  (hs (append fs gs))
                  (p (cps-primop op args out cont)))
       (if (null? hs)
         p
         (cps-fix hs p)))]
    [((cps-switch op v conts))
     (let* ((conts (map go conts))
            (fixes (filter cps-fix? conts))
            (fs (concat (map cps-fix-fs fixes)))
            (conts (map-fix cps-fix-cont conts))
            (gss-conts (unzip (map (curry eta-abstract (unbox fxs)) conts)))
            (gs (concat (first gss-conts)))
            (_ (for-each (compose add-fix-name car) gs))
            (conts (second gss-conts))
            (hs (append fs gs))
            (s (cps-switch op v conts)))
       (if (null? hs)
         s
         (cps-fix hs s)))])
  (go e))
