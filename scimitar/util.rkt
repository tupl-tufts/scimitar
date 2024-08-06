#lang racket

(require "contract-utils.rkt")

(provide
  on/c
  equal/c
  any<?
  compare
  range-class
  snoc
  concat
  zip
  unzip
  swap
  replicate
  intersperse
  only
  init
  omit
  scanl
  foldl1
  foldr1
  work
  sort-using
  enumerate-with
  permutation?
  nub
  duplicates
  map-car map-cadr map-caddr map-values
  filter-car filter-cadr
  remove*-by-car
  sum
  pad-to/null
  extend-symbol
  call-with-temp-file
  tree-indexes-of
  tree-index-of
  tree-ref
  tree-prune
  tree-map
  tree-fold
  tree-preorder-zip)

(define/contract (on/c a c)
  (-> procedure? flat-contract? flat-contract?)
  (flat-named-contract
    `(on/c ,a ,c)
    (lambda (v)
      (property/c a (c (a v))))))

(define/contract (equal/c x)
  (-> any/c flat-contract?)
  (flat-named-contract
    (if (or (cons? x)
            (null? x)
            (symbol? x))
      `(equal/c ',x)
      `(equal/c ,x))
    (curry equal? x)))

(define (any<? a b)
  (match* (a b)
    [((? symbol?)  (? symbol?))  (symbol<? a b)]
    [((? symbol?)  _)            #t]
    [(_            (? symbol?))  #f]
    [((? number?)  (? number?))  (< a b)]
    [((? number?)  _)            #t]
    [(_            (? number?))  #f]
    [(#f #t)                     #t]
    [((? boolean?) (? boolean?)) #f]
    [((? boolean?) _)            #t]
    [(_            (? boolean?)) #f]
    [((? char?)    (? char?))    (char<? a b)]
    [((? char?)    _)            #t]
    [(_            (? char?))    #f]
    [((? string?)  (? string?))  (string<? a b)]
    [((? string?)  _)            #t]
    [(_            (? string?))  #f]
    [((? null?)    (? null?))    #f]
    [((? null?)    _)            #t]
    [(_            (? null?))    #f]
    [(`(,ah . ,at) `(,bh . ,bt)) (or (any<? ah bh) (and (not (any<? bh ah)) (any<? at bt)))]
    [((? pair?)    _)            #t]
    [(_            (? pair?))    #f]
    [(_ _) (error (format "No comparison defined between objects ~v and ~v" a b))]))

(define (compare n m)
  ;contract commented out for performance
  ;(-> number? number? (or/c 'LT 'EQ 'GT))
  (cond [(< n m) 'LT]
        [(= n m) 'EQ]
        [(> n m) 'GT]))

(define/contract (range-class inf sup)
  (->i ([inf_ number?]
        [sup_ (inf_) (and/c number? (>=/c inf_))])
       [result symbol?])
  (cond
    [(and (member inf '(0 1))  (member sup '(0 1)))  'binary]
    [(and (exact-integer? inf) (exact-integer? sup)) 'integer]
    [else 'continuous]))

(define (snoc xs x)
  (foldr cons (list x) xs))

(define (concat xss)
  (foldr append '() xss))

(define (zip . xss)
  (if (null? xss)
    '()
    (if (ormap null? xss)
      '()
      (cons (map car xss)
            (apply zip (map cdr xss))))))

(define (unzip xs #:default [default '()])
  (if (null? xs)
    default
    (apply zip xs)))

(define (swap xy)
  `(,(second xy) ,(first xy)))

(define (replicate n v)
  (if (<= n 0)
    '()
    (cons v (replicate (- n 1) v))))

(define (intersperse i vs)
  (if (null? vs)
    vs
    (foldl
      (lambda (v vs)
        (append vs `(,i ,v)))
      (take vs 1)
      (cdr vs))))

(define/contract (only xs)
  (-> (list/c any/c) any/c)
  (car xs))

(define/contract (init xs)
  (-> (*list/c any/c any/c) (listof any/c))
  (drop-right xs 1))

(define (omit xs n)
  (define (go ys m)
    (if (null? ys)
      (error (format "Index ~a is not in the list ~a" n xs))
      (if (= 0 m)
        (cdr ys)
        (cons (car ys) (go (cdr ys) (- m 1))))))
  (go xs n))

(define (scanl f a vs)
  (reverse (foldl (lambda (v a) (cons (f v (car a)) a)) (list a) vs)))

(define/contract (foldl1 f vs)
  (-> procedure? (*list/c any/c any/c) any/c)
  (foldl f (car vs) (cdr vs)))

(define/contract (foldr1 f vs)
  (-> procedure? (*list/c any/c any/c) any/c)
  (foldr f (last vs) (init vs)))

(define (work f q acc)
  (if (null? q)
    acc
    (let-values ([(p acc) (f (car q) acc)])
      (work f (append (cdr q) p) acc))))

(define (sort-using xs ys lt? #:key [key identity])
  (map cadr (sort (zip ys xs) lt? #:key (compose key car))))

(define (enumerate-with f xs)
  (define j 0)
  (map (lambda (x)
         (set! j (add1 j))
         (f (sub1 j) x))
       xs))

(define (permutation? xs ys)
  (equal? (sort xs any<?) (sort ys any<?)))

(define (nub xs #:take-first [take-first #t])
  (map cdr
    (sort
      (foldr (lambda (ox ys)
               (if (null? ys)
                 `(,ox)
                 (if (equal? (cdr ox) (cdar ys))
                   (cons (if take-first ox (car ys)) (cdr ys))
                   (cons ox ys))))
             '()
             (sort (enumerate-with cons xs) any<? #:key cdr))
      < #:key car)))

(define (duplicates xs #:take-first [take-first #t])
  (map cdr
    (sort
      (cadr
        (foldr (lambda (ox ys-dups)
                 (let ((ys (car ys-dups))
                       (dups (cadr ys-dups)))
                   (if (null? ys)
                     `((,ox) ())
                     (if (equal? (cdr ox) (cdar ys))
                       (if (null? dups)
                         (if take-first
                           `(,(cons ox (cdr ys)) (,ox))
                           `(,ys (,(car ys))))
                         (if (equal? (cdr ox) (cdar dups))
                           (if take-first
                             `(,(cons ox (cdr ys)) ,(cons ox (cdr dups)))
                             ys-dups)
                           (if take-first
                             `(,(cons ox (cdr ys)) ,(cons ox dups))
                             `(,ys ,(cons (car ys) dups)))))
                       `(,(cons ox ys) ,dups)))))
               '(() ())
               (sort (enumerate-with cons xs) any<? #:key cdr)))
      < #:key car)))

(define (map-car f xss)
  (map (lambda (xs) (cons (f (car xs)) (cdr xs))) xss))

(define (map-cadr f xss)
  (map (match-lambda [`(,x ,y . ,ys) `(,x ,(f y) . ,ys)]) xss))

(define (map-caddr f xss)
  (map (match-lambda [`(,x ,y ,z . ,zs) `(,x ,y ,(f z) . ,zs)]) xss))

(define (map-values f ss . sss)
  (define (wrap s . ss)
    (call-with-values (lambda () (apply f s ss)) list))
  (apply values (unzip (apply map wrap ss sss))))

(define (filter-car f xss)
  (filter (compose f car) xss))

(define (filter-cadr f xss)
  (filter (compose f cadr) xss))

(define (remove*-by-car xs yzs)
  (filter-car (lambda (y) (not (member y xs))) yzs))

(define (sum xs) (foldl + 0 xs))

(define (pad-to/null n v xs)
  (append xs (replicate (- n (length xs)) v)))

(define (extend-symbol root ext)
  (string->symbol
    (string-append (symbol->string root) "-" (symbol->string ext))))

(define (call-with-temp-file base f #:extn [extn #f])
  (define (mk-name base)
    (if extn
      (string-append base "." extn)
      base))
  (define (go name)
    (define e (box #f))
    (with-handlers
      ([exn:fail:filesystem?
         (lambda _ (go (mk-name (symbol->string (gensym base)))))])
      (call-with-output-file* name
        (lambda (out)
          (with-handlers
            ([exn:fail? (curry set-box! e)])
            (f out name)))))
    (delete-file name)
    (when (unbox e)
      (raise (unbox e))))
  (go (mk-name base)))

(define (tree-indexes-of root v)
  (define (go node)
    (if (equal? node v)
      '(())
      (if (list? node)
        (concat
          (enumerate-with
            (lambda (i node)
              (map (curry cons i) (go node)))
            node))
        '())))
  (go root))

(define (tree-index-of root v)
  (car (tree-indexes-of root v)))

(define (tree-ref root pos)
  (define (go node ix)
    (if (null? ix)
      node
      (if (and (list? node) (< (car ix) (length node)))
        (go (list-ref node (car ix)) (cdr ix))
        (error (format "tree-ref: index too large for tree~n  index: ~v~n  in: ~v" pos root)))))
  (go root pos))

(define (tree-prune root pos)
  (when (null? pos)
    (error "tree-prune: can't prune the root"))
  (define (go node ix)
    (if (and (list? node) (< (car ix) (length node)))
      (if (null? (cdr ix))
        (let-values ([(head tail) (split-at node (car ix))])
          (append head (cdr tail)))
        (list-set node (car ix) (go (list-ref node (car ix)) (cdr ix))))
      (error (format "tree-prune: index too large for tree~n  index: ~v~n  in: ~v" pos root))))
  (go root pos))

(define (tree-map f . roots)
  (define (go . nodes)
    (if (andmap list? nodes)
      (apply map go nodes)
      (apply f nodes)))
  (apply go roots))

(define (tree-fold f acc . roots)
  (define (go . nodes)
    (match nodes
      [(list (? list? nodes) ... acc)
       (apply foldr go acc nodes)]
      [(list nodes ... acc)
       (apply (curryr f acc) nodes)]))
  (apply (curryr go acc) roots))

(define (tree-preorder-zip r1 r2)
  (define (go t1 t2)
    (cond
      [(and (list? t1) (list? t2) (= (length t1) (length t2)))
       (concat (map go t1 t2))]
      [(and (list? t1) (list? t2))
       (error (format "tree-preorder-zip: tree nodes have unequal subtree counts~n  in: ~v~n      ~v" r1 r2))]
      [else `((,t1 ,t2))]))
  (go r1 r2))
