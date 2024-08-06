#lang racket

(require racket/struct)
(require racket/hash)
(require "util.rkt")

(provide
  (except-out (struct-out ordered-hash) ordered-hash)
  (rename-out [ordered-hash-ctor ordered-hash])
  ordered-hash-set
  ordered-hash-ref
  ordered-hash->list
  list->ordered-hash
  ordered-hash-empty?
  ordered-hash-count
  ordered-hash-keys
  ordered-hash-values
  ordered-hash-reorder
  ordered-hash-has-key?
  ordered-hash-remove
  ordered-hash-intersect
  ordered-hash-union
  ordered-hash-map
  ordered-hash-filter
  )

(struct ordered-hash
  (table keys)
  #:property prop:custom-write
  (make-constructor-style-printer
    (lambda _ 'ordered-hash)
    (lambda (self)
      (concat (ordered-hash->list self))))
  #:property prop:custom-print-quotable 'never
  #:transparent)

(define (make-oh . kvs)
  ;; O(n log n), but O(n) in most cases
  (define (keys kvs)
    (define set-k (mutable-set))
    (define (go kvs)
      (if (null? kvs)
        '()
        (if (null? (cdr kvs))
          (if (set-member? set-k (car kvs))
            '()
            `(,(car kvs)))
          (if (set-member? set-k (car kvs))
            (go (cddr kvs))
            (begin
              (set-add! set-k (car kvs))
              (cons (car kvs) (go (cddr kvs))))))))
    (go kvs))
  (ordered-hash (apply hash kvs) (keys kvs)))

(define-match-expander ordered-hash-ctor
  (lambda (stx)
    (syntax-case stx ()
      [(_ (k v) ...) #'(app ordered-hash->list (list (cons k v) ...))]))
  (lambda (stx)
    (syntax-case stx ()
      [(_ kvs ...)   #'(make-oh kvs ...)]
      [_ #'make-oh])))

(define (ordered-hash-set h k v)
  ;; O(1)
  (let ((already-there (ordered-hash-has-key? h k)))
    (ordered-hash (hash-set (ordered-hash-table h) k v)
                  (append
                    (if already-there '() `(,k))
                    (ordered-hash-keys h)))))

(define (ordered-hash-ref h k d)
  ;; O(1)
  (hash-ref (ordered-hash-table h) k d))

(define (ordered-hash-ref@ s h k)
  (ordered-hash-ref h k
    (lambda () (raise (make-exn:fail:contract
                        (format "bug in ~v.~n " s)
                        (current-continuation-marks))))))

(define (ordered-hash->list h)
  ;; O(n)
  (map 
    (lambda (k)
      `(,k ,(ordered-hash-ref@ "ordered-hash->list" h k)))
    (ordered-hash-keys h)))

(define (list->ordered-hash kvs)
  (-> (listof (list/c any/c any/c)) ordered-hash?)
  (apply make-oh (apply append kvs)))

(define (ordered-hash-empty? h)
  ;; O(1)
  (hash-empty? (ordered-hash-table h)))

(define (ordered-hash-count h)
  ;; O(1)
  (hash-count (ordered-hash-table h)))

(define (ordered-hash-values h)
  ;; O(n)
  (map 
    (lambda (k)
      (ordered-hash-ref@ "ordered-hash-values" h k))
    (ordered-hash-keys h)))

(define (ordered-hash-reorder h ks)
  ;; O(log n)
  (when (not (permutation? ks (ordered-hash-keys h)))
    (error (format "ordered-hash-reorder: not a permutation:~n~texpected permutation of: ~a~n~tgot: ~a" (ordered-hash-keys h) ks)))
  (ordered-hash (ordered-hash-table h) ks))

(define (ordered-hash-has-key? h k)
  ;; O(1)
  (hash-has-key? (ordered-hash-table h) k))

(define (ordered-hash-remove h k)
  ;; O(n)
  (ordered-hash (hash-remove (ordered-hash-table h) k)
                (filter (compose not (curry equal? k)) (ordered-hash-keys h))))

(define ((ordered-hash-duplicate-error name) key value1 value2)
  (error name "duplicate values for key ~e: ~e and ~e" key value1 value2))

(define (ordered-hash-intersect
         #:take-first [take-first #t]
         #:combine [combine #f]
         #:combine/key [combine/key
                        (if combine
                            (lambda (_ x y) (combine x y))
                            (ordered-hash-duplicate-error 'ordered-hash-intersect))]
				 h . hs)
  ;; O(n log n)
  (ordered-hash (if combine
                  (apply hash-intersect
                    (ordered-hash-table h)
                    (map ordered-hash-table hs)
                    #:combine combine
                    #:combine/key combine/key)
                  (apply hash-intersect
                    (ordered-hash-table h)
                    (map ordered-hash-table hs)
                    #:combine/key combine/key))
                (duplicates
                  (apply append
                    (ordered-hash-keys h)
                    (map ordered-hash-keys hs))
                  #:take-first take-first)))

;; A special version of nub that is O(n)
(define (nub-hash-keys hs take-first)
  (define set-k (mutable-set))
  (define (filter-keys h)
     (let ((ks (filter
                 (lambda (k) (not (set-member? set-k k)))
                 (ordered-hash-keys h))))
       (for ([k ks])
         (set-add! set-k k))
       ks))
  (define/match (go-take-first hs)
    [('()) '()]
    [(`(,h . ,hs))
     (let* ((hks (filter-keys h))
            (hsks (go-take-first hs)))
       (append hks hsks))])
  (define/match (go-take-last hs)
    [('()) '()]
    [(`(,h . ,hs))
     (let* ((hsks (go-take-last hs))
            (hks (filter-keys h)))
       (append hks hsks))])
  (if take-first
    (go-take-first hs)
    (go-take-last hs)))

(define (ordered-hash-union
         #:take-first [take-first #t]
         #:combine [combine #f]
         #:combine/key [combine/key
                        (if combine
                            (lambda (_ x y) (combine x y))
                            (ordered-hash-duplicate-error 'ordered-hash-union))]
				 . hs)
  ;; O(n log n)
  (ordered-hash (if combine
                  (apply hash-union
                    (map ordered-hash-table hs)
                    #:combine combine
                    #:combine/key combine/key)
                  (apply hash-union
                    (map ordered-hash-table hs)
                    #:combine/key combine/key))
                (nub-hash-keys hs take-first)))

(define (ordered-hash-map h f)
  ;; O(n)
  (map
    (lambda (k)
      (f k (ordered-hash-ref@ "ordered-hash-map" h k)))
    (ordered-hash-keys h)))

(define (ordered-hash-filter h p?)
  ;; O(n)
  (list->ordered-hash
    (map
      (lambda (k)
        `(,k ,(ordered-hash-ref h k)))
      (filter
        (lambda (k)
          (p? k (ordered-hash-ref@ "ordered-hash-filter" h k)))
        (ordered-hash-keys h)))))

(module+ test
  (require rackunit)
  (define ha (ordered-hash-ctor 'a 1))
  (define hb (ordered-hash-ctor 'b 2))
  (define hc (ordered-hash-ctor 'c 3))
  (define hd (ordered-hash-ctor 'd 4))
  (define hcba (ordered-hash-ctor 'c 3 'b 2 'a 1))
  (check-equal? (ordered-hash-set
                  (ordered-hash-set
                    (ordered-hash-set
                      (ordered-hash-ctor)
                      'a 1)
                    'b 2)
                  'c 3)
                hcba)
  (check-not-equal? (ordered-hash-set
                      (ordered-hash-set
                        (ordered-hash-set
                          (ordered-hash-ctor)
                          'c 3)
                        'b 2)
                      'a 1)
                    hcba)
  (check-equal? (ordered-hash-ref hcba 'a #f) 1)
  (check-equal? (ordered-hash-ref hcba 'd #f) #f)
  (check-equal? (ordered-hash->list hcba) '((c 3) (b 2) (a 1)))
  (check-equal? (ordered-hash-keys hcba) '(c b a))
  (check-equal? (ordered-hash-values hcba) '(3 2 1))
  (check-true  (ordered-hash-has-key? hcba 'b))
  (check-false (ordered-hash-has-key? hcba 'd))
  (check-equal? (ordered-hash-remove
                  (ordered-hash-remove
                    hcba 'c) 'b)
                ha)
  (check-equal? (ordered-hash-remove hc 'c)
                (ordered-hash-ctor))
  (define (drop-first _ v) v)
  (define (take-first v _) v)
  (check-equal? (ordered-hash-intersect hcba (ordered-hash-union ha hd) #:combine drop-first)
                ha)
  (check-equal? (ordered-hash-intersect (ordered-hash-union hd hcba) hcba #:combine drop-first)
                hcba)
  (check-equal? (ordered-hash-union hb hcba #:take-first #t #:combine take-first)
                (ordered-hash-ctor 'b 2 'c 3 'a 1))
  (check-equal? (ordered-hash-union hb hcba #:take-first #f #:combine drop-first)
                (ordered-hash-ctor 'c 3 'b 2 'a 1))
  (check-equal? (ordered-hash-union hc hb ha)
                hcba)
  (check-equal? (ordered-hash-union hc (ordered-hash-union hb ha))
                hcba)
  (check-equal? (ordered-hash-union (ordered-hash-union hc hb) ha)
                hcba)
  (check-equal? (ordered-hash-union hb hb hb #:combine drop-first)
                hb)
  (check-equal? (ordered-hash-map hcba (lambda (k v) (+ 1 v)))
                '(4 3 2))
  )
