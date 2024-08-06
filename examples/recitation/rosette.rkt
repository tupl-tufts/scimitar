#lang rosette

(require "common.rkt")
(require (except-in scimitar/profile #%app))

;; Norman wanted to write a program that maximized the number of students
;; in a recitation, which would require using a variable as the upper loop
;; bound.  He argued that he wanted some usability benefit, i.e., hand Scimitar
;; or a Scimitar program to Richard for 105 and he could just run with it.

(define-syntax-rule (sum ([i (n m)]) b)
  (foldl (lambda (i acc) (+ b acc)) 0 (range n m)))

(define-syntax-rule (define-symbolic-tensor* name type? dims)
  (define name
    (let ()
      (define (make-symbolic-tensor ds)
        (if (null? (cdr ds))
          (begin (define-symbolic* name type? #:length (car ds)) name)
          (build-list (car ds) (lambda _ (make-symbolic-tensor (cdr ds))))))
      (make-symbolic-tensor dims))))

(define (lists-ref l ds)
  (if (null? (cdr ds))
    (list-ref l (car ds))
    (lists-ref (list-ref l (car ds)) (cdr ds))))

(define-symbolic* rec-count real?)
(define-symbolic-tensor* recitation-scheduled integer? `(,(+ 1 (time-slots))))
(define-symbolic-tensor* recitation-attendance integer? `(,(time-slots)))
(define-symbolic-tensor* student-attendance integer? `(,(student-count) ,(time-slots)))

(define-syntax-rule (sym-for i n m body ...)
  (begin
    (define (go i)
      (when (< i m)
        body ...
        (go (+ i 1))))
    (go n)))

(time-expr problem-definition
(begin
  (>= rec-count 0)
  (<  rec-count (time-slots))
  (for ([i (range (time-slots))])
    (assert (<= 0 (list-ref recitation-scheduled i)))
    (assert (< (list-ref recitation-scheduled i) (time-slots)))
    (for ([j (range (student-count))])
      (assert (<= 0 (lists-ref student-attendance `(,j ,i))))
      (assert (>= 1 (lists-ref student-attendance `(,j ,i))))))
  (sym-for i 0 rec-count
                 ;; recitation-attendance for the selected recitation
                 ;; time slot must be between the minimum and the capacity
                 (assert
                   (<= (recitation-minimum-attendance)
                       (list-ref recitation-attendance i)))
                 (assert
                   (>= (recitation-capacity)
                       (list-ref recitation-attendance i)))
                 ;; different recitation-scheduled entries shouldn't be the same
                 ;; if a recitation isn't scheduled, then it should have zero attendees
                 (when (< (+ 1 i) rec-count)
                   (assert
                     (<= (+ 1 (list-ref recitation-scheduled i))
                         (list-ref recitation-scheduled (+ 1 i)))))
                 ;; this sums up all of the students available for the selected scheduled recitation
                 ;; and makes sure that it exceeds the attendance
                 (assert
                   (>= (sum ([j (0 (student-count))])
                         (lists-ref (student-availability)
                                    `(,(list-ref recitation-scheduled i) ,j)))
                       (list-ref recitation-attendance i)))
                 ;; this sums up all of the students in attendence at the scheduled recitation
                 ;; and says it must equal the attendance for that recitation
                 (assert
                   (= (sum ([j (0 (student-count))])
                        (lists-ref student-attendance
                                   `(,j ,(list-ref recitation-scheduled i))))
                      (list-ref recitation-attendance i))))
            ;; the number of recitations must exceed the number of time slots that have a non-zero attendance
            (assert
              (>= rec-count
                  (count identity
                    (build-list (time-slots)
                      (lambda (i)
                        (< 0 (sum ([j (0 (student-count))])
                               (lists-ref student-attendance `(,j ,i)))))))))
            ;; this asserts that for every student, for every time slot:
            ;;  the student is    available and didn't attend
            ;;           or is    available and did    attend
            ;;           or isn't available and didn't attend
            ;; This excludes the possibility:
            ;;  the student isn't available and did    attend
            (for ([i (range 0 (time-slots))])
              (for ([j (range 0 (student-count))])
                (assert
                  (= 1 (let ((a (- 1 (lists-ref student-attendance `(,j ,i))))
                             (b (lists-ref (student-availability) `(,i ,j))))
                         (- (+ a b) (* a b)))))))
            ;; the sum of all student attendances must be at least the attendance requirement * student count
            (assert
              (>= (sum ([i (0 (time-slots))])
                    (sum ([j (0 (student-count))])
                      (lists-ref student-attendance `(,j ,i))))
                  (* (student-attendance-requirement)
                     (student-count))))
))

(let ((s (time-expr
           (optimize
             #:minimize `(,rec-count)
             #:guarantee
             #t
             ))))
  (println (evaluate rec-count s))
  (println (evaluate recitation-scheduled  s))
  (println (evaluate recitation-attendance s))
  )
