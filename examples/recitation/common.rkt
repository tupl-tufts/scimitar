#lang racket

(provide
  time-slots
  student-count
  recitation-capacity
  recitation-minimum-attendance
  student-attendance-requirement
  student-conflicts
  student-availability)

;; the number of schedulable slots
(define time-slots
  (make-parameter 4
    (invariant-assertion
      (-> natural? natural?)
      identity)))
;; the number registered students
(define student-count
  (make-parameter 14
    (invariant-assertion
      (-> natural? natural?)
      identity)))
;; the maximum number of students that fit in a recitation
(define recitation-capacity
  (make-parameter 5
    (invariant-assertion
      (-> natural? natural?)
      identity)))
;; the minimum number of students to justify a recitation
(define recitation-minimum-attendance
  (make-parameter 2
    (invariant-assertion
      (-> natural? natural?)
      identity)))
;; the number registered students
(define student-attendance-requirement
  (make-parameter 0.9
    (invariant-assertion
      (-> real? real?)
      identity)))
;; the average number of recitations students can attend
(define student-conflicts
  (make-parameter 0.1
    (invariant-assertion
      (-> real? real?)
      identity)))


;; the recitation slots that each student is free
(define student-availability
  (parameterize ([current-pseudo-random-generator
                  (make-pseudo-random-generator)])
    (random-seed 98765)
    (make-parameter
      (build-list (time-slots)
        (lambda _ (build-list (student-count)
                    (lambda _ (if (> (random 1000) (* 1000 (student-conflicts))) 1 0))))))))
