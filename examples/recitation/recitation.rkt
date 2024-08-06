#lang racket

(require scimitar/ty)
(require scimitar/vec)
(require scimitar)
(require "common.rkt")

;; Norman wanted to write a program that maximized the number of students
;; in a recitation, which would require using a variable as the upper loop
;; bound.  He argued that he wanted some usability benefit, i.e., hand Scimitar
;; or a Scimitar program to Richard for 105 and he could just run with it.

(epsilon 1e-4)
(inline-amount (time-slots))

(student-availability (vec-dense (student-availability)))

; somehow this 'i conflicted with the local 'i  shoot me
;`(,(scimitar (for ([i (range (: rec-count (vecty (interval 0 1) '())))])
;    (begin)))
;  ,(scimitar (assert (= (: i (bitty)) (: 0 (bitty))))))
(let ((rec-count
        (optimum-ref (rec-count recitation-scheduled recitation-attendance)
          (minimize rec-count (rec-count recitation-scheduled recitation-attendance student-attendance)
            (for ([i (range (: rec-count (vecty (interval 0 (time-slots)) '())))])
               (begin
                 ;; recitation-attendance for the selected recitation
                 ;; time slot must be between the minimum and the capacity
                 (assert
                   (<= (racket (recitation-minimum-attendance))
                       (vec-ref (: recitation-attendance (vecty (interval 0 (recitation-capacity)) `(,(time-slots)))) i)))
                 (assert
                   (>= (racket (recitation-capacity))
                       (vec-ref (: recitation-attendance (vecty (interval 0 (recitation-capacity)) `(,(time-slots)))) i)))
                 ;; different recitation-scheduled entries shouldn't be the same
                 ;; if a recitation isn't scheduled, then it should have zero attendees
                 (if (< (+ 1 i) rec-count)
                   (assert
                     (<= (+ 1 (vec-ref (: recitation-scheduled (vecty (interval 0 (time-slots)) `(,(+ 1 (time-slots))))) i))
                         (vec-ref (: recitation-scheduled (vecty (interval 0 (time-slots)) `(,(+ 1 (time-slots))))) (+ 1 i))))
                   '())
                 ;; this sums up all of the students available for the selected scheduled recitation
                 ;; and makes sure that it exceeds the attendance
                 (assert
                   (>= (sum ([j (range (racket (student-count)))])
                         (vec-ref (racket (student-availability))
                                  `(,j ,(vec-ref (: recitation-scheduled (vecty (interval 0 (time-slots)) `(,(+ 1 (time-slots))))) i))))
                       (vec-ref (: recitation-attendance (vecty (interval 0 (recitation-capacity)) `(,(time-slots)))) i)))
                 ;; this sums up all of the students in attendence at the scheduled recitation
                 ;; and says it must equal the attendance for that recitation
                 (assert
                   (= (sum ([j (range (racket (student-count)))])
                        (vec-ref (: student-attendance (bitty (student-count) (time-slots)))
                                 `(,j ,(vec-ref (: recitation-scheduled (vecty (interval 0 (time-slots)) `(,(+ 1 (time-slots))))) i))))
                      (vec-ref (: recitation-attendance (vecty (interval 0 (recitation-capacity)) `(,(time-slots)))) i)))))
            ;; the number of recitations must exceed the number of time slots that have a non-zero attendance
            (assert
              (>= rec-count
                  (sum ([i (range (racket (time-slots)))])
                    (< 0 (sum ([j (range (racket (student-count)))])
                           (vec-ref (: student-attendance (bitty (student-count) (time-slots)))
                                    `(,j ,i)))))))
            ;; this asserts that for every student, for every time slot:
            ;;  the student is    available and didn't attend
            ;;           or is    available and did    attend
            ;;           or isn't available and didn't attend
            ;; This excludes the possibility:
            ;;  the student isn't available and did    attend
            (for ([i (range (racket (time-slots)))])
              (for ([j (range (racket (student-count)))])
                (assert
                  (= 1 (let ((a (: (- 1 (vec-ref (: student-attendance (bitty (student-count) (time-slots))) `(,j ,i))) (bitty)))
                             (b (: (vec-ref (racket (student-availability)) `(,j ,i)) (bitty))))
                         (- (+ a b) (* a b)))))))
            ;; the sum of all student attendances must be at least the attendance requirement * student count
            (assert
              (>= (sum ([i (range (racket (time-slots)))])
                    (sum ([j (range (racket (student-count)))])
                      (vec-ref (: student-attendance (bitty (student-count) (time-slots))) `(,j ,i))))
                  (racket (* (student-attendance-requirement)
                             (student-count)))))))))
  (println rec-count))
          ; this can also be done by having a bitvector of 30 slots , and minimizing the sum
          ; we can even pit these against each other, since the bitvector one will be quite slow
