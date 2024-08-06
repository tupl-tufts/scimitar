#lang racket

(require "solver/solvers.rkt")
(require "util.rkt")

(provide
  (all-from-out "solver/solvers.rkt"))

(current-solver gurobi)

(module+ test
  (require rackunit)
  (define lemonade.mps
"NAME          Lemonade
ROWS
 N  PROFIT
 L  TIME
 L  LEMONS
 L  SUGAR
 L  WATER
 L  VODKA
COLUMNS
    REGULAR   PROFIT    1.             TIME      0.25
    REGULAR   LEMONS    1.             SUGAR     2.
    REGULAR   WATER     2.             VODKA     0.
    SPECIAL   PROFIT    2.             TIME      0.5
    SPECIAL   LEMONS    1.             SUGAR     1.25
    SPECIAL   WATER     0.6            VODKA     0.5
RHS
    RHS       TIME      60.            LEMONS    200.
    RHS       SUGAR     250.           WATER     240.
    RHS       VODKA     50.
BOUNDS
 LI BND       REGULAR   0.
 LI BND       SPECIAL   0.
ENDATA")

  (struct solver-result
    (status obj opt pri abn rows cols))

  (define (test-example input-method [integer #f])
    (let* ((model (newModel))
           (_ (setLogLevel model 'none))
           (_ (input-method model))
           (_ (when integer (setInteger model 0) (setInteger model 1)))
           (_ (setObjSense model 'maximize))
           (_ (setPreprocess model 'off))
           (status (solve model)))
      (solver-result
        status
        (getObjValue model)
        (isProvenOptimal model)
        (isProvenInfeasible model)
        (isAbandoned model)
        (getRowActivity model)
        (getColSolution model))))

  (define (input-by-problem model)
    (loadProblem model
                 '((0 1 2 3 4) (0 1 2 3 4))
                 '((0.25 1 2 2 0) (0.5 1 1.25 0.6 0.5))
                 '(0 0)
                 '(+inf.0 +inf.0)
                 '(1 2)
                 '(<= <= <= <= <=)
                 '(60 200 250 240 50)))

  (define (input-by-cols model)
    (addRows model '() '() '(<= <= <= <= <=) '(60 200 250 240 50))
    (addColumns model '(0) '(+inf.0) '(1) '((0 1 2 3 4)) '((0.25 1 2 2 0)))
    (addColumns model '(0) '(+inf.0) '(2) '((0 1 2 3 4)) '((0.5 1 1.25 0.6 0.5))))

  (define (input-by-rows model)
    (addColumns model '(0 0) '(+inf.0 +inf.0) '(1 2) '() '())
    (addRows model '((0 1)) '((0.25 0.5))  '(<=) '(60))
    (addRows model '((0 1)) '((1    1))    '(<=) '(200))
    (addRows model '((0 1)) '((2    1.25)) '(<=) '(250))
    (addRows model '((0 1)) '((2    0.6))  '(<=) '(240))
    (addRows model '((0 1)) '((0    0.5))  '(<=) '(50)))

  (define expected-obj 240.0)
  (define expected-rows-lp '(60.0 156.36363636363637 250.0 195.63636363636365 41.81818181818182))
  (define expected-cols-lp '(72.72727272727273 83.63636363636364))
  (define expected-rows-milp '(60.0 156.0 249.0 194.4 42.0))
  (define expected-cols-milp '(72.0 84.0))

  (define epsilon 1e-6)

  (call-with-temp-file "/tmp/lemonade" #:extn "mps"
    (lambda (out name)
      (displayln lemonade.mps out)
      (flush-output out)
      (parameterize ([current-solver clp])
        (let ((r (test-example (curryr readMps name))))
          (check-equal? (solver-result-status r) 'finished)
          (check-within (solver-result-obj r) expected-obj epsilon)
          (check-true (solver-result-opt r))
          (check-false (solver-result-pri r))
          (check-false (solver-result-abn r))
          (check-within (solver-result-rows r) expected-rows-lp epsilon)
          (check-within (solver-result-cols r) expected-cols-lp epsilon)))
      (parameterize ([current-solver cbc])
        (let ((r (test-example (curryr readMps name) #t)))
          (check-equal? (solver-result-status r) 'finished)
          (check-equal? (solver-result-obj r) expected-obj)
          (check-true (solver-result-opt r))
          (check-false (solver-result-pri r))
          (check-false (solver-result-abn r))
          (check-equal? (solver-result-rows r) expected-rows-milp)
          (check-equal? (solver-result-cols r) expected-cols-milp)))
      (parameterize ([current-solver gurobi])
        (let ((r (test-example (curryr readMps name) #t)))
          (check-equal? (solver-result-status r) 'finished)
          (check-equal? (solver-result-obj r) expected-obj)
          (check-true (solver-result-opt r))
          (check-false (solver-result-pri r))
          (check-false (solver-result-abn r))
          (check-equal? (solver-result-rows r) expected-rows-milp)
          (check-equal? (solver-result-cols r) expected-cols-milp)))
      ))

  (parameterize ([current-solver clp])
    (let ((r (test-example input-by-problem)))
      (check-equal? (solver-result-status r) 'finished)
      (check-within (solver-result-obj r) expected-obj epsilon)
      (check-true (solver-result-opt r))
      (check-false (solver-result-pri r))
      (check-false (solver-result-abn r))
      (check-within (solver-result-rows r) expected-rows-lp epsilon)
      (check-within (solver-result-cols r) expected-cols-lp epsilon)))
  (parameterize ([current-solver cbc])
    (let ((r (test-example input-by-problem #t)))
      (check-equal? (solver-result-status r) 'finished)
      (check-equal? (solver-result-obj r) expected-obj)
      (check-true (solver-result-opt r))
      (check-false (solver-result-pri r))
      (check-false (solver-result-abn r))
      (check-equal? (solver-result-rows r) expected-rows-milp)
      (check-equal? (solver-result-cols r) expected-cols-milp)))
  (parameterize ([current-solver gurobi])
    (let ((r (test-example input-by-problem #t)))
      (check-equal? (solver-result-status r) 'finished)
      (check-equal? (solver-result-obj r) expected-obj)
      (check-true (solver-result-opt r))
      (check-false (solver-result-pri r))
      (check-false (solver-result-abn r))
      (check-equal? (solver-result-rows r) expected-rows-milp)
      (check-equal? (solver-result-cols r) expected-cols-milp)))

  (parameterize ([current-solver clp])
    (let ((r (test-example input-by-cols)))
      (check-equal? (solver-result-status r) 'finished)
      (check-within (solver-result-obj r) expected-obj epsilon)
      (check-true (solver-result-opt r))
      (check-false (solver-result-pri r))
      (check-false (solver-result-abn r))
      (check-within (solver-result-rows r) expected-rows-lp epsilon)
      (check-within (solver-result-cols r) expected-cols-lp epsilon)))
  (parameterize ([current-solver cbc])
    (let ((r (test-example input-by-cols #t)))
      (check-equal? (solver-result-status r) 'finished)
      (check-equal? (solver-result-obj r) expected-obj)
      (check-true (solver-result-opt r))
      (check-false (solver-result-pri r))
      (check-false (solver-result-abn r))
      (check-equal? (solver-result-rows r) expected-rows-milp)
      (check-equal? (solver-result-cols r) expected-cols-milp)))
  (parameterize ([current-solver gurobi])
    (let ((r (test-example input-by-cols #t)))
      (check-equal? (solver-result-status r) 'finished)
      (check-equal? (solver-result-obj r) expected-obj)
      (check-true (solver-result-opt r))
      (check-false (solver-result-pri r))
      (check-false (solver-result-abn r))
      (check-equal? (solver-result-rows r) expected-rows-milp)
      (check-equal? (solver-result-cols r) expected-cols-milp)))

  (parameterize ([current-solver clp])
    (let ((r (test-example input-by-rows)))
      (check-equal? (solver-result-status r) 'finished)
      (check-within (solver-result-obj r) expected-obj epsilon)
      (check-true (solver-result-opt r))
      (check-false (solver-result-pri r))
      (check-false (solver-result-abn r))
      (check-within (solver-result-rows r) expected-rows-lp epsilon)
      (check-within (solver-result-cols r) expected-cols-lp epsilon)))
  (parameterize ([current-solver cbc])
    (let ((r (test-example input-by-rows #t)))
      (check-equal? (solver-result-status r) 'finished)
      (check-equal? (solver-result-obj r) expected-obj)
      (check-true (solver-result-opt r))
      (check-false (solver-result-pri r))
      (check-false (solver-result-abn r))
      (check-equal? (solver-result-rows r) expected-rows-milp)
      (check-equal? (solver-result-cols r) expected-cols-milp)))
  (parameterize ([current-solver gurobi])
    (let ((r (test-example input-by-rows #t)))
      (check-equal? (solver-result-status r) 'finished)
      (check-equal? (solver-result-obj r) expected-obj)
      (check-true (solver-result-opt r))
      (check-false (solver-result-pri r))
      (check-false (solver-result-abn r))
      (check-equal? (solver-result-rows r) expected-rows-milp)
      (check-equal? (solver-result-cols r) expected-cols-milp)))
  )
