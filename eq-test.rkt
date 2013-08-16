#lang racket

;; **********************************************
;; Equivalency Test Assistant
;; Version 1.0
;; By Alex Truong
;; **********************************************

(provide
 test-one
 test-all)

;; A DelayExp is a (delay Any)
;; Any includes all unevaluated expressions.

;; Carries the number of testing sets
(define test-iteration 0)

;; Increments the number of testing sets
(define (test-increment)
   (set! test-iteration (add1 test-iteration)))

;; test-one: Any Any -> Void
;; Purpose: Evaluates equality between two expressions with
;;          non-disruptive error catching.

(define-syntax (test-one stx)
    (syntax-case stx ()
      ((_ a b)
       #'(with-handlers ((exn:fail? (lambda (exn) 
          (printf "Test failed with an exception as follows:\n~a\n" (exn-message exn)))))
          (define pass-or-fail 
              (cond [(equal? a b)
                     "passed"]
                    [else "failed"]))
          (printf "Test ~a.\n" pass-or-fail)))))

;; test-all: DelayExp ... -> Void
;; Purpose: Calls test-start using a list created from delaying 
;;          all given arguments. Initiates the evaluation process.
;;          Larger scale version of test-one.

(define-syntax (test-all stx)
    (syntax-case stx ()
      ((_ a ...)
       #'(test-start (list (delay a)  ...)))))

;; test-start: (listof DelayExp) -> Void
;; Purpose: Numbers the test set to be tested.
;;          Initiates the test-set procedure.

(define (test-start tests)
   (test-increment)
   (printf "Starting Test Set #~a\n" test-iteration)
   (test-set tests 0 0))

;; test-set: (listof DelayExp) Nat Nat -> Void
;; Purpose: Evaluates a list of DelayExp as follows:
;;
;;          For each DelayExp of the form (list expression expression ...),
;;          - test-set interchangeably accepts a value for an expression
;;          - test-set tests the equality of the set's members
;;          - test-set prints the result as a pass or fail message
;;            - if an error occurs, the error is listed and test-set resumes testing
;;          If any DelayExp is not of the form (list expression expression ...),
;;          an error is printed instead. The test is considered a fail.
;;
;;          Finally, test-set prints the total number of tests passed vs total tests.

(define (test-set tests index passed)
   (cond [(empty? tests)
          (printf "Testing complete.\n~a/~a tests passed.\n\n"
          passed index)]
         [else
          (with-handlers ((exn:fail? (lambda (exn) 
           (printf "Test #~a failed with an exception as follows:\n~a\n" index (exn-message exn))
           (test-set (cdr tests) (add1 index) passed))))
            (define (test-loop result test-list)
               (cond [(empty? test-list)
                      (set! passed (add1 passed))
                      "passed"]
                     [(equal? result (car test-list))
                      (test-loop result (cdr test-list))]
                     [else "failed"]))
            (define (start-loop test-list)
              (test-loop (car test-list) (cdr test-list)))
           (printf "Test #~a ~a.\n" index (start-loop (force (car tests))))
           (test-set (cdr tests) (add1 index) passed))]))

