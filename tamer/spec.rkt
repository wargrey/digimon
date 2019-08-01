#lang typed/racket/base

(require "../spec.rkt")

(require racket/port)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define /dev/stdin : Input-Port (open-input-string "123 123x #123"))

(spec-begin prelude #:do
            
  (describe "normal issues" #:do
            (describe read #:do

                      (it "returns a number when digits are given" #:do
                          (expect-satisfy number? (read /dev/stdin)))
            
                      (context "when provided with invalid value" #:do
                               (it "returns a number when digits are given" #:do
                                   (expect-satisfy number? (read /dev/stdin)
                                                   "non-digit char follows closely"))

                               (it "returns a parse error" #:do
                                   (expect-throw exn:fail:read? (λ [] (read /dev/stdin)))))))
  
  (describe "special issues" #:do
            (it "marks empty features with TODO automatically" #:do)

            (context "a panic issue indicates bugs in the specification" #:do
                     
                     (it "escapes the `expect-throw` since the task is not wrapped in a thunk" #:do
                         (expect-throw exn:fail? (error 'expect-throw "requires a thunk rather than an expression")))

                     (it "should be done regardless the prerequiste" #:do
                         #:before (λ [] (error 'expect-true "setup routine is buggy")) #:do
                         (expect-true #true)))

            (context "features and behaviors are skipped whenever a `exn:fail:unsupported` is caught" #:do
                     
                     (it "skips the extflonum since it is not the number in the sense `number?`" #:do
                         (expect-throw exn:fail:unsupported?
                                       (λ [] (raise (make-exn:fail:unsupported "`extflonum` is not available" (current-continuation-marks)))))
                         (expect-collapse "`(expect-throw exn:fail:unsupported? thunk)` suppresses the default SKIP mechanism"))

                     (it "skips the extflonum since it is not the number in the sense `number?`" #:do
                         (raise (make-exn:fail:unsupported "a buggy spec but it is forgiven" (current-continuation-marks))))
                     
                     (context "skips user scenarios since no network interface card is found" #:do
                              #:before (λ [] (raise (make-exn:fail:unsupported "NIC is not present" (current-continuation-marks)))) #:do

                              (describe "searches the network neighbors" #:do

                                        (it "should be skipped since its grandparent is skipped" #:do
                                            (expect-true #true)))))))
