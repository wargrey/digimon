#lang typed/racket/base

(provide (all-defined-out))

(require "../spec.rkt")

(require racket/port)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define /dev/stdin : Input-Port (open-input-string "123 123x #123"))

(define-feature prelude #:do
  (describe "normal issues" #:do
            (context "given function `read`, we can extract typed datum from the stream directly" #:do
                     (describe read #:do
                               (it "produces a number when digits are consumed" #:do
                                   (expect-satisfy number? (read /dev/stdin)))

                               (it "produces a symbol when consuming digits immediately followed by non-digit chars" #:do
                                   (expect-satisfy symbol? (read /dev/stdin)))
                               
                               (context "when provided with invalid value" #:do
                                        (it "produces a parse error" #:do
                                            (expect-throw exn:fail:read:eof? (λ [] (read /dev/stdin))))

                                        (it "procudes an end-of-file error" #:do
                                            (expect-throw exn:fail:read:eof? (λ [] (read /dev/stdin))
                                                          "EOF is neither a typical datum nor an error"))))))
  
  (describe "special issues" #:do
            (it "tags empty behavior with TODO automatically" #:do)

            (context "The panic issue indicates bugs in the specification" #:do
                     (it "should be caught by `expect-throw`" #:do
                         (expect-throw exn:fail? (error 'expect-throw "requires a thunk rather than an expression")))

                     (it "should be done regardless the prerequisite" #:do
                         #:before (λ [] (error 'expect-true "setup routine is buggy")) #:do
                         (expect-true #true)))

            (context "features and behaviors are tagged with SKIP whenever a `exn:fail:unsupported` is caught" #:do
                     (it "should skip the extflonum since it is not the number in the sense `number?`" #:do
                         (expect-throw exn:fail:unsupported?
                                       (λ [] (raise (make-exn:fail:unsupported "`extflonum` is not available" (current-continuation-marks)))))
                         (expect-collapse "`(expect-throw exn:fail:unsupported? thunk)` suppresses the default SKIP mechanism"))

                     (it "should skip the extflonum since it is not the number in the sense `number?`" #:do
                         (raise (make-exn:fail:unsupported "a buggy specification but it is forgiven for representation purpose" (current-continuation-marks))))
                     
                     (context "skips user scenarios since no network interface card is found" #:do
                              #:before (λ [] (raise (make-exn:fail:unsupported "NIC is not present" (current-continuation-marks)))) #:do

                              (describe "searches the network neighbors" #:do
                                        (it "should be skipped since its grandparent is skipped" #:do
                                            (expect-true #true)))))))

(module+ main
  (void (spec-prove prelude #:selector (list '* "normal issues")))
  (void (spec-prove prelude #:selector (list '* #px"^special"))))
