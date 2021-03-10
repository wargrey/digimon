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
            (describe "issues are allowed to be described later" #:do
                      (it "tags empty behavior with TODO automatically" #:do)
                      
                      (it "tags a TODO issue with useful information" #:do
                          (pending "I am here to tell you some tasks are pending")))

            (describe "panic issues indicate bugs in the specification" #:do
                      (it "should be caught by `expect-throw`" #:do
                          (expect-throw exn:fail? (error 'expect-throw "requires a thunk rather than an expression")))
                      
                      (it "should be done regardless the prerequisite" #:do
                          #:before (λ [] (error 'expect-true "setup routine is buggy")) #:do
                          (make-it)))
            
            (describe "features and behaviors are tagged with SKIP whenever a `exn:fail:unsupported` is caught" #:do
                      (it "should skip the extflonum since it is not the number in the sense `number?`" #:do
                          (expect-throw exn:fail:unsupported? (λ [] (ignore "`extflonum` is not available")))
                          (collapse "`(expect-throw exn:fail:unsupported? thunk)` suppresses the default SKIP mechanism"))
                      
                      (it "should skip the extflonum since it is not the number in the sense `number?`" #:do
                          (ignore "a buggy specification, but it's okay for representation purpose"))
                      
                      (describe "skips user scenarios since no network interface card is found" #:do
                                #:before (λ [] (ignore "NIC is not present")) #:do
                                
                                (describe "searches the network neighbors" #:do
                                          (it "should be skipped since its grandparent is skipped" #:do
                                              (make-it)))))))

(module+ main
  (void (spec-prove prelude #:selector (list '* "normal issues")))
  (void (spec-prove prelude #:selector (list '* #px"^special"))))
