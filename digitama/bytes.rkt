#lang typed/racket/base

(provide (all-defined-out))

(require racket/unsafe/ops)
(require racket/case)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Bytes-Scan-Line (-> Bytes Index Index (Values (Option Index) Byte)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define unsafe-bytes-scan-linefeed : Bytes-Scan-Line
  (lambda [bs start end]
    (values (let scan ([pos : Nonnegative-Fixnum start])
              (and (< pos end)
                   (cond [(eq? (unsafe-bytes-ref bs pos) 10) pos]
                         [else (scan (+ pos 1))])))
            1)))
    
(define unsafe-bytes-scan-return : Bytes-Scan-Line
  (lambda [bs start end]
    (values (let scan ([pos : Nonnegative-Fixnum start])
              (and (< pos end)
                   (cond [(eq? (unsafe-bytes-ref bs pos) 13) pos]
                         [else (scan (+ pos 1))])))
            1)))

(define unsafe-bytes-scan-refeed : Bytes-Scan-Line
  (lambda [bs start end]
    (values (let scan ([pos : Nonnegative-Fixnum start])
              (and (< pos end)
                   (let ([\l-pos (+ pos 1)])
                     (cond [(not (eq? (unsafe-bytes-ref bs pos) 13)) (scan \l-pos)]
                           [(>= \l-pos end) #false]
                           [else (let ([\l (unsafe-bytes-ref bs \l-pos)])
                                   (cond [(eq? \l 10) pos]
                                         [(eq? \l 13) (scan \l-pos)]
                                         [else (scan (+ \l-pos 1))]))]))))
            2)))

(define unsafe-bytes-scan-any : Bytes-Scan-Line
  (lambda [bs start end]
    (let scan ([pos : Nonnegative-Fixnum start])
      (cond [(>= pos end) (values #false 2)]
            [else (let ([b (unsafe-bytes-ref bs pos)]
                        [\l-pos (+ pos 1)])
                    (cond [(eq? b 10) (values pos 1)]
                          [(not (eq? b 13)) (scan \l-pos)]
                          [(>= \l-pos end) (values pos 1)]
                          [(eq? (unsafe-bytes-ref bs \l-pos) 10) (values pos 2)]
                          [else (values pos 1)]))]))))

(define unsafe-bytes-scan-anyone : Bytes-Scan-Line
  (lambda [bs start end]
    (values (let scan ([pos : Nonnegative-Fixnum start])
              (and (< pos end)
                   (let ([b (unsafe-bytes-ref bs pos)])
                     (cond [(eq? b 10) pos]
                           [(eq? b 13) pos]
                           [else (scan (+ pos 1))]))))
            1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bytes-scanline-select : (-> Symbol Bytes-Scan-Line)
  (lambda [mode]
    (case/eq mode
      [(any) unsafe-bytes-scan-any]
      [(return-linefeed) unsafe-bytes-scan-refeed]
      [(linefeed) unsafe-bytes-scan-linefeed]
      [(return) unsafe-bytes-scan-return]
      [(any-one) unsafe-bytes-scan-anyone]
      [else unsafe-bytes-scan-any])))
