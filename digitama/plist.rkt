#lang typed/racket/base

;;; https://opensource.apple.com/source/CF/CF-1153.18/CFBinaryPList.c.auto.html

(provide (all-defined-out))

(require racket/file)

(require "../format.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type PList-Stdin (U Path-String Bytes))

(struct plist
  ([version : Byte]
   [trailer : (Listof Symbol)])
  #:transparent
  #:type-name PList)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plist-stdin->bytes : (-> PList-Stdin Bytes)
  (lambda [/dev/plstin]
    (cond [(path? /dev/plstin) (file->bytes /dev/plstin)]
          [(string? /dev/plstin) (file->bytes /dev/plstin)]
          [else /dev/plstin])))

(define plist-from-bytes : (-> Bytes PList)
  (lambda [/dev/plstin]
    (plist 0 null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plist-pretty-hexdump : (-> Bytes Output-Port Void)
  (lambda [/dev/plstin /dev/stdout]
    (define total : Index (bytes-length /dev/plstin))
    (define trailer : Fixnum (- total 32))

    (let pretty-print ([pos : Nonnegative-Fixnum 0])
      (when (< pos total)
        (define hex : String (byte->hex-string (bytes-ref /dev/plstin pos)))
        
        (cond [(< pos 7)
               (display hex /dev/stdout)
               (display #\space /dev/stdout)
               (pretty-print (+ pos 1))]
              [(= pos 7)
               (displayln hex /dev/stdout)
               (pretty-print (+ pos 1))]
              [else (void)])))

    (newline /dev/stdout)))
