#lang typed/racket/base

;;; https://www.hanshq.net/zip.html
;;; https://pkware.cachefly.net/webdocs/APPNOTE/APPNOTE-2.0.txt
;;; https://www.ietf.org/rfc/rfc1951.txt
;;; collection: file/gunzip.rkt

(provide (all-defined-out))

(require racket/unsafe/ops)

(require "../evt.rkt")
(require "../ioexn.rkt")

(require "../../port.rkt")
(require "../../bitstream.rkt")

(require "../unsafe/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-input-deflated-block : (->* (Input-Port Natural) (Boolean #:name String #:error-name Symbol #:commit? Boolean) Input-Port)
  (lambda [/dev/zipin csize [close-orig? #false] #:name [name #false] #:error-name [ename 'zip] #:commit? [commit? #false]]
    (define-values (FEED PEEK FIRE $) (make-input-lsb-bitstream /dev/zipin #:padding-byte #xFF #:limited csize))

    (define BTYPE : (Option Symbol) #false)
    (define BFINAL : Boolean #false)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define huffman-extract-block-header : (-> (Values Symbol Boolean))
      (lambda []
        (if (FEED 3)
            (let ([BFINAL (PEEK 1 #b1)]
                  [BTYPE (PEEK 2 #b11 1)])
              (FIRE 3)
              (values (case BTYPE
                        [(#b00)
                         ($ 'align)
                         (FEED 32)
                         (set! stock (PEEK 16 #xFFFF))
                         (unless (= stock (bitwise-not (PEEK 16 #xFFFF 16)))
                           (throw-check-error /dev/blkin ename "stored: invalid block length"))
                         (FIRE 32)
                         'stored]
                        [(#b01)
                         (set! stock 8)
                         'fixed]
                        [(#b10)
                         (set! stock 16)
                         'dynamic]
                        [else 'reserved])
                      (= BFINAL 1)))
            (values 'EOB #true))))

    (define stock : Index 0)
    (define (read-stored-block! [zipout : Bytes] [start : Index] [end : Index]) : Index
      (define stop : Nonnegative-Fixnum (min (+ start stock) end))
      
      (let copy-bytes ([start : Index start])
        (cond [(= start stop) start]
              [(not (FEED 8)) start]
              [else (let ([start++ (unsafe-idx+ start 1)])
                      (unsafe-bytes-set! zipout start (PEEK 8 #xFF))
                      (FIRE 8)
                      (copy-bytes start++))])))

    (define (read-fixed-block! [zipout : Bytes] [start : Index] [end : Index]) : Index
      (read-stored-block! zipout start end))

    (define (read-dynamic-block! [zipout : Bytes] [start : Index] [end : Index]) : Index
      (read-stored-block! zipout start end))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (deflate-read! [zipout : Bytes]) : Port-Reader-Plain-Datum
      (define end : Index (bytes-length zipout))

      (let read-block ([start : Index 0])
        (cond [(not (eq? BTYPE 'EOB))
               (define-values (type last?)
                 (cond [(not BTYPE) (huffman-extract-block-header)]
                       [else (values BTYPE BFINAL)]))
                    
               (define start++ : Index
                 (case type
                   [(stored) (read-stored-block! zipout start end)]
                   [(fixed) (read-fixed-block! zipout start end)]
                   [(dynamic) (read-dynamic-block! zipout start end)]
                   [(EOB) start]
                   [else (throw-check-error /dev/blkin ename "reserved deflated block type")]))
               
               (cond [(>= start++ end) (set!-values (BTYPE BFINAL) (values type last?)) end]
                     [else (set! BTYPE (and last? 'EOB)) (read-block start++)])]
              [(= start 0) (unless (not commit?) ($ 'final-commit)) eof]
              [else start])))
    
    (define /dev/blkin : Input-Port
      (make-input-port (or name (object-name /dev/zipin))
                       
                       deflate-read! #false
                       
                       (λ [] (when close-orig? (close-input-port /dev/zipin)))
                       
                       #false #false
                       
                       (lambda () (port-next-location /dev/zipin))
                       (lambda () (port-count-lines! /dev/zipin))
                       
                       1 #| initial position |#))

    /dev/blkin))
