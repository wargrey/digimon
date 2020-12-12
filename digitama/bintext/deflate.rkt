#lang typed/racket/base

;;; https://www.hanshq.net/zip.html
;;; https://pkware.cachefly.net/webdocs/APPNOTE/APPNOTE-2.0.txt
;;; https://www.rfc-editor.org/rfc/rfc1951.html
;;; collection//file/gunzip.rkt

(provide (all-defined-out))

(require "../ioexn.rkt")

(require "../../port.rkt")
(require "../../bitstream.rkt")

(require "../unsafe/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-input-deflated-block : (->* (Input-Port Natural) (Boolean #:name String #:error-name Symbol #:commit? Boolean) Input-Port)
  (lambda [/dev/zipin csize [close-orig? #false] #:name [name #false] #:error-name [ename 'zip] #:commit? [commit? #false]]
    (define-values (FEED-BITS PEEK-BITS FIRE-BITS $SHELL) (make-input-lsb-bitstream /dev/zipin #:padding-byte #xFF #:limited csize))

    (define BTYPE : (Option Symbol) #false)
    (define BFINAL : Boolean #false)
    (define stock : Index 0)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define huffman-read-block-header! : (-> (Values Symbol Boolean))
      (lambda []
        (if (FEED-BITS 3)
            (let ([BFINAL (PEEK-BITS 1 #b1)]
                  [BTYPE (PEEK-BITS 2 #b11 1)])
              (FIRE-BITS 3)
              (values (case BTYPE
                        [(#b00) (huffman-begin-stored-block!) 'stored]
                        [(#b01) (huffman-begin-static-block!) 'static]
                        [(#b10) (huffman-begin-dynamic-block!) 'dynamic]
                        [else 'reserved])
                      (= BFINAL 1)))
            (values 'EOB #true))))

    (define huffman-begin-stored-block! : (-> Any)
      (lambda []
        ($SHELL 'align)
        (FEED-BITS 32)
        (set! stock (PEEK-BITS 16 #xFFFF))
        (unless (= stock (bitwise-not (PEEK-BITS 16 #xFFFF 16)))
          (throw-check-error /dev/blkin ename "stored: invalid block length"))
        (FIRE-BITS 32)))
    
    (define huffman-begin-static-block! : (-> Any)
      (lambda []
        (void)))

    (define huffman-begin-dynamic-block! : (-> Any)
      (lambda []
        (void)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (read-stored-block! [zipout : Bytes] [start : Index] [end : Index]) : Index
      (define stop : Nonnegative-Fixnum (min (+ start stock) end))
      
      (let copy-bytes ([pos : Index start])
        (cond [(= pos stop) (set! stock (unsafe-idx- stock (unsafe-idx- pos start))) pos]
              [(not (FEED-BITS 8)) (set! stock (unsafe-idx- stock (unsafe-idx- pos start))) pos] ; unexpected EOF
              [else (let ([start++ (unsafe-idx+ pos 1)])
                      (unsafe-bytes-set! zipout pos (PEEK-BITS 8 #xFF))
                      (FIRE-BITS 8)
                      (copy-bytes start++))])))

    (define (read-static-block! [zipout : Bytes] [start : Index] [end : Index]) : Index
      (read-stored-block! zipout start end))

    (define (read-dynamic-block! [zipout : Bytes] [start : Index] [end : Index]) : Index
      (read-stored-block! zipout start end))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (deflate-read! [zipout : Bytes]) : Port-Reader-Plain-Datum
      (define end : Index (bytes-length zipout))

      (let read-block ([start : Index 0])
        (cond [(not (eq? BTYPE 'EOB))
               (define-values (type last?)
                 (cond [(not BTYPE) (huffman-read-block-header!)]
                       [else (values BTYPE BFINAL)]))
                    
               (define start++ : Index
                 (case type
                   [(stored)  (read-stored-block! zipout start end)]
                   [(static)  (read-static-block! zipout start end)]
                   [(dynamic) (read-dynamic-block! zipout start end)]
                   [(EOB)     (values start)]
                   [else      (throw-check-error /dev/blkin ename "reserved deflated block type")]))
               
               (cond [(>= start++ end) (set!-values (BTYPE BFINAL) (values type last?)) end]
                     [else (set! BTYPE (and last? 'EOB)) (read-block start++)])]
              [(= start 0) (unless (not commit?) ($SHELL 'final-commit)) eof]
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
