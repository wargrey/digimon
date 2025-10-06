#lang typed/racket/base

(provide (all-defined-out))

(require "evt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Output-Gate-Write S) (-> Bytes Natural Natural S (Values S Natural)))
(define-type (Output-Gate-Flush S) (-> S S))
(define-type (Output-Gate-Close S) (-> S (U S Void False)))
(define-type (Output-Gate-Position0 S) (-> S Positive-Integer))

(struct (S) output-gate
  ([port : Output-Port]
   [&snapshot : (Boxof S)])
  #:type-name Output-Gate)

(struct (S) output-gate<%>
  ([write : (Output-Gate-Write S)]
   [flush : (Output-Gate-Flush S)]
   [close : (Output-Gate-Close S)]
   [position0 : (Output-Gate-Position0 S)])
  #:type-name Output-Gate<%>
  #:transparent)

(define #:forall (S) output-gate-snapshot : (-> (Output-Gate S) S)
  (lambda [self]
    (unbox (output-gate-&snapshot self))))

(define #:forall (S) make-output-gate<%> : (-> [#:write (Option (Output-Gate-Write S))]
                                               [#:flush (Option (Output-Gate-Flush S))]
                                               [#:close (Option (Output-Gate-Close S))]
                                               [#:position0 (Option (Output-Gate-Position0 S))]
                                               (Output-Gate<%> S))
  (lambda [#:write [write #false] #:flush [flush #false] #:close [close #false] #:position0 [position0 #false]]
    (output-gate<%> (or write (λ [bs start span s] (values s span)))
                    (or flush values)
                    (or close void)
                    (or position0 (λ _ 1)))))

(define #:forall (S) make-output-gate : (->* (S (Output-Gate<%> S))
                                             (#:name Any #:tee (U Output-Port (Listof Output-Port)) #:tee-close? Boolean)
                                             (Output-Gate S))
  (lambda [#:name [name #false] #:tee [tee null] #:tee-close? [close-origin? #false]
           initial self]
    (define &cabinet : (Boxof S) (box initial))
    (define fwrite : (Output-Gate-Write S) (output-gate<%>-write self))
    (define fflush : (Output-Gate-Flush S) (output-gate<%>-flush self))
    (define /dev/outs : (Listof Output-Port) (if (list? tee) tee (list tee)))
    
    (define (func-write [bs : Bytes] [start : Natural] [end : Natural] [non-block/buffered? : Boolean] [enable-break? : Boolean]) : Integer
      (define src-size : Integer (- end start))
      
      (if (> src-size 0) 
          (let dump ([span : Natural src-size]
                     [start : Natural start]
                     [state : S (unbox &cabinet)])
            (define-values (state++ proceeded) (fwrite bs start span state))
            (define span-- (- span proceeded))
            (if (<= span-- 0)
                (begin
                  (set-box! &cabinet state++)
                  (for ([/dev/teeout (in-list /dev/outs)])
                    (write-bytes bs /dev/teeout start end)))
                (dump span-- (+ start proceeded) (fflush state++))))

          (begin ; explicitly calling `flush-port`
            (for ([/dev/teeout (in-list /dev/outs)])
              (flush-output /dev/teeout))
            
            (set-box! &cabinet (fflush (unbox &cabinet)))))

      (when (and non-block/buffered?)
        (for ([/dev/teeout (in-list /dev/outs)])
          (write-bytes-avail* bs /dev/teeout start end))

        ; usually implies flush, and can return #false if failed.
        (set-box! &cabinet (fflush (unbox &cabinet))))
      
      src-size)

    (define (func-simple-write [bs : Bytes] [start : Natural] [end : Natural] [non-block/buffered? : Boolean] [enable-break? : Boolean]) : Integer
      (define src-size : Integer (- end start))
      
      (if (> src-size 0) 
          (let dump ([span : Natural src-size]
                     [start : Natural start]
                     [state : S (unbox &cabinet)])
            (define-values (state++ proceeded) (fwrite bs start span state))
            (define span-- (- span proceeded))
            (if (> span-- 0)
                (dump span-- (+ start proceeded) (fflush state++))
                (set-box! &cabinet state++)))

          ; explicitly calling `flush-port`
          (set-box! &cabinet (fflush (unbox &cabinet))))

      (when (and non-block/buffered?)
        ; do writing without block, say, calling `write-bytes-avail*`,
        ; usually implies flush, and can return #false if failed.
        (set-box! &cabinet (fflush (unbox &cabinet))))
      
      src-size)

    (define (func-close) : Void
      (define snapshot++ : S (fflush (unbox &cabinet)))

      (set-box! &cabinet
                (let ([s ((output-gate<%>-close self) snapshot++)])
                  (cond [(void? s) snapshot++]
                        [else (or s snapshot++)])))

      (when (and close-origin?)
        (for-each close-output-port /dev/outs)))

    ((inst output-gate S)
     (make-output-port (or name
                           (for/or : Any ([/dev/teeout (if (list? tee) (in-list tee) (in-value tee))])
                             (object-name /dev/teeout))
                           (gensym '/dev/gateout:))
                       always-evt
                       (if (null? /dev/outs) func-simple-write func-write)
                       func-close
                       #false port-always-write-evt #false
                       #false void
                       (λ [] ((output-gate<%>-position0 self) initial))
                       #false)
     &cabinet)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ports-filter-write : (->* ((Listof Output-Port) Bytes) (Index Index) (Listof Output-Port))
  (lambda [outs src [start 0] [end (bytes-length src)]]
    (reverse
     (for/fold ([outs++ : (Listof Output-Port) null])
               ([out (in-list outs)])
       (with-handlers ([exn:fail? (λ _ outs++)])
         (let copy ([s : Nonnegative-Fixnum start])
           (when (< s end)
             (copy (+ (write-bytes-avail/enable-break src out s end) s))))
         (flush-output out)
         (cons out outs++))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct hexstate
  ([magazine : Bytes]
   [payload : Natural]
   [cursor : Natural])
  #:transparent)
