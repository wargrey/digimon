#lang typed/racket/base

(provide (all-defined-out))
(provide PList-Stdin PList-Stdout)
(provide PList-Format PList-Datum PList-Object)

(require "digitama/plist.rkt")
(require "digitama/plist/bplist.rkt")
(require "digitama/plist/info.plist.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plist-datum? : (-> Any Boolean : #:+ PList-Datum)
  (lambda [val]
    (or (string? val)
        (symbol? val)
        (boolean? val)
        (exact-integer? val)
        (flonum? val)
        (void? val)
        (date? val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-plist : (-> PList-Stdin PList-Object)
  (lambda [/dev/bplin]
    (define body : Bytes (plist-stdin->bytes /dev/bplin))
    (cond [(bplist-bytes? body) (bplist-extract-object body)]
          [else (void)])))

(define write-plist : (->* (Any) (PList-Stdout #:format PList-Format #:exists (U 'error 'replace)) Void)
  (lambda [plst [/dev/bplout (current-output-port)] #:format [type 'bplist] #:exists [exists 'replace]]
    (if (output-port? /dev/bplout)
        (case type
          [else (bplist-write-datum plst /dev/bplout)])
        ((inst call-with-output-file* Void)
         /dev/bplout #:exists exists #:mode 'binary
         (λ [[/dev/bplout : Output-Port]]
           (write-plist plst /dev/bplout))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bplist-dissect : (->* (PList-Stdin) (Output-Port #:offset-table-column Byte #:show-unused-field? Boolean) Void)
  (lambda [/dev/bplin [/dev/stdout (current-output-port)] #:offset-table-column [offset-table-column 16] #:show-unused-field? [unused-field? #true]]
    (define body : Bytes (plist-stdin->bytes /dev/bplin))

    (when (bplist-bytes? body)
      (bplist-pretty-hexdump body /dev/stdout offset-table-column unused-field?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plist-copy-info.rkt : (->* (Path-String)
                                   (PList-Stdout #:filter (-> Symbol Any (Values Symbol Any)) #:excludes (Listof Symbol) 
                                                 #:format PList-Format #:exists (U 'error 'replace)) Void)
  (lambda [info.rkt [/dev/bplout (current-output-port)]
                    #:filter [filter apple-info.plist] #:excludes [omitted null]
                    #:format [type 'bplist] #:exists [exists 'replace]]
    (dynamic-require info.rkt #false)
    
    (define dict : (HashTable Symbol Any) (make-hasheq))
    (define ns : Namespace (module->namespace info.rkt))
    
    (for ([var (in-list (namespace-mapped-symbols ns))])
      (unless (memq var omitted)
        (define val : Any (namespace-variable-value var #false (λ [] void) ns))
        (unless (procedure? val)
          (define-values (key value) (filter var val))
          (hash-set! dict key value))))
    
    (write-plist dict /dev/bplout #:format type #:exists exists)))
