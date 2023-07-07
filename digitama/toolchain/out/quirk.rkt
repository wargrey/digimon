#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bytes->pecoff-section-name : (-> Bytes Natural Symbol)
  (lambda [name _]
    (define size : Index (bytes-length name))
    (define pure-name : String
      (let trim ([end : Index size])
        (if (> end 0)
            (let ([end-- (- end 1)])
              (cond [(= (bytes-ref name end--) 0) (trim end--)]
                    [else (bytes->string/utf-8 name #false 0 end)]))
            "")))
    
    (string->symbol pure-name)))

(define pecoff-section-name->bytes : (-> Symbol Natural Bytes)
  (lambda [name size]
    (define bs (string->bytes/utf-8 (symbol->immutable-string name)))
    (define zeros (- (bytes-length bs) size))

    (cond [(<= zeros 0) bs]
          [else (bytes-append bs (make-bytes zeros 0))])))

(define bytes->pecoff-symbol-name : (-> Bytes Natural (U Symbol Index))
  (lambda [name _]
    (if (= (integer-bytes->integer name #false #false 0 4) 0)
        (assert (integer-bytes->integer name #false #true 4 8) index?)
        (bytes->pecoff-section-name name _))))

(define pecoff-symbol-name->bytes : (-> (U Symbol Index) Natural Bytes)
  (lambda [name size]
    (cond [(symbol? name) (pecoff-section-name->bytes name size)]
          [else (let ([raw (make-bytes size 0)])
                  (integer->integer-bytes name 4 #false #false raw 4)
                  raw)])))
