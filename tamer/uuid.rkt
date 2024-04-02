#lang typed/racket/base

(require "../uuid.rkt")
(require "../symbol.rkt")

(require racket/future)
(require racket/symbol)
(require racket/pretty)

(require typed/db)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WARNING: This kind of tasks defeat futures!

(define-type UUID (Vector String Symbol Integer Integer))

(define :memory: : Connection (sqlite3-connect #:database 'memory))
(query-exec :memory: "CREATE TABLE uuid (id Text PRIMARY KEY, type VARCHAR, fid NUMERIC, seq NUMERIC);")

(define do-insert : (-> UUID Void)
  (lambda [record]
    (with-handlers ([exn:fail:sql? (λ [[e : exn:fail:sql]] (pretty-write (cons record (exn:fail:sql-info e)) (current-error-port)))])
      (query-exec :memory:
                  "INSERT INTO uuid (id, type, fid, seq) VALUES ($1, $2, $3, $4);"
                  (vector-ref record 0) (symbol->immutable-string (vector-ref record 1))
                  (vector-ref record 2) (vector-ref record 3)))))

(define make-job : (-> (-> String) Index (-> (Listof UUID)))
  (lambda [mkid fid]
    (λ [] (let ([ids (build-list (processor-count) (λ _ (mkid)))]
                [type (datum-name mkid)])
            (for/list : (Listof UUID) ([id (in-list ids)] [seq (in-naturals)])
              (vector id type fid seq))))))

(define uuids : (Listof (Listof (Futureof (Listof UUID))))
  (for/list ([mkid (in-list (list uuid:timestamp uuid:random))])
    (build-list (* (processor-count) 2) (λ [[fid : Index]] (future (make-job mkid fid))))))

(for ([jobs : (Listof (Futureof (Listof UUID))) (in-list uuids)])
  (for ([workers : (Futureof (Listof UUID)) (in-list jobs)])
    (map do-insert (touch workers))))

(query-rows :memory: "SELECT * FROM uuid;")
(disconnect :memory:)
