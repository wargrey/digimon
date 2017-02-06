#lang digimon

(define uuids : (HashTable String Integer) (make-hash))
  
(for ([i (in-range 64)])
  (define uuid : String (uuid:timestamp))
  (hash-set! uuids uuid (add1 (hash-ref uuids uuid (const 0)))))

(for ([i (in-range 64)])
  (define uuid : String (uuid:random))
  (hash-set! uuids uuid (add1 (hash-ref uuids uuid (const 0)))))

(define errno : Natural
  (for/fold ([errno : Natural 0]) ([(uuid count) (in-hash uuids)])
    (if (= count 1) errno (add1 errno))))

uuids

(unless (zero? errno)
  (printf "~a duplicates~n" errno))
