#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~space : (-> Integer String)
  (let ([space : (HashTable Integer String) (make-hasheq)])
    (lambda [n0]
      (define n : Index (if (index? n0) n0 0))
      
      (hash-ref! space n
                 (λ [] (make-string n #\space))))))

(define ~string : (-> String (Listof Any) String)
  (lambda [msgfmt argl]
    (if (null? argl) msgfmt (apply format msgfmt argl))))

(define ~string-lines : (-> (U String Bytes) (Pairof String (Listof String)))
  (let ([empty-lines (list "")])
    (lambda [s]
      (cond [(regexp-match? #px"[\r\n]+" s)
             (let ([lines (for/list : (Listof String) ([l (in-lines (if (bytes? s) (open-input-bytes s) (open-input-string s)))]) l)])
               (cond [(null? lines) empty-lines]
                     [else lines]))]
            [(bytes? s) (list (bytes->string/utf-8 s))]
            [else (list s)]))))
