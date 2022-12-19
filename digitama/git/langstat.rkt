#lang typed/racket/base

(provide (all-defined-out))

(require "linguist.rkt")
(require "numstat.rkt")

(require "../system.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct git-language
  ([id : Natural]
   [name : String]
   [color : (Option Keyword)]
   [extensions : Language-Extensions]
   [lines : (Listof Git-Numstat)])
  #:type-name Git-Language
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-numstat->langstat : (-> (Listof Git-Numstat) (Listof Symbol) Boolean (Immutable-HashTable Natural Git-Language))
  (let ([initial-stat : (Immutable-HashTable Natural Git-Language) (hasheq)])
    (lambda [numstats types merge-group?]
      (define languages : (Listof Github-Language)
        (parameterize ([current-digimon "digimon"])
          (time (read-language-metainfos* (digimon-path 'language "github.yml") #:count-lines? #false))))
      
      (for/fold ([stats : (Immutable-HashTable Natural Git-Language) initial-stat])
                ([numstat (in-list numstats)])
        stats))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
