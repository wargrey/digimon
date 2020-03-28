#lang typed/racket

(require digimon/plist)
(require digimon/location)

(define Info.plist (build-path (assert (path-only (#%file)) path?) "Info.plist"))

(plist-pretty-print Info.plist)
