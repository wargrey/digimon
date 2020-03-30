#lang typed/racket

(require digimon/plist)
(require digimon/location)

(define Info.plist (build-path (assert (path-only (#%file)) path?) "Info.plist"))

(read-bplist Info.plist)
(bplist-dissect Info.plist)
