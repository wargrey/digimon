#lang typed/racket/base

(require digimon/archive)

(module+ main
  (define /dev/zipout : Output-Port (open-output-bytes '/dev/zipout))
  (define entries : (Listof Archive-Entry)
    (list (make-archive-ascii-entry #"stored ascii" "stored/ascii.txt" #:methods '(stored))))

  (void (write-string "garbage" /dev/zipout))
  (zip-create /dev/zipout entries)
  (zip-extract (open-input-bytes (get-output-bytes /dev/zipout) 'zip)))
