#lang typed/racket/base

(provide (all-defined-out))

(define thread-safe-kill : (->* ((U Thread (Listof Thread))) (Index) Void)
  (lambda [thds [timeout-ms 1024]]
    (define thds : (Listof Thread)
      (for/list ([thd (if (list? thds) (in-list thds) (in-value thds))])
        (break-thread thd)
        thd))

    (when (pair? thds)
      (define timeout-evt (alarm-evt (+ (current-inexact-milliseconds) timeout-ms)))

      (let wait ([thds : (Listof Thread) thds])
        (with-handlers ([exn:break? void])
          (define who (apply sync/enable-break timeout-evt thds))

          (when (thread? who)
            (wait (remove who thds))))

        (when (pair? thds)
          (for-each kill-thread thds))))))
