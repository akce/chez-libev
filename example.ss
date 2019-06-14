#! /usr/bin/env scheme-script

;; Small example of using ev library under chez scheme.

(import
 (ev)
 (rnrs))

(define tw
  (ev-timer 1 5.
    (let ([j 0])
      (lambda (loop timer i)
        (set! j (+ 1 j))
        (display "timer called ")(display j)(newline)
        (when (> j 4)
          (ev-break EVBREAK_ONE))))))
(ev-timer-start tw)
(ev-run)
