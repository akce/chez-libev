#! /usr/bin/env scheme-script

;; Small example of using ev library under chez scheme.

(import
 (ev)
 (rnrs))

(define tw
  ;; ev-tstamp is a double, so must pass numbers of that type to ev-timer.
  (ev-timer 1.0 5.
    (let ([j 0])
      (lambda (loop timer i)
        (set! j (+ 1 j))
        (display "timer called ")(display j)(newline)
        (when (> j 4)
          (ev-break loop EVBREAK_ONE))))))
(ev-timer-start EV_DEFAULT tw)

(ev-run EV_DEFAULT)
