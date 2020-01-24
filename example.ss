#! /usr/bin/env scheme-script

;; Small example of using ev library under chez scheme.

(import
 (ev)
 (rnrs))

(define tw
  (ev-timer 1 5.
    (let ([j 0])
      (lambda (timer i)
        (set! j (+ 1 j))
        (display "timer called ")(display j)(newline)
        (when (> j 4)
          (ev-break (evbreak 'EVBREAK_ONE)))))))

(define stdinw
  (ev-io 0 (evmask 'EV_READ)
    (lambda (w rev)
      (display "key activity ")(display rev)(newline)
      (ev-io-stop w)
      (ev-break (evbreak 'EVBREAK_ALL)))))

(ev-run)
