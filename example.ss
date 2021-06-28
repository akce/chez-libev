#! /bin/sh
#|
exec /usr/bin/env chez-scheme --script "$0" "$@"

# BUG? Running via scheme-script results in watcher-guardian thinking every object is dereferenced
# after (collect) is first called.
#! /usr/bin/env scheme-script
|#

;; Small example of using ev library under chez scheme.

(import
 (ev)
 (chezscheme))

(collect-notify #t)
(define tw
  (ev-timer 1 5
    (let ([j 0])
      (lambda (w i)
        (set! j (+ 1 j))
        (display "timer called ")(display j)(newline)
        (when (= j 2)
          (display "de-reference stdin watcher\n")
          (set! stdinw #f))
        (collect)
        (free-watchers)
        (when (> j 5)
          (ev-break (evbreak 'ONE)))))))

(define stdinw
  (ev-io 0 (evmask 'READ)
    (lambda (w rev)
      (display "key pressed: ")(display (read-char))(newline)
      (ev-io-stop w)
      (ev-break (evbreak 'ALL)))))

(ev-run)
