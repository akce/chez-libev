#! /usr/bin/env -S chez-scheme --script

;; SPDX-License-Identifier: Unlicense

;; Small example of using ev library under Chez Scheme.
;; It demonstrates:
;; - ev-io waiting on a file (stdin)
;; - ev-prepare as a one-shot way to initialise ev-timer
;; - ev-timer to countdown to the end
;; - ev-async to display timer events and how useful this watcher would be for custom user events (via ev-async-send)
;; - rec as a way of defining self-contained watchers that cleanup after themselves without adding to any namespace.

(import
 (ev)
 (rnrs)
 (only (chezscheme) rec))

(define j 0)
(define tw #f)

(define asyncw
  (ev-async
    (lambda (w rev)
      (display "timer called ")(display j)(newline)
      (display "async after timer: pending ")(display (ev-async-pending? w))(newline))))

(define stdinw
  (ev-io 0 (evmask 'READ)
    (lambda (w rev)
      (display "key pressed: ")(display (read-char))(newline)
      (ev-io-stop w)
      (ev-break (evbreak 'ALL)))))

(rec prepw
  (ev-prepare
    (lambda (w rev)
      (display "enabling timer watcher..")(newline)
      (set! tw
        (ev-timer 1 5
          (lambda (timer i)
            (set! j (+ 1 j))
            (ev-async-send (ev-watcher-address asyncw))
            (display "async before timer: pending ")(display (ev-async-pending? (ev-watcher-address asyncw)))(newline)
            (when (> j 4)
              (ev-break (evbreak 'ONE))))))
      ;; once initialised, clear away the prepare watcher.
      (ev-free-watcher! prepw))))

(ev-run)
(ev-free-watcher! asyncw)
(ev-free-watcher! stdinw)
(ev-free-watcher! tw)
