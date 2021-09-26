#! /usr/bin/env -S chez-scheme --debug-on-exception --quiet --program

;; A simple demo of how a hex dump script could be written using ev and posix primitives.
;; ev-io is used to read 'chunk-size' bytes from a source file and hex-dumps that to screen.
;;
;; The library and #defines are Linux specific.
;;
;; Use:
;; $ hex-dump <filename>

(import
  (chezscheme)
  (ev))

(define chunk-size 16)

;; OS level defines. The default is for Linux.
(define lib
  (load-shared-object "libc.so.6"))
(define RDONLY
  #o000)
(define open
  (foreign-procedure "open" (string int) int))
;; Define as 'read-fd' as standard Scheme already has a 'read' function.
(define read-fd
  (foreign-procedure "read" (int (* unsigned-8) size_t) ssize_t))
(define close
  (foreign-procedure "close" (int) int))

(define source-file
  (guard (e
           [else
             (format #t "~a <file-name>~n" (car (command-line)))
             (exit 1)])
    (cadr (command-line))))

(define buf (make-ftype-pointer unsigned-8 (foreign-alloc 1024)))

(define ofd (open source-file RDONLY))

(define u8*->bytevector
  (lambda (memory len)
    (let ([bv (make-bytevector len)])
      (for-each
        (lambda (i)
          (bytevector-u8-set! bv i (ftype-ref unsigned-8 () memory i)))
        (iota len))
      bv)))

(define byte->padded-hex-string
  (lambda (b)
    ;; This should be quicker than calling format.
    (if (fx<? b 16)
      (string-append "0" (number->string b 16))
      (number->string b 16))))

;; Convert a byte to a displayable hex-char.
;; Derived from observing the output from:
;; > (map integer->char (iota 256))
(define byte->ascii-char
  (lambda (b)
    (cond
      [(fx=? b 32)
       #\space]
      [(fx<? b 33)
       #\.]
      [(fx<? b 126)
       (integer->char b)]
      [(fx<? b 161)
       #\.]
      [else
        (integer->char b)])))

;; Very basic hex dump.
;; Does not include address/offset in output.
(define hex-draw-bytes
  (lambda (bytes)
    (for-each
      (lambda (i)
        (format #t "~a " (byte->padded-hex-string (bytevector-u8-ref bytes i))))
      (iota (bytevector-length bytes)))
    (display " ")
    (for-each
      (lambda (i)
        (format #t "~a " (byte->ascii-char (bytevector-u8-ref bytes i))))
      (iota (bytevector-length bytes)))
    (newline)))

(define io-watcher
  (ev-io ofd (evmask 'READ)
    (lambda (w rev)
      (let ([rc (read-fd ofd buf chunk-size)])
        (cond
          [(> rc 0)
           #;(format #t "received ~d bytes~n" rc)
           (hex-draw-bytes (u8*->bytevector buf rc))]
          [(= rc 0)
           #;(format #t "received EOF~n")
           (ev-break (evbreak 'ONE))]
          [else
            (error 'fread "read error")])))))

(ev-run)
(close ofd)
(ev-free-watcher! io-watcher)
(foreign-free (ftype-pointer-address buf))
