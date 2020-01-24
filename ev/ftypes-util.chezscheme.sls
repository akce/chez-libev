;; chez scheme ftypes FFI util functions.
;; Written by Akce 2019, 2020.
;; SPDX-License-Identifier: Unlicense
(library (ev ftypes-util)
  (export
   c_funcs
   enum
   locate-library-object)
  (import
   (chezscheme))

  ;; [syntax] c_funcs: converts scheme-like function names to c-like function names before passing to foreign-procedure.
  ;; ie, word separating hyphens are converted to underscores for c.
  ;; eg,
  ;; (c_funcs (str-length (string) int) ....)
  ;; is converted to:
  ;; (begin
  ;;   (define str-length (foreign-procedure "str_length" (string) int))
  ;;   ...)
  (define-syntax c_funcs
    (lambda (stx)
      (define string-map
        (lambda (func str)
          (list->string (map func (string->list str)))))
      (define symbol->curses-name
        (lambda (sym)
          (string-map (lambda (c)
                        (if (eqv? c #\-)
                            #\_ c))
                      (symbol->string sym))))
      (syntax-case stx ()
        [(_ (name args return))
         (quasisyntax
          (define name
            (foreign-procedure (unsyntax (symbol->curses-name (syntax->datum #'name))) args return)))]
        [(_ f ...) (syntax (begin (c_funcs f) ...))])))

  (define-syntax enum
    (syntax-rules ()
      [(_ name (symbol value) ...)
       (begin (define symbol value) ...)]))

  ;; [procedure] locate-library-object: find first instance of filename within (library-directories) object directories.
  ;; Returns full path of located file, including the filename itself. filename only if not found.
  (define locate-library-object
    (lambda (filename)
      (let loop ([fps (map (lambda (d) (string-append (cdr d) "/" filename)) (library-directories))])
        (cond
         [(null? fps)
          filename]
         [(file-exists? (car fps))
          (car fps)]
         [else
          (loop (cdr fps))])))))
