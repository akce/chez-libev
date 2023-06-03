;; chez scheme ftypes FFI util functions.
;; Written by Jerry 2019-2021.
;; SPDX-License-Identifier: Unlicense
(library (ev ftypes-util)
  (export
   c-function c-default-function
   c-bitmap c-enum
   ftype-offsetof
   locate-library-object
   make-id-syntax)
  (import
   (chezscheme))

  (meta define string-map
        (lambda (func str)
          (list->string (map func (string->list str)))))

  (meta define syntax->function-name-string
        (lambda (stx)
          (datum->syntax
            stx
            (string-map (lambda (c)
                          (if (eqv? c #\-)
                              #\_ c))
                        (symbol->string (syntax->datum stx))))))

  (meta define has-ev-tstamp?
        (lambda (syms)
          (find
            (lambda (s)
              (eq? s 'ev-tstamp))
            (syntax->datum syms))))

  (meta define make-ev-tstamp-args
        (lambda (syms)
          (let ([ds (syntax->list syms)])
            (map
              (lambda (a v)
                (list
                  a
                  v
                  (if (eq? 'ev-tstamp (syntax->datum a))
                      #`(if (flonum? #,v)
                            #,v
                            (fixnum->flonum #,v))
                      v)))
              ds (generate-temporaries ds)))))

  ;; [syntax] c-function: converts scheme-like function names to c-like function names before passing to foreign-procedure.
  ;; ie, word separating hyphens are converted to underscores for c.
  ;; eg,
  ;; (c-function (str-length (string) int) ....)
  ;; is converted to:
  ;; (begin
  ;;   (define str-length (foreign-procedure "str_length" (string) int))
  ;;   ...)
  (define-syntax c-function
    (lambda (stx)
      (syntax-case stx ()
        [(_ (name args return))
         (has-ev-tstamp? #'args)
         (with-syntax ([function-string (syntax->function-name-string #'name)]
                       [((arg-type arg-name arg-call) ...) (make-ev-tstamp-args #'args)])
            #'(define name
                (lambda (arg-name ...)
                  (let ([fp (foreign-procedure function-string (arg-type ...) return)])
                    (fp arg-call ...)))))]
        [(_ (name args return))
         (with-syntax ([function-string (syntax->function-name-string #'name)])
            #'(define name
                (foreign-procedure function-string args return)))]
        [(_ fdef ...)
         #'(begin
             (c-function fdef) ...)])))

  ;; [syntax] c-default-function: define c functions that take a default argument.
  ;; This behaves like c-function, except it first takes a (type, instance) pair.
  ;; c-default-function is useful for those c modules that define a bunch of functions that take
  ;; the same struct as the first argument.
  ;;
  ;; The expansion of this definition:
  ;; (c-default-function (type (current-parameter))
  ;;   (func-name1 (arg1) int)
  ;;   ...)
  ;; will look like:
  ;; (begin
  ;;   (define func-name1
  ;;     (let ([ffi-func (foreign-procedure "func_name1" (type arg1) int)])
  ;;       (lambda args (apply ffi-func (current-parameter) args))))
  ;;   ...)
  (define-syntax c-default-function
    (lambda (stx)
      (syntax-case stx ()
        [(_ (def-type def-instance) (name args return) ...)
         (with-syntax ([(function-string ...)
                        (map syntax->function-name-string #'(name ...))]
                       [(((arg-type arg-name arg-call) ...) ...)
                        (map
                          make-ev-tstamp-args
                          #'(args ...))])
            #'(begin
                (define name
                  (let ([ffi-func (foreign-procedure function-string (def-type arg-type ...) return)])
                    (lambda (arg-name ...)
                      (ffi-func def-instance arg-call ...)))) ...))])))

  ;; parse-enum-bit-defs: internal function.
  ;; parses enumdefs (for c-enum) and bitdefs (for c-bitmap).
  ;;
  ;; ebdefs is expected to be a mixed list of symbols / (symbol . id-number)...
  ;;
  ;; Return a list containing (syntax symbol) . (syntax id-number) pairs, suitable for use in with-syntax.
  (meta define parse-enum-bit-defs
    (lambda (ebdefs)
      (let loop ([i 0] [ds ebdefs])
        (cond
         [(null? ds) #'()]
         [else
          (syntax-case (car ds) ()
            [(id val)
             (cons (list #'id #'val) (loop (+ (syntax->datum #'val) 1) (cdr ds)))]
            [id
             (identifier? #'id)
             (cons (list #'id (datum->syntax #'id i)) (loop (+ i 1) (cdr ds)))])]))))

  ;; [syntax] c-enum: creates a function representing the enumeration.
  ;; c-enum will create a function called 'name'.
  ;; enum values are assumed to start from 0 and increase by one from the previous value unless a value is provided.
  ;;
  ;; Without args, the function will return an assoc list of symbol/value pairs.
  ;;
  ;; With one arg, the function will check the type of the arg and return a value accordingly.
  ;; ie, return an identifier (symbol) when arg is a number, and a number if the arg is a symbol.
  ;;
  ;; Error conditions are raised for invalid or unknown input values.
  ;;
  ;; eg, a c-style enum
  ;;    typedef enum { a, b = 3, c, } name;
  ;;
  ;; could be represented as:
  ;; > (c-enum name a (b 3) c)
  ;; > name
  ;; #<procedure name>
  ;; > (name)
  ;; ((a . 0) (b . 3) (c . 4))
  ;; > (name 3)
  ;; b
  ;; > (name 'c)
  ;; 4
  ;; > (name 2)
  ;; Exception in name: identifier not defined for value 2 in enum
  ;; Type (debug) to enter the debugger.
  ;; > (name 'j)
  ;; Exception in name: value not defined for identifier j in enum
  ;; Type (debug) to enter the debugger.
  (define-syntax c-enum
    (lambda (stx)
      (syntax-case stx ()
        [(_ name enumdef1 enumdef* ...)
         (with-syntax
          ([((esym eid) ...) (parse-enum-bit-defs #'(enumdef1 enumdef* ...))])
          #'(define name
              (case-lambda
               [()
                '((esym . eid) ...)]
               [(x)
                (name
                 (cond
                  [(symbol? x)  'get-value]
                  [(number? x)  'get-id]
                  [else x])
                 x)]
               [(cmd arg)
                (case cmd
                  [(get-value)
                   (case arg
                     [(esym) eid] ...
                     [else (error (syntax->datum #'name) (format #f "value not defined for identifier ~s in enum" arg))])]
                  [(get-id)
                   (case arg
                     [(eid) 'esym] ...
                     [else (error (syntax->datum #'name) (format #f "identifier not defined for value ~d in enum" arg))])]
                  [else
                   (error (syntax->datum #'name) (format #f "unknown enum command ~s" cmd))])])))])))

  ;; [syntax] c-bitmap: define a bitmap enumeration.
  ;; Behaves as c-enum, except each field defines a bit. Querying for symbols returns a list.
  ;;
  ;; eg,
  ;; > (c-bitmap flags (A 1) (B 4) (C 8))
  ;; > (flags)
  ;; ((A . 1) (B . 4) (C . 8))
  ;; > (flags 'A)
  ;; 1
  ;; > (flags 'B)
  ;; 4
  ;; > (flags 'A 'C)
  ;; 9
  ;; > (flags #b0)
  ;; ()
  ;; > (flags #b1)
  ;; (A)
  ;; > (flags #b10)
  ;; ()
  ;; > (flags #b111)
  ;; (A B)
  ;; > (flags #b1111)
  ;; (A B C)
  ;; > (flags #b1100)
  ;; (B C)
  (define-syntax c-bitmap
    (lambda (stx)
      (syntax-case stx ()
        [(_ name bitdef1 bitdef* ...)
         (with-syntax
           ;; TODO define parser separate from enum that defaults to left shifting successive fields.
           ;; TODO or at least forces explicit setting of field bits.
           ([((esym eid) ...) (parse-enum-bit-defs #'(bitdef1 bitdef* ...))])
           #'(define name
               (let
                 ([sym->int
                    (lambda (sym)
                      (case sym
                        [(esym) eid] ...
                        [else
                          (errorf 'name "~a not found in bitmap" sym)]))])
                 (case-lambda
                   [()
                    '((esym . eid) ...)]
                   [args
                     (cond
                       [(symbol? (car args))
                        (fold-left
                          (lambda (acc x)
                            (bitwise-ior acc (sym->int x)))
                          0
                          args)]
                       [(number? (car args))
                        (let loop ([ids '(eid ...)] [syms '(esym ...)])
                          (cond
                            [(null? ids)
                             '()]
                            [(= (bitwise-and (car args) (car ids)) (car ids))
                             (cons (car syms) (loop (cdr ids) (cdr syms)))]
                            [else
                              (loop (cdr ids) (cdr syms))]))]
                       [else
                         (errorf 'name "unknown bitmap command ~a" args)])]))))])))

  ;; [syntax] ftype-offsetof byte offset of field from start of struct
  ;; Equivalent to C99's offsetof() macro.
  (define-syntax ftype-offsetof
    (syntax-rules ()
      [(_ type field)
       (ftype-pointer-address
         (ftype-&ref type (field) (make-ftype-pointer type 0)))]))

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
          (loop (cdr fps))]))))

  (define make-id-syntax
    (lambda (ctx . s-args)
      (datum->syntax
        ctx
        (string->symbol
          (apply string-append
                 (map (lambda (s)
                        (if (string? s)
                          s
                          (symbol->string (syntax->datum s)))) s-args))))))
  )
