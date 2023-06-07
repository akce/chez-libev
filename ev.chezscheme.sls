;; libev bindings for Chez Scheme.
;; Written by Jerry 2019-2023.
;; SPDX-License-Identifier: Unlicense
(library (ev)
  (export
    ;; watcher context functions.
   ev-free-watcher!
   ev-watcher-address

   ;; These have issues, so don't export for now.
   ;;free-watchers collect-watchers

    ;; enums, bitmaps and IDs.
   EV_VERSION_MAJOR EV_VERSION_MINOR
   evmask EV_NONE EV_IO
   evflag EVFLAG_AUTO
   evbackend
   evrun
   evbreak
   ev-child-trace
   EV_DEFAULT

   ;; parameter.
   current-loop

   ;; meta funcs.
   ev-version-major ev-version-minor ev-supported-backends ev-recommended-backends ev-embeddable-backends

   ;; time related.
   ev-time ev-sleep

   ;; loop related.
   ev-default-loop ev-loop-new ev-now ev-loop-destroy ev-loop-fork ev-backend ev-now-update ev-run ev-break ev-ref ev-unref
   ev-iteration ev-depth ev-verify ev-set-io-collect-interval ev-set-timeout-collect-interval

   ;; advanced threading.
   ev-set-userdata ev-userdata ev-set-invoke-pending-cb ev-set-loop-release-cb

   ev-pending-count ev-invoke-pending

   ;; timer handling.
   ev-suspend ev-resume

   ;; generic event watcher controls.
   ev-feed-event ev-feed-fd-event ev-feed-signal ev-feed-signal-event ev-invoke ev-clear-pending

   ;; Event watchers.
   ev-io
   (rename
     #;(ev-io-set ev-io-set!)
     (ev-io-fd-get ev-io-fd)
     (ev-io-events-get ev-io-events)
     #;(ev-io-modify ev-io-modify!))
   ev-io-start ev-io-stop

   ev-timer
   (rename
     #;(ev-timer-set ev-timer-set!)
     (ev-timer-repeat-get ev-timer-repeat)
     (ev-timer-repeat-set ev-timer-repeat-set!))
   ev-timer-start ev-timer-stop ev-timer-again ev-timer-remaining

   ev-periodic
   (rename
     #;(ev-periodic-set ev-periodic-set!)
     (ev-periodic-offset-get ev-periodic-offset)
     (ev-periodic-offset-set ev-periodic-offset-set!)
     (ev-periodic-interval-get ev-periodic-interval)
     (ev-periodic-interval-set ev-periodic-interval-set!)
     (ev-periodic-rcb-get ev-periodic-rcb)
     (ev-periodic-rcb-set ev-periodic-rcb-set!))
   ev-periodic-start ev-periodic-stop ev-periodic-again

   ev-signal
   (rename
     #;(ev-signal-set ev-signal-set!)
     (ev-signal-signum-get ev-signal-signum))
   ev-signal-start ev-signal-stop

   ev-child
   (rename
     #;(ev-child-set ev-child-set!)
     (ev-child-pid-get ev-child-pid)
     (ev-child-rpid-get ev-child-rpid)
     (ev-child-rpid-set ev-child-rpid-set!)
     (ev-child-rstatus-get ev-child-rstatus)
     (ev-child-rstatus-set ev-child-rstatus-set!))
   ev-child-start ev-child-stop

   ;; Disable for now. Handling path strings and stat structs needs to be thought about.
   #;ev-stat
   #;(rename
     (ev-stat-set ev-stat-set!))
   ;;ev-stat-start ev-stat-stop ev-stat-stat

   ev-idle
   #;(rename
     (ev-idle-set ev-idle-set!))
   ev-idle-start ev-idle-stop

   ev-prepare
   #;(rename
     (ev-prepare-set ev-prepare-set!))
   ev-prepare-start ev-prepare-stop

   ev-check
   #;(rename
     (ev-check-set ev-check-set!))
   ev-check-start ev-check-stop

   ev-embed
   #;(rename
     (ev-embed-set ev-embed-set!)
     (ev-embed-other-get ev-embed-other))
   ev-embed-start ev-embed-stop ev-embed-sweep

   ev-fork
   #;(rename
     (ev-fork-set ev-fork-set!))
   ev-fork-start ev-fork-stop

   ev-cleanup
   #;(rename
     (ev-cleanup-set ev-cleanup-set!))
   ev-cleanup-start ev-cleanup-stop

   ev-async
   #;(rename
     (ev-async-set ev-async-set!))
   (rename (ev-async-pending-get ev-async-pending?))
   ev-async-start ev-async-stop ev-async-send

   (rename
     (ev-watcher-is-active ev-watcher-active?))

   ev-ms

   ;; Convenience wrappers for the 3 modes of ev-periodic timers.
   ;; See libev(3).
   ev-absolute-timer
   ev-interval-timer
   ev-manual-timer
   )
  (import
   (chezscheme)
   (ev ftypes-util))

  ;; #t: use pure scheme re-implementation of libev C macros.
  ;; #f: use macro->function exports from ev-ffi shared lib.
  ;; Using the pure version is easier on systems without access to a C compiler and may
  ;; have benefits when compiling whole programs, however it has higher risk of being
  ;; out of sync with the installed libev.so shared lib.
  (meta define pure? #f)

  (define load-libev
    (load-shared-object "libev.so.4"))

  (c-bitmap evmask
   (UNDEF		#xFFFFFFFF)
   (READ		#x01)
   (WRITE		#x02)
   (_IOFDSET		#x80)
   (TIMER		#x00000100)
   (PERIODIC		#x00000200)
   (SIGNAL		#x00000400)
   (CHILD		#x00000800)
   (STAT		#x00001000)
   (IDLE		#x00002000)
   (PREPARE		#x00004000)
   (CHECK		#x00008000)
   (EMBED		#x00010000)
   (FORK		#x00020000)
   (CLEANUP		#x00040000)
   (ASYNC		#x00080000)
   (CUSTOM		#x01000000)
   (ERROR		#x80000000))
  (define EV_NONE	#x00)
  (define EV_IO		(evmask 'READ))

  (c-bitmap evflag
   (NOENV		#x01000000)
   (FORKCHECK		#x02000000)
   (NOINOTIFY		#x00100000)
   (SIGNALFD		#x00200000)
   (NOSIGMASK		#x00400000))
  (define EVFLAG_AUTO	#x00000000)

  (c-bitmap evbackend
   (MASK		#x0000FFFF)
   (SELECT		#x00000001)
   (POLL		#x00000002)
   (EPOLL		#x00000004)
   (KQUEUE		#x00000008)
   (DEVPOLL		#x00000010)
   (PORT		#x00000020)
   (ALL			#x0000003F))

  (c-enum evrun
   (NOWAIT		1)
   (ONCE		2))

  (c-enum evbreak
   (CANCEL		0)
   (ONE			1)
   (ALL			2))

  (c-enum ev-child-trace
    (TERM	0)
    (ANY	1))

  (define-ftype ev-loop*		void*)
  ;; tv-tstamp is a 'double' unless EV_TSTAMP_T overrides it.
  (define-ftype ev-tstamp		double)

  ;; memory alloc function prototype.
  (define-ftype realloc-fn	(function (void* long)	void*))
  (define-ftype msg-cb-fn	(function (string)	void))
  (define-ftype ev-loop-callback (function (ev-loop*)	void))

  (meta-cond
    [pure?
      ;;;; This section contains the pure scheme portion of these bindings.
      ;;;; It implements:
      ;;;; - the C watcher structs as ftypes as well as getter/setter functions for its fields
      ;;;; - struct memory allocators
      ;;;; - constructors: ie, alloc + init = make
      ;;;; - specialised watcher type field setters
      ;;;; - high level watcher context constructors.
      ;;;;
      ;;;; ev-TYPE-init functions are *not* (yet?) implemented.
      ;;;;
      ;;;; Scheme code that re-implements a C macro should contain a snippet of that code
      ;;;; prefixed by CREF to show what the scheme should do.
      ;;;; It may also help spot when/if that scheme code goes stale..

      ;; Hardcode these to the values in ev.h to keep track of which version this is developed for.
      (define ev-version-major-def (lambda () 4))
      (define ev-version-minor-def (lambda () 33))

      (define memset
        (foreign-procedure "memset" (void* int size_t) void*))

      (define-ftype ev-watcher-list*	void*)
      ;; !!! ev_stat embeds two 'struct stat' objects which have a number of members and are OS dependant.
      ;; Magic number 144 = sizeof(struct stat): generated via platform.c (sizeof-struct-stat).
      (define-ftype struct-stat (array 144 unsigned-8))
      ;; !!! For Linux, this is currently an int: see <bits/types.h>
      (define-ftype sig-atomic-t int)

      ;; [syntax] define-ev-type define ftype and base functions for an ev type struct.
      ;; field-acl:
      ;;  - private is readable anytime and not writable by us, ie. internal to libev
      ;;    However, we'll create setters for these as we're implementing our own init funcs.
      ;;  - ro is readable anytime, but only writable when watcher is inactive
      ;;  - rw is readable and writable anytime
      ;;  - unused is ignored, but only generated in the ftype struct for padding
      ;;  - (ref ev-acl) means reference (ftype-&ref) to embedded struct to be returned due to
      ;;    ftype-ref only working on simple/atomic types, not structs

      (define-syntax ev-type-builder
        (lambda (x)
          (syntax-case x ()
            [(_ type-name type-name-t ev-TYPE-cb-t (init-args* ...) (field-name field-type field-acl) ...)
             (with-syntax ([(ev-TYPE-FIELD-get ...) 
                            (map
                              (lambda (syn access)
                                (define get-name
                                  (make-id-syntax #'type-name #'type-name "-" syn "-get"))
                                (cond
                                  [(eq? access 'unused)
                                   #'(begin)]
                                  [(pair? access)
                                   #`(define #,get-name
                                       (lambda (ptr)
                                         (ftype-&ref type-name-t (#,syn) ptr)))]
                                  [else
                                    #`(define #,get-name
                                        (lambda (ptr)
                                          (ftype-ref type-name-t (#,syn) ptr)))]))
                              #'(field-name ...) (map syntax->datum #'(field-acl ...)))]
                           [(ev-TYPE-FIELD-set ...)
                            (map
                              (lambda (syn type access)
                                (define setter-funcname (make-id-syntax #'type-name #'type-name "-" syn "-set"))
                                (cond
                                  [(or (eq? access 'unused) (pair? access))
                                       ;; NOTE: setters are never generated for (ref ev-acl), instead get the member
                                       ;; reference and use setters on that.
                                       #'(begin)]
                                  [(eq? type 'ev-tstamp)
                                   ;; Generate ev-tstamp setters that cast integers to flonums.
                                   #`(define #,setter-funcname
                                       (lambda (ptr val)
                                         (ftype-set! type-name-t (#,syn) ptr (if (flonum? val) val (fixnum->flonum val)))))]
                                  [else
                                    #`(define #,setter-funcname
                                        (lambda (ptr val)
                                          (ftype-set! type-name-t (#,syn) ptr val)))]))
                                #'(field-name ...)
                                (map syntax->datum #'(field-type ...))
                                (map syntax->datum #'(field-acl ...)))]
                            [allocz-ev-TYPE (make-id-syntax #'type-name "allocz-" #'type-name)]
                            [ev-TYPE-init (make-id-syntax #'type-name #'type-name "-init")]
                            [make-ev-TYPE (make-id-syntax #'type-name "make-" #'type-name)]
                            [ev-TYPE-set (make-id-syntax #'type-name #'type-name "-set")]
                            [ev-TYPE-start (make-id-syntax #'type-name #'type-name "-start")]
                            [ev-TYPE-stop (make-id-syntax #'type-name #'type-name "-stop")])
             #'(begin
                 (define-ftype
                   [type-name-t
                     (struct
                       (field-name field-type)
                       ...)]
                   [ev-TYPE-cb-t
                     (function (ev-loop* (* type-name-t) int) void)])
                 ev-TYPE-FIELD-get ...
                 ev-TYPE-FIELD-set ...
                 (define allocz-ev-TYPE
                   (lambda ()
                     ;; Allocate and zero watcher struct memory.
                     (let ([mem (foreign-alloc (ftype-sizeof type-name-t))])
                       (memset mem 0 (ftype-sizeof type-name-t))
                       (make-ftype-pointer type-name-t mem))))
                 (define ev-TYPE-init
                   (lambda (ev-t cb)
                     ;; CREF: do {			\
                     ;;   ((ev_watcher *)(void *)(ev))->active  =	\
                     ;;   ((ev_watcher *)(void *)(ev))->pending = 0;	\
                     ;;   ev_set_priority ((ev), 0);			\
                     ;;   ev_set_cb ((ev), cb_);			\
                     ;; } while (0)
                     (ftype-set! type-name-t (active) ev-t 0)
                     (ftype-set! type-name-t (pending) ev-t 0)
                     ;; EV_DECL_PRIORITY
                     ;; XXX ev_set_priority() does nothing if EV_MINPRI == EV_MAXPRI !!!
                     (ftype-set! type-name-t (priority) ev-t 0)
                     (ftype-set! type-name-t (cb) ev-t cb)))
                 (define make-ev-TYPE
                   (lambda (init-args* ... cb)
                     (let ([ptr (allocz-ev-TYPE)])
                       (ev-TYPE-init ptr cb)
                       (ev-TYPE-set ptr init-args* ...)
                       ptr)))
                 (define type-name
                   (lambda (init-args* ... cb)
                     (let* ([fp-callback (make-ftype-pointer
                                           ev-TYPE-cb-t
                                           (lambda (loop w rev)
                                             (parameterize ([current-loop loop])
                                               (cb w rev))))]
                            [watcher (make-ev-TYPE init-args* ... fp-callback)]
                            [wcxt (make-wc watcher fp-callback (lambda () (ev-TYPE-stop watcher)))])
                       ;; Note: (locked-object? (foreign-callable-code-object (ftype-pointer-address fp-callback))) => #t
                       (watcher-guardian wcxt)
                       (ev-TYPE-start watcher)
                       ;; Return watcher-context. Caller should keep a reference to this and/or manage (collect-watchers).
                       ;; FIXME caller cannot pass wcxt to (stop-<event-type>).
                       wcxt)))))])))

      (define-syntax define-ev-type
        (lambda (x)
          (syntax-case x ()
            [(_ type-name init-args field-def* ...)
             (with-syntax ([type-name-t (make-id-syntax #'type-name #'type-name "-t")]
                           [ev-TYPE-cb-t (make-id-syntax #'type-name #'type-name "-cb-t")])
               #'(begin
                   (ev-type-builder type-name type-name-t ev-TYPE-cb-t init-args
                     ;; EV_WATCHER(type)
                     (active	int	private)
                     (pending	int	private)
                     ;; EV_DECL_PRIORITY
                     ;; TODO priority will not be defined if EV_MINPRI == EV_MAXPRI !!!
                     (priority	int	private)
                     ;; EV_COMMON
                     (data	void*	rw)
                     ;; EV_CB_DECLARE(type)
                     (cb	(* ev-TYPE-cb-t)	private)
                     ;; type specific field definitions..
                     field-def*
                     ...)))])))

      (define-syntax define-ev-type-list
        (syntax-rules ()
          [(_ type-name init-args field-def* ...)
           (define-ev-type type-name init-args
             (next	ev-watcher-list*	private)
             field-def*
             ...)]))

      (define-syntax define-ev-type-time
        (syntax-rules ()
          [(_ type-name init-args field-def* ...)
           (define-ev-type type-name init-args
             (at	ev-tstamp	private)
             field-def*
             ...)]))

      (define-syntax nop
        (syntax-rules ()
          [(_ name arg* ...)
           (define name
             (lambda args
               (void)))]))

      ;; Event watcher typedef and fields setter function defines.
      ;; The field setter function is useful in that it abstracts setting all the user facing event specific
      ;; fields. ie, combining these with allocz-TYPE + ev-init gives an easy make-TYPE function.
      ;; These could've been generated except some do custom/non-obvious things. eg, ev-io-set & _IOFDSET.
      ;; Keep these out of the grand syntax generator for readability and (hopefully) easier maintenance.
      ;; At least for now...
      (define-ev-type-list ev-io (fd events)
        (fd		int	ro)
        (events	int	ro))
      (define ev-io-set
        (lambda (ev-t fd events)
          ;; CREF: do { (ev)->fd = (fd_); (ev)->events = (events_) | EV__IOFDSET; } while (0)
          (ev-io-fd-set ev-t fd)
          (ev-io-events-set ev-t (bitwise-ior events (evmask '_IOFDSET)))))

      (define-ev-type-time ev-timer (after repeat)
        (repeat	ev-tstamp	rw))
      (define ev-timer-set
        (lambda (ev-t after repeat)
          ;; CREF: do { ((ev_watcher_time *)(ev))->at = (after_); (ev)->repeat = (repeat_); } while (0)
          (ev-timer-at-set ev-t after)
          (ev-timer-repeat-set ev-t repeat)))

      (define-ev-type-time ev-periodic (offset interval rcb)
        (offset	ev-tstamp	rw)
        (interval	ev-tstamp	rw)
        (rcb		(* ev-periodic-cb-t)	rw))
      (define ev-periodic-set
        (lambda (ev-t offset interval reschedule-cb)
          ;; CREF: do { (ev)->offset = (ofs_); (ev)->interval = (ival_); (ev)->reschedule_cb = (rcb_); } while (0)
          (ev-periodic-offset-set ev-t offset)
          (ev-periodic-interval-set ev-t interval)
          (ev-periodic-rcb-set ev-t reschedule-cb)))

      (define-ev-type-list ev-signal (signum)
        (signum	int	ro))
      (define ev-signal-set
        (lambda (ev-t signum)
          ;; CREF: do { (ev)->signum = (signum_); } while (0)
          (ev-signal-signum-set ev-t signum)))

      (define-ev-type-list ev-child (pid trace)
        (flags	int	private)
        (pid		int	ro)
        (rpid		int	rw)
        (rstatus	int	rw))
      ;; `trace` must be a value in (ev-child-trace).
      (define ev-child-set
        (lambda (ev-t pid trace)
          ;; CREF: do { (ev)->pid = (pid_); (ev)->flags = !!(trace_); } while (0)
          ;; !!trace is a C trick to convert the integer value to 0 or 1.
          (ev-child-pid-set ev-t pid)
          (ev-child-flags-set ev-t trace)))

      (define-ev-type-list ev-stat (path interval)
        (timer	ev-timer-t	(ref private))
        (interval	ev-tstamp	ro)
        (path		void*		ro)	; FIXME should be const char*, not void*
        (prev		struct-stat	(ref ro))
        (attr		struct-stat	(ref ro))
        (wd		int		private)	; acl not specified in header for this field
        )
      (define ev-stat-set
        (lambda (ev-t path interval)
          ;; CREF: do { (ev)->path = (path_); (ev)->interval = (interval_); (ev)->wd = -2; } while (0)
          (ev-stat-path-set ev-t path)
          (ev-stat-interval-set ev-t interval)
          (ev-stat-wd-set ev-t -2)))

      (define-ev-type ev-idle ())
      ;; CREF: /* nop, yes, this is a serious in-joke */
      (nop ev-idle-set)

      (define-ev-type ev-prepare ())
      ;; CREF: /* nop, yes, this is a serious in-joke */
      (nop ev-prepare-set)

      (define-ev-type ev-check ())
      ;; CREF: /* nop, yes, this is a serious in-joke */
      (nop ev-check-set)

      (define-ev-type ev-fork ())
      ;; CREF: /* nop, yes, this is a serious in-joke */
      (nop ev-fork-set)

      (define-ev-type ev-cleanup ())
      ;; CREF: /* nop, yes, this is a serious in-joke */
      (nop ev-cleanup-set)

      (define-ev-type ev-embed (other)
        (other	ev-loop*	ro)
        (io		ev-io-t		(ref private))
        (prepare	ev-prepare-t	(ref private))
        (check	ev-check-t	unused)
        (timer	ev-timer-t	unused)
        (periodic	ev-periodic-t	unused)
        (idle		ev-idle-t	unused)
        (fork		ev-fork-t	(ref private))
        (cleanup	ev-cleanup-t	unused))
      (define ev-embed-set
        (lambda (ev-t other)
          ;; CREF: do { (ev)->other = (other_); } while (0)
          (ev-embed-other-set ev-t other)))

      (define-ev-type ev-async ()
        (sent	sig-atomic-t	(ref private)))
      ;; CREF: /* nop, yes, this is a serious in-joke */
      (nop ev-async-set)

      (define-ftype ev-periodic-rcb-t	(function ((* ev-periodic-t) ev-tstamp)		ev-tstamp))

      ;;;; This section contains pure scheme re-implementations of libev ev.h C-macros.

      (define ev-io-modify
        (lambda (ev-t events)
          ;; CREF: do { (ev)->events = (ev)->events & EV__IOFDSET | (events_); } while (0)
          ;; bitwise & (AND) has higher precedence than bitwise | (OR), so this looks
          ;; like it disables all existing events except IOFDSET and enables new events.
          (ev-io-events-set ev-t (bitwise-ior events (evmask '_IOFDSET)))))

      (define ev-watcher-is-active
        (lambda (watcher)
          ;; CREF: (0 + ((ev_watcher *)(void *)(ev))->active) /* ro, true when the watcher has been started */
          ;; HACK: Always cast to an ev-io-t, this gets around ftypes structs not having parents.
          ;; This is safe so long as all our struct types share the same base field layout.
          (let ([ptr (make-ftype-pointer ev-io-t (ftype-pointer-address watcher))])
            (ev-io-active-get ptr))))
      ]
  [else

    ;;;; ev-ffi bindings.

    (define load-libev-ffi
      (load-shared-object (locate-library-object "ev/libchez-ffi.so")))

    ;; TODO need to look at these properly. These are parent types.
    (define-ftype ev-watcher*	void*)
    (define-ftype ev-watcher-time*	void*)

    (define-syntax define-watcher
      (lambda (x)
        (syntax-case x ()
          [(k watcher-name args ...)
           (with-syntax ([make-watcher-func (make-id-syntax #'k "make-" #'watcher-name)]
                         [start-watcher-func (make-id-syntax #'k #'watcher-name "-start")]
                         [stop-watcher-func (make-id-syntax #'k #'watcher-name "-stop")]
                         [ev-TYPE-t (make-id-syntax #'k #'watcher-name "-t")]
                         [ev-TYPE-cb-t (make-id-syntax #'k #'watcher-name "-cb-t")])
             #'(begin
                 (define-ftype
                   [ev-TYPE-t (struct)]
                   [ev-TYPE-cb-t (function (ev-loop* (* ev-TYPE-t) int) void)])
                 (define watcher-name
                   (lambda (args ... callback-func)
                     (let* ([fp-callback (make-ftype-pointer
                                           ev-TYPE-cb-t
                                           (lambda (loop w rev)
                                             (parameterize ([current-loop loop])
                                               (callback-func w rev))))]
                            [watcher (make-watcher-func args ... fp-callback)]
                            [wcxt (make-wc watcher fp-callback (lambda () (stop-watcher-func watcher)))])
                       ;; Note: (locked-object? (foreign-callable-code-object (ftype-pointer-address fp-callback))) => #t
                       (watcher-guardian wcxt)
                       (start-watcher-func watcher)
                       ;; Return watcher-context. Caller should keep a reference to this and/or manage (collect-watchers).
                       ;; FIXME caller cannot pass wcxt to (stop-<event-type>).
                       wcxt)))))])))

    (define-syntax batch
      (syntax-rules ()
        [(_ command (def ...) ...)
         (begin
           (command def ...)
           ...)]))

    (batch define-watcher
      (ev-io fd events)
      (ev-timer after repeat)
      (ev-periodic offset interval rcb)
      (ev-signal signum)
      (ev-child pid trace)
      (ev-stat path interval)
      (ev-idle)
      (ev-prepare)
      (ev-check)
      (ev-embed other)
      (ev-fork)
      (ev-cleanup)
      (ev-async))

    (define-ftype ev-periodic-rcb-t	(function ((* ev-periodic-t) ev-tstamp)		ev-tstamp))

    ;;; libev-ffi extensions. See ev/ev-ffi.c
    (c-function
      (ev-version-major-def	()		int)
      (ev-version-minor-def	()		int)
      ;; raw constructors (internal only).
      (make-ev-io		(int int (* ev-io-cb-t))	(* ev-io-t))
      (make-ev-timer	(ev-tstamp ev-tstamp (* ev-timer-cb-t))	(* ev-timer-t))
      (make-ev-periodic	(ev-tstamp ev-tstamp (* ev-periodic-rcb-t) (* ev-periodic-cb-t))	(* ev-periodic-t))
      (make-ev-signal	(int (* ev-signal-cb-t))	(* ev-signal-t))
      (make-ev-child	(int int (* ev-child-cb-t))	(* ev-child-t))
      (make-ev-stat	(string ev-tstamp (* ev-stat-cb-t))	(* ev-stat-t))
      (make-ev-idle	((* ev-idle-cb-t))	(* ev-idle-t))
      (make-ev-prepare	((* ev-prepare-cb-t))	(* ev-prepare-t))
      (make-ev-check	((* ev-check-cb-t))	(* ev-check-t))
      (make-ev-embed	(ev-loop* (* ev-embed-cb-t))	(* ev-embed-t))
      (make-ev-fork	((* ev-fork-cb-t))	(* ev-fork-t))
      (make-ev-cleanup	((* ev-cleanup-cb-t))	(* ev-cleanup-t))
      (make-ev-async	((* ev-async-cb-t))	(* ev-async-t))
      ;; watcher macro wrappers
      (ev-io-events-set		((* ev-io-t) int)	void)
      ;; watcher accessors
      (ev-io-fd-get		((* ev-io-t))	int)
      (ev-io-events-get		((* ev-io-t))	int)
      (ev-timer-repeat-get		((* ev-timer-t))	ev-tstamp)
      (ev-timer-repeat-set		((* ev-timer-t) ev-tstamp) void)
      (ev-periodic-offset-get	((* ev-periodic-t)) ev-tstamp)
      (ev-periodic-offset-set	((* ev-periodic-t) ev-tstamp) void)
      (ev-periodic-interval-get	((* ev-periodic-t)) ev-tstamp)
      (ev-periodic-interval-set	((* ev-periodic-t) ev-tstamp) void)
      (ev-periodic-rcb-get		((* ev-periodic-t)) (* ev-periodic-rcb-t))
      (ev-periodic-rcb-set		((* ev-periodic-t) (* ev-periodic-rcb-t)) void)
      (ev-signal-signum-get	((* ev-signal-t))	int)
      (ev-child-pid-get		((* ev-child-t))	int)
      (ev-child-rpid-get		((* ev-child-t))	int)
      (ev-child-rpid-set		((* ev-child-t) int) void)
      (ev-child-rstatus-get	((* ev-child-t))	int)
      (ev-child-rstatus-set	((* ev-child-t) int) void)
      ;; TODO ev-stat getters.
      (ev-embed-other-get		((* ev-embed-t)) ev-loop*)
      (ev-async-pending-get	((* ev-async-t)) boolean)

      (ev-watcher-is-active	(ev-watcher*)	boolean)

      ;; TODO need to look at these. Note the ev-watcher types.
      (ev-priority-get		(ev-watcher*) int)
      (ev-priority-set		(ev-watcher* int) void)
      (ev-periodic-at-get		(ev-watcher-time*) int)
      ;; TODO ev-cb, ev-cb-set
      )
    ])

  ;; raw ev prototypes: those declared with underscores will be wrapped with scheme functions later.
  (c-function
   ;;;;;;; EV_PROTOTYPES
   ;; meta funcs
   (ev-version-major		()		int)
   (ev-version-minor		()		int)
   (ev-supported-backends	()		unsigned)
   (ev-recommended-backends	()		unsigned)
   (ev-embeddable-backends	()		unsigned)
   ;; time related
   (ev-time			()		ev-tstamp)
   (ev-sleep			(ev-tstamp)	void)
   (ev_set_allocator		((* realloc-fn))	void)
   (ev_set_syserr_cb		((* msg-cb-fn))		void)
   (ev_default_loop		(int)			ev-loop*)
   (ev_loop_new			(int)			ev-loop*)

   ;; ev-break & ev-run use case-lambda, so leave outside the c-default-function section.
   (ev_break		(ev-loop* int)		void)
   (ev_run		(ev-loop* int)		boolean)
   (ev-feed-signal	(int)			void)
   )

  (define ev-default-loop
    (case-lambda
     [()	(ev-default-loop 0)]
     [(flags)	(ev_default_loop flags)]))

  (define EV_DEFAULT (ev-default-loop))

  ;; [parameter] current-loop
  (define current-loop
    (make-parameter EV_DEFAULT))

  (c-default-function (ev-loop* (current-loop))
   (ev-now		()			ev-tstamp)
   ;; TODO with destroy, maybe the (current-loop) should be invalidated?
   (ev-loop-destroy	()			void)
   (ev-loop-fork	()			void)
   (ev-backend		()			unsigned)
   (ev-now-update	()			void)
   (ev-ref		()			void)
   (ev-unref		()			void)
   ;; TODO
   #;(ev-once		(int int ev-tstamp (* ev-cb-t) void*) void)
   ;;;; EV_FEATURE_API
   (ev-iteration	()			unsigned)
   (ev-depth		()			unsigned)
   (ev-verify		()			void)
   (ev-set-io-collect-interval		(ev-tstamp)	void)
   (ev-set-timeout-collect-interval	(ev-tstamp)	void)
   (ev-set-userdata	(void*)	void)
   (ev-userdata		()		void*)
   ;; TODO wrap these threading callback setters.
   (ev-set-invoke-pending-cb	((* ev-loop-callback))	void)
   (ev-set-loop-release-cb	((* ev-loop-callback) (* ev-loop-callback)) void)
   (ev-pending-count	()			unsigned)
   (ev-invoke-pending	()			void)
   (ev-suspend		()			void)
   (ev-resume		()			void)
   ;; event loop manipulation.
   (ev-feed-event	(void* int)	void)
   (ev-feed-fd-event	(int int)	void)
   (ev-feed-signal-event(int)		void)
   (ev-invoke		(void* int)	void)
   (ev-clear-pending	(void*)	int)
   ;; event watcher control.
   (ev-io-start		((* ev-io-t))	void)
   (ev-io-stop		((* ev-io-t))	void)
   (ev-timer-start	((* ev-timer-t))	void)
   (ev-timer-stop	((* ev-timer-t))	void)
   (ev-timer-again	((* ev-timer-t))	void)
   (ev-timer-remaining	((* ev-timer-t))	ev-tstamp)
   (ev-periodic-start	((* ev-periodic-t))	void)
   (ev-periodic-stop	((* ev-periodic-t))	void)
   (ev-periodic-again	((* ev-periodic-t))	void)
   (ev-signal-start	((* ev-signal-t))	void)
   (ev-signal-stop	((* ev-signal-t))	void)
   (ev-child-start	((* ev-child-t))	void)
   (ev-child-stop	((* ev-child-t))	void)
   (ev-stat-start	((* ev-stat-t))	void)
   (ev-stat-stop	((* ev-stat-t))	void)
   (ev-stat-stat	((* ev-stat-t))	void)
   (ev-idle-start	((* ev-idle-t))	void)
   (ev-idle-stop	((* ev-idle-t))	void)
   (ev-prepare-start	((* ev-prepare-t))	void)
   (ev-prepare-stop	((* ev-prepare-t))	void)
   (ev-check-start	((* ev-check-t))	void)
   (ev-check-stop	((* ev-check-t))	void)
   (ev-fork-start	((* ev-fork-t))	void)
   (ev-fork-stop	((* ev-fork-t))	void)
   (ev-cleanup-start	((* ev-cleanup-t))	void)
   (ev-cleanup-stop	((* ev-cleanup-t))	void)
   (ev-embed-start	((* ev-embed-t))	void)
   (ev-embed-stop	((* ev-embed-t))	void)
   (ev-embed-sweep	((* ev-embed-t))	void)
   (ev-async-start	((* ev-async-t))	void)
   (ev-async-stop	((* ev-async-t))	void)
   (ev-async-send	((* ev-async-t))	void)
   )

  (define EV_VERSION_MAJOR (ev-version-major-def))
  (define EV_VERSION_MINOR (ev-version-minor-def))

  ;; TODO
  ;; (define ev-set-allocator)
  ;; (define ev-set-syserr-cb)

  (define ev-loop-new
    (case-lambda
     [()	(ev-loop-new 0)]
     [(flags)	(ev_loop_new flags)]))

  (define ev-run
    (case-lambda
     [()	(ev-run 0)]
     [(flags)	(ev_run (current-loop) flags)]))

  (define ev-break
    (case-lambda
     [()	(ev-break (evbreak 'ONE))]
     [(how)	(ev_break (current-loop) how)]))

  (define watcher-guardian
    (make-guardian))

  (define-record-type wc	; wc = watcher-context.
    (fields
      (mutable watcher ev-watcher-address ev-watcher-address-set!)
      (immutable ftype-callback)
      (immutable stop-thunk)))

  (define-record-type pwc	; pwc = periodic-watcher-context
    (parent wc)
    (fields
      (immutable ftype-reschedule-callback)))

  (define collect-watchers
    (make-parameter 3
      (lambda (val)
        (cond
          [(and (integer? val) (fx>? val 0))
           ;; Collect up to val orphaned watchers.
           val]
          [(eq? val #t)
           ;; Collect all orphaned watchers.
           +inf.0]
          [else
            ;; Do not collect any orphaned watchers.
            #f]))))

  (define ev-free-watcher!
    (lambda (w)
      (when (ev-watcher-address w)
        ((wc-stop-thunk w))
        ;; free() the pointer returned by make-<watcher-type>.
        (foreign-free (ftype-pointer-address (ev-watcher-address w)))
        ;; ensure there's no double free.
        (ev-watcher-address-set! w #f)
        ;; unlock the callback function address so that the Chez gc can remove it.
        (unlock-object (foreign-callable-code-object (ftype-pointer-address (wc-ftype-callback w))))
        (when (pwc? w)
          (unlock-object (foreign-callable-code-object (ftype-pointer-address (pwc-ftype-reschedule-callback w))))))))

  (define free-watchers
    (case-lambda
      [()
       (free-watchers (collect-watchers))]
      [(c)
       (when c
         (let loop ([i c] [wcxt (watcher-guardian)])
           (when (and wcxt (> i 0))
             (when (collect-notify)
               (display "free watcher: ")(display wcxt)(newline))
             (ev-free-watcher! wcxt)
             (loop (- i 1) (watcher-guardian)))))]))

  (define-syntax ev-ms
    (syntax-rules ()
      [(_ ms)
       (inexact (/ ms 1000))]))

  (define time->number
    (lambda (t)
      (+ (time-second t)
         (inexact (* (time-nanosecond t) (expt 10 -9))))))

  (define time->absolute-number
    (lambda (t)
      (time->number
        (case (time-type t)
          [(time-duration)
           (add-duration (current-time) t)]
          [(time-utc)
           t]
          [else
            (error 'time->absolute-number "time-utc or time-duration argument required" t)]))))

  ;; There are three main ways to use ev-periodic. Expose convenience wrappers for them rather than the low level ev-periodic.
  ;; Do these as syntax just to be the same as the other ev-watcher types.
  ;; See libev(3)
  (define-syntax ev-absolute-timer
    (syntax-rules ()
      [(_ time cb)
       (ev-periodic (time->absolute-number time) 0 (make-ftype-pointer ev-periodic-rcb-t 0) cb)]))

  (define-syntax ev-interval-timer
    (syntax-rules ()
      [(_ interval cb)
       (ev-periodic 0. interval (make-ftype-pointer ev-periodic-rcb-t 0) cb)]
      [(_ offset interval cb)
       (ev-periodic offset interval (make-ftype-pointer ev-periodic-rcb-t 0) cb)]))

  (define-syntax ev-manual-timer
    (syntax-rules ()
      [(_ rcb cb)
       ;; Consider wrapping rcb-t such that 'now' is converted to a time object
       ;; and return is assumed to be another time object that's converted to ev-tstamp.
       (let* ([fp-rcb (make-ftype-pointer ev-periodic-rcb-t rcb)]
              [wc (ev-periodic 0 0 fp-rcb cb)])
         (make-pwc
           (ev-watcher-address wc)
           (wc-ftype-callback wc)
           (wc-stop-thunk wc)
           fp-rcb))]))
  )
