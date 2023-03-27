;; libev bindings for Chez Scheme.
;; Written by Jerry 2019-2022.
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

   ;; specific event watcher controls.
   ev-io-start ev-io-stop
   ev-timer-start ev-timer-stop ev-timer-again ev-timer-remaining
   ev-periodic-start ev-periodic-stop ev-periodic-again
   ev-signal-start ev-signal-stop
   ev-child-start ev-child-stop
   ev-stat-start ev-stat-stop ev-stat-stat
   ev-idle-start ev-idle-stop
   ev-prepare-start ev-prepare-stop
   ev-check-start ev-check-stop
   ev-fork-start ev-fork-stop
   ev-cleanup-start ev-cleanup-stop
   ev-embed-start ev-embed-stop ev-embed-sweep
   ev-async-start ev-async-stop ev-async-send

   ;; Event watchers.
   ev-io (rename
           (ev-io-fd-get ev-io-fd)
           (ev-io-events-get ev-io-events)
           (ev-io-events-set ev-io-events-set!))
   ev-timer (rename
              (ev-timer-repeat-get ev-timer-repeat)
              (ev-timer-repeat-set ev-timer-repeat-set!))
   ev-periodic ev-periodic-offset-get ev-periodic-offset-set ev-periodic-interval-get ev-periodic-interval-set ev-periodic-rcb-get ev-periodic-rcb-set
   ev-signal ev-signal-signum-get
   ev-child ev-child-pid-get ev-child-rpid-get ev-child-rpid-set ev-child-rstatus-get ev-child-rstatus-set
   ev-stat
   ev-idle
   ev-prepare
   ev-check
   ev-embed ev-embed-other-get
   ev-fork
   ev-cleanup
   ev-async
   (rename (ev-async-pending-get ev-async-pending?))

   (rename
     (ev-watcher-is-active ev-watcher-active?))

   ev-ms

   ;; Convenience wrappers for the 3 modes of ev-periodic timers.
   ;; See libev(3).
   ev-absolute-timer
   ev-interval-timer
   ev-manual-timer


   ev-stat-t
   )
  (import
   (chezscheme)
   (ev ftypes-util))

  (define load-libev
    (load-shared-object "libev.so.4"))
  (define load-libev-ffi
    (load-shared-object (locate-library-object "ev/libchez-ffi.so")))

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
  (define-ftype ev-watcher-list*	void*)
  ;; tv-tstamp is a 'double' unless EV_TSTAMP_T overrides it.
  (define-ftype ev-tstamp		double)
  ;; !!! ev_stat embeds two 'struct stat' objects which have a number of members and are OS dependant.
  ;; Magic number 144 = sizeof(struct stat): generated via platform.c (sizeof-struct-stat).
  (define-ftype struct-stat (array 144 unsigned-8))
  ;; !!! For Linux, this is currently an int: see <bits/types.h>
  (define-ftype sig-atomic-t int)

  ;; field-acl:
  ;;  - private is readable anytime, but not writable by us
  ;;  - ro is readable anytime, but only writable when watcher is inactive
  ;;  - rw is readable and writable anytime
  ;;  - unused is ignored, but only generated in the ftype struct for padding
  (define-syntax define-ev-watcher
    (lambda (x)
      (syntax-case x ()
        [(_ type-name ptr-type-name (field-name field-type field-acl) ...)
         #'(begin
             (define-ftype type-name
               (struct
                 (field-name field-type)
                 ...))
             (define-ftype ptr-type-name (* type-name)))])))

  (define-syntax define-ev-watcher-base
    (syntax-rules ()
      [(_ type-name ptr-type-name field-def* ...)
       (define-ev-watcher type-name ptr-type-name
         ;; EV_WATCHER(type)
         (active	int	private)
         (pending	int	private)
         ;; EV_DECL_PRIORITY
         (priority	int	private)
         ;; EV_COMMON
         (data	void*	rw)
         ;; EV_CB_DECLARE(type)
         (cb	(* (function (ev-loop* (* type-name) int) void))	private)
         ;; type specific field definitions..
         field-def*
         ...)]))

  (define-syntax define-ev-watcher-list
    (syntax-rules ()
      [(_ type-name ptr-type-name field-def* ...)
       (define-ev-watcher-base type-name ptr-type-name
         (next	ev-watcher-list*	private)
         field-def*
         ...)]))

  (define-syntax define-ev-watcher-time
    (syntax-rules ()
      [(_ type-name ptr-type-name field-def* ...)
       (define-ev-watcher-base type-name ptr-type-name
         (at	ev-tstamp	private)
         field-def*
         ...)]))

  ;; event watcher types.
  (define-ev-watcher-list ev-io-t ev-io-t*
    (fd		int	ro)
    (events	int	ro)
    )

  (define-ev-watcher-time ev-timer-t ev-timer-t*
    (repeat	ev-tstamp	rw)
    )

  (define-ev-watcher-time ev-periodic-t ev-periodic-t*
    (offset		ev-tstamp	rw)
    (interval		ev-tstamp	rw)
    (reschedule-cb	(* (function ((* ev-periodic-t) ev-tstamp) ev-tstamp))	rw)
    )

  (define-ev-watcher-list ev-signal-t ev-signal-t*
    (signum	int	ro)
    )

  (define-ev-watcher-list ev-child-t ev-child-t*
    (flags	int	private)
    (pid	int	ro)
    (rpid	int	rw)
    (rstatus	int	rw)
    )

  (define-ev-watcher-list ev-stat-t ev-stat-t*
    (timer	ev-timer-t	private)
    (interval	ev-tstamp	ro)
    (path	void*		ro)	; XXX const char*, not void*
    (prev	struct-stat	ro)
    (attr	struct-stat	ro)
    (wd		int		private)	; acl not specified in header for this field
    )

  (define-ev-watcher ev-idle-t ev-idle-t*)

  (define-ev-watcher ev-prepare-t ev-prepare-t*)

  (define-ev-watcher ev-check-t ev-check-t*)

  (define-ev-watcher ev-fork-t ev-fork-t*)

  (define-ev-watcher ev-cleanup-t ev-cleanup-t*)

  (define-ev-watcher ev-embed-t ev-embed-t*
    (other	ev-loop*	ro)
    (io		ev-io-t		private)
    (prepare	ev-prepare-t	private)
    (check	ev-check-t	unused)
    (timer	ev-timer-t	unused)
    (periodic	ev-periodic-t	unused)
    (idle	ev-idle-t	unused)
    (fork	ev-fork-t	private)
    (cleanup	ev-cleanup-t	unused)
    )

  (define-ev-watcher ev-async-t ev-async-t*
    (sent	sig-atomic-t	private)
    )

   ;; TODO need to look at these properly. These are parent types.
  (define-ftype ev-watcher*	void*)
  (define-ftype ev-watcher-time*	void*)

  ;; event callback types.
  (define-ftype ev-io-cb-t		(function (ev-loop* (* ev-io-t) int)			void))
  (define-ftype ev-timer-cb-t		(function (ev-loop* (* ev-timer-t) int)		void))
  (define-ftype ev-periodic-cb-t	(function (ev-loop* (* ev-periodic-t) int)		void))
  (define-ftype ev-signal-cb-t		(function (ev-loop* (* ev-signal-t) int)		void))
  (define-ftype ev-child-cb-t		(function (ev-loop* (* ev-child-t) int)		void))
  (define-ftype ev-stat-cb-t		(function (ev-loop* (* ev-stat-t) int)		void))
  (define-ftype ev-idle-cb-t		(function (ev-loop* (* ev-idle-t) int)		void))
  (define-ftype ev-prepare-cb-t		(function (ev-loop* (* ev-prepare-t) int)		void))
  (define-ftype ev-check-cb-t		(function (ev-loop* (* ev-check-t) int)		void))
  (define-ftype ev-embed-cb-t		(function (ev-loop* (* ev-embed-t) int)		void))
  (define-ftype ev-fork-cb-t		(function (ev-loop* (* ev-fork-t) int)		void))
  (define-ftype ev-cleanup-cb-t		(function (ev-loop* (* ev-fork-t) int)		void))
  (define-ftype ev-async-cb-t		(function (ev-loop* (* ev-async-t) int)		void))

  (define-ftype ev-periodic-rcb-t	(function ((* ev-periodic-t) ev-tstamp)		ev-tstamp))

  ;; memory alloc function prototype.
  (define-ftype realloc-fn	(function (void* long)	void*))
  (define-ftype msg-cb-fn	(function (string)	void))
  (define-ftype ev-loop-callback (function (ev-loop*)	void))

  ;; raw ev prototypes: those declared with underscores will be wrapped with scheme functions later.
  (c-function
   ;;;;;;; EV_PROTOTYPES
   ;; meta funcs
   (ev-version-major-def	()		int)
   (ev-version-minor-def	()		int)
   (ev-version-major		()		int)
   (ev-version-minor		()		int)
   (ev-supported-backends	()		unsigned)
   (ev-recommended-backends	()		unsigned)
   (ev-embeddable-backends	()		unsigned)
   ;; time related
   (ev-time			()		ev-tstamp)
   (ev-sleep			(ev-tstamp)	void)
   (ev_set_allocator	((* realloc-fn))	void)
   (ev_set_syserr_cb	((* msg-cb-fn))		void)
   (ev_default_loop	(int)			ev-loop*)
   (ev_loop_new		(int)			ev-loop*)

   ;; ev-break & ev-run use case-lambda, so leave outside the c-default-function section.
   (ev_break		(ev-loop* int)		void)
   (ev_run		(ev-loop* int)		boolean)
   (ev-feed-signal	(int)			void)
   ;;; libev-ffi extensions.
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
        (foreign-free (ev-watcher-address w))
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

  (define-syntax define-watcher
    (lambda (x)
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
      (syntax-case x ()
        [(k watcher-name args ...)
         (with-syntax ([make-watcher-func (make-id-syntax #'k "make-" #'watcher-name)]
                       [start-watcher-func (make-id-syntax #'k #'watcher-name "-start")]
                       [stop-watcher-func (make-id-syntax #'k #'watcher-name "-stop")]
                       [cb-func-type (make-id-syntax #'k #'watcher-name "-cb-t")])
           #'(define watcher-name
               (lambda (args ... callback-func)
                 (let* ([fp-callback (make-ftype-pointer
                                       cb-func-type
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
                   wcxt))))])))

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
