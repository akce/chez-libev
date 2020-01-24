;; libev bindings for chez scheme.
;; Written by Akce 2019, 2020.
;; SPDX-License-Identifier: Unlicense
(library (ev)
  (export
    ;; enums, bitmaps and IDs.
   EV_VERSION_MAJOR EV_VERSION_MINOR
   evmask EV_NONE EV_IO
   evflag EVFLAG_AUTO
   evbackend
   evrun
   evbreak
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
   ev-io ev-io-fd-get ev-io-events-get
   ev-timer ev-timer-repeat-get ev-timer-repeat-set
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
   ev-async ev-async-pending-get)
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

  (define-ftype ev-loop* void*)

  ;; event watcher types.
  (define-ftype ev-io*		void*)
  (define-ftype ev-timer*	void*)
  (define-ftype ev-periodic*	void*)
  (define-ftype ev-signal*	void*)
  (define-ftype ev-child*	void*)
  (define-ftype ev-stat*	void*)
  (define-ftype ev-idle*	void*)
  (define-ftype ev-prepare*	void*)
  (define-ftype ev-check*	void*)
  (define-ftype ev-embed*	void*)
  (define-ftype ev-fork*	void*)
  (define-ftype ev-cleanup*	void*)
  (define-ftype ev-async*	void*)

  (define-ftype ev-tstamp	double)

   ;; TODO need to look at these properly. These are parent types.
  (define-ftype ev-watcher*	void*)
  (define-ftype ev-watcher-time*	void*)

  ;; event callback types.
  (define-ftype ev-io-cb-t		(function (ev-loop* ev-io* int)			void))
  (define-ftype ev-timer-cb-t		(function (ev-loop* ev-timer* int)		void))
  (define-ftype ev-periodic-cb-t	(function (ev-loop* ev-periodic* int)		void))
  (define-ftype ev-signal-cb-t		(function (ev-loop* ev-signal* int)		void))
  (define-ftype ev-child-cb-t		(function (ev-loop* ev-child* int)		void))
  (define-ftype ev-stat-cb-t		(function (ev-loop* ev-stat* int)		void))
  (define-ftype ev-idle-cb-t		(function (ev-loop* ev-idle* int)		void))
  (define-ftype ev-prepare-cb-t		(function (ev-loop* ev-prepare* int)		void))
  (define-ftype ev-check-cb-t		(function (ev-loop* ev-check* int)		void))
  (define-ftype ev-embed-cb-t		(function (ev-loop* ev-embed* int)		void))
  (define-ftype ev-fork-cb-t		(function (ev-loop* ev-fork* int)		void))
  (define-ftype ev-cleanup-cb-t		(function (ev-loop* ev-fork* int)		void))
  (define-ftype ev-async-cb-t		(function (ev-loop* ev-async* int)		void))

  (define-ftype ev-periodic-rcb-t	(function (ev-periodic* ev-tstamp)		ev-tstamp))

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
   (ev_sleep			(ev-tstamp)	void)
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
   (make-ev-io		(int int (* ev-io-cb-t))	ev-io*)
   (make-ev-timer	(ev-tstamp ev-tstamp (* ev-timer-cb-t))	ev-timer*)
   (make-ev-periodic	(ev-tstamp ev-tstamp (* ev-periodic-rcb-t) (* ev-periodic-cb-t))	ev-periodic*)
   (make-ev-signal	(int (* ev-signal-cb-t))	ev-signal*)
   (make-ev-child	(int int (* ev-child-cb-t))	ev-child*)
   (make-ev-stat	(string ev-tstamp (* ev-stat-cb-t))	ev-stat*)
   (make-ev-idle	((* ev-idle-cb-t))	ev-idle*)
   (make-ev-prepare	((* ev-prepare-cb-t))	ev-prepare*)
   (make-ev-check	((* ev-check-cb-t))	ev-check*)
   (make-ev-embed	(ev-loop* (* ev-embed-cb-t))	ev-embed*)
   (make-ev-fork	((* ev-fork-cb-t))	ev-fork*)
   (make-ev-cleanup	((* ev-cleanup-cb-t))	ev-cleanup*)
   (make-ev-async	((* ev-async-cb-t))	ev-async*)
   ;; watcher accessors
   (ev-io-fd-get		(ev-io*)	int)
   (ev-io-events-get		(ev-io*)	int)
   (ev-timer-repeat-get		(ev-timer*)	ev-tstamp)
   (ev_timer_repeat_set		(ev-timer* ev-tstamp) void)
   (ev-periodic-offset-get	(ev-periodic*) ev-tstamp)
   (ev_periodic_offset_set	(ev-periodic* ev-tstamp) void)
   (ev-periodic-interval-get	(ev-periodic*) ev-tstamp)
   (ev_periodic_interval_set	(ev-periodic* ev-tstamp) void)
   (ev-periodic-rcb-get		(ev-periodic*) (* ev-periodic-rcb-t))
   (ev-periodic-rcb-set		(ev-periodic* (* ev-periodic-rcb-t)) void)
   (ev-signal-signum-get	(ev-signal*)	int)
   (ev-child-pid-get		(ev-child*)	int)
   (ev-child-rpid-get		(ev-child*)	int)
   (ev-child-rpid-set		(ev-child* int) void)
   (ev-child-rstatus-get	(ev-child*)	int)
   (ev-child-rstatus-set	(ev-child* int) void)
   ;; TODO ev-stat getters.
   (ev-embed-other-get		(ev-embed*) ev-loop*)
   (ev-async-pending-get	(ev-async*) int)

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
   (ev_set_io_collect_interval		(ev-tstamp)	void)
   (ev_set_timeout_collect_interval	(ev-tstamp)	void)
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
   (ev-io-start		(ev-io*)	void)
   (ev-io-stop		(ev-io*)	void)
   (ev-timer-start	(ev-timer*)	void)
   (ev-timer-stop	(ev-timer*)	void)
   (ev-timer-again	(ev-timer*)	void)
   (ev-timer-remaining	(ev-timer*)	ev-tstamp)
   (ev-periodic-start	(ev-periodic*)	void)
   (ev-periodic-stop	(ev-periodic*)	void)
   (ev-periodic-again	(ev-periodic*)	void)
   (ev-signal-start	(ev-signal*)	void)
   (ev-signal-stop	(ev-signal*)	void)
   (ev-child-start	(ev-child*)	void)
   (ev-child-stop	(ev-child*)	void)
   (ev-stat-start	(ev-stat*)	void)
   (ev-stat-stop	(ev-stat*)	void)
   (ev-stat-stat	(ev-stat*)	void)
   (ev-idle-start	(ev-idle*)	void)
   (ev-idle-stop	(ev-idle*)	void)
   (ev-prepare-start	(ev-prepare*)	void)
   (ev-prepare-stop	(ev-prepare*)	void)
   (ev-check-start	(ev-check*)	void)
   (ev-check-stop	(ev-check*)	void)
   (ev-fork-start	(ev-fork*)	void)
   (ev-fork-stop	(ev-fork*)	void)
   (ev-cleanup-start	(ev-cleanup*)	void)
   (ev-cleanup-stop	(ev-cleanup*)	void)
   (ev-embed-start	(ev-embed*)	void)
   (ev-embed-stop	(ev-embed*)	void)
   (ev-embed-sweep	(ev-embed*)	void)
   (ev-async-start	(ev-async*)	void)
   (ev-async-stop	(ev-async*)	void)
   (ev-async-send	(ev-async*)	void)
   )

  (define EV_VERSION_MAJOR (ev-version-major-def))
  (define EV_VERSION_MINOR (ev-version-minor-def))

  (define ev-sleep
    (lambda (len)
      (ev_sleep (->double len))))

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

  (define ev-set-io-collect-interval
    (lambda (interval)
      (ev_set_io_collect_interval (->double interval))))

  (define ev-set-timeout-collect-interval
    (lambda (interval)
      (ev_set_timeout_collect_interval (->double interval))))

  (define-syntax make-cb
    (syntax-rules ()
      [(_ cb-t cb)
       (make-ftype-pointer cb-t
         (lambda (loop w rev)
           (parameterize ([current-loop loop])
             (cb w rev))))]))

  ;; [syntax] sar: start and return the watcher.
  (define-syntax sar
    (syntax-rules ()
      [(_ func w)
       (let ([x w])
         (func x)
         x)]))

  (define ev-io
    (lambda (fd events cb)
      (sar ev-io-start (make-ev-io fd events (make-cb ev-io-cb-t cb)))))

  (define ev-timer
    (lambda (after repeat cb)
      (sar ev-timer-start (make-ev-timer (->double after) (->double repeat) (make-cb ev-timer-cb-t cb)))))

  (define ev-periodic
    (lambda (offset interval rcb cb)
      (sar ev-periodic-start (make-ev-periodic (->double offset) (->double interval) (make-ftype-pointer ev-periodic-rcb-t rcb) (make-cb ev-periodic-cb-t cb)))))

  (define ev-signal
    (lambda (signum cb)
      (sar ev-signal-start (make-ev-signal signum (make-cb ev-signal-cb-t cb)))))

  (define ev-child
    (lambda (pid trace cb)
      (sar ev-child-start (make-ev-child pid trace (make-cb ev-child-cb-t cb)))))

  (define ev-stat
    (lambda (path interval cb)
      (sar ev-stat-start (make-ev-stat path (->double interval) (make-cb ev-stat-cb-t cb)))))

  (define ev-idle
    (lambda (cb)
      (sar ev-idle-start (make-ev-idle (make-cb ev-idle-cb-t cb)))))

  (define ev-prepare
    (lambda (cb)
      (sar ev-prepare-start (make-ev-prepare (make-cb ev-prepare-cb-t cb)))))

  (define ev-check
    (lambda (cb)
      (sar ev-check-start (make-ev-check (make-cb ev-check-cb-t cb)))))

  (define ev-embed
    (lambda (other cb)
      (sar ev-embed-start (make-ev-embed other (make-cb ev-embed-cb-t cb)))))

  (define ev-fork
    (lambda (cb)
      (sar ev-fork-start (make-ev-fork (make-cb ev-fork-cb-t cb)))))

  (define ev-cleanup
    (lambda (cb)
      (sar ev-cleanup-start (make-ev-cleanup (make-cb ev-cleanup-cb-t cb)))))

  (define ev-async
    (lambda (cb)
      (sar ev-async-start (make-ev-async (make-cb ev-async-cb-t cb)))))

  (define ev-timer-repeat-set
    (lambda (timer repeat)
      (ev_timer_repeat_set timer (->double repeat))))

  (define ev-periodic-offset-set
    (lambda (periodic offset)
      (ev_periodic_offset_set periodic (->double offset))))

  (define ev-periodic-interval-set
    (lambda (periodic interval)
      (ev_periodic_interval_set periodic (->double interval))))

  ;; [procedure] ->double: ensures number is converted to a double if necessary.
  (define ->double
    (lambda (num)
      (cond
       [(fixnum? num) (fixnum->flonum num)]
       [else num]))))
