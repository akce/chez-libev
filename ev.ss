;; libev bindings for chez scheme.
;; Copyright (C) 2019 Akce. Released under the MIT license.
(library (ev)
  (export
   EV_VERSION_MAJOR EV_VERSION_MINOR

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
   (ftypes-util))

  (define load-libev
    (load-shared-object "libev.so.4"))
  (define load-libev-ffi
    (load-shared-object "./libev-ffi.so"))

  (define EV_VERSION_MAJOR
    ((foreign-procedure "ev_version_major" () int)))
  (define EV_VERSION_MINOR
    ((foreign-procedure "ev_version_minor" () int)))

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

  (c_funcs
   ;; raw constructors
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
   (ev-timer-repeat-set		(ev-timer* ev-tstamp) void)
   (ev-periodic-offset-get	(ev-periodic*) ev-tstamp)
   (ev-periodic-offset-set	(ev-periodic* ev-tstamp) void)
   (ev-periodic-interval-get	(ev-periodic*) ev-tstamp)
   (ev-periodic-interval-set	(ev-periodic* ev-tstamp) void)
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

  (define ev-io
    (lambda (fd events cb)
      (make-ev-io fd events (make-ftype-pointer ev-io-cb-t cb))))

  (define ev-timer
    (lambda (after repeat cb)
      (make-ev-timer after repeat (make-ftype-pointer ev-timer-cb-t cb))))

  (define ev-periodic
    (lambda (offset interval rcb cb)
      (make-ev-periodic offset interval (make-ftype-pointer ev-periodic-rcb-t rcb) (make-ftype-pointer ev-periodic-cb-t cb))))

  (define ev-signal
    (lambda (signum cb)
      (make-ev-signal signum (make-ftype-pointer ev-signal-cb-t cb))))

  (define ev-child
    (lambda (pid trace cb)
      (make-ev-child pid trace (make-ftype-pointer ev-child-cb-t cb))))

  (define ev-stat
    (lambda (path interval cb)
      (make-ev-stat path interval (make-ftype-pointer ev-stat-cb-t cb))))

  (define ev-idle
    (lambda (cb)
      (make-ev-idle (make-ftype-pointer ev-idle-cb-t cb))))

  (define ev-prepare
    (lambda (cb)
      (make-ev-prepare (make-ftype-pointer ev-prepare-cb-t cb))))

  (define ev-check
    (lambda (cb)
      (make-ev-check (make-ftype-pointer ev-check-cb-t cb))))

  (define ev-embed
    (lambda (other cb)
      (make-ev-embed other (make-ftype-pointer ev-embed-cb-t cb))))

  (define ev-fork
    (lambda (cb)
      (make-ev-fork (make-ftype-pointer ev-fork-cb-t cb))))

  (define ev-cleanup
    (lambda (cb)
      (make-ev-cleanup (make-ftype-pointer ev-cleanup-cb-t cb))))

  (define ev-async
    (lambda (cb)
      (make-ev-async (make-ftype-pointer ev-async-cb-t cb)))))
