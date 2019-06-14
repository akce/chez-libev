/*
 * Macros from ev.h converted to, or embedded in, C functions useful for FFI bindings.
 *
 * Copyright (C) 2019 Akce. Released under the MIT license.
 */

/* Allow site to adjust configurables in ev.h by defining CONFIG_H */
#ifdef CONFIG_H
# include CONFIG_H
#endif

#include <ev.h>

#include <stdlib.h>	// memset

/* EV_VERSION_* may not equal ev_version_*() so provide accessors to the #defines. */
int ev_version_major_def(void) {return EV_VERSION_MAJOR;}
int ev_version_minor_def(void) {return EV_VERSION_MINOR;}

/* Define ev_loop_t shorthand. Saves on typing struct ev_loop. */
typedef struct ev_loop ev_loop_t;

/*
 * Explicitly define callback types.
 * These are defined inline via MACROS in ev.h rather than typedef'd.
 */
typedef ev_tstamp (* ev_periodic_rcb_t)(ev_periodic *w, ev_tstamp now);

typedef void (*ev_io_cb_t)	(EV_P_ ev_io* watcher, int revents);
typedef void (*ev_timer_cb_t)	(EV_P_ ev_timer* watcher, int revents);
typedef void (*ev_periodic_cb_t)(EV_P_ ev_periodic* watcher, int revents);
typedef void (*ev_signal_cb_t)	(EV_P_ ev_signal* watcher, int revents);
typedef void (*ev_child_cb_t)	(EV_P_ ev_child* watcher, int revents);
typedef void (*ev_stat_cb_t)	(EV_P_ ev_stat* watcher, int revents);
typedef void (*ev_idle_cb_t)	(EV_P_ ev_idle* watcher, int revents);
typedef void (*ev_prepare_cb_t)	(EV_P_ ev_prepare* watcher, int revents);
typedef void (*ev_check_cb_t)	(EV_P_ ev_check* watcher, int revents);
typedef void (*ev_embed_cb_t)	(EV_P_ ev_embed* watcher, int revents);
typedef void (*ev_fork_cb_t)	(EV_P_ ev_fork* watcher, int revents);
typedef void (*ev_cleanup_cb_t)	(EV_P_ ev_cleanup* watcher, int revents);
typedef void (*ev_async_cb_t)	(EV_P_ ev_async* watcher, int revents);

/*
 * Watcher accessors.
 * These structs can vary layout across compilations due to EV_COMMON being customisable.
 * Define accessors so the C compiler can take care of member offsets.
 *
 * ev.h marks fields as ro, rw, private and unused.
 * Provide getters for ro, and getter/setters for rw, otherwise ignore the rest.
 */
int		ev_io_fd_get		(const ev_io* watcher)		{return watcher->fd;}
int		ev_io_events_get	(const ev_io* watcher)		{return watcher->events;}
ev_tstamp	ev_timer_repeat_get	(const ev_timer* watcher)	{return watcher->repeat;}
void		ev_timer_repeat_set	(ev_timer* watcher, ev_tstamp value) {watcher->repeat = value;}
ev_tstamp	ev_periodic_offset_get	(const ev_periodic* watcher)	{return watcher->offset;}
void		ev_periodic_offset_set	(ev_periodic* w, ev_tstamp value) {w->offset = value;}
ev_tstamp	ev_periodic_interval_get(const ev_periodic* watcher)	{return watcher->interval;}
void		ev_periodic_interval_set(ev_periodic* w, ev_tstamp value) {w->interval = value;}
ev_periodic_rcb_t ev_periodic_rcb_get	(const ev_periodic* w)		{return w->reschedule_cb;}
void		ev_periodic_rcb_set	(ev_periodic* w, ev_periodic_rcb_t value)	{w->reschedule_cb = value;}
int		ev_signal_signum_get	(const ev_signal* watcher)	{return watcher->signum;}
int		ev_child_pid_get	(const ev_child* watcher)	{return watcher->pid;}
int		ev_child_rpid_get	(const ev_child* watcher)	{return watcher->rpid;}
int		ev_child_rstatus_get	(const ev_child* watcher)	{return watcher->rstatus;}
void		ev_child_rpid_set	(ev_child* watcher, int value)	{watcher->rpid = value;}
void		ev_child_rstatus_set	(ev_child* watcher, int value)	{watcher->rstatus = value;}
/*
 * TODO ev_stat getters
 * ev_stat contains two ro ev_statdata structures. Need to think about how to return those.
 */
ev_loop_t*	ev_embed_other_get	(const ev_embed* watcher)	{return watcher->other;}
int		ev_async_pending_get	(const ev_async* watcher)	{return ev_async_pending(watcher);}

/*
 * Watcher constructors.
 * These follow the Perl::EV convention of putting the callback argument last.
 * Each return a pointer to the ev type object allocated on the heap and initialised.
 * The benefit is that the FFI layer does not need to define any of these structures
 * and clients can treat these as opaque pointers.
 * TODO provide a way to free.
 */
#define ALLOCZ(type)				\
    type* watcher;      			\
    watcher = malloc(sizeof(*watcher));         \
    memset(watcher, 0, sizeof(*watcher))

ev_io* make_ev_io(int fd, int events, ev_io_cb_t cb)
    {
    ALLOCZ(ev_io);
    ev_io_init(watcher, cb, fd, events);
    return watcher;
    }

ev_timer* make_ev_timer(ev_tstamp after, ev_tstamp repeat, ev_timer_cb_t cb)
    {
    ALLOCZ(ev_timer);
    ev_timer_init(watcher, cb, after, repeat);
    return watcher;
    }

ev_periodic* make_ev_periodic(ev_tstamp offset, ev_tstamp interval, ev_periodic_rcb_t rcb, ev_periodic_cb_t cb)
    {
    ALLOCZ(ev_periodic);
    ev_periodic_init(watcher, cb, offset, interval, rcb);
    return watcher;
    }

ev_signal* make_ev_signal(int signum, ev_signal_cb_t cb)
    {
    ALLOCZ(ev_signal);
    ev_signal_init(watcher, cb, signum);
    return watcher;
    }

ev_child* make_ev_child(int pid, int trace, ev_child_cb_t cb)
    {
    ALLOCZ(ev_child);
    ev_child_init(watcher, cb, pid, trace);
    return watcher;
    }

ev_stat* make_ev_stat(const char* path, ev_tstamp interval, ev_stat_cb_t cb)
    {
    ALLOCZ(ev_stat);
    ev_init(watcher, cb);
    // TODO provide a way to free(watcher->path).
    ev_stat_set(watcher, strdup(path), interval);
    return watcher;
    }

ev_idle* make_ev_idle(ev_idle_cb_t cb)
    {
    ALLOCZ(ev_idle);
    ev_idle_init(watcher, cb);
    return watcher;
    }

ev_prepare* make_ev_prepare(ev_prepare_cb_t cb)
    {
    ALLOCZ(ev_prepare);
    ev_prepare_init(watcher, cb);
    return watcher;
    }

ev_check* make_ev_check(ev_check_cb_t cb)
    {
    ALLOCZ(ev_check);
    ev_check_init(watcher, cb);
    return watcher;
    }

ev_embed* make_ev_embed(ev_loop_t* other, ev_embed_cb_t cb)
    {
    ALLOCZ(ev_embed);
    ev_embed_init(watcher, cb, other);
    return watcher;
    }

ev_fork* make_ev_fork(ev_fork_cb_t cb)
    {
    ALLOCZ(ev_fork);
    ev_fork_init(watcher, cb);
    return watcher;
    }

ev_cleanup* make_ev_cleanup(ev_cleanup_cb_t cb)
    {
    ALLOCZ(ev_cleanup);
    ev_cleanup_init(watcher, cb);
    return watcher;
    }

ev_async* make_ev_async(ev_async_cb_t cb)
    {
    ALLOCZ(ev_async);
    ev_async_init(watcher, cb);
    return watcher;
    }

/* TODO ev_cb */

int ev_priority_get(ev_watcher* watcher) {return ev_priority(watcher);}
void ev_priority_set(ev_watcher* watcher, int priority) {ev_set_priority(watcher, priority);}
ev_tstamp ev_periodic_at_get(ev_watcher_time* watcher) {return ev_periodic_at(watcher);}

/* TODO ev_set_cb */
