/*
 * Macros from ev.h converted to, or embedded in, C functions useful for FFI bindings.
 *
 * With the pure scheme layer, these functions remain useful for testing.
 *
 * Written by Jerry 2019,2023.
 * SPDX-License-Identifier: Unlicense
 */

/* Allow site to adjust configurables in ev.h by defining CONFIG_H */
#ifdef CONFIG_H
# include CONFIG_H
#endif

#include <ev.h>

#include <stdlib.h>	// memset
#include <stddef.h>	// offsetof
#include <sys/stat.h>	// struct stat

/* struct stat is embedded inline into ev_stat a couple of times. */
size_t	Struct_stat_sizeof()	{return sizeof(struct stat);}

/* EV_VERSION_* may not equal ev_version_*() so provide accessors to the #defines. */
int Ev_version_major_def(void) {return EV_VERSION_MAJOR;}
int Ev_version_minor_def(void) {return EV_VERSION_MINOR;}

/* Define ev_loop_t shorthand. Saves on typing struct ev_loop. */
typedef struct ev_loop ev_loop_t;

/*
 * Explicitly define callback types.
 * These are defined inline via MACROS in ev.h rather than typedef'd.
 */
typedef ev_tstamp (* ev_periodic_reschedule_cb_t)(ev_periodic *w, ev_tstamp now);

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

/* Non-specific watcher wrappers. */
int
Ev_is_active(const ev_watcher* watcher)
	{
	return ev_is_active(watcher);
	}

int
Ev_priority_get(ev_watcher* watcher)
	{
	return ev_priority(watcher);
	}

void
Ev_priority_set(ev_watcher* watcher, int priority)
	{
	ev_set_priority(watcher, priority);
	}

/* TODO ev_cb */

/* TODO ev_set_cb */

/*
 * Watcher specific struct constructors, initialisers, accessors and reflection functions.
 *
 * Watcher constructors (ie, Make_ev_TYPE):
 * - each return a pointer to an ev_TYPE struct allocated on the heap and initialised
 *   according to ev_TYPE_init,
 * - use free() or foreign-free (from Chez scheme) to free returned memory,
 * - these follow the Perl::EV calling convention of putting the callback argument last.
 *
 * Wrappers are not defined for no-op ev_TYPE_set macros.
 *
 * Since watchers can vary layout across compilations (eg, due to customisation of EV_COMMON),
 * struct sizeof and public member offset functions are defined for testing purposes.
 *
 * Public members described by libev(3) and/or used in ev_TYPE_set macros are exposed
 * with get and set functions as appropriate.
 *
 * Watcher definitions are defined in the same sequence as found in the manpage libev(3).
 */

#define ALLOCZ(type)				\
    type* watcher;      			\
    watcher = malloc(sizeof(*watcher));         \
    memset(watcher, 0, sizeof(*watcher))

ev_io*
Make_ev_io(int fd, int events, ev_io_cb_t cb)
	{
	ALLOCZ(ev_io);
	ev_io_init(watcher, cb, fd, events);
	return watcher;
	}

void
Ev_io_init(ev_io* watcher, ev_io_cb_t cb, int fd, int events)
	{
	ev_io_init(watcher, cb, fd, events);
	}

void
Ev_io_set(ev_io* watcher, int fd, int events)
	{
	ev_io_set(watcher, fd, events);
	}

void
Ev_io_modify(ev_io* watcher, int events)
	{
	ev_io_modify(watcher, events);
	}

size_t
Ev_io_sizeof()
	{
	return sizeof(ev_io);
	}

int
Ev_io_fd_get(const ev_io* watcher)
	{
	return watcher->fd;
	}

void
Ev_io_fd_set(ev_io* watcher, int value)
	{
	watcher->fd = value;
	}

size_t
Ev_io_fd_offsetof()
	{
	return offsetof(ev_io, fd);
	}

int
Ev_io_events_get(const ev_io* watcher)
	{
	return watcher->events;
	}

void
Ev_io_events_set(ev_io* watcher, int value)
	{
	watcher->events = value;
	}

size_t
Ev_io_events_offsetof()
	{
	return offsetof(ev_io, events);
	}

ev_timer*
Make_ev_timer(ev_tstamp after, ev_tstamp repeat, ev_timer_cb_t cb)
	{
	ALLOCZ(ev_timer);
	ev_timer_init(watcher, cb, after, repeat);
	return watcher;
	}

void
Ev_timer_init(ev_timer* watcher, ev_timer_cb_t cb, ev_tstamp after, ev_tstamp repeat)
	{
	ev_timer_init(watcher, cb, after, repeat);
	}

void
Ev_timer_set(ev_timer* watcher, ev_tstamp after, ev_tstamp repeat)
	{
	ev_timer_set(watcher, after, repeat);
	}

size_t
Ev_timer_sizeof()
	{
	return sizeof(ev_timer);
	}

/* NOTE EV_WATCHER_TIME::at is used for ev_timer_after_* */
size_t
Ev_timer_after_offsetof()
	{
	return offsetof(ev_timer, at);
	}

ev_tstamp
Ev_timer_repeat_get(const ev_timer* watcher)
	{
	return watcher->repeat;
	}

void
Ev_timer_repeat_set(ev_timer* watcher, ev_tstamp value)
	{
	watcher->repeat = value;
	}

size_t
Ev_timer_repeat_offsetof()
	{
	return offsetof(ev_timer, repeat);
	}

ev_periodic*
Make_ev_periodic(ev_tstamp offset, ev_tstamp interval, ev_periodic_reschedule_cb_t rcb, ev_periodic_cb_t cb)
	{
	ALLOCZ(ev_periodic);
	ev_periodic_init(watcher, cb, offset, interval, rcb);
	return watcher;
	}

void
Ev_periodic_init(ev_periodic* watcher, ev_periodic_cb_t cb, ev_tstamp offset, ev_tstamp interval, ev_periodic_reschedule_cb_t rcb)
	{
	ev_periodic_init(watcher, cb, offset, interval, rcb);
	}

void
Ev_periodic_set(ev_periodic* watcher, ev_tstamp offset, ev_tstamp interval, ev_periodic_reschedule_cb_t rcb)
	{
	ev_periodic_set(watcher, offset, interval, rcb);
	}

size_t
Ev_periodic_sizeof()
	{
	return sizeof(ev_periodic);
	}

ev_tstamp
Ev_periodic_at(ev_periodic* watcher)
	{
	return ev_periodic_at(watcher);
	}

ev_tstamp
Ev_periodic_offset_get(const ev_periodic* watcher)
	{
	return watcher->offset;
	}

void
Ev_periodic_offset_set(ev_periodic* w, ev_tstamp value)
	{
	w->offset = value;
	}

size_t
Ev_periodic_offset_offsetof()
	{
	return offsetof(ev_periodic, offset);
	}

ev_tstamp
Ev_periodic_interval_get(const ev_periodic* watcher)
	{
	return watcher->interval;
	}

void
Ev_periodic_interval_set(ev_periodic* w, ev_tstamp value)
	{
	w->interval = value;
	}

size_t
Ev_periodic_interval_offsetof()
	{
	return offsetof(ev_periodic, interval);
	}

ev_periodic_reschedule_cb_t
Ev_periodic_reschedule_cb_get(const ev_periodic* w)
	{
	return w->reschedule_cb;
	}

void
Ev_periodic_reschedule_cb_set(ev_periodic* w, ev_periodic_reschedule_cb_t value)
	{
	w->reschedule_cb = value;
	}

size_t
Ev_periodic_reschedule_cb_offsetof()
	{
	return offsetof(ev_periodic, reschedule_cb);
	}

ev_signal*
Make_ev_signal(int signum, ev_signal_cb_t cb)
	{
	ALLOCZ(ev_signal);
	ev_signal_init(watcher, cb, signum);
	return watcher;
	}

void
Ev_signal_init(ev_signal* watcher, ev_signal_cb_t cb, int signum)
	{
	ev_signal_init(watcher, cb, signum);
	}

void
Ev_signal_set(ev_signal* watcher, int signum)
	{
	ev_signal_set(watcher, signum);
	}

size_t
Ev_signal_sizeof()
	{
	return sizeof(ev_signal);
	}

int
Ev_signal_signum_get(const ev_signal* watcher)
	{
	return watcher->signum;
	}

void
Ev_signal_signum_set(ev_signal* watcher, int value)
	{
	watcher->signum = value;
	}

size_t
Ev_signal_signum_offsetof()
	{
	return offsetof(ev_signal, signum);
	}

ev_child*
Make_ev_child(int pid, int trace, ev_child_cb_t cb)
	{
	ALLOCZ(ev_child);
	ev_child_init(watcher, cb, pid, trace);
	return watcher;
	}

void
Ev_child_init(ev_child* watcher, ev_child_cb_t cb, int pid, int trace)
	{
	ev_child_init(watcher, cb, pid, trace);
	}

void
Ev_child_set(ev_child* watcher, int pid, int trace)
	{
	ev_child_set(watcher, pid, trace);
	}

size_t
Ev_child_sizeof()
	{
	return sizeof(ev_child);
	}

// NOTE ev_child trace is stored in `flags`.
size_t
Ev_child_trace_offsetof()
	{
	return offsetof(ev_child, flags);
	}

int
Ev_child_pid_get(const ev_child* watcher)
	{
	return watcher->pid;
	}

void
Ev_child_pid_set(ev_child* watcher, int value)
	{
	watcher->pid = value;
	}

size_t
Ev_child_pid_offsetof()
	{
	return offsetof(ev_child, pid);
	}

int
Ev_child_rpid_get(const ev_child* watcher)
	{
	return watcher->rpid;
	}

void
Ev_child_rpid_set(ev_child* watcher, int value)
	{
	watcher->rpid = value;
	}

size_t
Ev_child_rpid_offsetof()
	{
	return offsetof(ev_child, rpid);
	}

int
Ev_child_rstatus_get(const ev_child* watcher)
	{
	return watcher->rstatus;
	}

void
Ev_child_rstatus_set(ev_child* watcher, int value)
	{
	watcher->rstatus = value;
	}

size_t
Ev_child_rstatus_offsetof()
	{
	return offsetof(ev_child, rstatus);
	}

/*
 * TODO ev_stat getters
 * ev_stat contains two ro ev_statdata structures. Need to think about how to return those.
 */

ev_stat*
Make_ev_stat(const char* path, ev_tstamp interval, ev_stat_cb_t cb)
	{
	ALLOCZ(ev_stat);
	ev_init(watcher, cb);
	// TODO provide a way to free(watcher->path).
	ev_stat_set(watcher, strdup(path), interval);
	return watcher;
	}

size_t
Ev_stat_sizeof()
	{
	return sizeof(ev_stat);
	}

ev_idle*
Make_ev_idle(ev_idle_cb_t cb)
	{
	ALLOCZ(ev_idle);
	ev_idle_init(watcher, cb);
	return watcher;
	}

void
Ev_idle_init(ev_idle* watcher, ev_idle_cb_t cb)
	{
	ev_idle_init(watcher, cb);
	}

size_t
Ev_idle_sizeof()
	{
	return sizeof(ev_idle);
	}

ev_prepare*
Make_ev_prepare(ev_prepare_cb_t cb)
	{
	ALLOCZ(ev_prepare);
	ev_prepare_init(watcher, cb);
	return watcher;
	}

void
Ev_prepare_init(ev_prepare* watcher, ev_prepare_cb_t cb)
	{
	ev_prepare_init(watcher, cb);
	}

size_t
Ev_prepare_sizeof()
	{
	return sizeof(ev_prepare);
	}

ev_check*
Make_ev_check(ev_check_cb_t cb)
	{
	ALLOCZ(ev_check);
	ev_check_init(watcher, cb);
	return watcher;
	}

void
Ev_check_init(ev_check* watcher, ev_check_cb_t cb)
	{
	ev_check_init(watcher, cb);
	}

size_t
Ev_check_sizeof()
	{
	return sizeof(ev_check);
	}

ev_embed*
Make_ev_embed(ev_loop_t* other, ev_embed_cb_t cb)
	{
	ALLOCZ(ev_embed);
	ev_embed_init(watcher, cb, other);
	return watcher;
	}

void
Ev_embed_init(ev_embed* watcher, ev_embed_cb_t cb, struct ev_loop* other)
	{
	ev_embed_init(watcher, cb, other);
	}

void
Ev_embed_set(ev_embed* watcher, struct ev_loop* other)
	{
	ev_embed_set(watcher, other);
	}

size_t
Ev_embed_sizeof()
	{
	return sizeof(ev_embed);
	}

ev_loop_t*
Ev_embed_other_get(const ev_embed* watcher)
	{
	return watcher->other;
	}

size_t
Ev_embed_other_offsetof()
	{
	return offsetof(ev_embed, other);
	}

ev_fork*
Make_ev_fork(ev_fork_cb_t cb)
	{
	ALLOCZ(ev_fork);
	ev_fork_init(watcher, cb);
	return watcher;
	}

void
Ev_fork_init(ev_fork* watcher, ev_fork_cb_t cb)
	{
	ev_fork_init(watcher, cb);
	}

size_t
Ev_fork_sizeof()
	{
	return sizeof(ev_fork);
	}

ev_cleanup*
Make_ev_cleanup(ev_cleanup_cb_t cb)
	{
	ALLOCZ(ev_cleanup);
	ev_cleanup_init(watcher, cb);
	return watcher;
	}

void
Ev_cleanup_init(ev_cleanup* watcher, ev_cleanup_cb_t cb)
	{
	ev_cleanup_init(watcher, cb);
	}

size_t
Ev_cleanup_sizeof()
	{
	return sizeof(ev_cleanup);
	}

ev_async*
Make_ev_async(ev_async_cb_t cb)
	{
	ALLOCZ(ev_async);
	ev_async_init(watcher, cb);
	return watcher;
	}

void
Ev_async_init(ev_async* watcher, ev_async_cb_t cb)
	{
	ev_async_init(watcher, cb);
	}

size_t
Ev_async_sizeof()
	{
	return sizeof(ev_async);
	}

int
Ev_async_pending(const ev_async* watcher)
	{
	return ev_async_pending(watcher);
	}
