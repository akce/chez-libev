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
size_t	struct_stat_sizeof()	{return sizeof(struct stat);}

/* EV_VERSION_* may not equal ev_version_*() so provide accessors to the #defines. */
int ev_version_major_def(void) {return EV_VERSION_MAJOR;}
int ev_version_minor_def(void) {return EV_VERSION_MINOR;}

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

/*
 * Watcher struct accessors and reflection functions.
 *
 * These structs can vary layout across compilations due to EV_COMMON being customisable.
 * Define accessors so the C compiler can take care of member offsets.
 *
 * Public members described by libev(3) and/or used in ev_TYPE_set macros are exposed
 * with get and set functions as appropriate.
 *
 * Struct sizeof and member offset functions are also provided.
 */
size_t		ev_io_sizeof		()				{return sizeof(ev_io);}
int		ev_io_fd_get		(const ev_io* watcher)		{return watcher->fd;}
//void		ev_io_fd_set		(ev_io* watcher, int value)	{watcher->fd = value;}
size_t		ev_io_fd_offsetof	()				{return offsetof(ev_io, fd);}
int		ev_io_events_get	(const ev_io* watcher)		{return watcher->events;}
// Original public releases used `ev_io_events_set` to wrap macro `ev_io_modify`, don't break that interface yet.
//void		ev_io_events_set	(ev_io* watcher, int value)	{watcher->events = value;}
size_t		ev_io_events_offsetof	()				{return offsetof(ev_io, events);}
size_t		ev_timer_sizeof		()				{return sizeof(ev_timer);}
/* NOTE EV_WATCHER_TIME::at is used for ev_timer_after_* */
size_t		ev_timer_after_offsetof	()				{return offsetof(ev_timer, at);}
ev_tstamp	ev_timer_repeat_get	(const ev_timer* watcher)	{return watcher->repeat;}
void		ev_timer_repeat_set	(ev_timer* watcher, ev_tstamp value) {watcher->repeat = value;}
size_t		ev_timer_repeat_offsetof	() 			{return offsetof(ev_timer, repeat);}
size_t		ev_periodic_sizeof		()			{return sizeof(ev_periodic);}
ev_tstamp	ev_periodic_offset_get	(const ev_periodic* watcher)	{return watcher->offset;}
void		ev_periodic_offset_set	(ev_periodic* w, ev_tstamp value) {w->offset = value;}
size_t		ev_periodic_offset_offsetof	() 			{return offsetof(ev_periodic, offset);}
ev_tstamp	ev_periodic_interval_get(const ev_periodic* watcher)	{return watcher->interval;}
void		ev_periodic_interval_set(ev_periodic* w, ev_tstamp value) {w->interval = value;}
size_t		ev_periodic_interval_offsetof	()			{return offsetof(ev_periodic, interval);}
ev_periodic_reschedule_cb_t ev_periodic_reschedule_cb_get	(const ev_periodic* w)		{return w->reschedule_cb;}
void		ev_periodic_reschedule_cb_set	(ev_periodic* w, ev_periodic_reschedule_cb_t value)	{w->reschedule_cb = value;}
size_t		ev_periodic_reschedule_cb_offsetof	()		{return offsetof(ev_periodic, reschedule_cb);}
size_t		ev_signal_sizeof		()			{return sizeof(ev_signal);}
int		ev_signal_signum_get	(const ev_signal* watcher)	{return watcher->signum;}
//void		ev_signal_signum_set	(ev_signal* watcher, int value)	{watcher->signum = value;}
size_t		ev_signal_signum_offsetof	()			{return offsetof(ev_signal, signum);}
size_t		ev_child_sizeof		()				{return sizeof(ev_child);}
// NOTE ev_child trace is stored in `flags`.
size_t		ev_child_trace_offsetof	()				{return offsetof(ev_child, flags);}
int		ev_child_pid_get	(const ev_child* watcher)	{return watcher->pid;}
//void		ev_child_pid_set	(ev_child* watcher, int value)	{watcher->pid = value;}
size_t		ev_child_pid_offsetof	()				{return offsetof(ev_child, pid);}
int		ev_child_rpid_get	(const ev_child* watcher)	{return watcher->rpid;}
void		ev_child_rpid_set	(ev_child* watcher, int value)	{watcher->rpid = value;}
size_t		ev_child_rpid_offsetof	()				{return offsetof(ev_child, rpid);}
int		ev_child_rstatus_get	(const ev_child* watcher)	{return watcher->rstatus;}
void		ev_child_rstatus_set	(ev_child* watcher, int value)	{watcher->rstatus = value;}
size_t		ev_child_rstatus_offsetof	()			{return offsetof(ev_child, rstatus);}
/*
 * TODO ev_stat getters
 * ev_stat contains two ro ev_statdata structures. Need to think about how to return those.
 */
size_t		ev_stat_sizeof		()				{return sizeof(ev_stat);}
// TODO ev_idle
size_t		ev_idle_sizeof		()				{return sizeof(ev_idle);}
// TODO ev_prepare
size_t		ev_prepare_sizeof	()				{return sizeof(ev_prepare);}
// TODO ev_check
size_t		ev_check_sizeof		()				{return sizeof(ev_check);}
// TODO ev_fork
size_t		ev_fork_sizeof		()				{return sizeof(ev_fork);}
// TODO ev_cleanup
size_t		ev_cleanup_sizeof	()				{return sizeof(ev_cleanup);}
size_t		ev_embed_sizeof		()				{return sizeof(ev_embed);}
ev_loop_t*	ev_embed_other_get	(const ev_embed* watcher)	{return watcher->other;}
size_t		ev_embed_other_offsetof	()				{return offsetof(ev_embed, other);}
// TODO ev_async::sent?
size_t		ev_async_sizeof		()				{return sizeof(ev_async);}

/* Macro wrappers. */
int		ev_watcher_is_active	(const ev_watcher* watcher)	{return ev_is_active(watcher);}
void		ev_io_events_set	(ev_io* watcher, int events)	{ev_io_modify(watcher, events);}
//void		ev_watcher_io_modify	(ev_io* watcher, int events)	{ev_io_modify(watcher, events);}
int		ev_async_pending_get	(const ev_async* watcher)	{return ev_async_pending(watcher);}

int ev_priority_get(ev_watcher* watcher) {return ev_priority(watcher);}
void ev_priority_set(ev_watcher* watcher, int priority) {ev_set_priority(watcher, priority);}
ev_tstamp ev_periodic_at_get(ev_watcher_time* watcher) {return ev_periodic_at(watcher);}

/* TODO ev_cb */

/* TODO ev_set_cb */

/*
 * Watcher constructors.
 *
 * Each return a pointer to the ev type object allocated on the heap and initialised.
 *
 * The benefit is that the C compiler handles structure sizes and scheme can treat these as opaque pointers.
 *
 * There's also no need to provide ev_TYPE_init function wrappers.
 *
 * Use free() or foreign-free to free returned memory.
 *
 * Note: These follow the Perl::EV convention of putting the callback argument last.
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

ev_periodic* make_ev_periodic(ev_tstamp offset, ev_tstamp interval, ev_periodic_reschedule_cb_t rcb, ev_periodic_cb_t cb)
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
