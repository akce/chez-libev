# Chez ev library

chez-libev: [Chez Scheme] bindings for [libev].

[libev] provides an event loop, in a nutshell.

[libev] is a small and efficient library that abstracts operating system services like waiting for file I/O, timers, signal handling and more. Clients register callback functions that are executed upon event activity.

`libev` is only available for U*nix like operating systems.

Beware, this API is unstable and likely to change.

## Compiling and installing

The recommended install method is to use [GNU make](https://www.gnu.org/software/make/).

A C compiler is required, along with the [libev] headers.

ie, to compile and install library source and shared-object files to LIBDIR:

    $ make install

Override LIBDIR to change install location (default is ~/lib/csv\<CHEZ-SCHEME-VERSION\>). eg,

    $ make LIBDIR=/some/other/libdir install

Note that LIBDIR must be in (library-directories). One way to add is by setting CHEZSCHEMELIBDIRS.

The default install target will install source files, shared object files, and [whole program optimisation](https://cisco.github.io/ChezScheme/csug9.5/system.html#./system:s117) files to LIBDIR. Other targets exist that install source only (install-src) and objects only (install-so).

Those using this library as part of [compiled programs](https://cisco.github.io/ChezScheme/csug9.5/system.html#./system:s76) will need to also distribute chez-libev's C compiled library `ev/chez-ffi.so`.

## How To Use

```scheme
(import (ev))
```

An [example](example.ss) script is provided that demonstrates `async`, `io`, `prepare` and `timer` watchers. eg,

```shell
$ ./example.ss
enabling timer watcher..
async before timer: pending #t
timer called 1
async after timer: pending #f
async before timer: pending #t
timer called 2
async after timer: pending #f
h
key pressed: h
$
```

Note that the `ENTER` or `RETURN` key is pressed after the `h` key in the above demo.

## API

This document should only be viewed as a companion to the [libev(3)] manpage as this only shows differences between the C level API and the scheme one.

Problems with these bindings should be raised here only.

Conceptually, these bindings can be split into high-level and low-level functions.

The high-level functions are specific to these bindings and return and deal with watcher contexts; watcher contexts are [records](https://scheme.com/tspl4/records.html#./records:h0) that wrap the underlying watcher address and callback with enough data to allow for stopping and freeing associated resources.

The low-level bindings map almost directly with [libev] functions except for those that require an ev_loop argument. An ev_loop parameter is omitted in these bindings and instead taken from a `current-loop` parameter.

### current-loop parameter

```
[parameter]: current-loop
[default]: EV_DEFAULT. ie, (ev-default-loop)
```

All C functions that take loop argument (usually as their first arg) will, in the scheme interface, refer to it via `current-loop`.

eg, for `ev-run`

C version | Scheme version
--------- | --------------
ev_run(EV_DEFAULT_ 0) | (ev-run) or (ev-run 0)

This also affects the callback signature for high-level bindings. `current-loop` is modified for the duration of the callback and omitted from the arg list.

C signature | Scheme signature
----------- | ----------------
int ev_\<type>_callback(struct ev_loop*, struct ev_\<type>*, int revents) | (lambda (ev-watcher* revents))

Where:
- `ev-watcher*` is an ftype watcher address.
- `revents` is an int.

Note that the low-level bindings include the ev_loop pointer in their callbacks and thus look like their C counterparts.

### High level bindings 

These bindings are similar to the extra ones added to the perl [EV] bindings. They are written in scheme via the `define-watcher` syntax transformer and can be thought of as constructors that combine allocating and starting watchers.

They return a watcher context record that should be freed by client code using `ev-free-watcher!`.

Parameter ordering also follows the perl [EV] style where callbacks and anonymous functions are the last argument to the function.

The following sections will document the higher level functions as defined by these bindings.

#### ev-free-watcher!

```
[procedure] ev-free-watcher!
[args] watcher-record
[return] none
```

This function takes a watcher record, as created by one of the high-level functions - stops the watcher, unlocks the ftype function callback (as discussed [here](https://cisco.github.io/ChezScheme/csug9.5/foreign.html#./foreign:s143)), and frees associated memory.

Warning: an earlier release accidentally published this as `free-watcher`. Please change code to the new name as `free-watcher` will be removed at some point soon.

#### ev-watcher-address

```
[procedure] ev-watcher-address
[args] watcher-record
[return] void* address for the libev watcher
```

This function takes a watcher record, as created by one of the high-level functions.

`ev-watcher-address` is used to access the underlying watcher address as used by libev. This address is still required by the low-level functions like `ev-timer-stop` etc.

#### ev-io

```
[procedure] ev-io
[args] file-descriptor evmask callback
[return] watcher-record
```

Watches `file-descriptor` for event types specified by `evmask`. Event types are usually `(evmask 'READ)` or `(evmask 'WRITE)`.

#### ev-timer

```
[procedure] ev-timer
[args] after repeat callback
[return] watcher-record
```

`ev-timer` creates and starts an ev_timer watcher that initially fires `after` delay, and then continuously fires at `repeat` intervals.

Both `after` and `repeat` are in seconds and may be fixnums or flonums.

#### ev-periodic

```
[procedure] ev-periodic
[args] offset interval reschedule-callback callback
[return] watcher-record
```

Similar to `ev-timer` but tied to computer clock time. This one is a bit involved so it's probably best to refer to [libev(3)] for details.

`ev-periodic` has 3 major modes for which `ev-absolute-timer`, `ev-interval-timer` and `ev-manual-timer` are locally defined convenience functions so hopefully it won't be necessary to use `ev-periodic` directly.

##### ev-absolute-timer

```
[syntax] ev-absolute-timer
[args] time callback
[return] watcher-record
```

Execute `callback` after `time` delay or at absolute `time`.

[time](https://cisco.github.io/ChezScheme/csug9.5/system.html#./system:h10) must be a `time-utc` for an absolute time or `time-duration` for an offset added to the `(current-time)`.

##### ev-interval-timer

```
[syntax] ev-interval-timer
[args] [offset] interval callback
[return] watcher-record
```

Creates a timer that triggers based on computer clock times. eg, to create a timer that runs at the start of every minute:
```
(ev-interval-timer 60 (lambda (w rev) ...))
```

Using the optional `offset` argument, it becomes possible to create a timer that triggers every `interval` + `offset` period.

eg, for a timer that fires ten minutes into the hour, every hour:
```
(ev-interval-timer (* 60 10) (* 60 60) (lambda (w rev) ...))
```

The default value for `offset` is 0.

##### ev-manual-timer

```
[syntax] ev-manual-timer
[args] reschedule-callback callback
[return] watcher-record
```

TBD

#### ev-signal

```
[procedure] ev-signal
[args] signum callback
[return] watcher-record
```

Invokes `callback` on receipt of signal `signum`.

eg, fake a **KEY_RESIZE** keypress to `ev-io` STDIN watchers in order to handle window resize events for an [ncurses](https://invisible-island.net/ncurses/) app running via a [libev] event loop:
```scheme
;; Values on Linux.
(define STDIN_FD 0)
(define SIGWINCH 28)

(ev-signal SIGWINCH
  (lambda (w rev)
    (endwin)
    (refresh)
    (ungetch KEY_RESIZE)
    (ev-feed-fd-event STDIN_FD (evmask 'READ))))
```

#### ev-child

```
[procedure] ev-child
[args] pid trace callback
[return] watcher-record
```

Watch for status changes on a process or processes. See your system's *wait(2)* manpage for values of `pid`.

`trace` is 0 to wait only for terminating processes, or 1 to include stopped and continued state changes.

I've had luck waiting for processes created via [open-process-ports](https://cisco.github.io/ChezScheme/csug9.5/foreign.html#./foreign:s5). Chez reaps child processes as part of garbage collection (search S_child_processes in the Chez code) and so i don't think these systems clash with each other.

#### ev-stat

```
[procedure] ev-stat
[args] path interval callback
[return] watcher-record
```

TBD

#### ev-idle

```
[procedure] ev-idle
[args] callback
[returns] watcher-record
```

TBD

#### ev-prepare

```
[procedure] ev-prepare
[args] callback
[returns] watcher-record
```

ev-prepare creates watchers that execute their callback just before the ev-loop would go to sleep.

#### ev-check

```
[procedure] ev-check
[args] callback
[returns] watcher-record
```

ev-check creates watchers whose callback functions are executed just after the ev-loop has woken up.

#### ev-embed

```
[procedure] ev-embed
[args] other callback
[returns] watcher-record
```

TBD

#### ev-fork

```
[procedure] ev-fork
[args] callback
[returns] watcher-record
```

TBD

#### ev-cleanup

```
[procedure] ev-cleanup
[args] callback
[returns] watcher-record
```

TBD

#### ev-async

```
[procedure] ev-async
[args] callback
[returns] watcher-record
```

ev-async watchers create callbacks whose execution will be triggered via calls to `ev-async-send`.

`ev-async-pending?` will return `#t` if a watcher's callback will be run.

## Links

[libev](http://software.schmorp.de/pkg/libev.html)

[libev]: http://software.schmorp.de/pkg/libev.html "libev"

[libev(3)]: http://pod.tst.eu/http://cvs.schmorp.de/libev/ev.pod "libev(3)"

Perl [EV](http://software.schmorp.de/pkg/EV.html) bindings.

[EV]: http://software.schmorp.de/pkg/EV.html "EV"

[Chez Scheme](https://cisco.github.io/ChezScheme/)

[Chez Scheme]: https://cisco.github.io/ChezScheme/ "Chez Scheme"

[R6RS](http://r6rs.org/)

[R6RS]: http://r6rs.org "R6RS"

## Hacking

The **GNUmakefile** includes a `CONFIG_H` variable. This is for sites that have compiled their own custom libev with added ev_watcher fields.

## License

chez-libev is an [unlicensed](LICENSE) work released into the Public Domain.
