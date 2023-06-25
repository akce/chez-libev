# Chez Scheme libev bindings

This project provides [Chez Scheme] bindings for [libev].

[libev] is a small and efficient library that abstracts operating system services like waiting for file I/O, timers, signal handling and more. Clients register callback functions that are executed upon event activity.

Typically, [libev] will be used for an application's event loop.

## News

As of June 2023, these bindings have undergone a major revamp and now use a pure scheme implementation as well as removal of watcher context records. Many missing functions have also been added, although the interface is still incomplete.

This means that:
- distribution is simplified as there's no need for a compiled shared library
- usage is easier with the abolition of the extra watcher context record layer, functions now deal with watcher structs.

## Installing

The recommended install method is to use [GNU make](https://www.gnu.org/software/make/).

The only dependancies are [Chez Scheme] and [libev].

ie, to install library source to the default LIBDIR location:

```shell
$ make install
```

Override LIBDIR to change install location (default is ~/lib/csv\<CHEZ-SCHEME-VERSION\>). eg,

```shell
$ make LIBDIR=/some/other/libdir install
```

Note that LIBDIR must be in `(library-directories)`. One way to add is by setting CHEZSCHEMELIBDIRS.

## How To Use

An [example](example.ss) script is provided that demonstrates:
- `ev-io` waiting on a file (stdin)
- `ev-prepare` as a one-shot way to initialise `ev-timer`
- `ev-timer` to countdown to the end
- `ev-async` to display timer events and how useful this watcher would be for custom user events (via `ev-async-send`)
- `rec` as a way of defining self-contained watchers that cleanup after themselves without adding to any namespace.

```scheme
(import
 (ev)
 (chezscheme))

(define j 0)
(define tw #f)

(define asyncw
  (ev-async
    (lambda (w rev)
      (display "timer called ")(display j)(newline)
      (display "async after timer: pending ")(display (ev-async-pending? w))(newline))))

(define stdinw
  (ev-io 0 (evmask 'READ)
    (lambda (w rev)
      (display "key pressed: ")(display (read-char))(newline)
      (ev-io-stop w)
      (ev-break (evbreak 'ALL)))))

(rec prepw
  (ev-prepare
    (lambda (w rev)
      (display "enabling timer watcher..")(newline)
      (set! tw
        (ev-timer 1 5
          (lambda (timer i)
            (set! j (+ 1 j))
            (ev-async-send asyncw)
            (display "async before timer: pending ")(display (ev-async-pending? asyncw))(newline)
            (when (> j 4)
              (ev-break (evbreak 'ONE))))))
      ;; Once initialised, clear away the prepare watcher.
      (ev-prepare-free prepw))))

(ev-run)
(ev-async-free asyncw)
(ev-io-free stdinw)
(ev-timer-free tw)
```

## API

These bindings are very thin wrappers around [libev]'s C API. As such, it's probably best to learn the general pattern for translation of the native C API to the one provided here. Full education of [libev] should be from their official docs. eg, the [libev(3)] manpage and/or the perl [EV] bindings.

All scheme functions follow so-called *lisp-case*. ie, C function `ev_TYPE_start` will be `ev-TYPE-start` here.

Some functions have been created specifically for these bindings, namely constructors and destructors.

### Construction and destruction functions

Constructor functions use naming convention `ev-TYPE` and destructors use `ev-TYPE-free`, where **TYPE** is the type of the watcher like **io** or **timer** etc. eg, `ev-io` or `ev-timer` etc.

These constructors are similar to those found in the perl [EV] bindings and follow the same parameter order where callbacks are last in the argument list. And just like their perl counterparts, they also automatically start the watcher.

ie, they combine memory allocation + `ev-TYPE-init` + `ev-TYPE-start`.

Use the perl [EV] docs to learn about each type's constructor parameters or look at the C API `ev_TYPE_init` functions but move the callback parameter to the end of the argument list.

Destructors have the naming convention `ev-TYPE-free`. eg, call `ev-io-free` when finished with an `ev-io` watcher.

Destructors will stop the watcher and free associated resources and memory. ie, `ev-TYPE-free` calls `ev-TYPE-stop`, unlocks all function callbacks, frees path strings in the case of `ev-stat` watchers, and finally deallocates watcher memory itself.

Destructors must be called else suffer resource leaks.

#### ev-io / ev-io-free

```
[procedure] ev-io
[args] file-descriptor evmask callback
[return] ev-io-t
```

Watches `file-descriptor` for event types specified by `evmask`. Event types are usually `(evmask 'READ)` or `(evmask 'WRITE)`.

#### ev-timer / ev-timer-free

```
[procedure] ev-timer
[args] after repeat callback
[return] ev-timer-t
```

`ev-timer` creates and starts an ev_timer watcher that initially fires `after` delay, and then continuously fires at `repeat` intervals.

Both `after` and `repeat` are in seconds and may be fixnums or flonums.

#### ev-periodic / ev-periodic-free

`ev-periodic` has 3 major modes for which `ev-absolute-timer`, `ev-interval-timer` and `ev-manual-timer` are locally defined convenience functions so hopefully it won't be necessary to use `ev-periodic` directly.

##### ev-absolute-timer

```
[syntax] ev-absolute-timer
[args] time callback
[return] ev-periodic-t
```

Execute `callback` after `time` delay or at absolute `time`.

[time](https://cisco.github.io/ChezScheme/csug9.5/system.html#./system:h10) must be a `time-utc` for an absolute time or `time-duration` for an offset added to the `(current-time)`.

##### ev-interval-timer

```
[syntax] ev-interval-timer
[args] [offset] interval callback
[return] ev-periodic-t
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
[return] ev-periodic-t
```

TBD

#### ev-signal / ev-signal-free

```
[procedure] ev-signal
[args] signum callback
[return] ev-signal-t
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

#### ev-child / ev-child-free

```
[procedure] ev-child
[args] pid trace callback
[return] ev-child-t
```

Watch for status changes on a process or processes. See your system's *wait(2)* manpage for values of `pid`.

`trace` is 0 to wait only for terminating processes, or 1 to include stopped and continued state changes.

I've had luck waiting for processes created via [open-process-ports](https://cisco.github.io/ChezScheme/csug9.5/foreign.html#./foreign:s5). Chez Scheme reaps child processes as part of garbage collection (search `S_child_processes` in Chez Scheme's code) so i can't guarantee that these systems won't clash with each other.

#### ev-stat / ev-stat-free

```
[procedure] ev-stat
[args] path interval callback
[return] ev-stat-t
```

TBD

#### ev-idle / ev-idle-free

```
[procedure] ev-idle
[args] callback
[returns] ev-idle-t
```

TBD

#### ev-prepare / ev-prepare-free

```
[procedure] ev-prepare
[args] callback
[returns] ev-prepare-t
```

ev-prepare creates watchers that execute their callback just before the ev-loop would go to sleep.

#### ev-check / ev-check-free

```
[procedure] ev-check
[args] callback
[returns] ev-check-t
```

ev-check creates watchers whose callback functions are executed just after the ev-loop has woken up.

#### ev-embed / ev-embed-free

```
[procedure] ev-embed
[args] other callback
[returns] ev-embed-t
```

TBD

#### ev-fork / ev-fork-free

```
[procedure] ev-fork
[args] callback
[returns] ev-fork-t
```

TBD

#### ev-cleanup / ev-cleanup-free

```
[procedure] ev-cleanup
[args] callback
[returns] ev-cleanup-t
```

TBD

#### ev-async / ev-async-free

```
[procedure] ev-async
[args] callback
[returns] ev-async-t
```

ev-async watchers create callbacks whose execution will be triggered via calls to `ev-async-send`.

`ev-async-pending?` will return `#t` if a watcher's callback will be run.

### Multiplicity

[libev] provides build-time configuration of loop multiplicity. ie, whether to handle multiple event loops or not. This manifests chiefly in two ways:
- C callback functions may or may not start with a ev-loop parameter
- a number of C watcher control functions like `ev_TYPE_stop()` may or may not start with an ev-loop parameter.

Regardless of whether multiplicity is true or not, public scheme functions and callbacks do **NOT** include the loop parameter in function argument lists.

Instead, if multiplicity is true, then these bindings will grab the value of the `(current-loop)` [parameter](https://cisco.github.io/ChezScheme/csug9.5/system.html#./system:h13) and pass that to the underlying C function. eg, if C function `ev_now` has signature `ev_now(ev_loop* loop)`, then our bindings take no arguments but call the underlying C function with the value of `(current-loop)`. So a scheme call `(ev-now)` will end up with a translated C call of `ev_now(current_loop)`.

The same is true for all callbacks coming from [libev] to scheme. These bindings override `(current-loop)` with the provided ev-loop value before calling the user callback function. This means that all ev-TYPE-callback functions must have signature:

```scheme
(define my-ev-TYPE-callback
 (lambda (ev-t revents)
   ;; (current-loop) will be set to the callback loop value if EV_MULTIPLITY is true.
   ..
   ))
```

#### current-loop parameter

```
[parameter]: current-loop
[default]: (ev-default-loop)
```

This parameter is implicitly passed to all multiplicity controlled [libev] C functions. It needs to be overridden or manually set for a user to affect [libev] ev-loop choice.

eg, use [parameterize](https://cisco.github.io/ChezScheme/csug9.5/system.html#./system:s250) to change ev-loop used by `ev-TYPE-stop` call:
```scheme
(parameterize ([current-loop my-second-ev-loop])
  (ev-io-stop my-io-watcher))
```

`current-loop` still exists if multiplicity is false, it's just ignored.

### Obsolete watcher context records

Before June 2023, the idea of high level functions equated to constructors creating watcher context records.

eg, `ev-io` returned a watcher context record that contained a reference to an ev-watcher ftype pointer as well as other bits of internal housekeeping. This watcher record was freed via `ev-free-watcher!`.

Now all functions deal with the ev-watcher object directly.

#### ev-free-watcher!

Removed as of June 2023.

Instead use watcher specific free functions. eg, Use `ev-io-free` for watchers created with `ev-io`.

#### ev-watcher-address

Removed as of June 2023.

Watcher context records no longer exist. Use returned objects in functions directly. eg, Use `ev-io-stop` on objects returned by `ev-io`.

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

[ev.chezscheme.sls](ev.chezscheme.sls) can be modified to turn multiplicity on or off (on by default) and can be changed to use the old compiled shared library version (pure scheme is used by default).

It's highly recommended to test the pure bindings on any new system or on [libev] upgrades. Use `make test` for that. It requires GNU sed, GCC, and [libev] development headers.

The **GNUmakefile** includes a `CONFIG_H` variable. This is for sites that have compiled their own custom libev with added ev_watcher fields.

## License

chez-libev is an [unlicensed](LICENSE) work released into the Public Domain.
