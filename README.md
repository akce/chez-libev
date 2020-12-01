# Chez ev library

chez-libev: [Chez Scheme] bindings for [libev].

## Compiling and installing

The recommended install method is to use the Makefile.

ie, to install (and if necessary, compile) library source and shared-object files to LIBDIR:

    $ make install

Override LIBDIR to change install location (default is ~/lib/csv<CHEZ-SCHEME-VERSION>). eg,

    $ make LIBDIR=/some/other/libdir install

Note that LIBDIR must be in (library-directories). One way to add is by setting CHEZSCHEMELIBDIRS.

Most chez-libev files will be installed under $(LIBDIR)/ev, only the main ev.sls module remains outside.

## How To Use

```scheme
(import (ev))
```
The bindings themselves follow the perl [EV] bindings rather than the [libev] C interface.

This interface stores the current ev loop in the *current-loop* _parameter_, and most functions that need it will refer to it via that rather than as the first function argument.

Parameter ordering also follows the perl [EV] style where callbacks and anonymous functions are generally the last argument to the function.

An example script is provided that demonstrates *io* and *timer* watchers. eg,

```shell
$ ./example.ss
timer called 1
timer called 2
timer called 3
h
key pressed: h
```

## Links

[Chez Scheme]: https://cisco.github.io/ChezScheme/ "Chez Scheme"
[libev]: http://software.schmorp.de/pkg/libev.html "libev"
[EV]: http://software.schmorp.de/pkg/EV.html "EV"

## Hacking

The *Makefile* includes a 'CONFIG_H' variable. This is for sites that have compiled their own custom libev.

## License

Unless otherwise noted, chez-libev is an Unlicensed work released into the Public Domain.
