# chez-libev Makefile.
# Placed in the public domain.

# Provide a CONFIG_H to tailor header to libev as installed at site.
# For now that should probably be limited to EV_COMMON as ev.ss assumes the default libev config.
# eg,
#CONFIG_H = -DCONFIG_H='"site-config.h"'

# Install destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
DEST = ~/lib

# Path to chez scheme executable.
SCHEME = /usr/bin/scheme

# Scheme compile flags.
SFLAGS = -q

# Path to install executable.
INSTALL = /usr/bin/install

CFLAGS = -c -fpic $(CONFIG_H)

LIBFLAGS = -shared

## Should be no need to edit anything below here.

COBJS = ev-ffi.o

# SOBJS need to be in order of dependencies first, library last so that they can be joined for the distribution file.
SOBJS = ftypes-util.so ev.so

BINS = libchez-ev.so ev.chezscheme.so

all: $(BINS)

libchez-ev.so: $(COBJS)
	$(CC) $(LIBFLAGS) $^ -o $@

ev.chezscheme.so: $(SOBJS)
	cat $^ > $@

%.o: %.c
	$(CC) $(CFLAGS) $< -o $@

%.so: %.ss
	echo '(reset-handler abort) (compile-library "'$<'")' | $(SCHEME) $(SFLAGS)

install: all
	$(INSTALL) -D -t $(DEST) $(BINS)

clean:
	rm -f $(COBJS) $(SOBJS) $(BINS)
