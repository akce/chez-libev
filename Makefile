# chez-libev Makefile.
# Written by Akce 2019, 2020.
# SPDX-License-Identifier: Unlicense

# Provide a CONFIG_H to tailor header to libev as installed at site.
# For now that should probably be limited to EV_COMMON as ev.ss assumes the default libev config.
# eg,
#CONFIG_H = -DCONFIG_H='"site-config.h"'

# Install destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
LIBDIR = ~/lib/csv$(shell $(SCHEME) --version 2>&1)

# Path to chez scheme executable.
SCHEME = /usr/bin/scheme

# Scheme compile flags.
SFLAGS = -q

# Path to install executable.
INSTALL = /usr/bin/install

CFLAGS = -c -fpic $(CONFIG_H)

LIBFLAGS = -shared

## Should be no need to edit anything below here.

PROJDIR = ev

LIBOBJ = $(PROJDIR)/ev-ffi.o
LIBSO = $(PROJDIR)/libchez-ffi.so

# Source files for the library subdirectory.
SUBSRC = $(PROJDIR)/ftypes-util.chezscheme.sls
# Source objects for the library subdirectory.
SUBOBJ = $(PROJDIR)/ftypes-util.chezscheme.so

# Root source file.
ROOTSRC = ev.chezscheme.sls
# Root shared object.
ROOTOBJ = ev.chezscheme.so

all: $(LIBSO) $(SUBOBJ) $(ROOTOBJ)

$(LIBSO): $(LIBOBJ)
	$(CC) $(LIBFLAGS) $^ -o $@

%.o: %.c
	$(CC) $(CFLAGS) $< -o $@

%.so: %.sls
	echo '(reset-handler abort) (compile-library "'$<'")' | $(SCHEME) $(SFLAGS)

# install-lib is always required, installations then need to decide what combination of src/so they want.
# Default install target is for everything.
install: install-lib install-so install-src

install-lib: all
	$(INSTALL) -D -p -t $(LIBDIR)/$(PROJDIR) $(LIBSO)

install-so: all
	$(INSTALL) -D -p -t $(LIBDIR)/$(PROJDIR) $(SUBOBJ)
	$(INSTALL) -D -p -t $(LIBDIR) $(ROOTOBJ)

install-src: all
	$(INSTALL) -D -p -t $(LIBDIR)/$(PROJDIR) $(SUBSRC)
	$(INSTALL) -D -p -t $(LIBDIR) $(ROOTSRC)

clean:
	$(RM) $(LIBOBJ) $(LIBSO) $(ROOTOBJ) $(SUBOBJ)
