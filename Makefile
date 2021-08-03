# chez-libev Makefile.
# Written by Jerry 2019-2021.
# SPDX-License-Identifier: Unlicense

# Provide a CONFIG_H to tailor header to libev as installed at site.
# For now that should probably be limited to EV_COMMON as ev.ss assumes the default libev config.
# eg,
#CONFIG_H = -DCONFIG_H='"site-config.h"'

# Install destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
LIBDIR = ~/lib/csv$(shell $(SCHEME) --version 2>&1)

# Path to chez scheme executable.
SCHEME = /usr/bin/chez-scheme

# Scheme compile flags.
SFLAGS = -q

# Path to install executable.
INSTALL = /usr/bin/install

CFLAGS = -c -fpic $(CONFIG_H)

LIBFLAGS = -shared

## Should be no need to edit anything below here.

LIBOBJ = ev-ffi.o
LIBSO = ev-ffi.so

# Root source file.
SRC = ev.chezscheme.sls
OBJ = ev.chezscheme.so

# Installed paths.
ILIBSO = $(addprefix $(LIBDIR)/,$(LIBSO))
ISRC = $(addprefix $(LIBDIR)/,$(SRC))
IOBJ = $(addprefix $(LIBDIR)/,$(OBJ))

all: install

$(LIBSO): $(LIBOBJ)
	$(CC) $(LIBFLAGS) $^ -o $@

$(ILIBSO): $(LIBSO)
	$(INSTALL) -D -p -t $(LIBDIR) $(LIBSO)

%.o: %.c
	$(CC) $(CFLAGS) $< -o $@

%.so: %.sls
	echo '(reset-handler abort) (compile-library "'$<'")' | $(SCHEME) $(SFLAGS)

$(LIBDIR)/%.sls: %.sls
	$(INSTALL) -D -p $< $@

# install-lib is always required, installations then need to decide what combination of src/so they want.
# Default install target is for everything.
install: install-so

install-lib: $(ILIBSO)
	$(INSTALL) -D -p -t $(LIBDIR) $(LIBSO)

install-src: install-lib $(ISRC)
	$(INSTALL) -D -p -t $(LIBDIR) $(SRC)

install-so: install-src $(IOBJ)

clean:
	$(RM) $(LIBOBJ) $(LIBSO) $(ISRC) $(ILIBSO)
