# Chez Scheme libev bindings GNUmakefile.
# Written by Jerry 2019-2021,2023.
# SPDX-License-Identifier: Unlicense

# Provide a CONFIG_H to tailor header to libev as installed at site.
# For now that should probably be limited to EV_COMMON as ev.ss assumes the default libev config.
# eg,
#CONFIG_H = -DCONFIG_H='"site-config.h"'

# Path to chez scheme executable.
SCHEME = /usr/bin/chez-scheme
SCHEMEVERSION = $(shell $(SCHEME) --version 2>&1)
SED = /bin/sed

# Install destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
LIBDIR = ~/lib/csv$(SCHEMEVERSION)
BUILDDIR = BUILD-csv$(SCHEMEVERSION)

# Scheme compile flags.
SFLAGS = -q

# Path to install executable.
INSTALL = /usr/bin/install

CFLAGS = -c -fpic $(CONFIG_H)

LIBFLAGS = -shared

## Should be no need to edit anything below here.

# This makefile assumes a library layout as follows:
# TOP
# PROJDIR/
#   FFISRC
#   FFILIB
#   SUBSRC ..
# BUILDDIR/
#   FFIOBJ ..
#   BSUBOBJ ..
#   BSUBWPO ..
#   BTOPOBJ
#   BTOPWPO
#
# Where TOP is the high level library definition that imports all sub libs within PROJDIR.
# FFISRC (if needed) is a C compilable lib (FFILIB).
# The rest are scheme libraries.
# Scheme compilation is handled by building TOP and letting Chez scheme automatically manage dependants.

PROJDIR = ev

FFIOBJ = ev-ffi.o
FFILIB = $(PROJDIR)/libchez-ffi.so

# Source files, shared objects, and whole program optimisations for the library subdirectory.
SUBSRC = $(PROJDIR)/ftypes-util.chezscheme.sls
SUBOBJ = $(SUBSRC:.sls=.so)
SUBWPO = $(SUBSRC:.sls=.wpo)

# Top level (ie, root) library source, .. etc.
TOPSRC = ev.chezscheme.sls
TOPOBJ = $(TOPSRC:.sls=.so)
TOPWPO = $(TOPSRC:.sls=.wpo)

# Built versions of scheme code above.
BSUBOBJ = $(addprefix $(BUILDDIR)/,$(SUBOBJ))
BSUBWPO = $(addprefix $(BUILDDIR)/,$(SUBWPO))
BTOPOBJ = $(addprefix $(BUILDDIR)/,$(TOPOBJ))
BTOPWPO = $(addprefix $(BUILDDIR)/,$(TOPWPO))

# Installed versions of all the above.
IFFILIB = $(addprefix $(LIBDIR)/,$(FFILIB))
ISUBSRC = $(addprefix $(LIBDIR)/,$(SUBSRC))
ISUBOBJ = $(addprefix $(LIBDIR)/,$(SUBOBJ))
ISUBWPO = $(addprefix $(LIBDIR)/,$(SUBWPO))
ITOPSRC = $(addprefix $(LIBDIR)/,$(TOPSRC))
ITOPOBJ = $(addprefix $(LIBDIR)/,$(TOPOBJ))
ITOPWPO = $(addprefix $(LIBDIR)/,$(TOPWPO))

# Default to just a local build.
all: build

$(FFILIB): $(BUILDDIR)/$(FFIOBJ)
	$(CC) $(LIBFLAGS) $^ -o $@

$(BUILDDIR)/%.o: $(PROJDIR)/%.c
	@mkdir -p $(BUILDDIR)
	$(CC) $(CFLAGS) $< -o $@

# In-place local development test compile. This is built in a separate
# directory BUILDDIR so as to keep build files out of the way.
$(BUILDDIR)/%.wpo: %.sls
	echo	\
		"(reset-handler abort)"			\
		"(compile-imported-libraries #t)"	\
		"(generate-wpo-files #t)"		\
		"(library-directories"			\
		'  (list (cons "." "$(BUILDDIR)")))'	\
		'(import ($(PROJDIR)))'			\
		| $(SCHEME) $(SFLAGS)

# Installed compile. Source files must be copied to destination LIBDIR first
# (via make rules) where this recipe compiles in the remote location.
# This rule is available but not really necessary given that apps should do
# their own whole program compilation and optimisations..
# ie, make install-src should be sufficient.
%.wpo: %.sls
	echo	\
		"(reset-handler abort)"			\
		"(compile-imported-libraries #t)"	\
		"(generate-wpo-files #t)"		\
		'(library-directories "$(LIBDIR)")'	\
		'(import ($(PROJDIR)))'			\
		| $(SCHEME) $(SFLAGS)

# Build target is structured so that the main wpo file is dependant on all scheme source files and triggers
# a compile such that Chez Scheme rebuilds all dependancies on demand.
$(ITOPWPO): $(ITOPSRC) $(ISUBSRC)

$(LIBDIR)/%: %
	$(INSTALL) -m 644 -p -D "$<" "$@"

build: $(BTOPWPO)

install: install-src

# ffi lib is used by tests.
ffi: $(FFILIB) $(TOPSRC) $(SUBSRC)

# install-ffi is deprecated in favour of using the pure scheme implementation.
# It's provided for cases where an installed ffi lib is still required.
install-ffi: $(IFFILIB) ffi

install-so: $(ITOPWPO) $(ISUBWPO)

install-src: $(ITOPSRC) $(ISUBSRC)

clean:
	$(RM) -d pure.scm $(BUILDDIR)/$(FFIOBJ) $(FFILIB) $(BTOPOBJ) $(BTOPWPO) $(BSUBOBJ) $(BSUBWPO) $(BUILDDIR)/$(PROJDIR) $(BUILDDIR)

clean-install:
	$(RM) -d $(IFFIOBJ) $(IFFILIB) $(ITOPOBJ) $(ITOPWPO) $(ISUBOBJ) $(ISUBWPO) $(ITOPSRC) $(ISUBSRC) $(LIBDIR)/$(PROJDIR)

clean-all: clean clean-install

pure.scm: $(TOPSRC)
	$(SED) -n '/PURE_TEST_START/,/PURE_TEST_END/w $@' $<

test: ffi pure.scm test_pure.ss
	./test_pure.ss
