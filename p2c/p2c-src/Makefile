# Makefile for "p2c", the Pascal to C translator.
#  Copyright (C) 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
#                1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
#                David Gillespie.
#  Author's address: daveg@csvax.caltech.edu; 256-80 Caltech/Pasadena CA 91125.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation (any version).

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

srcdir = .


# This file compiles and installs p2c in the "home" subdirectory of this
# directory, then runs it on various example programs in the "examples"
# subdirectory.  To install a permanent, public copy of p2c, see "README"
# and src/Makefile.

test:
	@echo "Compiling p2c, installing in `pwd`/home..."
	cd src; $(MAKE) install
	@echo ""
	@echo "Translating and compiling example files..."
	cd examples; $(MAKE) comp
	@echo ""
	@echo "Running the factorial example..."
	examples/fact
	@echo ""
	@echo "Running the 'e' computation example..."
	examples/e
	@echo ""
	@echo "Running the self-printing Pascal example..."
	examples/self >examples/self.out
	cat examples/self.out
	diff examples/self.p examples/self.out
	@echo ""
	@echo "Also try 'cref' and 'basic' in the examples directory."


install: ${srcdir}/configure
	cd src; $(MAKE) install

all: ${srcdir}/configure force
	( cd src ; $(MAKE) all install )

${srcdir}/configure: configure.in # aclocal.m4
	cd ${srcdir} && autoconf

# autoheader might not change auto-host.h.in, so touch a stamp file.
${srcdir}/auto-host.h.in: stamp-h.in
${srcdir}/stamp-h.in: configure.in
	cd ${srcdir} && autoheader
	echo timestamp > ${srcdir}/stamp-h.in

auto-host.h: stamp-h
stamp-h: auto-host.h.in config.status
	./config.status

Makefile: Makefile.in config.status
	./config.status

config.status: configure
	./config.status --recheck

clean: force
	( cd src ; $(MAKE) clean )

force:

