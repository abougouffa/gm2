# Generated automatically from Makefile.in by configure.
# The makefile built from this file lives in the language subdirectory.
# It's purpose is to provide support for:
#
# 1) recursion where necessary, and only then (building .o's), and
# 2) building and debugging cc1 from the language subdirectory, and
# 3) nothing else.
#
# The parent makefile handles all other chores, with help from the
# language makefile fragment, of course.
#
# The targets for external use are:
# all, TAGS, ???mostlyclean, ???clean.

# Suppress smart makes who think they know how to automake Yacc files
.y.c:

# Variables that exist for you to override.
# See below for how to change them for certain systems.

# Various ways of specifying flags for compilations:  
# CFLAGS is for the user to override to, e.g., do a bootstrap with -O2.
# BOOT_CFLAGS is the value of CFLAGS to pass
# to the stage2 and stage3 compilations
# XCFLAGS is used for most compilations but not when using the GCC just built.
XCFLAGS =
CFLAGS = -g
BOOT_CFLAGS = -O $(CFLAGS)
# These exists to be overridden by the x-* and t-* files, respectively.
X_CFLAGS =
T_CFLAGS =

X_CPPFLAGS =
T_CPPFLAGS =

CC = gcc
BISON = bison
BISONFLAGS =
LEX = flex
LEXFLAGS =
AR = ar
AR_FLAGS = rc
SHELL = /bin/sh
MAKEINFO = makeinfo
TEXI2DVI = texi2dvi

# Define this as & to perform parallel make on a Sequent.
# Note that this has some bugs, and it seems currently necessary 
# to compile all the gen* files first by hand to avoid erroneous results.
P =

# This is used instead of ALL_CFLAGS when compiling with GCC_FOR_TARGET.
# It omits XCFLAGS, and specifies -B./.
# It also specifies -B$(tooldir)/ to find as and ld for a cross compiler.
GCC_CFLAGS=$(INTERNAL_CFLAGS) $(X_CFLAGS) $(T_CFLAGS) $(CFLAGS) $(XCFLAGS) \
	-DHAVE_CONFIG_H


# Tools to use when building a cross-compiler.
# These are used because `configure' appends `cross-make'
# to the makefile when making a cross-compiler.

# We don't use cross-make.  Instead we use the tools
# from the build tree, if they are available.
# program_transform_name and objdir are set by configure.in.
program_transform_name =
objdir = .

target=i686-pc-linux-gnu
xmake_file= ./config/x-linux
tmake_file= ./config/t-linux ./config/i386/t-crtstuff ./config/t-install-cpp
#version=`sed -e 's/.*\"\([^ \"]*\)[ \"].*/\1/' < $(srcdir)/version.c`
#mainversion=`sed -e 's/.*\"\([0-9]*\.[0-9]*\).*/\1/' < $(srcdir)/version.c`

# Directory where sources are, from where we are.
srcdir = .

# Additional system libraries to link with.
CLIB=

# Top build directory, relative to here.
top_builddir = ..

# Internationalization library.
INTLLIBS = 

# Choose the real default target.
ALL=all

# End of variables for you to override.

# Definition of `all' is here so that new rules inserted by sed
# do not specify the default target.
all: all.indirect

# This tells GNU Make version 3 not to put all variables in the environment.
.NOEXPORT:

# sed inserts variable overrides after the following line.
####target overrides

# Don't run fixproto
STMP_FIXPROTO =

# Don't install "assert.h" in gcc. We use the one in glibc.
INSTALL_ASSERT_H =

# Compile crtbeginS.o and crtendS.o with pic.
CRTSTUFF_T_CFLAGS_S = -fPIC
# Compile libgcc2.a with pic.
TARGET_LIBGCC2_CFLAGS = -fPIC

# Do not build libgcc1. Let gcc generate those functions. The GNU/Linux
# C library can handle them.
LIBGCC1 = 
CROSS_LIBGCC1 =
LIBGCC1_TEST =
# The pushl in CTOR initialization interferes with frame pointer elimination.
CRTSTUFF_T_CFLAGS = -fno-omit-frame-pointer
# Handle cpp installation.
INSTALL_CPP=install-cpp
UNINSTALL_CPP=uninstall-cpp
####host overrides

# Don't run fixproto
STMP_FIXPROTO =

# Don't install "assert.h" in gcc. We use the one in glibc.
INSTALL_ASSERT_H =
####cross overrides


####build overrides

#
# Now figure out from those variables how to compile and link.

all.indirect: Makefile ../cc1gm2$(exeext)

# IN_GCC tells obstack.h that we are using gcc's <stddef.h> file.
INTERNAL_CFLAGS = $(CROSS) -DIN_GCC 

# This is the variable actually used when we compile.
ALL_CFLAGS = $(INTERNAL_CFLAGS) $(X_CFLAGS) $(T_CFLAGS) $(CFLAGS) $(XCFLAGS) -W -Wall -DGM2

# Likewise.
ALL_CPPFLAGS = $(CPPFLAGS) $(X_CPPFLAGS) $(T_CPPFLAGS)

# This is where we get libiberty.a from.
LIBIBERTY = ../../libiberty/libiberty.a

# How to link with both our special library facilities
# and the system's installed libraries.
LIBS = $(INTLLIBS) $(LIBIBERTY) $(CLIB)
LIBDEPS = $(INTLLIBS) $(LIBIBERTY)

# Specify the directories to be searched for header files.
# Both . and srcdir are used, in that order,
# so that tm.h and config.h will be found in the compilation
# subdirectory rather than in the source directory.
INCLUDES = -I. -I.. -I$(srcdir) -I$(srcdir)/.. -I$(srcdir)/../config -I$(srcdir)/../../include

# Always use -I$(srcdir)/config when compiling.
.c.o:
	$(CC) -c $(ALL_CFLAGS) $(ALL_CPPFLAGS) $(INCLUDES) $<

# The only suffixes we want for implicit rules are .c and .o.
.SUFFIXES:
.SUFFIXES: .c .o

# This tells GNU make version 3 not to export all the variables
# defined in this file into the environment.
.NOEXPORT:

# Lists of files for various purposes.

# Language-specific object files for our gm2 compiler.

GM2_OBJS = gm2.o
GM2_LIBS = m2/comp/bin1/libgccgm2.a m2/comp/bin1/libm2.a m2/comp/p2c/home/libp2c.a   # the Modula-2 front end

# Remove patched files from language-independent object file list.

GCC_OBJDEPS = ../stamp-objlist c-convert.o toplev.o
GCC_OBJS = `cat ../stamp-objlist | sed -e 's:  : :g' \
        -e 's:../toplev.o::g'` c-convert.o toplev.o

# GBE files recompiled with different flags for GM2.
# Rebuild these with our flags
GCC_PATCHED = toplev.o

compiler: ../cc1gm2$(exeext)
../cc1gm2$(exeext): $(GM2_OBJS) $(GCC_PATCHED) $(GCC_OBJDEPS) $(LIBDEPS) gccgm2
	echo $(CC) $(ALL_CFLAGS) $(LDFLAGS) -o $@ $(GM2_OBJS) $(GM2_LIBS) $(GCC_OBJS) $(LIBS)
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o $@ $(GM2_OBJS) $(GM2_LIBS) $(GCC_OBJS) $(LIBS)

Makefile: $(srcdir)/Makefile.in $(srcdir)/../configure
	cd ..; $(SHELL) config.status

native: config.status ../cc1gm2$(exeext)

# Compiling object files from source files.

# Note that dependencies on obstack.h are not written
# because that file is not part of GCC.

RTL_H = $(srcdir)/../rtl.h $(srcdir)/../rtl.def \
	$(srcdir)/../machmode.h $(srcdir)/../machmode.def
TREE_H = $(srcdir)/../tree.h $(srcdir)/../real.h $(srcdir)/../tree.def \
	$(srcdir)/../machmode.h $(srcdir)/../machmode.def
FLAGS_H = $(srcdir)/../flags.h
INPUT_H = $(srcdir)/../input.h
CONVERT_H = $(srcdir)/../convert.h

gm2.o : gm2.c $(CONFIG_H) $(TREE_H) $(RTL_H) $(srcdir)/../flags.h

toplev.o : $(srcdir)/../toplev.c $(CONFIG_H) $(TREE_H) $(RTL_H) \
 $(FLAGS_H) $(INPUT_H) \
 ../insn-attr.h $(srcdir)/../xcoffout.h $(srcdir)/../defaults.h \
 $(srcdir)/../output.h
	$(CC) $(ALL_CFLAGS) $(ALL_CPPFLAGS) $(INCLUDES) \
	  $(MAYBE_TARGET_DEFAULT) $(MAYBE_USE_COLLECT2) \
	  -DTARGET_NAME=\"$(target_alias)\" \
	  -c `echo $(srcdir)/../toplev.c | sed 's,^\./,,'`

c-convert.o : $(srcdir)/../c-convert.c $(CONFIG_H) $(TREE_H) $(RTL_H) \
 $(FLAGS_H) $(INPUT_H) \
 ../insn-attr.h $(srcdir)/../xcoffout.h $(srcdir)/../defaults.h \
 $(srcdir)/../output.h
	$(CC) $(ALL_CFLAGS) $(ALL_CPPFLAGS) $(INCLUDES) \
	  $(MAYBE_TARGET_DEFAULT) $(MAYBE_USE_COLLECT2) \
	  -DTARGET_NAME=\"$(target_alias)\" \
	  -c `echo $(srcdir)/../c-convert.c | sed 's,^\./,,'`

gccgm2: force
	( cd m2/comp ; export M2PWD="`pwd`" ; echo $$M2PWD ; \
          make "BINDIR=$$M2PWD/bin" COPY="cp" stage0 )
	( cd m2/comp ; export M2PWD="`pwd`" ; echo $$M2PWD ; \
          make \
               "PUSH=LEFTTORIGHT" \
               "DEBUG=-g" \
               "BINDIR=$$M2PWD/bin" \
               "BINDIR1=$$M2PWD/bin1" \
               "INCFILE=$$M2PWD/tools/incfile" \
               "MAKEVERSION=$$M2PWD/tools/makeversion" \
               "MAKECONFIGURE=$$M2PWD/tools/makeconfigure" \
               "STAGEFILE=$$M2PWD/.stage" \
               "MAJORFILE=$$M2PWD/.major" \
               "MINORFILE=$$M2PWD/.minor" \
               "ARCHIVEFILE=$$M2PWD/.archive" \
               "AR=ar" \
               "COPY=cp" \
               "LN=ln -s" \
               "MV=mv" \
               "P2C=$$M2PWD/bin/p2c" \
               "P2CDIR=$$M2PWD/p2c/home" \
               "PRE=$$M2PWD/bin/mkfor" \
               "M2LINK=$$M2PWD/bin/mklink" \
               stage1.gcc )

gm2tools: force
	( cd m2/comp ; export M2PWD="`pwd`" ; echo $$M2PWD ; \
          make "BINDIR=$$M2PWD/bin" COPY="cp" stage0 )
	( cd m2/comp ; export M2PWD="`pwd`" ; echo $$M2PWD ; \
          make \
               "PUSH=LEFTTORIGHT" \
               "DEBUG=-g" \
               "BINDIR=$$M2PWD/bin" \
               "BINDIR1=$$M2PWD/bin1" \
               "INCFILE=$$M2PWD/tools/incfile" \
               "MAKEVERSION=$$M2PWD/tools/makeversion" \
               "MAKECONFIGURE=$$M2PWD/tools/makeconfigure" \
               "STAGEFILE=$$M2PWD/.stage" \
               "MAJORFILE=$$M2PWD/.major" \
               "MINORFILE=$$M2PWD/.minor" \
               "ARCHIVEFILE=$$M2PWD/.archive" \
               "AR=ar" \
               "COPY=cp" \
               "LN=ln -s" \
               "MV=mv" \
               "P2C=$$M2PWD/bin/p2c" \
               "P2CDIR=$$M2PWD/p2c/home" \
               "PRE=$$M2PWD/bin/mkfor" \
               "M2LINK=$$M2PWD/bin/mklink" \
               "CFLAGS=-g" \
               stage1.gcctools )

clean: force
	$(RM) *.o *.html *.ps *.dvi *.log *.aux *.info *.ps *.aux
	( cd m2 ; make clean )

force: