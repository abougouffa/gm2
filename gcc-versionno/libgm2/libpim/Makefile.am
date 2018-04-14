
SUFFIXES = .c .mod .def .o .obj .lo .a .la

ACLOCAL_AMFLAGS = -I . -I .. -I ../config

VPATH = . @srcdir@ @srcdir@/../../gcc/gm2/gm2-libs

# Multilib support.
MAKEOVERRIDES=

version := $(shell $(CC) -dumpversion)

# Directory in which the compiler finds libraries etc.
libsubdir = $(libdir)/gcc/$(host_noncanonical)/$(version)
# Used to install the shared libgcc.
slibdir = @slibdir@

toolexeclibdir=@toolexeclibdir@
toolexecdir=@toolexecdir@

MULTIDIR := $(shell $(CC) $(CFLAGS) -print-multi-directory)
MULTIOSDIR := $(shell $(CC) $(CFLAGS) -print-multi-os-directory)

MULTIOSSUBDIR := $(shell if test $(MULTIOSDIR) != .; then echo /$(MULTIOSDIR); fi)
inst_libdir = $(libsubdir)$(MULTISUBDIR)
inst_slibdir = $(slibdir)$(MULTIOSSUBDIR)


# Work around what appears to be a GNU make bug handling MAKEFLAGS
# values defined in terms of make variables, as is the case for CC and
# friends when we are called from the top level Makefile.
AM_MAKEFLAGS = \
        "GCC_DIR=$(GCC_DIR)" \
        "GM2_SRC=$(GM2_SRC)" \
	"AR_FLAGS=$(AR_FLAGS)" \
	"CC_FOR_BUILD=$(CC_FOR_BUILD)" \
	"CC_FOR_TARGET=$(CC_FOR_TARGET)" \
	"GM2_FOR_TARGET=$(GM2_FOR_TARGET)" \
	"CFLAGS=$(CFLAGS)" \
	"CXXFLAGS=$(CXXFLAGS)" \
	"CFLAGS_FOR_BUILD=$(CFLAGS_FOR_BUILD)" \
	"CFLAGS_FOR_TARGET=$(CFLAGS_FOR_TARGET)" \
	"INSTALL=$(INSTALL)" \
	"INSTALL_DATA=$(INSTALL_DATA)" \
	"INSTALL_PROGRAM=$(INSTALL_PROGRAM)" \
	"INSTALL_SCRIPT=$(INSTALL_SCRIPT)" \
	"LDFLAGS=$(LDFLAGS)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"LIBCFLAGS_FOR_TARGET=$(LIBCFLAGS_FOR_TARGET)" \
	"MAKE=$(MAKE)" \
	"MAKEINFO=$(MAKEINFO) $(MAKEINFOFLAGS)" \
        "MULTISUBDIR=$(MULTISUBDIR)" \
        "MULTIOSDIR=$(MULTIOSDIR)" \
        "MULTIFLAGS=$(MULTIFLAGS)" \
	"PICFLAG=$(PICFLAG)" \
	"PICFLAG_FOR_TARGET=$(PICFLAG_FOR_TARGET)" \
	"SHELL=$(SHELL)" \
	"RUNTESTFLAGS=$(RUNTESTFLAGS)" \
	"exec_prefix=$(exec_prefix)" \
	"infodir=$(infodir)" \
	"libdir=$(libdir)" \
	"includedir=$(includedir)" \
	"prefix=$(prefix)" \
	"tooldir=$(tooldir)" \
        "toolexeclibdir=$(toolexeclibdir)" \
        "toolexecdir=$(toolexecdir)" \
	"gxx_include_dir=$(gxx_include_dir)" \
	"AR=$(AR)" \
	"AS=$(AS)" \
	"LD=$(LD)" \
	"RANLIB=$(RANLIB)" \
	"NM=$(NM)" \
	"NM_FOR_BUILD=$(NM_FOR_BUILD)" \
	"NM_FOR_TARGET=$(NM_FOR_TARGET)" \
	"DESTDIR=$(DESTDIR)" \
	"WERROR=$(WERROR)" \
        "TARGET_LIB_PATH_libgm2=$(TARGET_LIB_PATH_libgm2)"

# Subdir rules rely on $(FLAGS_TO_PASS)
FLAGS_TO_PASS = $(AM_MAKEFLAGS)


# if BUILD_PIMLIB
toolexeclib_LTLIBRARIES = libgm2.la

M2MODS = ASCII.mod IO.mod \
       Args.mod M2RTS.mod \
       Assertion.mod NumberIO.mod \
       Break.mod SYSTEM.mod \
       CmdArgs.mod Scan.mod \
       StrCase.mod FIO.mod \
       StrIO.mod StrLib.mod \
       TimeString.mod Environment.mod \
       FpuIO.mod Debug.mod \
       SysStorage.mod Storage.mod \
       StdIO.mod SEnvironment.mod \
       DynamicStrings.mod SFIO.mod \
       SArgs.mod SCmdArgs.mod \
       PushBackInput.mod \
       StringConvert.mod FormatStrings.mod \
       Builtins.mod MathLib0.mod \
       M2EXCEPTION.mod RTExceptions.mod \
       SMathLib0.mod RTint.mod \
       COROUTINES.mod Indexing.mod \
       LMathLib0.mod LegacyReal.mod \
       MemUtils.mod gdbif.mod

M2DEFS = Args.def   ASCII.def \
         Assertion.def  Break.def \
         Builtins.def  cbuiltin.def \
         CmdArgs.def  COROUTINES.def \
         cxxabi.def  Debug.def \
         dtoa.def  DynamicStrings.def \
         Environment.def  errno.def \
         FIO.def  FormatStrings.def \
         FpuIO.def  gdbif.def \
         Indexing.def \
         IO.def  ldtoa.def \
         LegacyReal.def  libc.def \
         libm.def  LMathLib0.def \
         M2EXCEPTION.def  M2RTS.def \
         MathLib0.def  MemUtils.def \
         NumberIO.def  PushBackInput.def \
         RTExceptions.def  RTint.def \
         SArgs.def  SCmdArgs.def \
         Scan.def \
         sckt.def  Selective.def \
         SEnvironment.def  SFIO.def \
         SMathLib0.def  StdIO.def \
         Storage.def  StrCase.def \
         StringConvert.def  StrIO.def \
         StrLib.def  SysExceptions.def \
         SysStorage.def  SYSTEM.def \
         termios.def  TimeString.def \
         UnixArgs.def  wrapc.def

libgm2_la_SOURCES = $(M2MODS) \
                    UnixArgs.c \
                    Selective.c sckt.c \
                    errno.c dtoa.c \
                    ldtoa.c termios.c \
                    SysExceptions.c target.c \
                    wrapc.c

libgm2dir = libpim
libgm2_la_DEPENDENCIES = SYSTEM.def
libgm2_la_CFLAGS = -I. -I$(GM2_SRC)/gm2-libs -I$(GM2_SRC)/gm2-libs-iso -DBUILD_GM2_LIBS -I@srcdir@/../
libgm2_la_M2FLAGS = -I. -I$(GM2_SRC)/gm2-libs -I$(GM2_SRC)/gm2-libs-iso
libgm2_la_LINK = $(LINK)
BUILT_SOURCES = SYSTEM.def
CLEANFILES = SYSTEM.def

M2LIBDIR = /m2/pim/

SYSTEM.def: Makefile
	bash $(GM2_SRC)/tools-src/makeSystem -fpim \
             $(GM2_SRC)/gm2-libs/SYSTEM.def \
             $(GM2_SRC)/gm2-libs/SYSTEM.mod \
             -I$(GM2_SRC)/gm2-libs \
             "$(GM2_FOR_TARGET)" $@

.mod.lo:
	$(LIBTOOL) --tag=CC $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=compile $(GM2_FOR_TARGET) -c $(CFLAGS_FOR_TARGET) $(LIBCFLAGS) $(libgm2_la_M2FLAGS) $< -o $@

install-data-local: force
	mkdir -p $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)
	$(INSTALL_DATA) .libs/libgm2.la $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)
	chmod 644 $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)libgm2.la
	$(INSTALL_DATA) .libs/libgm2.a $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)
	$(RANLIB) $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)libgm2.a
	for i in $(M2DEFS) $(M2MODS) ; do \
           if [ -f $$i ] ; then \
              $(INSTALL_DATA) $$i '$(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)'; \
           elif [ -f @srcdir@/../../gcc/gm2/gm2-libs/$$i ] ; then \
              $(INSTALL_DATA) @srcdir@/../../gcc/gm2/gm2-libs/$$i '$(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)'; \
           else \
              echo "cannot find $$i" ; exit 1 ; \
           fi ; \
           chmod 644 $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)$$i ; \
	done

force:

# endif
