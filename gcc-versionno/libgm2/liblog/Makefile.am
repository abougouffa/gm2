
SUFFIXES = .c .mod .def .o .obj .lo .a .la

ACLOCAL_AMFLAGS = -I . -I .. -I ../config

VPATH = . @srcdir@ @srcdir@/../../gcc/gm2/gm2-libs-pim

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

MULTIOSSUBDIR := $(shell if test x$(MULTIOSDIR) != x.; then echo /$(MULTIOSDIR); fi)
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


# if BUILD_LOGLIB
M2DEFS = BitBlockOps.def  BitByteOps.def \
         BitWordOps.def  BlockOps.def \
         Break.def  CardinalIO.def \
         Conversions.def  DebugPMD.def \
         DebugTrace.def  Delay.def \
         Display.def  ErrorCode.def \
         FileSystem.def  FloatingUtilities.def \
         InOut.def  Keyboard.def \
         LongIO.def  NumberConversion.def \
         Random.def  RealConversions.def \
         RealInOut.def  Strings.def \
         Termbase.def  Terminal.def \
         TimeDate.def

M2MODS = BitBlockOps.mod  BitByteOps.mod \
         BitWordOps.mod  BlockOps.mod \
         CardinalIO.mod  Conversions.mod \
         DebugPMD.mod  DebugTrace.mod \
         Delay.mod  Display.mod \
         ErrorCode.mod  FileSystem.mod \
         FloatingUtilities.mod  InOut.mod \
         Keyboard.mod  LongIO.mod \
         NumberConversion.mod  Random.mod \
         RealConversions.mod  RealInOut.mod \
         Strings.mod  Termbase.mod \
         Terminal.mod  TimeDate.mod


liblogdir = liblog
toolexeclib_LTLIBRARIES = liblog.la
liblog_la_SOURCES = $(M2MODS) Break.c

liblog_la_LINK = $(LINK)
liblog_la_DEPENDENCIES = ../libpim/SYSTEM.def
liblog_la_CFLAGS = -I. -DBUILD_GM2_LIBS -I@srcdir@/../
liblog_la_M2FLAGS = -I../libpim -I$(GM2_SRC)/gm2-libs-pim -I$(GM2_SRC)/gm2-libs -I$(GM2_SRC)/gm2-libs-iso

M2LIBDIR = /m2/log/

../libpim/SYSTEM.def: ../libpim/Makefile
	cd ../libpim ; $(MAKE) $(AM_MAKEFLAGS) SYSTEM.def

.mod.lo:
	$(LIBTOOL) --tag=CC $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=compile $(GM2_FOR_TARGET) -c $(CFLAGS_FOR_TARGET) $(LIBCFLAGS) $(liblog_la_M2FLAGS) $< -o $@

install-data-local: force
	mkdir -p $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)
	$(INSTALL_DATA) .libs/liblog.la $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)
	chmod 644 $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)liblog.la
	$(INSTALL_DATA) .libs/liblog.a $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)
	chmod 644 $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)liblog.a
	$(RANLIB) $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)liblog.a
	for i in $(M2DEFS) $(M2MODS) ; do \
           if [ -f $$i ] ; then \
              $(INSTALL_DATA) $$i '$(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)'; \
           elif [ -f @srcdir@/../../gcc/gm2/gm2-libs-pim/$$i ] ; then \
              $(INSTALL_DATA) @srcdir@/../../gcc/gm2/gm2-libs-pim/$$i '$(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)'; \
           else \
              echo "cannot find $$i" ; exit 1 ; \
           fi ; \
           chmod 644 $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)$$i ; \
	done

force:

# endif
