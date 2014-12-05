
SUFFIXES = .c .mod .def .o .obj .lo .a .la

ACLOCAL_AMFLAGS = -I . -I .. -I ../config

VPATH = . @srcdir@ @srcdir@/../../gcc/gm2/ulm-lib-gm2/sys @srcdir@/../../gcc/gm2/ulm-lib-gm2/std

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


# if BUILD_ULMLIB
M2DEFS = Errno.def  SysAccess.def \
         SysAlarm.def  SysBreak.def \
         SysClose.def  SysCreat.def \
         Sys.def  SysDup.def \
         SysExec.def  SysExit.def \
         SysFcntl.def  SysFork.def \
         SysGetpid.def  SysGetuid.def \
         SysIoctl.def  SysKill.def \
         SysLink.def  SysLocations.def \
         SysLseek.def  SysOpen.def \
         SysPanic.def  SysPause.def \
         SysPipe.def  SysRead.def \
         SysSetuid.def  SysSignal.def \
         SysStat.def  SYSTEM.def \
         SystemTypes.def  SysTermIO.def \
         SysTime.def  SysUnlink.def \
         SysWait.def  SysWrite.def \
         UnixString.def  Archive.def \
         Arguments.def  ASCII.def \
         Calendar.def  CallShell.def \
         Clock.def  Conversions.def \
         Directories.def  Environment.def \
         EtcGroup.def  Files.def \
         FtdIO.def  Functions.def \
         GetPass.def  InOut.def \
         MathLib.def  Passwd.def \
         PipeIO.def  Plot.def \
         RandomGenerator.def  ReadIntCard.def \
         RealConv.def  RealInOut.def \
         RTErrors.def  ScanPwfile.def \
         StdFuncs.def  StdIO.def \
         Storage.def  Strings.def \
         StrSpec.def  StrToNum.def \
         StrToReal.def  SysConf.def \
         SysPerror.def  Terminal.def \
         TimeIO.def

M2MODS = Errno.mod  SysAccess.mod \
         SysAlarm.mod  SysBreak.mod \
         SysClose.mod  SysCreat.mod \
         SysDup.mod  SysExec.mod \
         SysExit.mod  SysFcntl.mod \
         SysFork.mod  SysGetpid.mod \
         SysGetuid.mod  SysIoctl.mod \
         SysKill.mod  SysLink.mod \
         SysLocations.mod  SysLseek.mod \
         Sys.mod  SysOpen.mod \
         SysPanic.mod  SysPause.mod \
         SysPipe.mod  SysRead.mod \
         SysSetuid.mod  SysSignal.mod \
         SysStat.mod  SystemTypes.mod \
         SysTermIO.mod  SysTime.mod \
         SysUnlink.mod  SysWait.mod \
         SysWrite.mod  UnixString.mod \
         Archive.mod  Arguments.mod \
         ASCII.mod  Calendar.mod \
         CallShell.mod  Clock.mod \
         Conversions.mod  Directories.mod \
         Environment.mod  EtcGroup.mod \
         Files.mod  FtdIO.mod \
         Functions.mod  GetPass.mod \
         InOut.mod  M2EXCEPTION.mod \
         M2RTS.mod  MathLib.mod \
         Passwd.mod  PipeIO.mod \
         Plot.mod  RandomGenerator.mod \
         ReadIntCard.mod  RealConv.mod \
         RealInOut.mod  RTErrors.mod \
         RTExceptions.mod  ScanPwfile.mod \
         StdFuncs.mod  StdIO.mod \
         Storage.mod  Strings.mod \
         StrSpec.mod  StrToNum.mod \
         StrToReal.mod  SysConf.mod \
         SysPerror.mod  Terminal.mod \
         TimeIO.mod

toolexeclib_LTLIBRARIES = libulm.la
libulm_la_SOURCES = $(M2MODS) SYSTEM.c

libulmdir = libulm
libulm_la_LINK = $(LINK)
libulm_la_DEPENDENCIES = SYSTEM.def
libulm_la_CFLAGS = -I. -DBUILD_GM2_LIBS -I@srcdir@/../
libulm_la_M2FLAGS = -I. -I$(GM2_SRC)/ulm-lib-gm2/sys -I$(GM2_SRC)/ulm-lib-gm2/std -I$(GM2_SRC)/gm2-libs
## nodist_EXTRA_libulm_la_SOURCES = dummy.c  ## forces automake to generate the LINK definition

BUILT_SOURCES = SYSTEM.c
CLEANFILES = SYSTEM.c

M2LIBDIR = /m2/ulm/

SYSTEM.c: Makefile
	bash $(GM2_SRC)/tools-src/createUlmSys ../config.h > $@

.mod.lo:
	../libtool --tag=CC $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=compile $(GM2_FOR_TARGET) -c $(CFLAGS_FOR_TARGET) $(LIBCFLAGS) $(libulm_la_M2FLAGS) $< -o $@

install-data-local: force
	mkdir -p $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)
	$(INSTALL_DATA) .libs/libulm.a $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)
	chmod 644 $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)libulm.a
	$(RANLIB) $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)libulm.a
	for i in $(M2DEFS) $(M2MODS) ; do \
           if [ -f $$i ] ; then \
              $(INSTALL_DATA) $$i '$(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)'; \
           elif [ -f @srcdir@/../../gcc/gm2/ulm-lib-gm2/sys/$$i ] ; then \
              $(INSTALL_DATA) @srcdir@/../../gcc/gm2/ulm-lib-gm2/sys/$$i '$(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)'; \
           elif [ -f @srcdir@/../../gcc/gm2/ulm-lib-gm2/std/$$i ] ; then \
              $(INSTALL_DATA) @srcdir@/../../gcc/gm2/ulm-lib-gm2/std/$$i '$(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)'; \
           else \
              echo "cannot find $$i" ; exit 1 ; \
           fi ; \
           chmod 644 $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)$$i ; \
	done

force:

# endif
