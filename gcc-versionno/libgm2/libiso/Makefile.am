
SUFFIXES = .c .mod .def .o .obj .lo .a .la

ACLOCAL_AMFLAGS = -I . -I .. -I ../config

VPATH = . @srcdir@ @srcdir@/../../gcc/gm2/gm2-libs-iso

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


# if BUILD_ISOLIB
M2DEFS = ChanConsts.def  CharClass.def \
         ClientSocket.def  ComplexMath.def \
         ConvStringLong.def  ConvStringReal.def \
         ConvTypes.def  COROUTINES.def \
         ErrnoCategory.def  EXCEPTIONS.def \
         GeneralUserExceptions.def  IOChan.def \
         IOConsts.def  IOLink.def \
         IOResult.def  LongComplexMath.def \
         LongConv.def  LongIO.def \
         LongMath.def  LongStr.def \
         LongWholeIO.def  LowLong.def \
         LowReal.def  LowShort.def \
         M2EXCEPTION.def  M2RTS.def \
         Processes.def  ProgramArgs.def \
         pth.def  RandomNumber.def \
         RawIO.def  RealConv.def \
         RealIO.def  RealMath.def \
         RealStr.def  RndFile.def \
         RTdata.def  RTentity.def \
         RTfio.def  RTgen.def \
         RTgenif.def  RTio.def \
         Semaphores.def  SeqFile.def \
         ServerSocket.def  ShortComplexMath.def \
         ShortIO.def  ShortWholeIO.def \
         SimpleCipher.def  SIOResult.def \
         SLongIO.def  SLongWholeIO.def \
         SRawIO.def  SRealIO.def \
         SShortIO.def  SShortWholeIO.def \
         StdChans.def  STextIO.def \
         Storage.def  StreamFile.def \
         StringChan.def  Strings.def \
         SWholeIO.def  SysClock.def \
         SYSTEM.def  TermFile.def \
         TERMINATION.def  TextIO.def \
         WholeConv.def  WholeIO.def \
         WholeStr.def  wrapsock.def \
         wraptime.def

M2MODS = ChanConsts.mod  CharClass.mod \
         ClientSocket.mod  ComplexMath.mod \
         ConvStringLong.mod  ConvStringReal.mod \
         ConvTypes.mod  COROUTINES.mod \
         EXCEPTIONS.mod  GeneralUserExceptions.mod \
         IOChan.mod  IOConsts.mod \
         IOLink.mod  IOResult.mod \
         LongComplexMath.mod  LongConv.mod \
         LongIO.mod  LongMath.mod \
         LongStr.mod  LongWholeIO.mod \
         LowLong.mod  LowReal.mod \
         LowShort.mod  M2EXCEPTION.mod \
         M2RTS.mod  Processes.mod \
         ProgramArgs.mod  RandomNumber.mod \
         RawIO.mod  RealConv.mod \
         RealIO.mod  RealMath.mod \
         RealStr.mod  RndFile.mod \
         RTdata.mod  RTentity.mod \
         RTfio.mod  RTgenif.mod \
         RTgen.mod  RTio.mod \
         Semaphores.mod  SeqFile.mod \
         ServerSocket.mod  ShortComplexMath.mod \
         ShortIO.mod  ShortWholeIO.mod \
         SimpleCipher.mod  SIOResult.mod \
         SLongIO.mod  SLongWholeIO.mod \
         SRawIO.mod  SRealIO.mod \
         SShortIO.mod  SShortWholeIO.mod \
         StdChans.mod  STextIO.mod \
         Storage.mod  StreamFile.mod \
         StringChan.mod  Strings.mod \
         SWholeIO.mod  SysClock.mod \
         SYSTEM.mod  TermFile.mod \
         TERMINATION.mod  TextIO.mod \
         WholeConv.mod  WholeIO.mod \
         WholeStr.mod

toolexeclib_LTLIBRARIES = libiso.la
libiso_la_SOURCES =  $(M2MODS) \
                     ErrnoCategory.c  wrapsock.c \
                     wraptime.c

libisodir = libiso
libiso_la_DEPENDENCIES = SYSTEM.def
libiso_la_CFLAGS = -I. -I$(GM2_SRC)/gm2-libs-iso -I$(GM2_SRC)/gm2-libs -DBUILD_GM2_LIBS -I@srcdir@/../
libiso_la_M2FLAGS = -I. -Ilibiso -I$(GM2_SRC)/gm2-libs-iso -I$(GM2_SRC)/gm2-libs -fiso -fextended-opaque
libiso_la_LINK = $(LINK)
CLEANFILES = SYSTEM.def
BUILT_SOURCES = SYSTEM.def

M2LIBDIR = /m2/iso/

SYSTEM.def: Makefile
	bash $(GM2_SRC)/tools-src/makeSystem -fiso \
             $(GM2_SRC)/gm2-libs-iso/SYSTEM.def \
             $(GM2_SRC)/gm2-libs-iso/SYSTEM.mod \
             -I$(GM2_SRC)/gm2-libs-iso:$(GM2_SRC)/gm2-libs \
             "$(GM2_FOR_TARGET)" $@

## add these to the .mod.o rule when optimization is fixed $(CFLAGS_FOR_TARGET) $(LIBCFLAGS)

.mod.lo:
	../libtool --tag=CC $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=compile $(GM2_FOR_TARGET) -c $(CFLAGS_FOR_TARGET) $(LIBCFLAGS) $(libiso_la_M2FLAGS) $< -o $@

install-data-local: force
	mkdir -p $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)
	$(INSTALL_DATA) .libs/libiso.la $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)
	chmod 644 $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)libiso.la
	$(INSTALL_DATA) .libs/libiso.a $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)
	chmod 644 $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)/libiso.a
	$(RANLIB) $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)/libiso.a
	for i in $(M2DEFS) $(M2MODS) ; do \
           if [ -f $$i ] ; then \
              $(INSTALL_DATA) $$i '$(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)'; \
           elif [ -f @srcdir@/../../gcc/gm2/gm2-libs-iso/$$i ] ; then \
              $(INSTALL_DATA) @srcdir@/../../gcc/gm2/gm2-libs-iso/$$i '$(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)'; \
           else \
              echo "cannot find $$i" ; exit 1 ; \
           fi ; \
           chmod 644 $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)$$i ; \
	done

force:

# endif

