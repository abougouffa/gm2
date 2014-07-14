
SUFFIXES = .c .mod .def .o .obj .lo .a

ACLOCAL_AMFLAGS = -I . -I .. -I ../config

VPATH = . @srcdir@/../../gcc/gm2/gm2-libs-min

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

M2DEFS = libc.def  M2RTS.def \
         SYSTEM.def

M2MODS = M2RTS.mod  SYSTEM.mod

libmindir = libmin
toolexeclib_LTLIBRARIES = libmin.la
libmin_la_SOURCES = $(M2MODS) libc.c
libmin_la_DEPENDENCIES = SYSTEM.def
libmin_la_CFLAGS = -I. -I$(GM2_SRC)/gm2-libs-min -I$(GM2_SRC)/gm2-libs
libmin_la_M2FLAGS = -I. -I$(GM2_SRC)/gm2-libs-min -I$(GM2_SRC)/gm2-libs
BUILT_SOURCES = SYSTEM.def
CLEANFILES = SYSTEM.def

M2LIBDIR = /m2/min/

.mod.lo:
	../libtool --tag=CC $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=compile $(GM2_FOR_TARGET) -c $(CFLAGS_FOR_TARGET) $(LIBCFLAGS) $(libmin_la_M2FLAGS) $< -o $@

libc.o: $(GM2_SRC)/gm2-libs-min/libc.c


SYSTEM.def: Makefile
	echo "CC = $(CC_FOR_BUILD)  CC_FOR_TARGET = $(CC_FOR_TARGET)  GM2 = $(GM2)  GM2_FOR_TARGET = $(GM2_FOR_TARGET) GM2_FOR_BUILD = $(GM2_FOR_BUILD)"
	bash $(GM2_SRC)/tools-src/makeSystem -fpim \
             $(GM2_SRC)/gm2-libs-min/SYSTEM.def \
             $(GM2_SRC)/gm2-libs-min/SYSTEM.mod \
             -I$(GM2_SRC)/gm2-libs-min:$(GM2_SRC)/gm2-libs \
             "$(GM2_FOR_TARGET)" $@

install-data-local: force
	mkdir -p $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)
	$(INSTALL_DATA) .libs/libmin.a $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)
	chmod 644 $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)libmin.a
	$(RANLIB) $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)libmin.a
	for i in $(M2DEFS) $(M2MODS) ; do \
           if [ -f $$i ] ; then \
              $(INSTALL_DATA) $$i '$(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)'; \
           elif [ -f @srcdir@/../../gcc/gm2/gm2-libs-min/$$i ] ; then \
              $(INSTALL_DATA) @srcdir@/../../gcc/gm2/gm2-libs-min/$$i '$(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)'; \
           else \
              echo "cannot find $$i" ; exit 1 ; \
           fi ; \
           chmod 644 $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)$$i ; \
	done

force:
