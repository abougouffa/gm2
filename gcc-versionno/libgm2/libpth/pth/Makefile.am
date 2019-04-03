
SUFFIXES = .c .o .obj .lo .a .la

ACLOCAL_AMFLAGS = -I . -I .. -I ../config

VPATH = . @srcdir@

# Multilib support.
MAKEOVERRIDES=

version := $(shell $(CC) -dumpversion)

# Directory in which the compiler finds libraries etc.
libsubdir = $(libdir)/gcc/$(host_noncanonical)/$(version)
# Used to install the shared libgcc.
slibdir = @slibdir@

# if BUILD_PTHLIB
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

M2LIBDIR = /m2/pth/

PTHSRC = pth_compat.c pth_debug.c pth_syscall.c pth_errno.c pth_ring.c pth_mctx.c \
         pth_uctx.c pth_clean.c pth_time.c pth_tcb.c pth_util.c pth_pqueue.c pth_event.c \
         pth_sched.c pth_data.c pth_msg.c pth_cancel.c pth_sync.c pth_attr.c pth_lib.c \
         pth_fork.c pth_high.c pth_ext.c pth_string.c pthread.c

toolexeclib_LTLIBRARIES = libpth.la
libpth_la_SOURCES = $(PTHSRC)

libpth_la_CFLAGS = -I. -I@srcdir@
libpth_la_LINK = $(LINK)
srcdir=@srcdir@
BUILT_SOURCES = pth_p.h

pth_p.h: force
	$(srcdir)/shtool scpp -o pth_p.h -t $(srcdir)/pth_p.h.in -Dcpp -Cintern -M '==#==' \
           $(srcdir)/pth_compat.c \
           $(srcdir)/pth_debug.c \
           $(srcdir)/pth_syscall.c \
           $(srcdir)/pth_errno.c \
           $(srcdir)/pth_ring.c \
           $(srcdir)/pth_mctx.c \
           $(srcdir)/pth_uctx.c \
           $(srcdir)/pth_clean.c \
           $(srcdir)/pth_time.c \
           $(srcdir)/pth_tcb.c \
           $(srcdir)/pth_util.c \
           $(srcdir)/pth_pqueue.c \
           $(srcdir)/pth_event.c \
           $(srcdir)/pth_sched.c \
           $(srcdir)/pth_data.c \
           $(srcdir)/pth_msg.c \
           $(srcdir)/pth_cancel.c \
           $(srcdir)/pth_sync.c \
           $(srcdir)/pth_attr.c \
           $(srcdir)/pth_lib.c \
           $(srcdir)/pth_fork.c \
           $(srcdir)/pth_high.c \
           $(srcdir)/pth_ext.c \
           $(srcdir)/pth_string.c \
           $(srcdir)/pthread.c

install-data-local: force
	mkdir -p $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)
	$(INSTALL_DATA) .libs/libpth.la $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)
	chmod 644 $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)libpth.la
	$(INSTALL_DATA) .libs/libpth.a $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)
	chmod 644 $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)/libpth.a
	$(RANLIB) $(DESTDIR)$(inst_libdir)/$(MULTIDIR)$(M2LIBDIR)/libpth.a

force:

# endif
