# Configure script for libgm2.
#   Copyright 2013 Free Software Foundation, Inc.
#
# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# Process this file with autoreconf to produce a configure script.

# AC_PREREQ(2.64)
AC_INIT(package-unused, version-unused,,libgm2)
AC_CONFIG_MACRO_DIR([config])
AC_CONFIG_HEADER(config.h)

m4_include([libpth/pth/pth.m4])
m4_include([libpth/pth/aclocal.m4])

libtool_VERSION=14:0:0
AC_SUBST(libtool_VERSION)

GCC_NO_EXECUTABLES

# Gets build, host, target, *_vendor, *_cpu, *_os, etc.
#
# You will slowly go insane if you do not grok the following fact:  when
# building this library, the top-level /target/ becomes the library's /host/.
#
# configure then causes --target to default to --host, exactly like any
# other package using autoconf.  Therefore, 'target' and 'host' will
# always be the same.  This makes sense both for native and cross compilers
# just think about it for a little while.  :-)
#
# Also, if this library is being configured as part of a cross compiler, the
# top-level configure script will pass the "real" host as $with_cross_host.
#
# Do not delete or change the following two lines.  For why, see
# http://gcc.gnu.org/ml/libstdc++/2003-07/msg00451.html

AC_CANONICAL_SYSTEM
ACX_NONCANONICAL_TARGET

AM_ENABLE_MULTILIB(, ..)

target_alias=${target_alias-$host_alias}
AC_SUBST(target_alias)

AM_INIT_AUTOMAKE([1.9.3 foreign no-dist -Wall -Wno-portability])

AH_TEMPLATE(PACKAGE, [Name of package])
AH_TEMPLATE(VERSION, [Version number of package])

AC_ARG_WITH(cross-host,
[  --with-cross-host=HOST           Configuring with a cross compiler])

# Checks for header files.
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADER([math.h],
	       [AC_DEFINE([HAVE_MATH_H], [1], [have math.h])])

AC_CHECK_HEADERS(limits.h stddef.h string.h strings.h stdlib.h \
                 time.h \
		 fcntl.h unistd.h sys/file.h sys/time.h sys/mman.h \
		 sys/resource.h sys/param.h sys/times.h sys/stat.h \
                 sys/socket.h \
                 sys/wait.h sys/ioctl.h errno.h sys/errno.h \
		 pwd.h direct.h dirent.h signal.h malloc.h langinfo.h \
                 pthread.h stdarg.h stdio.h sys/types.h termios.h \
                 netinet/in.h netdb.h sys/uio.h sys/stat.h wchar.h)


AC_CANONICAL_HOST
ACX_NONCANONICAL_HOST
ACX_NONCANONICAL_TARGET
GCC_TOPLEV_SUBDIRS

#
#  by default the local libpth is built if a system libpth in not found.
#  However the user can override this using --enable-libpth-m2 or
#  --disable-libpth-m2.
#

AC_MSG_CHECKING([for --enable-libpth-m2])
AC_ARG_ENABLE(libpth-m2,
[  --enable-libpth-m2    Specify that libpth should be built and installed],
[case "$enableval" in
 yes) libpth_m2=yes;;
 no)  libpth_m2=no;;
 *)   AC_MSG_ERROR([Unknown argument to enable/disable building and installing libpth]);;
 esac],
[libpth_m2=check])
AC_MSG_RESULT($libpth_m2)

AC_MSG_CHECKING([for --enable-version-specific-runtime-libs])
AC_ARG_ENABLE(version-specific-runtime-libs,
[  --enable-version-specific-runtime-libs    Specify that runtime libraries should be installed in a compiler-specific directory ],
[case "$enableval" in
 yes) version_specific_libs=yes ;;
 no)  version_specific_libs=no ;;
 *)   AC_MSG_ERROR([Unknown argument to enable/disable version-specific libs]);;
 esac],
[version_specific_libs=no])
AC_MSG_RESULT($version_specific_libs)

AC_ARG_WITH(slibdir,
[  --with-slibdir=DIR      shared libraries in DIR [LIBDIR]],
slibdir="$with_slibdir",
if test "${version_specific_libs}" = yes; then
  slibdir='$(libsubdir)'
elif test -n "$with_cross_host" && test x"$with_cross_host" != x"no"; then
  slibdir='$(exec_prefix)/$(host_noncanonical)/lib'
else
  slibdir='$(libdir)'
fi)
AC_SUBST(slibdir)

# Command-line options.
# Very limited version of AC_MAINTAINER_MODE.
AC_ARG_ENABLE([maintainer-mode],
  [AC_HELP_STRING([--enable-maintainer-mode],
                 [enable make rules and dependencies not useful (and
                  sometimes confusing) to the casual installer])],
  [case ${enable_maintainer_mode} in
     yes) MAINT='' ;;
     no) MAINT='#' ;;
     *) AC_MSG_ERROR([--enable-maintainer-mode must be yes or no]) ;;
   esac
   maintainer_mode=${enableval}],
  [MAINT='#'])
AC_SUBST([MAINT])dnl

toolexecdir=no
toolexeclibdir=no

# Calculate toolexeclibdir
# Also toolexecdir, though it's only used in toolexeclibdir
case ${version_specific_libs} in
  yes)
    # Need the gcc compiler version to know where to install libraries
    # and header files if --enable-version-specific-runtime-libs option
    # is selected.
    toolexecdir='$(libdir)/gcc/$(target_noncanonical)'
    toolexeclibdir='$(toolexecdir)/$(gcc_version)$(MULTISUBDIR)'
    ;;
  no)
    if test -n "$with_cross_host" &&
       test x"$with_cross_host" != x"no"; then
      # Install a library built with a cross compiler in tooldir, not libdir.
      toolexecdir='$(exec_prefix)/$(target_noncanonical)'
      toolexeclibdir='$(toolexecdir)/lib'
    else
      toolexecdir='$(libdir)/gcc-lib/$(target_noncanonical)'
      toolexeclibdir='$(libdir)'
    fi
    multi_os_directory=`$CC -print-multi-os-directory`
    case $multi_os_directory in
      .) ;; # Avoid trailing /.
      *) toolexeclibdir=$toolexeclibdir/$multi_os_directory ;;
    esac
    ;;
esac

AC_SUBST(toolexecdir)
AC_SUBST(toolexeclibdir)

AH_TEMPLATE(PACKAGE, [Name of package])
AH_TEMPLATE(VERSION, [Version number of package])

AM_MAINTAINER_MODE

# Check the compiler.
# The same as in boehm-gc and libstdc++. Have to borrow it from there.
# We must force CC to /not/ be precious variables; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS ourselves.

m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
AC_PROG_CXX
AM_PROG_AS
m4_rename_force([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AC_SUBST(CFLAGS)

# In order to override CFLAGS_FOR_TARGET, all of our special flags go
# in XCFLAGS.  But we need them in CFLAGS during configury.  So put them
# in both places for now and restore CFLAGS at the end of config.
save_CFLAGS="$CFLAGS"

# Find other programs we need.
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(NM, nm)
AC_CHECK_TOOL(RANLIB, ranlib, ranlib-not-found-in-path-error)
AC_PATH_PROG(PERL, perl, perl-not-found-in-path-error)
AC_PROG_MAKE_SET
AC_PROG_INSTALL

AM_PROG_LIBTOOL
AC_SUBST(enable_shared)
AC_SUBST(enable_static)

AC_CHECK_TYPES([struct timezone, struct stat, struct timeval])

AC_LANG_C
# Check the compiler.
# The same as in boehm-gc and libstdc++. Have to borrow it from there.
# We must force CC to /not/ be precious variables; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS ourselves.

m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
m4_rename_force([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AC_SUBST(CFLAGS)

AC_DEFUN([GM2_UNDEF],[
  $as_echo "#undef HAVE_$1" >>confdefs.h
])

AC_DEFUN([GM2_CHECK_LIB],[
  AC_MSG_CHECKING([m2 front end checking $1 library for $2])
  if test x$gcc_no_link != xyes; then
    AC_CHECK_LIB([$1],[$2],[AC_DEFINE([HAVE_$3],[1],[found $2])],[GM2_UNDEF([$3],[$2])])
  else
    if test "x$[ac_cv_lib_$1_$2]" = xyes; then
      AC_DEFINE([HAVE_$3],[1],[lib$1 includes $2])
    elif test "x$[ac_cv_func_$2]" = xyes; then
      AC_DEFINE([HAVE_$3],[1],[function $2 exists])
    else
      GM2_UNDEF([$3],[$2])
    fi
  fi
])

GM2_CHECK_LIB([c],[access],[ACCESS])
GM2_CHECK_LIB([c],[brk],[BRK])
GM2_CHECK_LIB([c],[cfmakeraw],[CFMAKERAW])
GM2_CHECK_LIB([c],[close],[CLOSE])
GM2_CHECK_LIB([c],[ctime],[CTIME])
GM2_CHECK_LIB([c],[creat],[CREAT])
GM2_CHECK_LIB([c],[dup],[DUP])
GM2_CHECK_LIB([c],[execve],[EXECVE])
GM2_CHECK_LIB([c],[exit],[EXIT])
GM2_CHECK_LIB([c],[fcntl],[FCNTL])
GM2_CHECK_LIB([c],[fstat],[FSTAT])
GM2_CHECK_LIB([c],[getdents],[GETDENTS])
GM2_CHECK_LIB([c],[getgid],[GETGID])
GM2_CHECK_LIB([c],[getpid],[GETPID])
GM2_CHECK_LIB([c],[gettimeofday],[GETTIMEOFD])
GM2_CHECK_LIB([c],[getuid],[GETUID])
GM2_CHECK_LIB([c],[ioctl],[IOCTL])
GM2_CHECK_LIB([c],[kill],[KILL])
GM2_CHECK_LIB([c],[link],[LINK])
GM2_CHECK_LIB([c],[lseek],[LSEEK])
GM2_CHECK_LIB([c],[open],[OPEN])
GM2_CHECK_LIB([c],[pause],[PAUSE])
GM2_CHECK_LIB([c],[pipe],[PIPE])
GM2_CHECK_LIB([c],[rand],[RAND])
GM2_CHECK_LIB([c],[read],[READ])
GM2_CHECK_LIB([c],[select],[SELECT])
GM2_CHECK_LIB([c],[setitimer],[SETITIMER])
GM2_CHECK_LIB([c],[setgid],[SETGID])
GM2_CHECK_LIB([c],[setuid],[SETUID])
GM2_CHECK_LIB([c],[stat],[STAT])
GM2_CHECK_LIB([c],[strsignal],[STRSIGNAL])
GM2_CHECK_LIB([c],[strtod],[STRTOD])
GM2_CHECK_LIB([c],[strtold],[STRTOLD])
GM2_CHECK_LIB([c],[times],[TIMES])
GM2_CHECK_LIB([c],[unlink],[UNLINK])
GM2_CHECK_LIB([c],[wait],[WAIT])
GM2_CHECK_LIB([c],[write],[WRITE])

GM2_CHECK_LIB([m],[signbit],[SIGNBIT])
GM2_CHECK_LIB([m],[signbitf],[SIGNBITF])
GM2_CHECK_LIB([m],[signbitl],[SIGNBITL])


CC_FOR_BUILD=${CC_FOR_BUILD:-gcc}
AC_SUBST(CC_FOR_BUILD)

AC_MSG_NOTICE([checking for system installed libpth])
pth_found=1
AC_CHECK_LIB(pth, pselect, [], [pth_found=0])
if test $libpth_m2 = no ; then
   AC_MSG_NOTICE([user has disabled building local libpth])
   BUILD_PTHLIB=false
elif test $libpth_m2 = yes ; then
   AC_MSG_NOTICE([user has enabled building local libpth])
   BUILD_PTHLIB=true
else
   AC_MSG_NOTICE([user has not specified if libpth should be built, will see if the system needs one])
   if test $pth_found = 0 ; then
      AC_MSG_NOTICE([no libpth installed, so we will build libpth for m2])
      BUILD_PTHLIB=true
   else
      AC_MSG_NOTICE([system libpth is installed, so no need to build libpth for m2])
      BUILD_PTHLIB=false
   fi
fi

BUILD_PIMLIB=true
BUILD_ISOLIB=true
BUILD_CORLIB=true
BUILD_LOGLIB=true

case "$target" in

  avr25*-*-* | avr31*-*-* | avr35*-*-* | avr4*-*-* | avr5*-*-* | avr51*-*-* | avr6*-*-*)
  BUILD_PIMLIB=false
  BUILD_ISOLIB=false
  BUILD_CORLIB=false
  BUILD_LOGLIB=false
  BUILD_PTHLIB=false
  ;;

  avrxmega2*-*-* | avrxmega4*-*-* | avrxmega5*-*-* | avrxmega6*-*-* | avrxmega7*-*-*)
  BUILD_PIMLIB=false
  BUILD_ISOLIB=false
  BUILD_CORLIB=false
  BUILD_LOGLIB=false
  BUILD_PTHLIB=false
  ;;

  avr3-*-*)
  BUILD_PIMLIB=true
  BUILD_ISOLIB=true
  BUILD_CORLIB=true
  BUILD_LOGLIB=true
  BUILD_PTHLIB=false
  ;;

esac

BUILD_PIMLIB=true
BUILD_ISOLIB=true
BUILD_CORLIB=true
BUILD_LOGLIB=true
BUILD_PTHDUMMY=false

if test $pth_found = 0 ; then
   if test $BUILD_PTHLIB = false ; then
      AC_MSG_NOTICE([no system libpth found and libpth has been disabled, will build and install dummypth.])
      BUILD_PTHDUMMY=true
   fi
fi

AM_CONDITIONAL([BUILD_PIMLIB], [test x$BUILD_PIMLIB = xtrue])
AM_CONDITIONAL([BUILD_ISOLIB], [test x$BUILD_ISOLIB = xtrue])
AM_CONDITIONAL([BUILD_CORLIB], [test x$BUILD_CORLIB = xtrue])
AM_CONDITIONAL([BUILD_LOGLIB], [test x$BUILD_LOGLIB = xtrue])
AM_CONDITIONAL([BUILD_PTHLIB], [test x$BUILD_PTHLIB = xtrue])
AM_CONDITIONAL([BUILD_PTHDUMMY], [test x$BUILD_PTHDUMMY = xtrue])


#
#  Now for the GNU Pth configuration
#

AC_HEADLINE(GNU Pth, Portable Threads, PTH_VERSION, pth_vers.c,
[Copyright (c) 1999-2006 Ralf S. Engelschall <rse@engelschall.com>])

# check whether we've to use a non-standard #include <sys/select.h>  to get
# the definition for fd_set under AIX and other platforms.

AC_HAVE_HEADERS(sys/select.h)
EXTRA_INCLUDE_SYS_SELECT_H="#include <sys/select.h>"
if test ".$ac_cv_header_sys_select_h" != .yes; then
    EXTRA_INCLUDE_SYS_SELECT_H="/* $EXTRA_INCLUDE_SYS_SELECT_H */"
fi
AC_SUBST(EXTRA_INCLUDE_SYS_SELECT_H)

# check whether we've to define sig_atomic_t
AC_CHECK_TYPEDEF(sig_atomic_t, signal.h)
FALLBACK_SIG_ATOMIC_T="typedef int sig_atomic_t;"
if test ".$ac_cv_typedef_sig_atomic_t" = .yes; then
    FALLBACK_SIG_ATOMIC_T="/* $FALLBACK_SIG_ATOMIC_T */"
fi
AC_SUBST(FALLBACK_SIG_ATOMIC_T)

# check whether we've to define pid_t
AC_CHECK_TYPEDEF(pid_t, sys/types.h)
FALLBACK_PID_T="typedef int pid_t;"
if test ".$ac_cv_typedef_pid_t" = .yes; then
    FALLBACK_PID_T="/* $FALLBACK_PID_T */"
fi
AC_SUBST(FALLBACK_PID_T)

FALLBACK_SIZE_T="/* size_t is an internal.  */"
AC_SUBST(FALLBACK_SIZE_T)

# check whether we've to define ssize_t
AC_CHECK_TYPEDEF(ssize_t, sys/types.h)
FALLBACK_SSIZE_T="typedef unsigned int ssize_t;"
if test ".$ac_cv_typedef_ssize_t" = .yes; then
    FALLBACK_SSIZE_T="/* $FALLBACK_SSIZE_T */"
fi
AC_SUBST(FALLBACK_SSIZE_T)

# check whether we've to define off_t
AC_CHECK_TYPEDEF(off_t, sys/types.h)
FALLBACK_OFF_T="typedef int off_t;"
if test ".$ac_cv_typedef_off_t" = .yes; then
    FALLBACK_OFF_T="/* $FALLBACK_OFF_T */"
fi
AC_SUBST(FALLBACK_OFF_T)

# check for network/socket size type
AC_CHECK_SOCKLENTYPE([
FALLBACK_SOCKLEN_T="typedef ${ac_type} socklen_t;"
if test ".$ac_type" = ".socklen_t"; then
    FALLBACK_SOCKLEN_T="/* $FALLBACK_SOCKLEN_T */"
fi
AC_SUBST(FALLBACK_SOCKLEN_T)
])

# check for struct timespec
AC_MSG_CHECKING(for struct timespec)
AC_TRY_COMPILE([
#include <time.h>
#include <sys/time.h>
],[
struct timespec ts;
],
msg="yes"
,
msg="no"
)
if test ".$msg" = .yes; then
    AC_DEFINE(HAVE_STRUCT_TIMESPEC, 1, [define if exists "struct timespec"])
fi

# check for filedescriptor number type
AC_CHECK_NFDSTYPE([
FALLBACK_NFDS_T="typedef ${ac_type} nfds_t;"
if test ".$ac_type" = ".nfds_t"; then
    FALLBACK_NFDS_T="/* $FALLBACK_NFDS_T */"
fi
AC_SUBST(FALLBACK_NFDS_T)
])

# check whether poll(2)'s input stuff has to be faked
AC_CHECK_FUNCS(poll)
AC_CHECK_DEFINE(POLLIN, poll.h)
AC_MSG_CHECKING(whether poll(2) facility has to be faked)
AC_IFALLYES(func:poll define:POLLIN, PTH_FAKE_POLL=0, PTH_FAKE_POLL=1)
if test .$PTH_FAKE_POLL = .1; then
    msg="yes"
else
    msg="no"
fi
AC_SUBST(PTH_FAKE_POLL)

# check for readv/writev environment
AC_HAVE_HEADERS(sys/uio.h)
AC_CHECK_FUNCS(readv writev)
AC_MSG_CHECKING(whether readv(2)/writev(2) facility has to be faked)
AC_IFALLYES(func:readv func:writev header:sys/uio.h, PTH_FAKE_RWV=0, PTH_FAKE_RWV=1)
if test .$PTH_FAKE_RWV = .1; then
    msg="yes"
else
    msg="no"
fi
AC_SUBST(PTH_FAKE_RWV)

#   whether to build against Sfio library

PTH_EXT_SFIO=0
AC_CHECK_EXTLIB(Sfio, sfio, sfdisc, sfio.h, PTH_EXT_SFIO=1)
AC_SUBST(PTH_EXT_SFIO)

#
#  MACHINE CONTEXT IMPLEMENTATION
#

AC_MSG_PART(Machine Context Implementation)

#
#  1. determine possibilities
#

#  check for MCSC method

AC_CHECK_HEADER(ucontext.h,,, [#include <sys/types.h>])
AC_CHECK_FUNCS(makecontext swapcontext getcontext setcontext)
AC_CHECK_MCSC(mcsc=yes, mcsc=no)

#  check for SJLJ method

AC_CHECK_HEADERS(signal.h)
AC_CHECK_FUNCS(sigsetjmp siglongjmp setjmp longjmp _setjmp _longjmp)
AC_CHECK_FUNCS(sigaltstack sigstack)
AC_CHECK_SJLJ(sjlj=yes, sjlj=no, sjlj_type)

#
#  2. make a general decision
#

if test ".$mcsc" = .yes; then
    mctx_mth=mcsc
    mctx_dsp=sc
    mctx_stk=mc
elif test ".$sjlj" = .yes; then
    mctx_mth=sjlj
    mctx_dsp=$sjlj_type
    mctx_stk=none
    AC_IFALLYES(func:sigstack,    mctx_stk=ss)
    AC_IFALLYES(func:sigaltstack, mctx_stk=sas)
    case $mctx_dsp in
         sjljlx|sjljisc|sjljw32 ) mctx_stk=none
    esac
else
    AC_ERROR([no appropriate mctx method found])
fi

#
#  3. allow decision to be overridden by user
#

AC_ARG_WITH(mctx-mth,dnl
[  --with-mctx-mth=ID      force mctx method      (mcsc,sjlj)],[
case $withval in
    mcsc|sjlj ) mctx_mth=$withval ;;
    * ) AC_ERROR([invalid mctx method -- allowed: mcsc,sjlj]) ;;
esac
])dnl
AC_ARG_WITH(mctx-dsp,dnl
[  --with-mctx-dsp=ID      force mctx dispatching (sc,ssjlj,sjlj,usjlj,sjlje,...)],[
case $withval in
    sc|ssjlj|sjlj|usjlj|sjlje|sjljlx|sjljisc|sjljw32 ) mctx_dsp=$withval ;;
    * ) AC_ERROR([invalid mctx dispatching -- allowed: sc,ssjlj,sjlj,usjlj,sjlje,sjljlx,sjljisc,sjljw32]) ;;
esac
])dnl
AC_ARG_WITH(mctx-stk,dnl
[  --with-mctx-stk=ID      force mctx stack setup (mc,ss,sas,...)],[
case $withval in
    mc|ss|sas|none ) mctx_stk=$withval ;;
    * ) AC_ERROR([invalid mctx stack setup -- allowed: mc,ss,sas,none]) ;;
esac
])dnl

#
#  4. determine a few additional details
#

#  whether sigaltstack has to use stack_t instead of struct sigaltstack

AC_CHECK_TYPEDEF(stack_t, signal.h)

dnl #  direction of stack grow
AC_CHECK_STACKGROWTH(PTH_STACKGROWTH)
if test ".$ac_cv_check_stackgrowth" = ".down"; then
    PTH_STACK_GROWTH="down"
else
    PTH_STACK_GROWTH="up"
fi
AC_SUBST(PTH_STACK_GROWTH)

#  GNU Hurd has a broken libc which has stubs for sigaltstack and
#  sigstack. When the following Autoconf checks for them are run, it
#  even crashes the system. So we have to disable these checks here.

case $PLATFORM in
    *86-*-gnu0* ) ac_cv_func_sigaltstack="no"; ac_cv_func_sigstack="no" ;;
esac

#  how to specify stacks for the various functions

AC_CHECK_STACKSETUP(makecontext, pth_skaddr_makecontext, pth_sksize_makecontext)
AC_CHECK_STACKSETUP(sigaltstack, pth_skaddr_sigaltstack, pth_sksize_sigaltstack)
AC_CHECK_STACKSETUP(sigstack,    pth_skaddr_sigstack,    pth_sksize_sigstack)

#  how to implement POSIX compliant sig{set,long}jmp()

case $mctx_dsp in [
    ssjlj )
        pth_sigjmpbuf='sigjmp_buf'
        pth_sigsetjmp='sigsetjmp(buf,1)'
        pth_siglongjmp='siglongjmp(buf,val)'
        ;;
    sjlj )
        pth_sigjmpbuf='jmp_buf'
        pth_sigsetjmp='setjmp(buf)'
        pth_siglongjmp='longjmp(buf,val)'
        ;;
    usjlj )
        pth_sigjmpbuf='jmp_buf'
        pth_sigsetjmp='_setjmp(buf)'
        pth_siglongjmp='_longjmp(buf,val)'
        ;;
    sjlje )
        pth_sigjmpbuf='jmp_buf'
        pth_sigsetjmp='setjmp(buf)'
        pth_siglongjmp='longjmp(buf,val)'
        ;;
    sjljlx|sjljisc|sjljw32 )
        pth_sigjmpbuf='sigjmp_buf'
        pth_sigsetjmp='sigsetjmp(buf,1)'
        pth_siglongjmp='siglongjmp(buf,val)'
        ;;
] esac
pth_sigjmpbuf="#define pth_sigjmpbuf $pth_sigjmpbuf"
pth_sigsetjmp="#define pth_sigsetjmp(buf) $pth_sigsetjmp"
pth_siglongjmp="#define pth_siglongjmp(buf,val) $pth_siglongjmp"
AC_SUBST(pth_sigjmpbuf)
AC_SUBST(pth_sigsetjmp)
AC_SUBST(pth_siglongjmp)

#
#  5. export the results
#

AC_DEFINE_UNQUOTED(PTH_MCTX_MTH_use, [PTH_MCTX_MTH_$mctx_mth], [define for machine context method])
AC_DEFINE_UNQUOTED(PTH_MCTX_DSP_use, [PTH_MCTX_DSP_$mctx_dsp], [define for machine context dispatching])
AC_DEFINE_UNQUOTED(PTH_MCTX_STK_use, [PTH_MCTX_STK_$mctx_stk], [define for machine context stack])

PTH_MCTX_ID="$mctx_mth/$mctx_dsp/$mctx_stk"
AC_MSG_RESULT([decision on mctx implementation... ${TB}${PTH_MCTX_ID}${TN}])
AC_SUBST(PTH_MCTX_ID)

#  check for the number of signals

AC_CHECK_NSIG(PTH_NSIG)

AC_CHECK_FUNCS(strerror)

AC_SUBST(PTH_SYSCALL_HARD,"0")
AC_SUBST(PTH_SYSCALL_SOFT,"0")

################### end of pth ###################

AC_CONFIG_SRCDIR([Makefile.am])
AC_CONFIG_FILES([Makefile libmin/Makefile libpim/Makefile libiso/Makefile
                 libcor/Makefile liblog/Makefile libpth/Makefile
		 libpth/pth/Makefile libpth/pth/pth.h
		 libpth/pth/pthread.h libpth/pth/pth_acmac.h])

AC_MSG_NOTICE([libgm2 has been configured.])

AC_OUTPUT
