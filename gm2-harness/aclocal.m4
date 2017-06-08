#
# set up the '--with-gcc=' command line option
#

AC_DEFUN([GM2_HARNESS_GCC_OPTION],
  [AC_ARG_WITH([gcc],
     [AS_HELP_STRING([--with-gcc=],
		    [graft the GNU Modula-2 front end onto a particular GCC release, choose 4.1.2, 4.7.4 or 5.2.0])],
     [gccrelease="$withval"],
     [gccrelease="5.2.0"])])
AC_SUBST([gccrelease])


#
# set up the '--with-dev=' command line option
#

AC_DEFUN([GM2_HARNESS_DEV_OPTION],
  [AC_ARG_WITH([dev],
     [AS_HELP_STRING([--with-dev=],
		    [graft the GNU Modula-2 front end using developer login git on savannagh])],
     [developer="$withval"],
     [developer="none"])])
AC_SUBST([developer])


#
# set up the '--with-cache=' command line option
#

AC_DEFUN([GM2_HARNESS_CACHE_OPTION],
  [AC_ARG_WITH([cache],
     [AS_HELP_STRING([--with-cache=],
		    [check the cache directory for gcc tarballs rather than downloading one from the gcc archive site])],
     [cachedir="$withval"],
     [cachedir="download"])])
AC_SUBST([cachedir])
