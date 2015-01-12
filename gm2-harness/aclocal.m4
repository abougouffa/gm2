#
# set up the '--with-gcc=' command line option
#

AC_DEFUN([GM2_HARNESS_GCC_OPTION],
  [AC_ARG_WITH([gcc],
     [AS_HELP_STRING([--with-gcc=],
		    [graft the GNU Modula-2 front end onto a particular GCC release, choose 4.1.2 or 4.7.4])],
     [gccrelease="$withval"],
     [gccrelease="4.7.4"])])
AC_SUBST([gccrelease])

