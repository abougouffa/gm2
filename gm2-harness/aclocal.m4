#
# set up the '--with-gdb' command line option
#

AC_DEFUN([GM2_HARNESS_GDB_OPTION],
  [AC_ARG_WITH([gdb],
     [AS_HELP_STRING([--with-gdb],
		    [patch, build and install gdb])],
     [withgdb="yes"],
     [withgdb="no"])])
AC_SUBST([withgdb])

#
# set up the '--with-cvs' command line option
#

AC_DEFUN([GM2_HARNESS_CVS_OPTION],
  [AC_ARG_WITH([cvs],
     [AS_HELP_STRING([--with-cvs],
		    [always attempt to download the latest GNU Modula-2 source from the cvs server])],
     [withcvs="yes"],
     [withcvs="no"])])
AC_SUBST([withcvs])

#
# set up the '--with-gcc=' command line option
#

AC_DEFUN([GM2_HARNESS_GCC_OPTION],
  [AC_ARG_WITH([gcc],
     [AS_HELP_STRING([--with-gcc=],
		    [graft the GNU Modula-2 front end onto a particular GCC release, choose 4.1.0 or 3.3.6.  GNU Modula-2 with gcc-3.3.6 is stable whereas GNU Modula-2 with gcc-4.1.0 is work in progress])],
     [gccrelease="$withval"],
     [gccrelease="3.3.6"])])
AC_SUBST([gccrelease])

#
# set up the '--enable-ulmlib' command line option
#

AC_DEFUN([GM2_HARNESS_ULMLIB_OPTION],
  [AC_ARG_ENABLE([ulmlib],
     [AS_HELP_STRING([--enable-ulmlib],
		    [build and install the University of Ulm libraries])],
     [ulmlib="$enable_ulmlib"],
     [ulmlib="yes"])])
AC_SUBST([ulmlib])
