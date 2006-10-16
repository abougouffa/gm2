#
# set up the '--enable-ulmlib' command line option
#

AC_DEFUN([GM2_ULMLIB_OPTION],
  [AC_ARG_ENABLE([ulmlib],
     AS_HELP_STRING([--enable-ulmlib],
		    [build and install the University of Ulm libraries]),
     [ulmlib="$withval"],
     [ulmlib="yes"])])
AC_SUBST([ulmlib])
