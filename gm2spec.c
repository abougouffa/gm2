/* Specific flags and argument handling of the C++ front-end.
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "gansidecl.h"
#include "intl.h"

#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>
#include "gcc.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

extern const char *find_file PARAMS ((const char *));
/* #include "gcc.h" */

/* This bit is set if we saw a `-xfoo' language specification.  */
#define LANGSPEC	(1<<1)
/* This bit is set if they did `-lm' or `-lmath'.  */
#define MATHLIB		(1<<2)
/* This bit is set if they did `-lc'.  */
#define WITHLIBC	(1<<3)

#ifndef MATH_LIBRARY
#define MATH_LIBRARY "-lm"
#endif

#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR '/'
#endif

extern char *xmalloc PARAMS ((size_t));

#define MAXPATHCHAR    64*1024         /* large enough not to worry */

int lang_specific_extra_outfiles = 0;


static int               found_verbose   = FALSE;
static int               need_to_link    =  TRUE;

/* #define DEBUGGING */

/*
 *  convert_into_m2path - converts an include -Ioption into the equivalent
 *                        Modula-2 M2PATH space delimitered path.
 */

static int convert_into_m2path (const char *incl)
{
  char *m2path=getenv("M2PATH");
  char *newm2path;
  int l=0;
  int j=0;

#if defined(DEBUGGING)
  fprintf(stderr, "entered include = %s\n", incl);
#endif
  if (incl != NULL) {
    l = strlen(incl);
  }
  if (m2path != NULL) {
    j=strlen(m2path);
  }
  newm2path=(char *)xmalloc(j+l+1);

  if (newm2path != NULL) {
    int i=2;  /* skip -I */

    if (m2path != NULL) {
      strcpy(newm2path, m2path);
    }
    if (l>0) {
      if (j>0) {
	newm2path[j] = ' ';
	j++;
      }
      while (i<l) {
	if (incl[i] == ':') {
	  newm2path[j] = ' ';
	} else {
	  newm2path[j] = incl[i];
	}
	j++;
	i++;
      }
    }
    newm2path[j] = (char)0;
#if defined(DEBUGGING)
    printf("M2PATH = %s\n", newm2path);
#endif
    return( setenv("M2PATH", newm2path, 1) );
  }
  return( FALSE );
}

/*
 *  remove_args - removes args: i..i+n-1 from in_argc, in_argv
 */

static void
remove_args (in_argc, in_argv, i, n)
     int *in_argc;
     char ***in_argv;
     int i;
     int n;
{
  int j=i+n;

  while (j<=*in_argc) {
    (*in_argv)[i] = (*in_argv)[j];
    i++;
    j++;
  }
  *in_argc -= n;
}


/*
 *  add_default_directories - finally we add the current working directory and
 *                            the GM2 default library directory to the end of
 *                            M2PATH.
 */

void
add_default_directories (void)
{
  char *gm2libs;
  char  sepstr[2];

  sepstr[0] = DIR_SEPARATOR;
  sepstr[1] = (char)0;
  gm2libs   = (char *) alloca( strlen("-I") + strlen(LIBSUBDIR) + sizeof(DIR_SEPARATOR) + strlen("gm2") + 1);
  strcpy(gm2libs, "-I");
  strcat(gm2libs, LIBSUBDIR);
  strcat(gm2libs, sepstr);
  strcat(gm2libs, "gm2");

#if defined(DEBUGGING)
  fprintf(stderr, "adding -I. and %s\n", gm2libs);
#endif
  if ((convert_into_m2path("-I.")) || (convert_into_m2path(gm2libs))) {
    /*
     *   want to say  	error ("out of memory trying to alter environment");
     */
  }
}

/*
 *  lang_specific_driver - is invoked if we are compiling/linking a
 *                         Modula-2 file. It checks for module paths
 *                         and linking requirements which are language
 *                         specific.
 */

void
lang_specific_driver (in_argc, in_argv, in_added_libraries)
     int *in_argc;
     const char *const **in_argv;
     int *in_added_libraries ATTRIBUTE_UNUSED;
{
  int i=1;

#if defined(DEBUGGING)
  while (i<*in_argc) {
    printf("in lang specific driver %s\n", (*in_argv)[i]);
    i++;
  }
  i=1;
#endif

  /*
   *  we need to remove the -M "thispath thatpath . whateverpath"
   *  as the -M option in GCC is a linker option and takes no argument.
   *  Since the Modula-2 front end will examine the M2PATH environment
   *  variable we will use that.
   */
  
  while (i<*in_argc) {
    if (strcmp((*in_argv)[i], "-M") == 0) {
      /* found the offending path option, remove it */
      if (i+1<*in_argc) {
	if (setenv("M2PATH", (*in_argv)[i+1], 1)) {
	  /*
	   *  want to say  error ("out of memory trying to alter environment");
	   */
	  return;
	}
	remove_args(in_argc, in_argv, i, 2);
      } else {
	return;
	/*
	 *  want to say  error ("path argument to -M flag is missing");
	 */
	i++;
      }
    } else if (strncmp((*in_argv)[i], "-I", 2) == 0) {
      if (convert_into_m2path((*in_argv)[i])) {
	return;
	/*
	 *   want to say  	error ("out of memory trying to alter environment");
	 */
      }
      remove_args(in_argc, in_argv, i, 1);
    } else if (strcmp((*in_argv)[i], "-v") == 0) {
      found_verbose = TRUE;
      i++;
    } else if (strcmp((*in_argv)[i], "-c") == 0) {
      need_to_link = FALSE;
      i++;
    } else {
      i++;
    }
  }
#if defined(DEBUGGING)
  i=1;
  while (i<*in_argc) {
    printf("out lang specific driver %s\n", (*in_argv)[i]);
    i++;
  }
#endif
  add_default_directories();
}


/*
 *  lang_specific_pre_link - the aim of this function is to generate the
 *                           module_gm2.a library (consisting of all dependant modules).
 */

int
lang_specific_pre_link ()
{
  return( 0 );
}
