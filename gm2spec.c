/* Specific flags and argument handling of the GNU Modula-2 front-end.
   Copyright (C) 2001 Free Software Foundation, Inc.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"

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


int lang_specific_extra_outfiles = 0;

/* #define DEBUGGING */

void add_default_directories (int incl, char ***in_argv);
void insert_arg (int  incl, int *in_argc, char ***in_argv);
void lang_specific_driver (int *in_argc, char ***in_argv,
			   int *in_added_libraries ATTRIBUTE_UNUSED);
int  lang_specific_pre_link (void);


/*
 *  add_default_directories - finally we add the current working directory and
 *                            the GM2 default library directory to the end of
 *                            the search path.
 */

void
add_default_directories (incl, in_argv)
     int incl;
     char ***in_argv;
{
  char *gm2libs;
  char  sepstr[2];

  sepstr[0] = DIR_SEPARATOR;
  sepstr[1] = (char)0;
  if (in_argv[incl] == NULL) {
    gm2libs = (char *) alloca(strlen("-I") +
			      strlen(LIBSUBDIR) + strlen(sepstr) + strlen("gm2") + 1);
    strcpy(gm2libs, "-I");
  }
  else {
    gm2libs = (char *) alloca(strlen(*in_argv[incl]) + strlen(":") +
			      strlen(LIBSUBDIR) + strlen(sepstr) + strlen("gm2") + 1);
    strcpy(gm2libs, *in_argv[incl]);
    strcat(gm2libs, ":");
  }
  strcat(gm2libs, LIBSUBDIR);
  strcat(gm2libs, sepstr);
  strcat(gm2libs, "gm2");

#if defined(DEBUGGING)
  fprintf(stderr, "adding -I. and %s\n", gm2libs);
#endif
  *in_argv[incl] = strdup(gm2libs);
}

/*
 *  insert_arg - inserts an empty argument at position, incl.
 *               in_argv and in_argc are updated accordingly.
 */

void
insert_arg (incl, in_argc, in_argv)
     int  incl;
     int *in_argc;
     char ***in_argv;
{
  int i=0;
  char **new_argv = (char **)xmalloc(sizeof(char *) * (*in_argc + 1));
  (*in_argc)++;

  while (i < incl) {
    new_argv[i] = *in_argv[i];
    i++;
  }
  new_argv[incl] = NULL;
  i = incl+1;
  while (i < *in_argc) {
    new_argv[i] = *in_argv[i-1];
    i++;
  }
  *in_argv = new_argv;
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
     char ***in_argv;
     int *in_added_libraries ATTRIBUTE_UNUSED;
{
  int i=1;
  int incl=-1;

#if defined(DEBUGGING)
  while (i<*in_argc) {
    printf("in lang specific driver %s\n", (*in_argv)[i]);
    i++;
  }
  i=1;
#endif

  while (i<*in_argc) {
    if ((strncmp((*in_argv)[i], "-I", 2) == 0) &&
	(strcmp((*in_argv)[i], "-I-") != 0))
      incl = i;
    i++;
  }
#if defined(DEBUGGING)
  i=1;
  while (i<*in_argc) {
    printf("out lang specific driver %s\n", (*in_argv)[i]);
    i++;
  }
#endif
  if (incl == -1) {
    incl = 1;
    insert_arg(incl, in_argc, in_argv);
  }
  add_default_directories(incl, in_argv);
}

/*
 *  lang_specific_pre_link - does nothing.
 */

int
lang_specific_pre_link ()
{
  return 0;
}
