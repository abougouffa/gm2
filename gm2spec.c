/* Specific flags and argument handling of the GNU Modula-2 front-end.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc.

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
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "gcc.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

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

#include "gm2/gm2config.h"

#undef DEBUGGING

typedef enum { iso, pim, ulm, logitech, pimcoroutine, maxlib } libs;

/* the last entry in libraryName must be the longest string in the list */
static const char *libraryName[maxlib+1] = { "iso", "pim", "ulm", "logitech",
				       "pim-coroutine", "pim-coroutine" };

void add_default_directories (int incl, char ***in_argv, libs which_lib);
void add_arg (int incl, char ***in_argv, const char *str);
void insert_arg (int incl, int *in_argc, char ***in_argv);
int  lang_specific_pre_link (void);
void add_exec_prefix(int, int *in_argc, char ***in_argv);
extern char *find_executable PARAMS ((const char *));

/*
 *  add_exec_prefix - adds the -Wtarget-ar= option so that we can tell
 *                    gm2lcc where to pick up the `ar' utility.
 */

void add_exec_prefix(int pos, int *in_argc, char ***in_argv)
{
  char *prefix;
  const char *ar = AR_PATH;
  const char *ranlib = RANLIB_PATH;

  /* insert ar */
  insert_arg(pos, in_argc, in_argv);
  prefix = (char *) alloca(strlen("-Wtarget-ar=") +
				    strlen(ar) + 1);
  strcpy(prefix, "-Wtarget-ar=");
  strcat(prefix, ar);
  (*in_argv)[pos] = xstrdup(prefix);

  /* and now insert ranlib */
  insert_arg(pos, in_argc, in_argv);
  prefix = (char *) alloca(strlen("-Wtarget-ranlib=") +
				    strlen(ranlib) + 1);
  strcpy(prefix, "-Wtarget-ranlib=");
  strcat(prefix, ranlib);
  (*in_argv)[pos] = xstrdup(prefix);
}

void
add_arg (int incl, char ***in_argv, const char *str)
{
  if ((*in_argv)[incl] == NULL)
      (*in_argv)[incl] = xstrdup(str);
  else
     fprintf(stderr, "internal error not adding to an empty space\n");
}

/*
 *  add_default_directories - add the current working
 *                            directory and the GM2 default library
 *                            directory to the end of the search path.
 */

void
add_default_directories (int incl, char ***in_argv, libs which_lib)
{
  char *gm2libs;
  char  sepstr[2];

  sepstr[0] = DIR_SEPARATOR;
  sepstr[1] = (char)0;

  if ((*in_argv)[incl] == NULL) {
    gm2libs = (char *) alloca(strlen("-I") +
			      strlen(LIBSUBDIR) + strlen(sepstr) + strlen("gm2") + strlen(sepstr) + strlen(libraryName[maxlib]) + 1 +
			      strlen(LIBSUBDIR) + strlen(sepstr) + strlen("gm2") + strlen(sepstr) + strlen(libraryName[maxlib]) + 1);
    strcpy(gm2libs, "-I");
  }
  else {
    gm2libs = (char *) alloca(strlen((*in_argv)[incl]) + strlen(":") +
			      strlen(LIBSUBDIR) + strlen(sepstr) + strlen("gm2") + strlen(sepstr) + strlen(libraryName[maxlib]) + 1 +
			      strlen(LIBSUBDIR) + strlen(sepstr) + strlen("gm2") + strlen(sepstr) + strlen(libraryName[maxlib]) + 1);
    strcpy(gm2libs, (*in_argv)[incl]);
    strcat(gm2libs, ":");
  }
  strcat(gm2libs, LIBSUBDIR);
  strcat(gm2libs, sepstr);
  strcat(gm2libs, "gm2");
  strcat(gm2libs, sepstr);

  strcat(gm2libs, libraryName[which_lib]);

  strcat(gm2libs, ":");
  strcat(gm2libs, LIBSUBDIR);
  strcat(gm2libs, sepstr);
  strcat(gm2libs, "gm2");
  strcat(gm2libs, sepstr);
  if (which_lib == pim)
    /*
     *  fall back to logitech libraries if using pim (the logitech
     *  libraries are extended pim libraries)
     */
    strcat(gm2libs, libraryName[logitech]);
  else
    strcat(gm2libs, "pim");   /* all other libraries fall back to pim */

#if defined(DEBUGGING)
  fprintf(stderr, "adding -I. and %s\n", gm2libs);
#endif
  (*in_argv)[incl] = xstrdup(gm2libs);
}

/*
 *  insert_arg - inserts an empty argument at position, incl.
 *               in_argv and in_argc are updated accordingly.
 */

void
insert_arg (int  incl, int *in_argc, char ***in_argv)
{
  int i=0;
  char **new_argv = (char **)xmalloc(sizeof(char *) * ((*in_argc) + 1));
  (*in_argc)++;

  while (i < incl) {
    new_argv[i] = (*in_argv)[i];
    i++;
  }
  new_argv[incl] = NULL;
  i = incl+1;
  while (i < *in_argc) {
    new_argv[i] = (*in_argv)[i-1];
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
lang_specific_driver (int *in_argc, const char *const **in_argv,
		      int *in_added_libraries ATTRIBUTE_UNUSED)
{
  int i=1;
  int incl=-1;
  int libraries=pim;
  int x=-1;
  const char *language = NULL;
  int moduleExtension = -1;

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
    if (strncmp((*in_argv)[i], "-Wiso", 5) == 0)
      libraries = iso;
    if (strncmp((*in_argv)[i], "-Wlibs=pim", 10) == 0)
      libraries = pim;
    if (strncmp((*in_argv)[i], "-Wlibs=ulm", 10) == 0)
      libraries = ulm;
    if (strncmp((*in_argv)[i], "-Wlibs=logitech", 15) == 0)
      libraries = logitech;
    if (strncmp((*in_argv)[i], "-Wlibs=pim-coroutine", 20) == 0)
      libraries = pimcoroutine;
    if (strncmp((*in_argv)[i], "-Wmod=", 6) == 0)
      moduleExtension = i;
    if (strcmp((*in_argv)[i], "-x") == 0) {
      x = i;
      if (i+1 < *in_argc)
	language = (*in_argv[i+1]);
    }
    i++;
  }
  if (language != NULL && (strcmp (language, "modula-2") != 0))
    return;
#if defined(DEBUGGING)
  i=1;
  while (i<*in_argc) {
    printf("out lang specific driver %s\n", (*in_argv)[i]);
    i++;
  }
#endif
  if (incl == -1) {
    incl = 1;
    insert_arg(incl, in_argc, (char ***)in_argv);
  }
  add_default_directories(incl, (char ***)in_argv, libraries);
  add_exec_prefix(1, in_argc, (char ***)in_argv);
  if (x == -1 && moduleExtension != -1) {
    insert_arg(1, in_argc, (char ***)in_argv);
    add_arg(1, (char ***)in_argv, "modula-2");
    insert_arg(1, in_argc, (char ***)in_argv);
    add_arg(1, (char ***)in_argv, "-x");
  }
#if defined(DEBUGGING)
  i=1;
  while (i<*in_argc) {
    printf("in lang specific driver %s\n", (*in_argv)[i]);
    i++;
  }
  i=1;
#endif
}

/*
 *  lang_specific_pre_link - does nothing.
 */

int
lang_specific_pre_link ()
{
  return 0;
}

/* Table of language-specific spec functions.  */ 
const struct spec_function lang_specific_spec_functions[] =
{
  { 0, 0 }
};
