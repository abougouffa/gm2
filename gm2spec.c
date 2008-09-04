/* Specific flags and argument handling of the GNU Modula-2 front-end.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc.

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
extern int force_no_linker;

#include "gm2/gm2config.h"

#undef DEBUGGING

typedef enum { iso, pim, ulm, min, logitech, pimcoroutine, maxlib } libs;

/* the last entry in libraryName must be the longest string in the list */
static const char *libraryName[maxlib+1] = { "iso", "pim", "ulm", "min", "logitech",
					     "pim-coroutine", "pim-coroutine" };

typedef enum { LIB, LIB_SO, LIB_O2, LIB_SO_O2, LIB_MAX } styles;

typedef struct {
  int shared;
  int o2;
} flag_set;

typedef struct {
  const char *directory;
  flag_set flags;
} style_sig;

/*                                           dir      -fshared    -O2
 */
static style_sig libraryStyle[LIB_MAX+1] = {{"",      { FALSE,    FALSE}},
					    {"SO",    {  TRUE,    FALSE}},
					    {"O2",    { FALSE,     TRUE}},
					    {"SO_O2", {  TRUE,     TRUE}},
					    {"",      { FALSE,    FALSE}}};

static void add_default_directories (int incl, char ***in_argv,
				     const char *option, libs which_lib, styles s);
static void add_arg (int incl, char ***in_argv, const char *str);
static void insert_arg (int incl, int *in_argc, char ***in_argv);
int  lang_specific_pre_link (void);
static void add_exec_prefix(int, int *in_argc, char ***in_argv);
extern char *find_executable (const char *);
static const char *get_objects (int argc, const char *argv[]);
static const char *get_link_args (int argc, const char *argv[]);
static void remove_arg (int i, int *in_argc, const char ***in_argv);
static int is_object (const char *s);
static void remember_object (const char *s);
static int is_link_arg (const char *s);
static void remember_link_arg (const char *s);
static void scan_for_link_args (int *in_argc, const char *const **in_argv);
static styles get_style (flag_set flags);
static void add_link_from_include (int link, char **in_argv[],
                                   int incl, const char *option);
static void add_lstdcpp (int *in_argc, const char *const **in_argv);


typedef struct object_list {
  const char *name;
  struct object_list *next;
} object_list;

static object_list *head_objects = NULL;
static object_list *head_link_args = NULL;
static int inclPos=-1;
static int linkPos=-1;

/* By default, the suffix for target object files is ".o".  */
#ifdef TARGET_OBJECT_SUFFIX
#define HAVE_TARGET_OBJECT_SUFFIX
#else
#define TARGET_OBJECT_SUFFIX ".o"
#endif


/*
 *  add_exec_prefix - adds the -ftarget-ar= option so that we can tell
 *                    gm2lcc where to pick up the `ar' utility.
 */

static
void add_exec_prefix (int pos, int *in_argc, char ***in_argv)
{
  char *prefix;
  const char *ar = AR_PATH;
  const char *ranlib = RANLIB_PATH;

  /* insert ar */
  insert_arg(pos, in_argc, in_argv);
  prefix = (char *) alloca(strlen("-ftarget-ar=") +
				    strlen(ar) + 1);
  strcpy(prefix, "-ftarget-ar=");
  strcat(prefix, ar);
  (*in_argv)[pos] = xstrdup(prefix);

  /* and now insert ranlib */
  insert_arg(pos, in_argc, in_argv);
  prefix = (char *) alloca(strlen("-ftarget-ranlib=") +
				    strlen(ranlib) + 1);
  strcpy(prefix, "-ftarget-ranlib=");
  strcat(prefix, ranlib);
  (*in_argv)[pos] = xstrdup(prefix);
}

static void
add_arg (int incl, char ***in_argv, const char *str)
{
  if ((*in_argv)[incl] == NULL)
      (*in_argv)[incl] = xstrdup(str);
  else
     fprintf(stderr, "internal error not adding to a non empty space\n");
}

static void
remove_arg (int i, int *in_argc, const char ***in_argv)
{
  while (i<(*in_argc)) {
    (*in_argv)[i] = (*in_argv)[i+1];
    i++;
  }
  (*in_argc)--;
}

static int
is_object (const char *s)
{
  return (strlen(s)>strlen(TARGET_OBJECT_SUFFIX) &&
	  (strcmp(s+strlen(s)-strlen(TARGET_OBJECT_SUFFIX),
		  TARGET_OBJECT_SUFFIX) == 0));
}

static void
remember_object (const char *s)
{
  object_list *n = (object_list *)xmalloc (sizeof (object_list));
  n->name = s;
  n->next = head_objects;
  head_objects = n;
}

static int
is_link_arg (const char *s)
{
  return ((strlen(s)>2) &&
	  ((strncmp(s, "-l", 2) == 0) || (strncmp(s, "-L", 2) == 0)));
}

static void
remember_link_arg (const char *s)
{
  object_list *n = (object_list *)xmalloc (sizeof (object_list));
  n->name = s;
  n->next = head_link_args;
  head_link_args = n;
}

/*
 *  add_default_directories - add the current working
 *                            directory and the GM2 default library
 *                            directory to the end of the search path.
 */

static void
add_default_directories (int incl, char ***in_argv,
			 const char *option, libs which_lib, styles s)
{
  char *gm2libs;
  char  sepstr[2];
  const char *style_name = libraryStyle[s].directory;
  int   style_len = strlen(style_name);

  sepstr[0] = DIR_SEPARATOR;
  sepstr[1] = (char)0;

  if (style_len > 0)
    style_len += strlen(sepstr);

  if ((*in_argv)[incl] == NULL) {
    gm2libs = (char *) alloca(strlen(option) +
			      strlen(LIBSUBDIR)+strlen(sepstr)+strlen("gm2")+strlen(sepstr)+strlen(libraryName[maxlib])+1+style_len+
			      strlen(LIBSUBDIR)+strlen(sepstr)+strlen("gm2")+strlen(sepstr)+strlen(libraryName[maxlib])+1+style_len);
    strcpy(gm2libs, option);
  }
  else {
    gm2libs = (char *) alloca(strlen((*in_argv)[incl]) + strlen(":") +
			      strlen(LIBSUBDIR)+strlen(sepstr)+strlen("gm2")+strlen(sepstr)+strlen(libraryName[maxlib])+1+style_len+
			      strlen(LIBSUBDIR)+strlen(sepstr)+strlen("gm2")+strlen(sepstr)+strlen(libraryName[maxlib])+1+style_len);
    strcpy(gm2libs, (*in_argv)[incl]);
    strcat(gm2libs, ":");
  }
  strcat(gm2libs, LIBSUBDIR);
  strcat(gm2libs, sepstr);
  strcat(gm2libs, "gm2");
  strcat(gm2libs, sepstr);
  strcat(gm2libs, libraryName[which_lib]);
  if (style_len > 0) {
    strcat(gm2libs, sepstr);
    strcat(gm2libs, style_name);
  }

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
  if (style_len > 0) {
    strcat(gm2libs, sepstr);
    strcat(gm2libs, style_name);
  }

#if defined(DEBUGGING)
  fprintf(stderr, "adding %s and %s\n", option, gm2libs);
#endif
  (*in_argv)[incl] = xstrdup(gm2libs);
}

/*
 *  add_link_from_include - adds option to (**in_argv)[pos] using the
 *                          include path.
 */

static void
add_link_from_include (int pos, char **in_argv[], int include, const char *option)
{
  int l=strlen("-I");
  char *new_opt = (char *)xmalloc(strlen(option)+strlen((*in_argv)[include])-l+1);

  strcpy(new_opt, option);
  strcat(new_opt, (*in_argv)[include]+l);
  (*in_argv)[pos] = new_opt;
}

/*
 *  insert_arg - inserts an empty argument at position, pos.
 *               in_argv and in_argc are updated accordingly.
 */

static void
insert_arg (int pos, int *in_argc, char ***in_argv)
{
  int i=0;
  char **new_argv = (char **)xmalloc(sizeof(char *) * ((*in_argc) + 1));
  (*in_argc)++;

  while (i < pos) {
    new_argv[i] = (*in_argv)[i];
    i++;
  }
  new_argv[pos] = NULL;
  i = pos+1;
  while (i < *in_argc) {
    new_argv[i] = (*in_argv)[i-1];
    i++;
  }
  *in_argv = new_argv;
  if (linkPos >= pos)
    linkPos++;
  if (inclPos >= pos)
    inclPos++;
}

/*
 *  get_style - returns the style of libraries required.
 */

static styles
get_style (flag_set flags)
{
  styles s;

  for (s=LIB; s<LIB_MAX; s++)
    if (flags.shared == libraryStyle[s].flags.shared &&
	flags.o2 == libraryStyle[s].flags.o2)
      return s;
  return LIB_MAX;
}

/*
 *  add_lstdcpp - add -lstdc++ to the command line.
 */

static void
add_lstdcpp (int *in_argc, const char *const **in_argv)
{
  insert_arg (1, in_argc, (char ***)in_argv);
  add_arg (1, (char ***)in_argv, "-lstdc++");
}

static void
scan_for_link_args (int *in_argc, const char *const **in_argv)
{
  int i=0;

  while (i<(*in_argc)) {
    if ((in_argv != NULL) && ((*in_argv)[i] != NULL) && is_link_arg ((*in_argv)[i]))
      remember_link_arg ((*in_argv)[i]);
    i++;
  }
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
  int libraries=pim;
  int x=-1;
  const char *language = NULL;
  int moduleExtension = -1;
  int linking = TRUE;
  flag_set seen_flags = {FALSE, FALSE};
  styles s;

#if defined(DEBUGGING)
  while (i<*in_argc) {
    printf("in lang specific driver %s\n", (*in_argv)[i]);
    i++;
  }
  i=1;
#endif

  while (i<*in_argc) {
    if ((strcmp((*in_argv)[i], "-c") == 0) || (strcmp((*in_argv)[i], "-S") == 0))
      linking = FALSE;
    if ((strncmp((*in_argv)[i], "-I", 2) == 0) &&
	(strcmp((*in_argv)[i], "-I-") != 0))
      inclPos = i;
    if (strncmp((*in_argv)[i], "-fobject-path=", 15) == 0)
      linkPos = i;
    if (strncmp((*in_argv)[i], "-fiso", 5) == 0)
      libraries = iso;
    if (strncmp((*in_argv)[i], "-flibs=pim", 10) == 0)
      libraries = pim;
    if (strncmp((*in_argv)[i], "-flibs=ulm", 10) == 0)
      libraries = ulm;
    if (strncmp((*in_argv)[i], "-flibs=min", 10) == 0)
      libraries = min;
    if (strncmp((*in_argv)[i], "-flibs=logitech", 15) == 0)
      libraries = logitech;
    if (strncmp((*in_argv)[i], "-flibs=pim-coroutine", 20) == 0)
      libraries = pimcoroutine;
    if (strncmp((*in_argv)[i], "-fmod=", 6) == 0)
      moduleExtension = i;
    if (strcmp((*in_argv)[i], "-x") == 0) {
      x = i;
      if (i+1 < *in_argc)
	language = (*in_argv[i+1]);
    }
    if (strcmp((*in_argv)[i], "-fshared") == 0)
      seen_flags.shared = TRUE;
    if ((strcmp((*in_argv)[i], "-O2") == 0) ||
	(strcmp((*in_argv)[i], "-O3") == 0))
      seen_flags.o2 = TRUE;
    if (strcmp((*in_argv)[i], "-o") == 0)
      i += 2;
    else if (is_object((*in_argv)[i]))
      remember_object ((*in_argv)[i]);
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
  if (inclPos != -1 && linkPos == -1) {
    insert_arg(1, in_argc, (char ***)in_argv);
    linkPos = 1;
    add_link_from_include(linkPos, (char ***)in_argv, inclPos, "-fobject-path=");
  }
  if (inclPos == -1) {
    insert_arg(1, in_argc, (char ***)in_argv);
    inclPos = 1;
  }
  s = get_style(seen_flags);
  add_default_directories(inclPos, (char ***)in_argv, "-I", libraries, s);
  add_exec_prefix(1, in_argc, (char ***)in_argv);

  if (linkPos == -1) {
    insert_arg(1, in_argc, (char ***)in_argv);
    linkPos = 1;
  }
  add_default_directories(linkPos, (char ***)in_argv, "-fobject-path=", libraries, s);

  if (x == -1 && moduleExtension != -1) {
    insert_arg(1, in_argc, (char ***)in_argv);
    add_arg(1, (char ***)in_argv, "modula-2");
    insert_arg(1, in_argc, (char ***)in_argv);
    add_arg(1, (char ***)in_argv, "-x");
  }
  if (linking)
    add_lstdcpp(in_argc, in_argv);
  scan_for_link_args(in_argc, in_argv);
#if defined(DEBUGGING)
  i = 1;
  while (i<*in_argc) {
    printf("in lang specific driver %s\n", (*in_argv)[i]);
    i++;
  }
#endif
}

/*
 *  lang_specific_pre_link - does nothing.
 */

int
lang_specific_pre_link (void)
{
  return 0;
}

/*
 *  get_objects - returns a string containing all objects
 *                specified on the command line.
 */

static const char *
get_objects (int argc ATTRIBUTE_UNUSED, const char *argv[] ATTRIBUTE_UNUSED)
{
  char *result = (char *)xmalloc (1);
  int len = 0;
  int flen, i;
  object_list *o;

  *result = (char)0;

  for (o = head_objects; o != NULL; o = o->next) {
    flen = strlen(o->name);
    result = (char *)xrealloc (result, len+flen+1);
    len += flen;
    strcat(result, o->name);
    strcat(result, " ");
  }
  for (i = 0; i < n_infiles; i++)
    outfiles[i] = NULL;

  return result;
}

/*
 *  get_link_args - returns a string containing all arguments
 *                  related to the link stage.
 */

static const char *
get_link_args (int argc ATTRIBUTE_UNUSED,
	       const char *argv[] ATTRIBUTE_UNUSED)
{
  char *result = (char *)xmalloc (1);
  int len = 0;
  int alen;
  object_list *o;

  *result = (char)0;

  for (o = head_link_args; o != NULL; o = o->next) {
    alen = strlen(o->name);
    result = (char *)xrealloc (result, len+alen+1);
    len += alen;
    strcat(result, o->name);
    strcat(result, " ");
  }
  return result;
}

/*
 *  no_link - tell gcc.c not to invoke its linker.
 */

static const char *
no_link (int argc ATTRIBUTE_UNUSED, const char *argv[] ATTRIBUTE_UNUSED)
{
  force_no_linker = TRUE;
  return "";
}

/* Table of language-specific spec functions.  */ 
const struct spec_function lang_specific_spec_functions[] =
{
  { "objects", get_objects},
  { "nolink", no_link},
  { "linkargs", get_link_args},
  { NULL, NULL }
};
