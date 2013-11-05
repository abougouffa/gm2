/* Specific flags and argument handling of the GNU Modula-2 front-end.
 * Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
 *               2010, 2011, 2012, 2013
 *               Free Software Foundation, Inc.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "gcc.h"
#include "opts.h"
#include "vec.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

/* This bit is set if we saw a `-xfoo' language specification.  */
#define LANGSPEC	(1<<1)
/* This bit is set if they did `-lm' or `-lmath'.  */
#define MATHLIB		(1<<2)
/* This bit is set if they did `-lc'.  */
#define WITHLIBC	(1<<3)

#ifndef MATH_LIBRARY
#define MATH_LIBRARY "m"
#endif

#ifndef MATH_LIBRARY_PROFILE
#define MATH_LIBRARY_PROFILE MATH_LIBRARY
#endif

#ifndef LIBSTDCXX
#define LIBSTDCXX "stdc++"
#endif

#ifndef LIBSTDCXX_PROFILE
#define LIBSTDCXX_PROFILE LIBSTDCXX
#endif

#ifndef LIBSTDCXX_STATIC
#define LIBSTDCXX_STATIC NULL
#endif

#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR '/'
#endif

/* Most every one is fine with LIBRARY_PATH.  For some, it conflicts.  */
#ifndef LIBRARY_PATH_ENV
#define LIBRARY_PATH_ENV "LIBRARY_PATH"
#endif

#ifndef GM2_ROOT_ENV
#define GM2_ROOT_ENV  "GM2_ROOT"
#endif

#ifndef GM2IPATH_ENV
#define GM2IPATH_ENV  "GM2IPATH"
#endif

#ifndef GM2OPATH_ENV
#define GM2OPATH_ENV  "GM2OPATH"
#endif

int lang_specific_extra_outfiles = 0;
extern int force_no_linker;

#include "gm2/gm2version.h"
#include "gm2/gm2config.h"

#undef DEBUGGING
/* #define DEBUGGING */

#define DEFAULT_DIALECT "pim"

typedef enum { iso, pim, ulm, min, logitech, pimcoroutine, maxlib } libs;

/* the last entry in libraryName must be the longest string in the list */
static const char *libraryName[maxlib+1] = { "iso", "pim", "ulm", "min", "logitech",
					     "pim-coroutine", "pim-coroutine" };

static const char *archiveName[maxlib+1] = { "gm2iso", "gm2", "gm2ulm", "gm2min", "gm2pim",
					     "gm2pco", "gm2min" };

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

int lang_specific_pre_link (void);
static void add_exec_prefix (void);
static void add_B_prefix (unsigned int *in_decoded_options_count, struct cl_decoded_option **in_decoded_options, const char *gm2_root);
static const char *get_objects (int argc, const char *argv[]);
static const char *get_link_args (int argc, const char *argv[]);
static const char *add_exec_dir (int argc, const char *argv[]);
static const char *add_exec_name (int argc, const char *argv[]);
static int is_object (const char *s);
static void remember_object (const char *s);
static void remember_link_arg (const char *s);
static void scan_for_link_args (unsigned int *in_decoded_options_count, struct cl_decoded_option **in_decoded_options);
static styles get_style (flag_set flags);
static void add_link_from_include (struct cl_decoded_option **in_options, int include);
static void add_lib (size_t opt_index, const char *lib, int joined);
static void check_gm2_root (void);
static char *add_include (char *prev, const char *libpath, char *library);
static const char *gen_gm2_root (const char *gm2_root);
static const char *get_prefix (void);
static const char *get_B_or_prefix (void);
static void insert_option (unsigned int *in_decoded_options_count, struct cl_decoded_option **in_decoded_options, unsigned int position);
static void printOption (const char *desc, struct cl_decoded_option **in_decoded_options, int i);

void fe_save_switch (const char *opt, size_t n_args, const char *const *args, bool validated);


typedef struct object_list {
  const char *name;
  struct object_list *next;
} object_list;

static object_list *head_objects = NULL;
static object_list *head_link_args = NULL;
static int inclPos=-1;
static int linkPos=-1;
static int seen_fonlylink = FALSE;
static int seen_fmakeall0 = FALSE;
static int seen_fmakeall = FALSE;
static int seen_B = FALSE;
static char *B_path = NULL;

/* By default, the suffix for target object files is ".o".  */
#ifdef TARGET_OBJECT_SUFFIX
#define HAVE_TARGET_OBJECT_SUFFIX
#else
#define TARGET_OBJECT_SUFFIX ".o"
#endif

#if 0
static const char *gm2_cpp_options = "-fcppbegin %:exec_prefix(cc1%s) -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -fcppend";
#endif


/*
 *  assert - simple assertion procedure.
 */

static void
assert (int b)
{
  if (! b)
    {
      printf ("assert failed in gm2spec.c");
      exit (1);
    }
}

/*
 *  fe_generate_option - wrap up arg and pass it to fe_save_switch.
 */

static void fe_generate_option (size_t opt_index, const char *arg, int joined)
{
  const struct cl_option *option = &cl_options[opt_index];
  const char *opt = (const char *) option->opt_text;
  const char *newopt = opt;

#if 0
  if (opt_index == OPT_l)
    return;
#endif

  if (joined)
    newopt = concat (opt, arg, NULL);

  if (arg == NULL || joined)
    {
#if 0
      if (opt_index == OPT_l)
	{
	  fe_add_infile (newopt, "*");
	}
#endif
      fe_save_switch (newopt, 0, NULL, 1);
    }
  else
    {
      const char **x = (const char **) XCNEWVEC (const char **, 2);

      x[0] = xstrdup (arg);
      x[1] = NULL;

      assert (opt_index != OPT_l);
      fe_save_switch (newopt, 1, x, 1);
    }
}

/*
 *  find_executable_path - if argv0 references an executable filename then use
 *                         this path.
 */

static
const char *find_executable_path (const char *argv0)
{
  if (access (argv0, X_OK) == 0)
    {
      char *n = strrchr (argv0, DIR_SEPARATOR);

      /* strip off the program name from argv0, but leave the DIR_SEPARATOR */
      if (n != NULL) {
	argv0 = xstrdup (argv0);
	n = strrchr (argv0, DIR_SEPARATOR);
	n[1] = (char)0;
	return argv0;
      }
    }
  return NULL;
}

/*
 *  add_B_prefix - adds the -Bprefix option so that we can tell
 *                 subcomponents of gm2 where to pick up its executables.
 *                 But we can only do this if the user explicitly gives
 *                 the path to argv[0].
 */

static
void add_B_prefix (unsigned int *in_decoded_options_count,
		   struct cl_decoded_option **in_decoded_options, const char *gm2_root)
{
  if ((*in_decoded_options)[0].arg != NULL)
    {
      const char *arg = (*in_decoded_options)[0].arg;
      const char *path = find_executable_path (arg);

      if (path == NULL || (strcmp (path, "") == 0))
	path = gen_gm2_root (gm2_root);

      if (path != NULL && (strcmp (path, "") != 0))
	{
	  unsigned int count = *in_decoded_options_count;

#if defined(DEBUGGING)
	  unsigned int i;

	  printf("going to add -B%s\n", path);
	  for (i = 0; i < *in_decoded_options_count; i++)
	    printOption("before add -B", in_decoded_options, i);
#endif

	  fe_B_prefix (xstrdup (path));
	  fe_generate_option (OPT_B, xstrdup (path), 1);
#if 0
	  insert_option (in_decoded_options_count, in_decoded_options, count);
	  generate_option (OPT_B, xstrdup (path), 1,
			   CL_DRIVER, &(*in_decoded_options)[count]);
#endif

#if defined(DEBUGGING)
	  for (i = 0; i < *in_decoded_options_count; i++)
	    printOption("after add -B", in_decoded_options, i);
#endif

	}
    }
}

/*
 *  add_exec_prefix - adds the -ftarget-ar= option so that we can tell
 *                    gm2lcc where to pick up the `ar' utility.
 */

static
void add_exec_prefix (void)
{
  const char *ar = AR_PATH;
  const char *ranlib = RANLIB_PATH;

  fe_generate_option (OPT_ftarget_ar_, ar, TRUE);
  fe_generate_option (OPT_ftarget_ranlib_, ranlib, TRUE);
}

static
const char *get_prefix (void)
{
  const char *prefix = getenv (GM2_ROOT_ENV);

  if (prefix == NULL || (strcmp (prefix, "") == 0))
    return PREFIX;
  else
    return prefix;
}

static
const char *get_B_or_prefix (void)
{
  if (seen_B)
    return B_path;
  else
    return get_prefix ();
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

static void
remember_link_arg (const char *s)
{
  object_list *n = (object_list *)xmalloc (sizeof (object_list));
  n->name = s;
  n->next = head_link_args;
  head_link_args = n;
}

/*
 *  add_link_from_include - adds option to (**in_argv)[pos] using the
 *                          include path.
 */

static void
add_link_from_include (struct cl_decoded_option **in_options, int include)
{
  struct cl_decoded_option *options = *in_options;
  const char *arg = options[include].arg;

  fe_generate_option (OPT_fobject_path_, arg, TRUE);
}

/*
 *  get_style - returns the style of libraries required.
 */

static styles
get_style (flag_set flags)
{
  styles s;

  for (s=LIB; s<LIB_MAX; s = (styles) ((int)s+1))
    if (flags.shared == libraryStyle[s].flags.shared &&
	flags.o2 == libraryStyle[s].flags.o2)
      return s;
  return LIB_MAX;
}

/*
 *  add_lib - add, lib, to the end of the command line.
 */

static void
add_lib (size_t opt_index, const char *lib, int joined)
{
  if (lib == NULL || (strcmp (lib, "") == 0))
    return;

  fe_generate_option (opt_index, lib, joined);
}

#if defined(DEBUGGING)
static void
printOption (const char *desc, struct cl_decoded_option **in_decoded_options, int i)
{
  printf("lang_specific_driver ");
  printf(desc);
  printf(" [%d]", i);

  switch ((*in_decoded_options)[i].opt_index) {

  case N_OPTS:  break;
  case OPT_SPECIAL_unknown:   printf(" flag <unknown>"); break;
  case OPT_SPECIAL_ignore:   printf(" flag <ignore>"); break;
  case OPT_SPECIAL_program_name:  printf(" flag <program name>"); break;
  case OPT_SPECIAL_input_file:  printf(" flag <input file name>"); break;

  default:
    printf(" flag [%s]", cl_options[(*in_decoded_options)[i].opt_index].opt_text);
  }

  if ((*in_decoded_options)[i].arg == NULL)
    printf(" no arg");
  else
    printf(" arg [%s]", (*in_decoded_options)[i].arg);
  printf(" orig text [%s]", (*in_decoded_options)[i].orig_option_with_args_text);
  printf(" value [%d]", (*in_decoded_options)[i].value);
  printf(" error [%d]\n", (*in_decoded_options)[i].errors);
}
#endif

/*
 *
 */

static void
insert_option (unsigned int *in_decoded_options_count,
	       struct cl_decoded_option **in_decoded_options,
	       unsigned int position)
{
  struct cl_decoded_option *new_decoded_options;
  unsigned int i;

  (*in_decoded_options_count)++;

  assert (position <= (*in_decoded_options_count));

  new_decoded_options = XNEWVEC (struct cl_decoded_option, (*in_decoded_options_count));
  for (i=0; i<position; i++)
    new_decoded_options[i] = (*in_decoded_options)[i];
  memset (&new_decoded_options[position], 0, sizeof (struct cl_decoded_option));

  for (i=position; i<(*in_decoded_options_count)-1; i++)
    new_decoded_options[i+1] = (*in_decoded_options)[i];
  *in_decoded_options = new_decoded_options;

}

/*
 *
 */

static void
add_library (const char *libraryname,
	     unsigned int *in_decoded_options_count,
	     struct cl_decoded_option **in_decoded_options,
	     unsigned int position)
{
  if (libraryname == NULL || (strcmp (libraryname, "") == 0))
    return;

  insert_option (in_decoded_options_count, in_decoded_options, position);

#if defined(DEBUGGING)
  unsigned int i;

  printf("going to add -l%s at position=%d  count=%d\n",
	 libraryname, position, *in_decoded_options_count);
  for (i = 0; i < *in_decoded_options_count; i++)
    printOption("before add_library", in_decoded_options, i);
#endif

  generate_option (OPT_l, libraryname, 1,
		   CL_DRIVER, &(*in_decoded_options)[position]);

#if defined(DEBUGGING)
  for (i = 0; i < *in_decoded_options_count; i++)
    printOption("after add_library", in_decoded_options, i);
#endif
}

/*
 *  getArchiveName - return the corresponding archive name given the library name.
 */

static const char *
getArchiveName (const char *library)
{
  libs i;

  for (i=iso; i<maxlib; i = (libs) ((int)i+1))
    if (strcmp(libraryName[i], library) == 0)
      return archiveName[i];
  return NULL;
}

/*
 *  build_archive_path - returns a string containing the a path to the
 *                       archive defined by, libpath, s, and, dialectLib.
 */

static const char *
build_archive_path (const char *libpath,
		    const char *library,
		    styles style)
{
  if (library != NULL) {
    const char *style_name = libraryStyle[style].directory;
    const char *libdir = (const char *)library;

    if (libdir != NULL) {
      int l = strlen (libpath) + 1 +
	strlen("lib") + 1 + strlen("gcc") + 1 +
	strlen (DEFAULT_TARGET_MACHINE) + 1 +
	strlen (DEFAULT_TARGET_VERSION) + 1 +
	strlen (style_name) + 1 +
	strlen (libdir) + 1;
      char *s = (char *) xmalloc (l);
      char dir_sep[2];

      dir_sep[0] = DIR_SEPARATOR;
      dir_sep[1] = (char)0;
    
      strcpy (s, libpath);
      strcat (s, dir_sep);
      strcat (s, "gm2");
      strcat (s, dir_sep);
      strcat (s, libdir);
      if (strlen(style_name) != 0) {
	strcat (s, dir_sep);
	strcat (s, style_name);
      }
      return s;
    }
  }
  return NULL;
}

/*
 *  build_archive - returns a string containing the a path to the
 *                  archive defined by, libpath, s, and, dialectLib.
 */

static char *
build_archive (const char *library)
{
  if (library != NULL) {
    const char *a = getArchiveName(library);
    if (a != NULL) {
      char *s = (char *) xmalloc (strlen (a) + 1);
      strcpy (s, a);
      return s;
    }
  }
  return NULL;
}

/*
 *  add_default_combination - adds the correct link path and then the library name.
 */

static void
add_default_combination (const char *libpath,
			 const char *library,
			 styles style,
			 unsigned int *in_decoded_options_count,
			 struct cl_decoded_option **in_decoded_options,
			 unsigned int position)
{
  if (library != NULL) {
    add_lib (OPT_L, build_archive_path (libpath, library, style), TRUE);
    add_library (build_archive (library), in_decoded_options_count, in_decoded_options, position);
  }
}

/*
 *  add_default_archives - adds the default archives to the end of the current
 *                         command line.
 */

static void
add_default_archives (const char *libpath,
		      styles s,
		      const char *libraries,
		      unsigned int *in_decoded_options_count,
		      struct cl_decoded_option **in_decoded_options,
		      unsigned int position)
{
  char *prev;
  const char *l = libraries;
  const char *e;
  char *c;
  unsigned int libcount = 0;

  do {
    if (libpath == NULL)
      prev = NULL;
    else
      prev = xstrdup (libpath);

    e = index (l, ',');
    if (e == NULL) {
      c = xstrdup(l);
      l = NULL;
    }
    else {
      c = strndup(l, e-l);
      l = e+1;
    }
    add_default_combination (libpath, c, s, in_decoded_options_count, in_decoded_options, position+libcount);
    libcount++;
    prev = add_include (prev, libpath, c);

    fe_generate_option (OPT_L, prev, TRUE);
    free((void *)c);
  } while ((l != NULL) && (l[0] != (char)0));
}

/*
 *  build_include_path - builds the component of the include path referenced by
 *                       the, which, libs.
 */

static char *
build_include_path (const char *prev, const char *libpath, const char *library)
{
  char  sepstr[2];
  char *gm2libs;
  const char *option = "";

  sepstr[0] = DIR_SEPARATOR;
  sepstr[1] = (char)0;

  if (prev == NULL) {
    gm2libs = (char *) alloca(strlen(option) +
			      strlen(libpath)+strlen(sepstr)+strlen("gm2")+strlen(sepstr)+strlen(library)+1+
			      strlen(libpath)+strlen(sepstr)+strlen("gm2")+strlen(sepstr)+strlen(library)+1);
    strcpy(gm2libs, option);
  }
  else {
    gm2libs = (char *) alloca(strlen(prev) + strlen(":") +
			      strlen(libpath)+strlen(sepstr)+strlen("gm2")+strlen(sepstr)+strlen(library)+1+
			      strlen(libpath)+strlen(sepstr)+strlen("gm2")+strlen(sepstr)+strlen(library)+1);
    strcpy(gm2libs, prev);
    strcat(gm2libs, ":");
  }
  strcat(gm2libs, libpath);
  strcat(gm2libs, sepstr);
  strcat(gm2libs, "gm2");
  strcat(gm2libs, sepstr);
  strcat(gm2libs, library);

  return xstrdup (gm2libs);
}

/*
 *  add_include - add the correct include path given the libpath and library.
 *                The new path is returned.
 */

static char *
add_include (char *prev, const char *libpath, char *library)
{
  if (library == NULL)
    return prev;
  else
    return build_include_path (prev, libpath, library);
}

/*
 *  add_default_includes - add the appropriate default include paths depending
 *                         upon the style of libraries chosen.
 */

static void
add_default_includes (const char *libpath,
		      const char *libraries,
		      const char *envpath)
{
  const char *l = libraries;
  const char *e;
  char *prev;
  char *c;

  if (envpath == NULL || (strlen (envpath) == 0))
    prev = NULL;
  else
    prev = xstrdup (envpath);

  do {
    e = index (l, ',');
    if (e == NULL) {
      c = xstrdup(l);
      l = NULL;
    }
    else {
      c = strndup(l, e-l);
      l = e+1;
    }
    prev = add_include (prev, libpath, c);
    free((void *)c);
  } while ((l != NULL) && (l[0] != (char)0));

  fe_generate_option (OPT_I, prev, TRUE);
}

/*
 *  build_fobject_path - returns a string containing the a path to the
 *                       objects defined by, libpath, s, and, dialectLib.
 */

static char *
build_fobject_path (const char *prev, const char *libpath, const char *library,
		    styles style)
{
  char  sepstr[2];
  char *gm2objs;
  const char *style_name = libraryStyle[style].directory;
  const char *libName = library;

  sepstr[0] = DIR_SEPARATOR;
  sepstr[1] = (char)0;

  if (prev == NULL) {
    gm2objs = (char *) alloca(strlen(libpath)+strlen(sepstr)+strlen("gm2")+strlen(sepstr)+strlen(libName)+1+
			      strlen(libpath)+strlen(sepstr)+strlen("gm2")+strlen(sepstr)+strlen(libName)+1);
    strcpy(gm2objs, "");
  }
  else {
    gm2objs = (char *) alloca(strlen(prev) + strlen(":") +
			      strlen(libpath)+strlen(sepstr)+strlen("gm2")+strlen(sepstr)+strlen(libName)+1+
			      strlen(libpath)+strlen(sepstr)+strlen("gm2")+strlen(sepstr)+strlen(libName)+1);
    strcpy(gm2objs, prev);
    strcat(gm2objs, ":");
  }
  strcat(gm2objs, libpath);
  strcat(gm2objs, sepstr);
  strcat(gm2objs, "gm2");
  strcat(gm2objs, sepstr);
  strcat(gm2objs, libName);

  if (strlen(style_name) != 0) {
    strcat (gm2objs, sepstr);
    strcat (gm2objs, style_name);
  }

  return xstrdup (gm2objs);
}

/*
 *  add_fobject_path - add all required path to satisfy the link for library.
 */

static void
add_fobject_path (const char *prev,
		  const char *libpath,
		  const char *library,
		  styles s)
{
  if (library != NULL)
    fe_generate_option (OPT_fobject_path_, build_fobject_path (prev, libpath, library, s), TRUE);
}

/*
 *  add_default_fobjects - add the appropriate default include paths depending
 *                         upon the style of libraries chosen.
 */

static void
add_default_fobjects (const char *prev,
		      const char *libpath,
		      const char *libraries,
		      styles s,
		      const char *envpath)
{
  const char *l = libraries;
  const char *e;
  char *c;

  if (envpath != NULL && (strcmp (envpath, "") != 0))
    fe_generate_option (OPT_I, envpath, TRUE);

  do {
    e = index (l, ',');
    if (e == NULL) {
      c = xstrdup(l);
      l = NULL;
    }
    else {
      c = strndup(l, e-l);
      l = e+1;
    }
    add_fobject_path (prev, libpath, c, s);
    free(c);
  } while ((l != NULL) && (l[0] != (char)0));
}

static void
scan_for_link_args (unsigned int *in_decoded_options_count,
		    struct cl_decoded_option **in_decoded_options)
{
  struct cl_decoded_option *decoded_options = *in_decoded_options;
  unsigned int i;
  
  for (i = 0; i < *in_decoded_options_count; i++) {
    const char *arg = decoded_options[i].arg;
    size_t opt = decoded_options[i].opt_index;

    if ((opt == OPT_l) || (opt == OPT_L))
      remember_link_arg (arg);
  }
}

/*
 *  purge_include_options - remove any -I option found from
 *                          in_decoded_options.
 */

static void
purge_include_options (unsigned int *in_decoded_options_count,
		       struct cl_decoded_option **in_decoded_options)
{
  struct cl_decoded_option *decoded_options = *in_decoded_options;
  unsigned int i, j;
  
  for (i = 0; i < *in_decoded_options_count; i++)
    {
      size_t opt = decoded_options[i].opt_index;

      if (opt == OPT_I)
	{
	  for (j = i; j+1 < *in_decoded_options_count; j++)
	    decoded_options[j] = decoded_options[j+1];
	  (*in_decoded_options_count)--;
	}
    }
}

/*
 *  convert_include_into_link - convert the include path options into link path
 *                              options.
 */

static void
convert_include_into_link (struct cl_decoded_option **in_decoded_options,
			   unsigned int *in_decoded_options_count)
{
  unsigned int i;

  for (i = 1; i < *in_decoded_options_count; i++) {
    size_t opt = (*in_decoded_options)[i].opt_index;

    if (opt == OPT_I)
      add_link_from_include (in_decoded_options, i);
  }
}

/*
 *  build_path - implements export PATH=$(gm2_root)/bin:$PATH
 *
 *               where gm2_root is a C variable.
 */

static void
build_path (char *gm2_root)
{
  int l = strlen ("PATH=") + strlen (gm2_root) + 1 + strlen("bin") + 1;
  char *s;
  char dir_sep[2];
  const char *path;

  path = getenv ("PATH");
  if (path != NULL && (strcmp(path, "") != 0))
    l += strlen(":") + strlen(path);
  s = (char *) xmalloc (l);
  dir_sep[0] = DIR_SEPARATOR;
  dir_sep[1] = (char)0;

  strcpy (s, "PATH=");
  strcat (s, gm2_root);
  strcat (s, dir_sep);
  strcat (s, "bin");
  if (path != NULL && (strcmp(path, "") != 0)) {
    strcat (s, ":");
    strcat (s, path);
  }
  putenv (s);
}

/*
 *  gen_gm2_root - return a string containing the gm2_root path.
 */

static const char *
gen_gm2_root (const char *gm2_root)
{
  int l = strlen (gm2_root) + 1 +
    strlen("lib") + 1 + strlen("gcc") + 1 +
    strlen (DEFAULT_TARGET_MACHINE) + 1 +
    strlen (DEFAULT_TARGET_VERSION) + 1;
  char *s = (char *) xmalloc (l);
  char dir_sep[2];

  dir_sep[0] = DIR_SEPARATOR;
  dir_sep[1] = (char)0;

  strcpy (s, gm2_root);
  strcat (s, dir_sep);
  strcat (s, "lib");
  strcat (s, dir_sep);
  strcat (s, "gcc");
  strcat (s, dir_sep);
  strcat (s, DEFAULT_TARGET_MACHINE);
  strcat (s, dir_sep);
  strcat (s, DEFAULT_TARGET_VERSION);
  return s;
}

/*
 *  build_library_path - implements export
 *                       LIBRARY_PATH=$(gm2_root)/lib/gcc/\
 *                       $(default_target_machine)/\
 *                       $(default_target_version)
 *
 *                       where gm2_root, default_target_machine
 *                       and default_target_version are C
 *                       variables.
 */

static void
build_library_path (char *gm2_root)
{
  const char *gm2_root_path = gen_gm2_root (gm2_root);
  int l = strlen ("LIBRARY_PATH=") + strlen (gm2_root_path) + 1;
  char *s = (char *) xmalloc (l);

  strcpy (s, "LIBRARY_PATH=");
  strcat (s, gm2_root_path);
  putenv (s);
}

/*
 *  build_compiler_path - implements export
 *                        COMPILER_PATH=$(gm2_root)/libexec/gcc/\
 *                        $(default_target_machine)/\
 *                        $(default_target_version)
 *
 *                        where gm2_root, default_target_machine
 *                        and default_target_version are C
 *                        variables.
 */

static void
build_compiler_path (char *gm2_root)
{
  const char *gm2_root_path = gen_gm2_root (gm2_root);
  int l = strlen ("COMPILER_PATH=") + strlen (gm2_root_path) + 1;
  char *s = (char *) xmalloc (l);

  strcpy (s, "COMPILER_PATH=");
  strcat (s, gm2_root_path);
  putenv (s);
}

/*
 *  check_gm2_root - checks to see whether GM2_ROOT has been defined,
 *                   if it has and also COMPILER_PATH and LIBRARY_PATH
 *                   are both unset then it sets COMPILER_PATH and
 *                   LIBRARY_PATH using GM2_ROOT as its prefix.
 */

static void
check_gm2_root (void)
{
  const char *library_path;
  const char *compiler_path;
  char *gm2_root;

  library_path = getenv (LIBRARY_PATH_ENV);
  compiler_path = getenv ("COMPILER_PATH");
  gm2_root = getenv (GM2_ROOT_ENV);
  if ((library_path == NULL || (strcmp (library_path, "") == 0)) && 
      (compiler_path == NULL || (strcmp (compiler_path, "") == 0))) {
#if defined(DEBUGGING)
    fprintf(stderr, "STANDARD_LIBEXEC_PREFIX = %s\n", STANDARD_LIBEXEC_PREFIX);
    fprintf(stderr, "STANDARD_BINDIR_PREFIX = %s\n", STANDARD_BINDIR_PREFIX);
    fprintf(stderr, "TOOLDIR_BASE_PREFIX = %s\n", TOOLDIR_BASE_PREFIX);
    fprintf(stderr, "DEFAULT_TARGET_VERSION = %s\n", DEFAULT_TARGET_VERSION);
    fprintf(stderr, "DEFAULT_TARGET_MACHINE = %s\n", DEFAULT_TARGET_MACHINE);
#endif

    if (gm2_root != NULL && (strcmp (gm2_root, "") != 0)) {
      build_path (gm2_root);
      build_library_path (gm2_root);
      build_compiler_path (gm2_root);
    }
  }
  else if (gm2_root != NULL && !seen_fmakeall0)
    /*  
     *  no need to issue a warning if seen_fmakeall0 as the parent will
     *  have set COMPILER_PATH and LIBRARY_PATH because of GM2_ROOT and
     *  users should not be using -fmakeall0 as it is an internal option.
     */
    fprintf(stderr, "warning it is not advisible to set " GM2_ROOT_ENV
	    " as well as either " LIBRARY_PATH_ENV " or COMPILER_PATH\n");
}

/*
 *  lang_specific_driver - is invoked if we are compiling/linking a
 *                         Modula-2 file. It checks for module paths
 *                         and linking requirements which are language
 *                         specific.
 */

void
lang_specific_driver (struct cl_decoded_option **in_decoded_options,
		      unsigned int *in_decoded_options_count,
		      int *in_added_libraries)
{
  unsigned int i;

  /* Nonzero if we saw a `-xfoo' language specification on the
     command line.  This function will add a -xmodula-2 if the user
     has not already placed one onto the command line.  */
  int seen_x_flag = FALSE;
  const char *language = NULL;

  const char *libraries = NULL;
  const char *dialect = DEFAULT_DIALECT;

  int seen_module_extension = -1;
  int linking = TRUE;
  flag_set seen_shared_opt_flags = {FALSE, FALSE};
  styles shared_opt;
  int seen_source = FALSE;
  int seen_fexceptions = TRUE;
  const char *libpath;
  const char *gm2ipath;
  const char *gm2opath;

  /* By default, we throw on the math library if we have one.  */
  int need_math = (MATH_LIBRARY[0] != '\0');

  /* True if we should add -shared-libgcc to the command-line.  */
  int shared_libgcc = 1;

  /* The total number of arguments with the new stuff.  */
  unsigned int argc;

  argc = *in_decoded_options_count;

  /* initially scan the options for key values.  */
  for (i = 1; i < argc; i++) {
    if ((*in_decoded_options)[i].errors & CL_ERR_MISSING_ARG)
      continue; /* Avoid examining arguments of options missing them.  */

    switch ((*in_decoded_options)[i].opt_index)
      {
      case OPT_fexceptions:
	seen_fexceptions = ((*in_decoded_options)[i].value);
	break;
      case OPT_fonlylink:
	seen_fonlylink = TRUE;
	break;
      case OPT_fmakeall:
	seen_fmakeall = TRUE;
	break;
      case OPT_fmakeall0:
	seen_fmakeall0 = TRUE;
	break;
      case OPT_B:
	seen_B = TRUE;
	B_path = (*in_decoded_options)[i].arg;
	break;
      }
  }
  /*
   *  -fmakeall implies that the first invoked driver only does the link and should
   *  leave all compiles to the makefile otherwise we will try and link two main
   *  applications.
   */
  if (seen_fmakeall && (! seen_fonlylink))
    fe_generate_option (OPT_fonlylink, NULL, FALSE);

  check_gm2_root ();
  libpath = getenv (LIBRARY_PATH_ENV);
  if (libpath == NULL || (strcmp (libpath, "") == 0))
    libpath = LIBSUBDIR;

  gm2ipath = getenv (GM2IPATH_ENV);
  gm2opath = getenv (GM2OPATH_ENV);

#if defined(DEBUGGING)
  for (i = 0; i < *in_decoded_options_count; i++)
    printOption("at beginning", in_decoded_options, i);
#endif
  i = 1;
  for (i = 1; i < *in_decoded_options_count; i++) {
    const char *arg = (*in_decoded_options)[i].arg;
    size_t opt = (*in_decoded_options)[i].opt_index;

    if ((opt == OPT_c) || (opt == OPT_S))
      linking = FALSE;
    if (opt == OPT_I) {
      fe_generate_option (OPT_I, arg, TRUE);
      inclPos = i;
    }
    if (opt == OPT_fobject_path_)
      linkPos = i;
    if (opt == OPT_fiso)
      dialect = "iso";
    if (opt == OPT_fpim2)
      dialect = "pim2";
    if (opt == OPT_fpim3)
      dialect = "pim3";
    if (opt == OPT_fpim4)
      dialect = "pim4";
    if (opt == OPT_fpim)
      dialect = "pim";
    if (opt == OPT_flibs_)
      libraries = arg;
    if (opt == OPT_fmod_)
      seen_module_extension = TRUE;
    if ((opt == OPT_version) || (opt == OPT_fgm2_version))
      gm2_version();
    if (opt == OPT_x) {
      seen_x_flag = TRUE;
      language = arg;
    }
    if (opt == OPT_fshared)
      seen_shared_opt_flags.shared = TRUE;
    if ((opt == OPT_O) && ((*in_decoded_options)[i].value >= 2))
      seen_shared_opt_flags.o2 = TRUE;
    if (opt == OPT_SPECIAL_input_file)
      seen_source = TRUE;
    if ((opt == OPT_SPECIAL_ignore) && (is_object(arg)))
      remember_object (arg);
  }
  if (linking && (! seen_source))
    linking = FALSE;

  if (language != NULL && (strcmp (language, "modula-2") != 0))
    return;
#if defined(DEBUGGING)
  for (i = 0; i < *in_decoded_options_count; i++)
    printOption("in the middle", in_decoded_options, i);
#endif

  /*
   *  work out which libraries to use
   */
  if (libraries == NULL) {
    if (strncmp (dialect, "pim", 3) == 0)
      libraries = "pim";
    else if (strcmp (dialect, "iso") == 0)
      libraries = "iso,pim";
  }

  if (inclPos != -1 && linkPos == -1) {
#if defined(DEBUGGING)
    printf("inclPos = %d,  linkPos = %d\n", inclPos, linkPos);
#endif

    linkPos = 1;

    convert_include_into_link (in_decoded_options,
			       in_decoded_options_count);
  }
  shared_opt = get_style (seen_shared_opt_flags);

  add_default_includes (libpath, libraries, gm2ipath);
  add_exec_prefix ();

#if 1
  if (! seen_B)
    add_B_prefix (in_decoded_options_count, in_decoded_options, get_prefix ());
#endif

#if defined(DEBUGGING)
  for (i = 0; i < *in_decoded_options_count; i++)
    printOption("after B prefix", in_decoded_options, i);
#endif

  if (linkPos == -1)
    {
      linkPos = 1;
      if (inclPos == -1)
	add_default_fobjects (NULL, libpath,
			      libraries, shared_opt, gm2opath);
      else
	{
	  struct cl_decoded_option *options = *in_decoded_options;
	  const char *prev = options[inclPos].arg;
	  
	  add_default_fobjects (prev, libpath,
				libraries, shared_opt, gm2opath);
	}
    }

  if ((! seen_x_flag) && seen_module_extension)
    fe_generate_option (OPT_x, "modula-2", FALSE);

  if (linking) {
    add_default_archives (libpath, shared_opt, libraries, in_decoded_options_count, in_decoded_options, *in_decoded_options_count);
    (*in_added_libraries)++;

    if (need_math) {
      add_library (MATH_LIBRARY, in_decoded_options_count, in_decoded_options, *in_decoded_options_count);
      (*in_added_libraries)++;
    }

    if (seen_fexceptions) {
      add_library ("stdc++", in_decoded_options_count, in_decoded_options, *in_decoded_options_count);
      (*in_added_libraries)++;
    }
  /* There's no point adding -shared-libgcc if we don't have a shared
     libgcc.  */
#if !defined(ENABLE_SHARED_LIBGCC)
    shared_libgcc = 0;
#endif

    if (shared_libgcc) {
      fe_generate_option (OPT_shared_libgcc, NULL, FALSE);
      add_library ("gcc_eh", in_decoded_options_count, in_decoded_options, *in_decoded_options_count);
    }
  }
  scan_for_link_args (in_decoded_options_count, in_decoded_options);

#if defined(DEBUGGING)
  for (i = 0; i < *in_decoded_options_count; i++)
    printOption("before include purge", in_decoded_options, i);
#endif
  purge_include_options (in_decoded_options_count, in_decoded_options);
#if defined(DEBUGGING)
  for (i = 0; i < *in_decoded_options_count; i++)
    printOption("after include purge", in_decoded_options, i);
#endif


#if defined(DEBUGGING)
  for (i = 0; i < *in_decoded_options_count; i++)
    printOption("at end", in_decoded_options, i);
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
    len = strlen(result);
    alen = strlen(o->name);
    result = (char *)xrealloc (result, len+alen+1+1);
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

/*
 *  add_exec_dir - prepends the exec path to the given executable filename.
 */

static const char *
add_exec_dir (int argc, const char *argv[])
{
  if (argc == 1 && argv[0] != NULL) {
    char *path = gen_gm2_root (get_prefix ());
    if (path != NULL) {
      char *opt = (char *) xmalloc (strlen ("-fcppprog=") + strlen (path) + 1 + strlen (argv[0]) + 1);
      char *sep = alloca (2);
      
      sep[0] = DIR_SEPARATOR;
      sep[1] = (char)0;

      strcpy (opt, "-fcppprog=");
      strcat (opt, path);
      strcat (opt, sep);
      strcat (opt, argv[0]);
      return opt;
    }
  }
  return "-fcppprog=none";
}

/*  add_exec_name - generate binary name.
 */

static const char *
add_exec_name (int argc, const char *argv[])
{
  if (argc == 1 && argv[0] != NULL)
    return argv[0];
  return "";
}

/*
 *  lang_register_spec_functions - register the Modula-2 associated spec functions.
 */

void lang_register_spec_functions (void)
{
  fe_add_spec_function ("objects", get_objects);
  fe_add_spec_function ("nolink", no_link);
  fe_add_spec_function ("linkargs", get_link_args);
  fe_add_spec_function ("exec_prefix", add_exec_dir);
  fe_add_spec_function ("exec_name", add_exec_name);
}


/* Table of language-specific spec functions.  */ 
const struct spec_function lang_specific_spec_functions[] =
{
  { NULL, NULL }
};
