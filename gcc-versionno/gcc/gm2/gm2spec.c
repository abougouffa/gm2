/* Specific flags and argument handling of the GNU Modula-2 front-end.
 * Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
 *               2010, 2011
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
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "gcc.h"
#include "defaults.h"

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

#include "gm2/gm2config.h"
#include "gm2/gm2version.h"

#undef DEBUGGING

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

static void add_arg (int incl, char ***in_argv, const char *str);
static void insert_arg (int incl, int *in_argc, char ***in_argv);
int lang_specific_pre_link (void);
static void add_exec_prefix(int, int *in_argc, char ***in_argv);
static void add_B_prefix (int pos, int *in_argc, char ***in_argv);
static const char *get_objects (int argc, const char *argv[]);
static const char *get_link_args (int argc, const char *argv[]);
static const char *add_exec_dir (int argc, const char *argv[]);
static void remove_arg (int i, int *in_argc, const char ***in_argv);
static int is_object (const char *s);
static void remember_object (const char *s);
static int is_link_arg (const char *s);
static void remember_link_arg (const char *s);
static void scan_for_link_args (int *in_argc, const char *const **in_argv);
static styles get_style (flag_set flags);
static void add_link_from_include (int link, char **in_argv[],
                                   int incl, const char *option);
static void add_lib (int *in_argc, const char *const **in_argv, const char *lib);
static void check_gm2_root (void);
static void add_include (int incl, const char ***in_argv, const char *libpath, const char *library);



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

/* By default, the suffix for target object files is ".o".  */
#ifdef TARGET_OBJECT_SUFFIX
#define HAVE_TARGET_OBJECT_SUFFIX
#else
#define TARGET_OBJECT_SUFFIX ".o"
#endif


/*
 *  find_executable_path - if argv0 references an executable filename then use
 *                         this path.
 */

static
char *find_executable_path (char *argv0)
{
  if (access (argv0, X_OK) == 0)
    {
      char *n = strrchr (argv0, DIR_SEPARATOR);

      /* strip of the program name from argv0, but leave the DIR_SEPARATOR */
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
void add_B_prefix (int pos, int *in_argc, char ***in_argv)
{
  char *path = find_executable_path ((*in_argv)[0]);
  
  if (path != NULL)
    {
      char *prefix;
      /* insert -B */
      insert_arg (pos, in_argc, in_argv);
      prefix = (char *) alloca (strlen("-B") + strlen (path) + 1);
      strcpy (prefix, "-B");
      strcat (prefix, path);
      (*in_argv)[pos] = xstrdup (prefix);
    }
}

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
  prefix = (char *) alloca (strlen ("-ftarget-ar=") +
			    strlen (ar) + 1);
  strcpy (prefix, "-ftarget-ar=");
  strcat (prefix, ar);
  (*in_argv)[pos] = xstrdup (prefix);

  /* and now insert ranlib */
  insert_arg(pos, in_argc, in_argv);
  prefix = (char *) alloca(strlen("-ftarget-ranlib=") +
				    strlen(ranlib) + 1);
  strcpy (prefix, "-ftarget-ranlib=");
  strcat (prefix, ranlib);
  (*in_argv)[pos] = xstrdup (prefix);
}

static void
add_arg (int incl, char ***in_argv, const char *str)
{
  if ((*in_argv)[incl] == NULL)
      (*in_argv)[incl] = xstrdup(str);
  else
    fprintf(stderr, "%s:%d:internal error not adding to a non empty space\n",
	    __FILE__, __LINE__);
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
 *  add_lib - add, lib, to the end of the command line.
 */

static void
add_lib (int *in_argc, const char *const **in_argv, const char *lib)
{
  int end = *in_argc;

  if (lib == NULL || (strcmp (lib, "") == 0))
    return;
  insert_arg (end, in_argc, (char ***)in_argv);
  add_arg (end, (char ***)in_argv, lib);
}

/*
 *  getArchiveName - return the corresponding archive name given the library name.
 */

static const char *
getArchiveName (const char *library)
{
  libs i;

  for (i=iso; i<maxlib; i++)
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
      int l = strlen("-L") + strlen (libpath) + 1 +
	strlen("lib") + 1 + strlen("gcc") + 1 +
	strlen (DEFAULT_TARGET_MACHINE) + 1 +
	strlen (DEFAULT_TARGET_VERSION) + 1 +
	strlen (style_name) + 1 +
	strlen (libdir) + 1;
      char *s = (char *) xmalloc (l);
      char dir_sep[2];

      dir_sep[0] = DIR_SEPARATOR;
      dir_sep[1] = (char)0;
    
      strcpy (s, "-L");
      strcat (s, libpath);
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
      char *s = (char *) xmalloc (strlen ("-l") + strlen (a) + 1);
      strcpy (s, "-l");
      strcat (s, a);
      return s;
    }
  }
  return NULL;
}

/*
 *  add_default_combination - adds the correct link path and then the library name.
 */

static void
add_default_combination (int *in_argc,
			 const char *const **in_argv,
			 const char *libpath,
			 const char *library,
			 styles style)
{
  if (library != NULL) {
    add_lib (in_argc, in_argv, build_archive_path (libpath, library, style));
    add_lib (in_argc, in_argv, build_archive (library));
  }
}

/*
 *  add_default_archives - adds the default archives to the end of the current
 *                         command line.
 */

static void
add_default_archives (int incl,
		      int *in_argc,
		      const char *const **in_argv,
		      const char *libpath,
		      styles s,
		      const char *libraries)
{
  const char *l = libraries;
  const char *e;
  const char *c;

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
    add_default_combination (in_argc, in_argv, libpath, c, s);
    add_include (incl, in_argv, libpath, c);
    free((void *)c);
  } while ((l != NULL) && (l[0] != (char)0));
}

/*
 *  build_include_path - builds the component of the include path referenced by
 *                       the, which, libs.
 */

static const char *
build_include_path (const char *prev, const char *libpath, const char *library)
{
  char  sepstr[2];
  char *gm2libs;
  const char *option = "-I";

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
 *  do_set - providing that path is non null and contains characters replace
 *           the existing include path with path.
 */

static void
do_set (int i,
	const char ***in_argv,
	const char *path)
{
  if (path != NULL && (strlen (path) != 0))
    (*in_argv)[i] = path;
}

/*
 *  add_include - add the correct include path given the libpath and library.
 */

static void
add_include (int incl,
	     const char ***in_argv,
	     const char *libpath,
	     const char *library)
{
  const char *prev = (char *)(*in_argv)[incl];

  if (library != NULL)
    do_set (incl, in_argv, build_include_path (prev, libpath, library));
}

/*
 *  add_default_includes - add the appropriate default include paths depending
 *                         upon the style of libraries chosen.
 */

static void
add_default_includes (int incl,
		      const char ***in_argv,
		      const char *libpath,
		      const char *libraries,
		      const char *envpath)
{
  const char *l = libraries;
  const char *e;
  const char *c;

  do_set (incl, in_argv, envpath);
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
    add_include (incl, in_argv, libpath, c);
    free((void *)c);
  } while ((l != NULL) && (l[0] != (char)0));
}

/*
 *  build_fobject_path - returns a string containing the a path to the
 *                       objects defined by, libpath, s, and, dialectLib.
 */

static const char *
build_fobject_path (const char *prev, const char *libpath, const char *library,
		    styles style)
{
  char  sepstr[2];
  char *gm2objs;
  const char *option = "-fobject-path=";
  const char *style_name = libraryStyle[style].directory;
  const char *libName = library;

  sepstr[0] = DIR_SEPARATOR;
  sepstr[1] = (char)0;

  if (prev == NULL) {
    gm2objs = (char *) alloca(strlen(option) +
			      strlen(libpath)+strlen(sepstr)+strlen("gm2")+strlen(sepstr)+strlen(libName)+1+
			      strlen(libpath)+strlen(sepstr)+strlen("gm2")+strlen(sepstr)+strlen(libName)+1);
    strcpy(gm2objs, option);
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
 *  add_fobject - add all required path to satisfy the link for library.
 */

static void
add_fobject (int incl,
	     const char ***in_argv,
	     const char *libpath,
	     const char *library,
	     styles s)
{
  const char *prev = (char *)(*in_argv)[incl];

  if (library != NULL)
    do_set (incl, in_argv, build_fobject_path (prev, libpath, library, s));
}

/*
 *  add_default_fobjects - add the appropriate default include paths depending
 *                         upon the style of libraries chosen.
 */

static void
add_default_fobjects (int incl,
		      const char ***in_argv,
		      const char *libpath,
		      const char *libraries,
		      styles s,
		      const char *envpath)
{
  const char *l = libraries;
  const char *e;
  const char *c;

  do_set (incl, in_argv, envpath);
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
    add_fobject (incl, in_argv, libpath, c, s);
    free(c);
  } while ((l != NULL) && (l[0] != (char)0));
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

  GET_ENVIRONMENT (path, "PATH");
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
  int l = strlen ("LIBRARY_PATH=") + strlen (gm2_root) + 1 +
    strlen("lib") + 1 + strlen("gcc") + 1 +
    strlen (DEFAULT_TARGET_MACHINE) + 1 +
    strlen (DEFAULT_TARGET_VERSION) + 1;
  char *s = (char *) xmalloc (l);
  char dir_sep[2];

  dir_sep[0] = DIR_SEPARATOR;
  dir_sep[1] = (char)0;

  strcpy (s, "LIBRARY_PATH=");
  strcat (s, gm2_root);
  strcat (s, dir_sep);
  strcat (s, "lib");
  strcat (s, dir_sep);
  strcat (s, "gcc");
  strcat (s, dir_sep);
  strcat (s, DEFAULT_TARGET_MACHINE);
  strcat (s, dir_sep);
  strcat (s, DEFAULT_TARGET_VERSION);
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
  int l = strlen ("COMPILER_PATH=") + strlen (gm2_root) + 1 +
    strlen("libexec") + 1 + strlen("gcc") + 1 +
    strlen (DEFAULT_TARGET_MACHINE) + 1 +
    strlen (DEFAULT_TARGET_VERSION) + 1;
  char *s = (char *) xmalloc (l);
  char dir_sep[2];

  dir_sep[0] = DIR_SEPARATOR;
  dir_sep[1] = (char)0;

  strcpy (s, "COMPILER_PATH=");
  strcat (s, gm2_root);
  strcat (s, dir_sep);
  strcat (s, "libexec");
  strcat (s, dir_sep);
  strcat (s, "gcc");
  strcat (s, dir_sep);
  strcat (s, DEFAULT_TARGET_MACHINE);
  strcat (s, dir_sep);
  strcat (s, DEFAULT_TARGET_VERSION);
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

  GET_ENVIRONMENT (library_path, LIBRARY_PATH_ENV);
  GET_ENVIRONMENT (compiler_path, "COMPILER_PATH");
  GET_ENVIRONMENT (gm2_root, GM2_ROOT_ENV);
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
lang_specific_driver (int *in_argc, const char *const **in_argv,
		      int *in_added_libraries ATTRIBUTE_UNUSED)
{
  int i;
  const char *libraries = NULL;
  const char *dialect = DEFAULT_DIALECT;
  int x=-1;
  const char *language = NULL;
  int moduleExtension = -1;
  int linking = TRUE;
  flag_set seen_flags = {FALSE, FALSE};
  styles s;
  int seen_source = FALSE;
  int seen_fno_exceptions = FALSE;
  const char *libpath;
  const char *gm2ipath;
  const char *gm2opath;

  i=1;
  while (i<*in_argc) {
    if (strcmp((*in_argv)[i], "-fno-exceptions") == 0)
      seen_fno_exceptions = TRUE;
    if (strcmp((*in_argv)[i], "-fonlylink") == 0)
      seen_fonlylink = TRUE;
    if (strcmp((*in_argv)[i], "-fmakeall") == 0)
      seen_fmakeall = TRUE;
    if (strcmp((*in_argv)[i], "-fmakeall0") == 0)
      seen_fmakeall0 = TRUE;
    if (strlen((*in_argv)[i]) >= 1 && strncmp((*in_argv)[i], "-B", 2) == 0)
      seen_B = TRUE;
    i++;
  }
  /*
   *  -fmakeall implies that the first invoked driver only does the link and should
   *  leave all compiles to the makefile otherwise we will try and link two main
   *  applications.
   */
  if (seen_fmakeall && (! seen_fonlylink)) {
    insert_arg (1, in_argc, (char ***)in_argv);
    add_arg(1, (char ***)in_argv, "-fonlylink");
  }

  check_gm2_root ();
  GET_ENVIRONMENT (libpath, LIBRARY_PATH_ENV);
  if (libpath == NULL || (strcmp (libpath, "") == 0))
    libpath = LIBSUBDIR;

  GET_ENVIRONMENT (gm2ipath, GM2IPATH_ENV);
  GET_ENVIRONMENT (gm2opath, GM2OPATH_ENV);

#if defined(DEBUGGING)
  i=0;
  while (i<*in_argc) {
    printf("in lang specific driver argv[%d] = %s\n", i, (*in_argv)[i]);
    i++;
  }
#endif
  i=1;
  while (i<*in_argc) {
    if ((strcmp((*in_argv)[i], "-c") == 0) || (strcmp((*in_argv)[i], "-S") == 0))
      linking = FALSE;
    if ((strncmp((*in_argv)[i], "-I", strlen("-I")) == 0) &&
	(strcmp((*in_argv)[i], "-I-") != 0))
      inclPos = i;
    if (strncmp((*in_argv)[i], "-fobject-path=", strlen("-fobject-path=")) == 0)
      linkPos = i;
    if (strncmp((*in_argv)[i], "-fiso", strlen("-fiso")) == 0)
      dialect = "iso";
    if (strncmp((*in_argv)[i], "-fpim2", strlen("-fpim2")) == 0)
      dialect = "pim2";
    if (strncmp((*in_argv)[i], "-fpim3", strlen("-fpim3")) == 0)
      dialect = "pim3";
    if (strncmp((*in_argv)[i], "-fpim4", strlen("-fpim4")) == 0)
      dialect = "pim4";
    if (strncmp((*in_argv)[i], "-fpim", strlen("-fpim")) == 0)
      dialect = "pim";
    if (strncmp((*in_argv)[i], "-flibs=", strlen("-flibs=")) == 0)
      libraries = (*in_argv)[i] + strlen("-flibs=");
    if (strncmp((*in_argv)[i], "-fmod=", strlen("-fmod=")) == 0)
      moduleExtension = i;
    if ((strcmp((*in_argv)[i], "--version") == 0) ||
	(strcmp((*in_argv)[i], "-fversion") == 0))
      gm2_version();
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
    if ((strcmp((*in_argv)[i], "-") == 0) ||
	((*in_argv)[i][0] != '-'))
      seen_source = TRUE;
    if (strcmp((*in_argv)[i], "-o") == 0)
      i++;
    else if (is_object((*in_argv)[i]))
      remember_object ((*in_argv)[i]);
    i++;
  }
  if (linking && (! seen_source))
    linking = FALSE;

  if (language != NULL && (strcmp (language, "modula-2") != 0))
    return;
#if defined(DEBUGGING)
  i=0;
  while (i<*in_argc) {
    printf("middle lang specific driver argv[%d] = %s\n", i, (*in_argv)[i]);
    i++;
  }
#endif
  /*
   *  work out which libraries to use
   */
  if (libraries == NULL) {
    if (strncmp (dialect, "pim", 2) == 0)
      libraries = "pim";
    else if (strcmp (dialect, "iso") == 0)
      libraries = "iso,pim";
  }

  if (inclPos != -1 && linkPos == -1) {
#if defined(DEBUGGING)
    printf("inclPos = %d,  linkPos = %d\n", inclPos, linkPos);
#endif
    insert_arg (1, in_argc, (char ***)in_argv);
    linkPos = 1;
    add_link_from_include (linkPos, (char ***)in_argv, inclPos, "-fobject-path=");
  }
  if (inclPos == -1) {
    insert_arg (1, in_argc, (char ***)in_argv);
    inclPos = 1;
  }
  s = get_style (seen_flags);
  
  add_default_includes (inclPos, (char ***)in_argv, libpath,
			libraries, gm2ipath);

  add_exec_prefix (1, in_argc, (char ***)in_argv);

  if ((! seen_B) && seen_fmakeall)
    add_B_prefix (1, in_argc, (char ***)in_argv);

  if (linkPos == -1) {
    insert_arg (1, in_argc, (char ***)in_argv);
    linkPos = 1;
  }
  add_default_fobjects (linkPos, (char ***)in_argv, libpath,
			libraries, s, gm2opath);

  if (x == -1 && moduleExtension != -1) {
    insert_arg (1, in_argc, (char ***)in_argv);
    add_arg(1, (char ***)in_argv, "modula-2");
    insert_arg (1, in_argc, (char ***)in_argv);
    add_arg(1, (char ***)in_argv, "-x");
  }
  if (linking) {
    add_default_archives (inclPos, in_argc, in_argv, libpath, s, libraries);
    add_lib (in_argc, in_argv, MATH_LIBRARY);
    if (! seen_fno_exceptions)
      add_lib (in_argc, in_argv, "-lstdc++");
#if defined(ENABLE_SHARED_LIBGCC)
    insert_arg (1, in_argc, (char ***)in_argv);
    add_arg (1, (char ***)in_argv, "-shared-libgcc");
    add_lib (in_argc, in_argv, "-lgcc_eh");
#endif
  }
  scan_for_link_args (in_argc, in_argv);
#if defined(DEBUGGING)
  i=0;
  while (i<*in_argc) {
    printf("out lang specific driver argv[%d] = %s\n", i, (*in_argv)[i]);
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
  if (argc == 1 && argv[0] != NULL)
    return find_executable (argv[0]);
  return "";
}

/* Table of language-specific spec functions.  */ 
const struct spec_function lang_specific_spec_functions[] =
{
  { "objects", get_objects},
  { "nolink", no_link},
  { "linkargs", get_link_args},
  { "exec_prefix", add_exec_dir},
  { NULL, NULL }
};