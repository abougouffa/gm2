/* Definitions for switches for Modula-2.
   Copyright (C) 1997 Free Software Foundation, Inc.

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
along with GNU Modula-2; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#define LINK_COMMAND_SPEC ""

/* This is the contribution to the `documented_lang_options' array in
   toplev.c for gm2.  */

DEFINE_LANG_NAME ("Modula-2")

  { "-I",
      N_("specifies a library path (C syntax)") },
  { "-Wiso",
      N_("use ISO dialect of Modula-2 (currently just defines SYSTEM.def)") },
  { "-Wpim",
      N_("use PIM [234] dialect of Modula-2 (currently just defines SYSTEM.def)") },
  { "-Wbounds",
      N_("turns on runtime subrange, array index and indirection via NIL pointer checking") },
  { "-Wreturn",
      N_("turns on runtime checking for functions which finish without executing a RETURN statement") },
  { "-Wstudents",
      N_("extra compile time semantic checking, typically tries to catch bad style") },
  { "-Wpedantic",
      N_("compiler checks nested WITH statements (referencing same type) and multiple identical imports") },
  { "-Wextended-opaque",
      N_("allows opaque types to be implemented as any type (a GNU Modula-2 extension)") },
  { "-Wuselist",
      N_("use ordered list of modules when linking") },
  { "-Wmakelist",
      N_("created ordered list of modules") },
  { "-Wmodules",
      N_("display list of modules and location") },
  { "-Wmakeall",
      N_("build a project using make") },
  { "-Wmakeall0",
      N_("do not build a project using make (internal)") },
  { "-Wmake-I=",
      N_("build project path (internal)") },
  { "-Wcpp",
      N_("use cpp to preprocess the module") },
  { "-Wq",
      N_("debugging information - dump the list of quadruples") },
  { "-Wverbose",
      N_("display the location of module source files as they are compiled") },
  { "-version",
      N_("display the version of the GNU Modula-2 front end") },
  { "-O",
      N_("optimize code") },
