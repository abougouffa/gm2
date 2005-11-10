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
Boston, MA 02110-1301, USA.  */

#define LINK_COMMAND_SPEC ""

/* This is the contribution to the `documented_lang_options' array in
   toplev.c for gm2.  */

DEFINE_LANG_NAME ("Modula-2")

  { "-I",
      N_("specifies a library path (C syntax)") },
  { "-Wiso",
      N_("use ISO dialect of Modula-2") },
  { "-Wpim",
      N_("use PIM [234] dialect of Modula-2") },
  { "-Wpim2",
      N_("use PIM 2 dialect of Modula-2") },
  { "-Wpim3",
      N_("use PIM 3 dialect of Modula-2") },
  { "-Wpim4",
      N_("use PIM 4 dialect of Modula-2") },
  { "-Wpositive-mod-floor-div",
      N_("force positive result from MOD and DIV result floor") },
  { "-Wlibs=ulm",
      N_("use the University of Ulm libraries and PIM dialect of Modula-2") },
  { "-Wlibs=logitech",
      N_("use the Logitech compatible (PIM dialect) set of libraries") },
  { "-Wlibs=pim-coroutine",
      N_("use the PIM dialect libraries which include coroutine support") },
  { "-Wbounds",
      N_("turns on runtime subrange, array index and indirection via NIL pointer checking") },
  { "-Wreturn",
      N_("turns on runtime checking for functions which finish without executing a RETURN statement") },
  { "-Wnil", 
      N_("turns on runtime checking to detect accessing data through a NIL value pointer") },
  { "-Wcase",
      N_("turns on runtime checking to check whether a CASE statement requires an ELSE clause when on was not specified") },
  { "-Wcheck-all",
      N_("turns on all runtime checking (an abbreviation for -Wcase -Wbounds -Wreturn -Wnil)") },
  { "-Wstudents",
      N_("extra compile time semantic checking, typically tries to catch bad style") },
  { "-Wpedantic",
      N_("compiler checks nested WITH statements (referencing same type) and multiple identical imports") },
  { "-Wpedantic-param-names",
      N_("compiler checks to force definition module procedure parameter names with their implementation module counterpart") },
  { "-Wpedantic-cast",
      N_("compiler warns if a cast is being used on types of differing sizes") },
  { "-Wextended-opaque",
      N_("allows opaque types to be implemented as any type (a GNU Modula-2 extension)") },
  { "-Wverbose-unbounded",
      N_("inform user which parameters will be passed by reference") },
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
  { "-Wtarget-ar=",
      N_("full path to target archiver") },
  { "-Wtarget-ranlib=",
      N_("full path to target ranlib") },
  { "-Wcpp",
      N_("use cpp to preprocess the module") },
  { "-Wq",
      N_("debugging information - dump the list of quadruples") },
  { "-Wsources",
      N_("display the location of module source files as they are compiled") },
  { "-funbounded-by-reference",
      N_("optimize non var unbounded parameters by passing it by reference, providing it is not written to within the callee procedure.") },
  { "-Wdef=",
      N_("recognise the specified suffix as a definition module filename") },
  { "-Wmod=",
      N_("recognise the specified suffix as implementation and module filenames") },
  { "-version",
      N_("display the version of the GNU Modula-2 front end") },
  { "-O",
      N_("optimize code") },
