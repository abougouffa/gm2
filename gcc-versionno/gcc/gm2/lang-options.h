/* Definitions for switches for Modula-2.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
 *               2010
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
along with GNU Modula-2; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#define LINK_COMMAND_SPEC ""

/* This is the contribution to the `documented_lang_options' array in
   toplev.c for gm2.  */

DEFINE_LANG_NAME ("Modula-2")

  { "-I",
      N_("specifies a library path (C syntax)") },
  { "-fiso",
      N_("use ISO dialect of Modula-2") },
  { "-fpim",
      N_("use PIM [234] dialect of Modula-2") },
  { "-fpim2",
      N_("use PIM 2 dialect of Modula-2") },
  { "-fpim3",
      N_("use PIM 3 dialect of Modula-2") },
  { "-fpim4",
      N_("use PIM 4 dialect of Modula-2") },
  { "-fpositive-mod-floor-div",
      N_("force positive result from MOD and DIV result floor") },
  { "-flibs=ulm",
      N_("use the University of Ulm libraries and PIM dialect of Modula-2") },
  { "-flibs=min",
      N_("use a minimal SYSTEM, M2RTS and libc and no other system libraries") },
  { "-flibs=logitech",
      N_("use the Logitech compatible (PIM dialect) set of libraries") },
  { "-flibs=pim-coroutine",
      N_("use the PIM dialect libraries which include coroutine support") },
  { "-fnil", 
      N_("turns on runtime checking to detect accessing data through a NIL value pointer") },
  { "-fno-nil", 
      N_("turns off runtime checking to detect accessing data through a NIL value pointer") },
  { "-fwholediv",
      N_("generate code to detect whole number division by zero or modulus by zero") },
  { "-fno-wholediv",
      N_("do not generate code to detect whole number division by zero or modulus by zero") },
  { "-findex",
      N_("generate code to check whether array index values are out of bounds") },
  { "-fno-index",
      N_("do not generate code to check whether array index values are out of bounds") },
  { "-frange",
      N_("generate code to check the assignment range, return value range, set range and constructor range") },
  { "-fno-range",
      N_("do not generate code to check the assignment range, return value range, set range and constructor range") },
  { "-freturn",
      N_("turns on runtime checking for functions which finish without executing a RETURN statement") },
  { "-fno-return",
      N_("turns off runtime checking for functions which finish without executing a RETURN statement") },
  { "-fcase",
      N_("turns on runtime checking to check whether a CASE statement requires an ELSE clause when on was not specified") },
  { "-fno-case",
      N_("turns off runtime checking to check whether a CASE statement requires an ELSE clause when on was not specified") },
  { "-fsoft-check-all",
      N_("turns on all runtime checking (an abbreviation for -fnil -frange -findex -fwholediv -fcase -freturn -fwholediv)") },
  { "-fno-soft-check-all",
      N_("turns off all runtime checking (an abbreviation for -fno-nil -fno-range -fno-index -fno-wholediv -fno-case -fno-return -fwholediv)") },
  { "-fexceptions",
      N_("turns on all exception generating code (this is on by default)") },
  { "-fno-exceptions",
      N_("turns off all exception generating code, this flag should be used with -flibs=min") },
  { "-Wstudents",
      N_("extra compile time semantic checking, typically tries to catch bad style") },
  { "-Wpedantic",
      N_("compiler checks nested WITH statements (referencing same type) and multiple identical imports") },
  { "-Wpedantic-param-names",
      N_("compiler checks to force definition module procedure parameter names with their implementation module counterpart") },
  { "-Wpedantic-cast",
      N_("compiler warns if a cast is being used on types of differing sizes") },
  { "-fextended-opaque",
      N_("allows opaque types to be implemented as any type (a GNU Modula-2 extension)") },
  { "-fverbose-unbounded",
      N_("inform user which parameters will be passed by reference") },
  { "-fxcode",
      N_("issue all errors and warnings in the Xcode format") },
  { "-fuselist",
      N_("use ordered list of modules when linking") },
  { "-fmakelist",
      N_("created ordered list of modules") },
  { "-fmodules",
      N_("display list of modules and location") },
  { "-fruntime-modules=",
      N_("specify the list of runtime modules and their initialization order") },
  { "-fclean",
      N_("cleans all the project objects in the current directory using make") },
  { "-fmakeall",
      N_("build a project using make") },
  { "-fmakeall0",
      N_("do not build a project using make (internal)") },
  { "-fmake-I=",
      N_("build project path (internal)") },
  { "-ftarget-ar=",
      N_("full path to target archiver") },
  { "-ftarget-ranlib=",
      N_("full path to target ranlib") },
  { "-fcpp",
      N_("use cpp to preprocess the module") },
  { "-fq",
      N_("debugging information - dump the list of quadruples") },
  { "-fsources",
      N_("display the location of module source files as they are compiled") },
  { "-funbounded-by-reference",
      N_("optimize non var unbounded parameters by passing it by reference, providing it is not written to within the callee procedure.") },
  { "-fdef=",
      N_("recognise the specified suffix as a definition module filename") },
  { "-fmod=",
      N_("recognise the specified suffix as implementation and module filenames") },
  { "-fdump-system-exports",
      N_("display all inbuilt system items") },
  { "-fswig",
      N_("generate a swig interface file") },
  { "-fshared",
      N_("generate a shared library from the module") },
  { "-fmakeinit",
      N_("generate the start up C code for the module, a file _m2_modulename.c is created") },
  { "-fobject-path=",
      N_("set the object path") },
  { "-fonlylink",
      N_("only link the module and do not compile module (internal)"),
  { "--version",
      N_("display the GNU Modula-2 version") },
  { "-fversion",
      N_("display the GNU Modula-2 version") },
  { "-fgm2-version",
      N_("display the GNU Modula-2 version") },
  { "-O",
      N_("optimize code") },


  /*
   *  --fixme-- add these when gm2 can detect integer/cardinal overflow in expressions
   *  and add "-fwholevalue" to the list of options contained in -fsoft-check-all 
   *
   *  { "-fwholevalue",
   *   N_("generate code to detect whole number overflow and underflow") },
   *  { "-fno-wholevalue",
   *   N_("do not generate code to detect whole number overflow and underflow") },
   */