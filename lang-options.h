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

/* This is the contribution to the `lang_options' array in gcc.c for
   gm2.  */

/* #include "gcc-version.h" */
/* was #ifdef EGCS */
#if 1
DEFINE_LANG_NAME ("GNU Modula-2")
#define GM2_OPT(NAME, DESCRIPTION) { NAME , DESCRIPTION }
#else
#define GM2_OPT(NAME, DESCRIPTION) NAME
#endif
  GM2_OPT("-I", "specifies a library path (C syntax)"),
  GM2_OPT("-M", "specifies a library path (space seperated list in quotes)"),
  GM2_OPT("-bounds", "turns on runtime subrange, array index and indirection via NIL pointer checking"),
  GM2_OPT("-return", "turns on runtime checking for functions which finish without executing a RETURN statement"),
