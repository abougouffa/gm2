/* Definitions for specs for Modula-2.
   Copyright (C) 1999 Free Software Foundation, Inc.

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

/* This is the contribution to the `default_compilers' array in gcc.c for
   GNU Modula-2.  */

  {".mod", {"@modula-2"}},
  {"@modula-2",
   "cc1gm2 %1 %2\
       %{!Q:-quiet} %{d*} %{m*} %{a}\
       %{I*} %{g*} %{O*} %{W*} %{w}\
       %{M}\
       %{v:-version} %{pg:-p} %{p} %{q} %{students}\
       %{statistics} %{pedantic} %{verbose} %{!v:%{version}}\
       %{f*} %{+e*} %{aux-info*}\
       %{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
       %{S:%W{o*}%{!o*:-o %b.s}}%{!S:-o %{|!pipe:%g.s}}\
       %b|\n\
   %{!S:as %a %Y\
       %{c:%W{o*}%{!o*:-o %w%b%O}}%{!c:-o %d%w%u%O}\
       %{!pipe:%g.s} %A\n }"},
