/* Definitions for specs for GNU Modula-2.
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Gaius Mulley.

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

  {".mod", "@modula-2" },
  {".def", "@modula-2" },
  {"@modula-2",
      "%{c:%{Wuselist:%{!Wmodules:%eGNU Modula-2 does not know what to do with -Wuselist given these arguments}} \
           %{!Wmakelist:%{!Wmodules:%{!gm2gcc:%{Wcpp:tradcpp0 -lang-asm %(cpp_options) -C -o %g.mod \n\
                                                     cc1gm2 %(cc1_options) %{+e*} %{I*} %{MD} %{MMD} %{M} %{MM} %g.mod \
                                                     %{!fsyntax-only:%{!S:-o %{|!pipe:%g.s} |\n\
                                                     as %(asm_options) %{!pipe:%g.s} %A }}} \n\
                                               %{!Wcpp:cc1gm2 %(cc1_options) %{+e*} %{I*} %{MD} %{MMD} %{M} %{MM} %B \
                                                     %{!fsyntax-only:%{!S:-o %{|!pipe:%g.s} |\n\
                                                     as %(asm_options) %{!pipe:%g.s} %A }}}}}} \n\
           %{Wmakelist:gm2l -o %b.lst %b} \n\
           %{Wmodules:%{!Wuselist:gm2l %{!pipe:-o %g.l} %b|\n\
                                  gm2lsub %{!pipe:%g.l} -o %g.lst \n\
                                  gm2lcc %{v} -c %g.lst} \n\
                       %{Wuselist:gm2lcc %{v} -c %b.lst}}} \n\
      %{!c:%{Wmakelist:%eGNU Modula-2 does not support -Wmakelist without -c}} \
      %{!c:%{Wmodules:%eGNU Modula-2 does not support -Wmodules without -c}} \
      %{!c:%{!S:%{!gm2gcc:%{!Wuselist:gm2l %{!pipe:-o %g.l} %b|\n\
                                      gm2lsub %{!pipe:%g.l} -o %g.lst \n\
                                      gm2lgen %g.lst -o %g.c \n\
                                      gcc -g -c -o %d%w%g%O %g.c \n\
                                      rm -f %w%d%g.a \n\
                                      gm2lcc %{v} -exec -ar -startup %w%g%O -o %w%d%g.a %g.lst} \n\
                           %{Wuselist:gm2lgen %b.lst -o %g.c \n\
                                      gcc -g -c -o %d%w%g%O %g.c \n\
                                      rm -f %w%d%g.a \n\
                                      gm2lcc %{v} -exec -ar -startup %w%g%O -o %w%d%g.a %b.lst}}}} \n\
    "},
