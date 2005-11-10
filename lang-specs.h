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
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* This is the contribution to the `default_compilers' array in gcc.c for
   GNU Modula-2.  */

  {".mod", "@modula-2", 0},
  {"@modula-2",
      "%{c|S:%{Wuselist:%{!Wmodules:%eGNU Modula-2 does not know what to do with -Wuselist given these arguments}} \
           %{!Wmakelist:%{!Wmodules:%{!gm2gcc:%{Wcpp:cc1gm2 -Wcppbegin cc1%s -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -Wcppend \
                                                     %(cc1_options) %{f*} %{+e*} %{I*} %{MD} %{MMD} %{M} %{MM} %{MA} %{MT*} %{MF*} %i \
                                                     %{!fsyntax-only:%{!S:-o %{|!pipe:%g.s} |\n\
                                                     as %(asm_options) %{!pipe:%g.s} %A }}} \n\
                                               %{!Wcpp:cc1gm2 %(cc1_options) %{f*} %{+e*} %{I*} %{MD} %{MMD} %{M} %{MM} %{MA} %{MT*} %{MF*} %i \
                                                     %{!fsyntax-only:%{!S:-o %{|!pipe:%g.s} |\n\
                                                     as %(asm_options) %{!pipe:%g.s} %A }}}}}} \n\
           %{Wmakelist:%{Wcpp:cc1%s -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) %g.mod \n\
                              gm2l %{I*} %{Wdef=*} %{Wmod=*} -o %b.lst %g.mod} \n\
                       %{!Wcpp:gm2l %{I*} %{Wdef=*} %{Wmod=*} -o %b.lst %i}} \n\
           %{Wmodules:%{!Wuselist:%{Wcpp:cc1%s -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) %g.mod \n\
                                         gm2l %{I*} %{Wdef=*} %{Wmod=*} %{!pipe:-o %g.l} %g.mod |\n\
                                         gm2lsub %{!pipe:%g.l} -o %g.lst \n\
                                         gm2lcc %{Wtarget-ar=*} %{Wtarget-ranlib=*} %{I*} %{v} -c %g.lst} \n\
                                 %{!Wcpp:gm2l %{I*} %{Wdef=*} %{Wmod=*} %{!pipe:-o %g.l} %i |\n\
                                         gm2lsub %{!pipe:%g.l} -o %g.lst \n\
                                         gm2lcc %{Wtarget-ar=*} %{Wtarget-ranlib=*} %{I*} %{v} -c %g.lst}} \n\
                       %{Wuselist:gm2lcc %{Wtarget-ar=*} %{Wtarget-ranlib=*} %{I*} %{v} -c %b.lst}}} \n\
      %{!c:%{Wmakelist:%eGNU Modula-2 does not support -Wmakelist without -c}} \n\
      %{!c:%{Wmodules:%eGNU Modula-2 does not support -Wmodules without -c}} \n\
      %{!c:%{Wmakeall:%{!Wmakeall0:%{Wcpp:gm2m -Wcppbegin cc1%s -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -Wcppend -nolink -Wgm2begin -Wmakeall0 %{g*} %{v*} %{O*} %{W*} %{D*} %{f*} %{I*} -Wgm2end -o %g.m %i \n\
                                     make -r -f %g.m } \n\
                                   %{!Wcpp:gm2m -nolink -Wgm2begin -Wmakeall0 %{g*} %{v*} %{O*} %{W*} %{D*} %{f*} %{I*} -Wgm2end -o %g.m %i \n\
                                   make -r -f %g.m }}}} \n\
      %{!c:%{!S:%{!gm2gcc:%{!Wuselist:%{Wcpp:cc1%s -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) %g.mod \n\
                                             gm2l -Wcppbegin cc1%s -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -Wcppend %{I*} %{Wdef=*} %{Wmod=*} %{!pipe:-o %g.l} %g.mod |\n\
                                             gm2lsub %{!pipe:%g.l} -o %g.lst \n\
                                             gm2lgen %g.lst -o %g.c \n\
                                             gcc %{v*} %{B*} %{g*} -c -o %d%w%g%O %g.c \n\
                                             rm -f %w%d%g.a \n\
                                             gm2lcc %{Wtarget-ar=*} %{Wtarget-ranlib=*} %{I*} %{v} -exec -ar -startup %w%g%O -o %w%d%g.a %g.lst} \n\
                                      %{!Wcpp:gm2l %{I*} %{Wdef=*} %{Wmod=*} %{!pipe:-o %g.l} %i |\n\
                                             gm2lsub %{!pipe:%g.l} -o %g.lst \n\
                                             gm2lgen %g.lst -o %g.c \n\
                                             gcc %{v*} %{B*} %{g*} -c -o %d%w%g%O %g.c \n\
                                             rm -f %w%d%g.a \n\
                                             gm2lcc %{Wtarget-ar=*} %{Wtarget-ranlib=*} %{I*} %{v} -exec -ar -startup %w%g%O -o %w%d%g.a %g.lst}} \n\
                           %{Wuselist:gm2lgen %b.lst -o %g.c \n\
                                      gcc %{v*} %{B*} %{g*} -c -o %d%w%g%O %g.c \n\
                                      rm -f %w%d%g.a \n\
                                      gm2lcc %{Wtarget-ar=*} %{Wtarget-ranlib=*} %{I*} %{v} -exec -ar -startup %w%g%O -o %w%d%g.a %b.lst}}}} \n\
    ", 0},
