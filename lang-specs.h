/* Definitions for specs for GNU Modula-2.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
                 2009
   Free Software Foundation, Inc.
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

  {".mod", "@modula-2", 0, 0, 0},
  {"@modula-2",
      "%{c|S:%{fuselist:%{fsources:%eGNU Modula-2 does not know what to do with -fsources and -fuselist}} \
           %{!fmakelist:%{!fmodules:%{!gm2gcc:%{fcpp:cc1gm2 -fcppbegin %:exec_prefix(cc1%s) -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -fcppend \
                                                     %(cc1_options) %{f*} %{+e*} %{I*} %{MD} %{MMD} %{M} %{MM} %{MA} %{MT*} %{MF*} %i \
                                                     %{!fsyntax-only:%(invoke_as)}} \n\
                                               %{!fcpp:cc1gm2 %(cc1_options) %{f*} %{+e*} %{I*} %{MD} %{MMD} %{M} %{MM} %{MA} %{MT*} %{MF*} %i \
                                                     %{!fsyntax-only:%(invoke_as)}}}}} \n\
           %{fmakelist:%{fcpp:cc1%s -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -o %g.mod \n\
                              gm2l %{I*} %{fdef=*} %{fmod=*} %{!pipe:-o %g.l} %g.mod |\n\
                              gm2lorder %{fruntime-modules=*} %{!pipe:%g.l} -o %b.lst} \n\
                       %{!fcpp:gm2l %{I*} %{fdef=*} %{fmod=*} %{!pipe:-o %g.l} %i |\n\
                               gm2lorder %{fruntime-modules=*} %{!pipe:%g.l} -o %b.lst}} \n\
           %{fmakeinit:gm2lgen %{fshared} %{fshared:-terminate -exit} %{!fno-exceptions:-cpp} %b.lst -o _m2_%b.cpp} \n\
           %{fmodules:%{!fuselist:%{fcpp:cc1%s -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -o %g.mod \n\
                                         gm2l %{I*} %{fdef=*} %{fmod=*} %{!pipe:-o %g.l} %g.mod |\n\
                                         gm2lorder %{fruntime-modules=*} %{!pipe:%g.l} -o %g.lst \n\
                                         gm2lcc %{fshared} %{fpic} %{fPIC} %{B*} %{ftarget-ar=*} %{ftarget-ranlib=*} %{fobject-path=*} %{v} -c %g.lst} \n\
                                 %{!fcpp:gm2l %{I*} %{fdef=*} %{fmod=*} %{!pipe:-o %g.l} %i |\n\
                                         gm2lorder %{fruntime-modules=*} %{!pipe:%g.l} -o %g.lst \n\
                                         gm2lcc %{fshared} %{fpic} %{fPIC} %{B*} %{ftarget-ar=*} %{ftarget-ranlib=*} %{fobject-path=*}  %{v} -c %g.lst}} \n\
                       %{fuselist:gm2lcc %{fshared} %{fpic} %{fPIC} %{B*} %{ftarget-ar=*} %{ftarget-ranlib=*} %{fobject-path=*} %{v} -c %b.lst}}} \n\
      %{!c:%{fmakelist:%eGNU Modula-2 does not support -fmakelist without -c}} \n\
      %{!c:%{fmodules:%eGNU Modula-2 does not support -fmodules without -c}} \n\
      %{!c:%{fmakeinit:%eGNU Modula-2 does not support -fmakeinit without -c}} \n\
      %{!c:%{fmakeall:%{!fmakeall0:%{fcpp:gm2m -fcppbegin %:exec_prefix(cc1%s) -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -fcppend -nolink -fgm2begin -fmakeall0 %{g*} %{v*} %{O*} %{W*} %{D*} %{f*} %{I*} -fgm2end -o %g.m %i \n\
                                     make -r -f %g.m } \n\
                                   %{!fcpp:gm2m -nolink -fgm2begin -fmakeall0 %{g*} %{v*} %{O*} %{W*} %{D*} %{f*} %{I*} -fgm2end -o %g.m %i \n\
                                   make -r -f %g.m }}}} \n\
      %{!c:%{!S:%{!gm2gcc:%{!fuselist:%{fcpp:cc1%s -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -o %g.mod \n\
                                             gm2l -fcppbegin %:exec_prefix(cc1%s) -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -fcppend %{I*} %{fdef=*} %{fmod=*} %{!pipe:-o %g.l} %g.mod |\n\
                                             gm2lorder %{fruntime-modules=*} %{!pipe:%g.l} -o %g.lst \n\
                                             gm2lgen %{fshared} %{fshared:-terminate -exit} %{!fno-exceptions:-cpp} %g.lst -o %{!g:%g.cpp} %{g:%b_m2.cpp} \n\
                                             gm2cc %{v*} %{B*} %{g*} %{O*} %{fPIC} %{fpic} %{fno-exceptions:-x c} -c -o %ustart%d%O %{!g:%g.cpp} %{g:%b_m2.cpp} \n\
                                             rm -f %w%d%g.a \n\
                                             gm2lcc %{fshared} %{fpic} %{fPIC} %{B*} %{ftarget-ar=*} %{ftarget-ranlib=*} \
                                                    %{fobject-path=*} %{v} -exec -startup %Ustart%d%O \
                                                    %{!fshared:-ar -o %w%d%g.a} \
                                                    %{fshared:%w%{o:%{o*}}%:nolink() %:objects() %:linkargs() } %g.lst } \n\
                                      %{!fcpp:gm2l %{I*} %{fdef=*} %{fmod=*} %{!pipe:-o %g.l} %i |\n\
                                             gm2lorder %{fruntime-modules=*} %{!pipe:%g.l} -o %g.lst \n\
                                             gm2lgen %{fshared} %{fshared:-terminate -exit} %{!fno-exceptions:-cpp} %g.lst -o %{!g:%g.cpp} %{g:%b_m2.cpp} \n\
                                             gm2cc %{v*} %{B*} %{g*} %{O*} %{fPIC} %{fpic} %{fno-exceptions:-x c} -c -o %ustart%d%O %{!g:%g.cpp} %{g:%b_m2.cpp} \n\
                                             rm -f %w%d%g.a \n\
                                             gm2lcc %{fshared} %{fpic} %{fPIC} %{B*} %{ftarget-ar=*} %{ftarget-ranlib=*} \
                                                    %{fobject-path=*} %{v} -exec -startup %Ustart%d%O \
                                                    %{!fshared:-ar -o %w%d%g.a} \
                                                    %{fshared:%w%{o:%{o*}}%:nolink() %:objects() %:linkargs() } %g.lst }} \n\
                           %{fuselist:gm2lgen %{fshared} %{fshared:-terminate -exit} %{!fno-exceptions:-cpp} %b.lst -o %{!g:%g.cpp} %{g:%b_m2.cpp} \n\
                                      gm2cc %{v*} %{B*} %{g*} %{O*} %{fPIC} %{fpic} %{fno-exceptions:-x c} -c -o %ustart%d%O %{!g:%g.cpp} %{g:%b_m2.cpp} \n\
                                      rm -f %w%d%g.a \n\
                                      gm2lcc %{fshared} %{fpic} %{fPIC} %{B*} %{ftarget-ar=*} %{ftarget-ranlib=*} \
                                             %{fobject-path=*} %{v} -exec -startup %Ustart%d%O \
                                             %{!fshared:-ar -o %w%d%g.a} \
                                             %{fshared:%w%{o:%{o*}}%:nolink() %:objects() %:linkargs() } %b.lst }}}} \n\
    ", 0, 0, 0},
