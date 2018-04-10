/* Definitions for specs for GNU Modula-2.
 * Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
                 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018
                 Free Software Foundation, Inc.
   Contributed by Gaius Mulley.

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

/* This is the contribution to the `default_compilers' array in gcc.c for
   GNU Modula-2.  */

#define AS(INPUT,OUTPUT)  "as %a %Y " INPUT " -o " OUTPUT

#define GM2CC_OPTIONS "%{v*} %{m*} %{g*} %{O*} %{fPIC} %{fpic} \
                       %{+e*} %{I*} %{MD} %{MMD} %{M} %{MM} %{MA} \
                       %{MT*} %{MF*} -quiet "

#define GM2CC(INPUT,OUTPUT) \
  "%{!fno-exceptions:cc1plus;:cc1} " GM2CC_OPTIONS " " INPUT " -o %b_m2.s \n\
  " AS("%b_m2.s",OUTPUT) " "

#define GM2LCC(OBJECT,LST) \
  "gm2lcc %{fshared} %{fpic} %{fPIC} %{B*} %{L*} %{ftarget-ar=*} %{ftarget-ranlib=*} \
          %{fobject-path=*} %{v} --exec --startup %Ustart%d%O \
          %{!fshared:--ar -o %w%d%g.a} \
          " OBJECT " \
          %{fshared:%w%{o:%{o*}}%:nolink() %:objects() %:linkargs() } " LST " "

#define GM2LORDER_GEN_CC(INPUT)	\
  "gm2lorder %{fruntime-modules=*} " INPUT " -o %g.lst \n\
   gm2lgen %{fshared} %{fshared:--terminate --exit} %{!fno-exceptions:-fcpp} %g.lst -o %{g:%b_m2.cpp;:%g.cpp} \n\
  " GM2CC("%{g:%b_m2.cpp;:%g.cpp}","%ustart%d%O") " \n\
   rm -f %w%d%g.a \n\
  " GM2LCC("%b.o","%g.lst") " \n\
   rm -f %Ustart"

#define GM2L(INPUT,OUTPUT) \
  "gm2l %{fcpp:-fcppbegin %:exec_prefix(cc1) -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -fcppend} \
        %{I*} %{fdef=*} %{fmod=*} " OUTPUT " " INPUT " "


#if 1
  {".mod", "@modula-2", 0, 0, 0},
  {"@modula-2",
      "%{c|S:%{fuselist:%{fsources:%eGNU Modula-2 cannot use -fsources and -fuselist at the same time}} \
           %{!fmakelist:%{!fmodules:%{!fmakeinit:%{!fswig:%{!gm2gcc:cc1gm2 %{fcpp:-fcppbegin %:exec_prefix(cc1) -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -fcppend} \
                                                                           %(cc1_options) %{f*} %{+e*} %{I*} %{MD} %{MMD} %{M} %{MM} %{MA} %{MT*} %{MF*} %i \
                                                                           %{!fsyntax-only:%(invoke_as)}}}}}} \n\
           %{fswig:cc1gm2 %{fcpp:-fcppbegin %:exec_prefix(cc1) -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -fcppend} \
                                                                           %(cc1_options) %{f*} %{+e*} %{I*} %{MD} %{MMD} %{M} %{MM} %{MA} %{MT*} %{MF*} %V %i} \n\
           %{fmakelist:%{fcpp:cc1 -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -o %g.mod \n\
                              gm2l %{I*} %{fdef=*} %{fmod=*} %{!pipe:-o %g.l} %g.mod |\n\
                              gm2lorder %{fruntime-modules=*} %{!pipe:%g.l} -o %b.lst} \n\
                       %{!fcpp:gm2l %{I*} %{fdef=*} %{fmod=*} %{!pipe:-o %g.l} %i |\n\
                               gm2lorder %{fruntime-modules=*} %{!pipe:%g.l} -o %b.lst}} \n\
           %{fmakeinit:gm2lgen %{fshared} %{fshared:--terminate --exit} %{!fno-exceptions:-fcpp} %b.lst -o _m2_%b.cpp} \n\
           %{fmodules:%{!fuselist:%{fcpp:cc1 -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -o %g.mod \n\
                                         gm2l %{I*} %{fdef=*} %{fmod=*} %{!pipe:-o %g.l} %g.mod |\n\
                                         gm2lorder %{fruntime-modules=*} %{!pipe:%g.l} -o %g.lst \n\
                                         gm2lcc %{fshared} %{fpic} %{fPIC} %{B*} %{L*} %{ftarget-ar=*} %{ftarget-ranlib=*} %{fobject-path=*} %{v} -c %g.lst} \n\
                                 %{!fcpp:gm2l %{I*} %{fdef=*} %{fmod=*} %{!pipe:-o %g.l} %i |\n\
                                         gm2lorder %{fruntime-modules=*} %{!pipe:%g.l} -o %g.lst \n\
                                         gm2lcc %{fshared} %{fpic} %{fPIC} %{B*} %{L*} %{ftarget-ar=*} %{ftarget-ranlib=*} %{fobject-path=*} %{v} -c %g.lst}} \n\
                       %{fuselist:gm2lcc %{fshared} %{fpic} %{fPIC} %{B*} %{L*} %{ftarget-ar=*} %{ftarget-ranlib=*} %{fobject-path=*} %{v} -c %b.lst}}} \n\
      %{!c:%{fmakelist:%eGNU Modula-2 does not support -fmakelist without -c}} \n\
      %{!c:%{fmodules:%eGNU Modula-2 does not support -fmodules without -c}} \n\
      %{!c:%{fmakeinit:%eGNU Modula-2 does not support -fmakeinit without -c}} \n\
      %{!c:%{fswig:%eGNU Modula-2 does not support -fswig without -c}} \n\
      %{!c:%{fmakeall:%{!fmakeall0:%{fcpp:gm2m %{B*} %{fmake-I*} -fcppbegin %:exec_prefix(cc1) -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -fcppend -nolink -fgm2begin -fmakeall0 %{B*} %{g*} %{v*} %{O*} %{W*} %{D*} %{f*} %{I*} -fgm2end -o %g.m %i \n\
                                     %{fclean:make -r -f %g.m clean %b } \n\
                                     %{!fclean:make -r -f %g.m }} \n\
                                   %{!fcpp:gm2m -nolink %{B*} %{fmake-I*} -fgm2begin -fmakeall0 %{B*} %{g*} %{v*} %{O*} %{W*} %{D*} %{f*} %{I*} -fgm2end -o %g.m %i \n\
                                     %{fclean:make -r -f %g.m clean %b } \n\
                                     %{!fclean:make -r -f %g.m }}}}} \n\
      %{!c:%{!S:%{!gm2gcc:%{!fuselist:%{fcpp:cc1 -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -o %g.mod} \n\
                                      %{!fonlylink:cc1gm2 %{fcpp:-fcppbegin %:exec_prefix(cc1) -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -fcppend} \
                                                          %(cc1_options) %{f*} %{+e*} %{I*} %{MD} %{MMD} %{M} %{MM} %{MA} %{MT*} %{MF*} -o %d%g.s %{fcpp:%g.mod;:%i} \n\
                                                 " AS("%g.s","%b.o") " } \n\
                                      " GM2L("%{fcpp:%g.mod;:%i}","%{!pipe:-o %g.l}") " |\n\
                                      " GM2LORDER_GEN_CC("%{!pipe:%g.l}") " } \n \
                           %{fuselist:gm2lgen %{fshared} %{fshared:--terminate --exit} %{!fno-exceptions:-fcpp} %b.lst -o %{!g:%g.cpp} %{g:%b_m2.cpp} \n\
                                      " GM2CC("%{!g:%g.cpp} %{g:%b_m2.cpp}","%ustart%d%O") " \n\
                                      rm -f %w%d%g.a \n\
                                      " GM2LCC("","%b.lst") " \n\
                                      rm -f %Ustart }}}} \n\
    ", 0, 0, 0},
#endif
