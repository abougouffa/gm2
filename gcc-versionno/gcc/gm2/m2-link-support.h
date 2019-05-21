/* Link support specs for GNU Modula-2.
   Copyright (C) 2019 Free Software Foundation, Inc.
   Contributed by Gaius Mulley.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


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
          %{!fshared:--ar %:objects() %:noobjects() -o %w%d%g.a } \
          " OBJECT " \
          %{fshared:%w%{o:%{o*}}%:nolink() %:objects() %:noobjects() %:linkargs() } " LST " "

#define GM2LORDER_GEN_CC(INPUT)	\
  "gm2lorder %{fruntime-modules=*} " INPUT " -o %g.lst \n\
   gm2lgen %{fshared} %{fshared:--terminate --exit} \
           %{!fno-exceptions:-fcpp} %g.lst -o %{g:%b_m2.cpp;:%g.cpp} \n\
  " GM2CC("%{g:%b_m2.cpp;:%g.cpp}","%ustart%d%O") " \n\
   rm -f %w%d%g.a \n\
  " GM2LCC("--mainobject %b.o","%g.lst") " \n\
   rm -f %Ustart"

/* Pass the preprocessor options on the command line together with the
   exec prefix.  */

#define M2CPP "%{fcpp:-fcppbegin %:exec_prefix(cc1)" \
              "      -E -lang-asm -traditional-cpp " \
              "      -quiet %(cpp_unique_options) -fcppend}"

/* Generate a list of topologically sorted dependent modules.  */

#define GM2L(INPUT,OUTPUT) \
  "gm2l %{v} " M2CPP " %{I*} %{fdef=*} %{fmod=*} " OUTPUT " " INPUT " "

/* Run the compiler using standard GNU options.  */

#define CC1GM2 "cc1gm2 " M2CPP " %(cc1_options) %{f*} %{+e*} %{I*} " \
               " %{MD} %{MMD} %{M} %{MM} %{MA} %{MT*} %{MF*} %V"

/*  Generate a swig interface file and exit.  */

#define SWIG "%{fswig:" CC1GM2 "%i \n\
                 %:exit()}"

/* Generate a basename.lst containing a list of all dependent modules for
   the project and exit.  */

#define MAKELIST "%{fmakelist:" GM2L("%i","%{!pipe:-o %g.l}") " |\n\
                     gm2lorder %{fruntime-modules=*} %{!pipe:%g.l} -o %b.lst \n\
                     %:exit()}"

/* Generate a scaffold from basename.lst and store the output source into
   _m2_basename.cpp.  */

#define MAKEINIT "%{fmakeinit:gm2lgen %{fshared} %{fshared:--terminate --exit} \
                     %{!fno-exceptions:-fcpp} %b.lst -o _m2_%b.cpp} \n\
                     %:exit()}"

/* Display the filesystem location of the all object files in the project list.  */

#define REPORT_OBJECTS "gm2lcc %{fshared} %{fpic} %{fPIC} %{B*} %{L*} \
                        %{ftarget-ar=*} %{ftarget-ranlib=*} %{fobject-path=*} %{v} -c "

/* Generate a list of modules used within a project and report the object file location.  */

#define MODULES  "%{fmodules:%{fuselist:" REPORT_OBJECTS " %b.lst}" \
                             "%{!fuselist:" GM2L("%g.mod","%{!pipe:-o %g.l}") " |\n\
                                            gm2lorder %{fruntime-modules=*} %{!pipe:%g.l} -o %g.lst \n\
                                          " REPORT_OBJECTS " %g.lst} \n\
                   %:exit()}"

#define MODULA_PROJECT_SUPPORT  SWIG MAKELIST MAKEINIT MODULES

#define MODULA_LINK_SUPPORT  " "

#if 0
      "%{c|S:%{fuselist:%{fsources:%eGNU Modula-2 cannot use -fsources and -fuselist at the same time}} \
           %{!fmakelist:%{!fmodules:%{!fmakeinit:%{!fswig:%{!gm2gcc:cc1gm2 %{fcpp:-fcppbegin %:exec_prefix(cc1) -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -fcppend} \
                                                                           %(cc1_options) %{f*} %{+e*} %{I*} %{MD} %{MMD} %{M} %{MM} %{MA} %{MT*} %{MF*} %i \
                                                                           %{!fsyntax-only:%(invoke_as)}}}}}} \n\
           %{fswig:cc1gm2 %{fcpp:-fcppbegin %:exec_prefix(cc1) -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -fcppend} \
                                                                           %(cc1_options) %{f*} %{+e*} %{I*} %{MD} %{MMD} %{M} %{MM} %{MA} %{MT*} %{MF*} %V %i} \n\
           %{fmakelist:%{fcpp:cc1 -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -o %g.mod \n\
                            " GM2L("%g.mod","%{!pipe:-o %g.l}") " |\n\
                              gm2lorder %{fruntime-modules=*} %{!pipe:%g.l} -o %b.lst} \n\
                       %{!fcpp:" GM2L("%i","%{!pipe:-o %g.l}") " |\n\
                               gm2lorder %{fruntime-modules=*} %{!pipe:%g.l} -o %b.lst}} \n\
           %{fmakeinit:gm2lgen %{fshared} %{fshared:--terminate --exit} %{!fno-exceptions:-fcpp} %b.lst -o _m2_%b.cpp} \n\
           %{fmodules:%{!fuselist:%{fcpp:cc1 -E -lang-asm -traditional-cpp -quiet %(cpp_unique_options) -o %g.mod \n\
                                       " GM2L("%g.mod","%{!pipe:-o %g.l}") " |\n\
                                         gm2lorder %{fruntime-modules=*} %{!pipe:%g.l} -o %g.lst \n\
                                         gm2lcc %{fshared} %{fpic} %{fPIC} %{B*} %{L*} %{ftarget-ar=*} %{ftarget-ranlib=*} %{fobject-path=*} %{v} -c %g.lst} \n\
                                 %{!fcpp:" GM2L("%i","%{!pipe:-o %g.l}") " |\n		\
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
