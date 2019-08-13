/* Link support specs for GNU Modula-2.
   Copyright (C) 2019 Free Software Foundation, Inc.
   Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

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


/* The subprograms used by Modula-2 to facilitate linking are:
   gm2l - parses the top level module and all other dependent
          modules.  It creates a dependency tree and emits a list
          of dependent modules.
   gm2lcc - generates a link command for all dependent modules or
            display module object filesystem location.
   gm2m - parses the top level module and all other dependent
          module and generates a Makefile from the import tree.
   gm2lgen - generate a C or C++ scaffold from the list of modules.
   gm2lorder - manipulate the dependent list of modules to force
               critcal runtime modules to be initialized at the
               beginning of the init sequence.  */

/* AS run the assembler with default options.  */

#define AS(INPUT,OUTPUT)  "as %a %Y " INPUT " -o " OUTPUT

/* GM2CC_OPTIONS a list of all CC options used by gm2 during the
   link scaffold generation.  */

#define GM2CC_OPTIONS "%{v*} %{m*} %{g*} %{O*} %{fPIC} %{fpic} \
                       %{+e*} %{I*} %{MD} %{MMD} %{M} %{MM} %{MA} \
                       %{MT*} %{MF*} -quiet "

/* GM2CC compile the link scaffold either with the C or C++
   compiler.  */

#define GM2CC(INPUT,OUTPUT) \
  "%{!fno-exceptions:cc1plus;:cc1} " GM2CC_OPTIONS " " INPUT " \
     -o %d%b_m2.s \n\
  " AS("%b_m2.s",OUTPUT) " "

/* GM2LCC invoke the sub program gm2lcc with the object path options
   and user supplied objects.  It will search for Modula-2 object
   if they are not already present on the command line.  The
   current module is contained in OBJECT and LST is a list of all
   the dependant modules.  */

#define GM2LCC(OBJECT,LST) \
  "gm2lcc %{fshared} %{fpic} %{fPIC} %{B*} %{L*} %{ftarget-ar=*} \
          %{ftarget-ranlib=*} \
          %{fobject-path=*} %{v} --exec --startup %Ustart%d%O \
          %{!fshared:--ar %:objects() %:noobjects() -o %w%d%g.a } \
          " OBJECT " \
          %{fshared:%w%{o:%{o*}}%:nolink() %:objects() %:noobjects() \
            %:linkargs() } " LST " "

/* GM2LORDER run the gm2lorder sub program.  It generates a module
   list LST by parsing INPUT and all dependant modules.  */

#define GM2LORDER(INPUT,LST) \
  "gm2lorder %{fruntime-modules=*} " INPUT " -o " LST " \n"

/* GM2LGEN run the gm2lgen sub program which generates a C or C++
   scaffold.  Compile the scaffold and link it with all dependant
   modules in LST.  MAIN is the main module object.  */

#define GM2LGEN(LST,CPP,MAIN) \
  "gm2lgen %{fshared} %{fshared:--terminate --exit} \
           %{!fno-exceptions:-fcpp} " LST " -o " CPP " \n\
   " GM2L_COMBINE(LST,CPP,MAIN)

/* GM2L_COMBINE compiles the scaffold CPP and links all dependant
   modules in LST.  MAIN is the main module object.  */

#define GM2L_COMBINE(LST,CPP,MAIN) \
  GM2CC(CPP,"%ustart%d%O") " \n\
   rm -f %w%d%g.a \n\
  " GM2LCC("--mainobject " MAIN,LST) " \n\
   rm -f %Ustart"

/* Pass the preprocessor options on the command line together with
   the exec prefix.  */

#define M2CPP "%{fcpp:-fcppbegin %:exec_prefix(cc1)" \
              "      -E -lang-asm -traditional-cpp " \
              "      -quiet %(cpp_unique_options) -fcppend}"

/* Generate a list of topologically sorted dependent modules.  */

#define GM2L(INPUT,OUTPUT) \
  "gm2l %{v} " M2CPP " %{I*} %{fdef=*} %{fmod=*} " OUTPUT " " INPUT " "

/* General GNU options.  */

#define GENERAL_OPTIONS "%{f*} %{+e*} %{I*} %{MD} %{MMD} %{M} %{MM} \
 %{MA} %{MT*} %{MF*} %V"

/* Run the compiler using standard GNU options.  */

#define CC1GM2 "cc1gm2 " M2CPP " %(cc1_options) " GENERAL_OPTIONS

/*  Generate a swig interface file and exit.  */

#define SWIG "%{fswig:" CC1GM2 "%i \n\
                 %:exit()}"

/* Generate a basename.lst containing a list of all dependent modules
   for the project and exit.  */

#define MAKELIST "%{fmakelist:" GM2L("%i","%{!pipe:-o %g.l}") " |\n\
                     gm2lorder %{fruntime-modules=*} %{!pipe:%g.l} \
                       -o %b.lst \n\
                     %:exit()}"

/* Generate a scaffold from basename.lst and store the output source
   into _m2_basename.cpp and exit.  */

#define MAKEINIT "%{fmakeinit:gm2lgen %{fshared} \
                     %{fshared:--terminate --exit} \
                     %{!fno-exceptions:-fcpp} %b.lst -o _m2_%b.cpp \n\
                     %:exit()}"

/* Display the filesystem location of the all object files in the
   project list.  */

#define REPORT_OBJECTS "gm2lcc %{fshared} %{fpic} %{fPIC} %{B*} %{L*} \
                        %{ftarget-ar=*} %{ftarget-ranlib=*} \
                        %{fobject-path=*} %{v} -c "

/* Generate a list of modules used within a project and report the
   object file location and exit.  */

#define MODULES \
  "%{fmodules:%{fuselist:" REPORT_OBJECTS " %b.lst}" \
               "%{!fuselist:" GM2L("%g.mod","%{!pipe:-o %g.l}") " |\n\
                              gm2lorder %{fruntime-modules=*} \
                                 %{!pipe:%g.l} -o %g.lst \n\
                            " REPORT_OBJECTS " %g.lst} \n\
     %:exit()}"

/* MODULA_PROJECT_SUPPORT contains a list of all project support
   sub components.  */

#define MODULA_PROJECT_SUPPORT  SWIG MAKELIST MAKEINIT MODULES

/* GM2M generate a Makefile from the module import dependancies.  */

#define GM2M "gm2m %{fcpp:%{B*} %{fmake-I*}} " M2CPP \
                 " -nolink -fgm2begin -fmakeall0 %(cc1_options) " \
                    GENERAL_OPTIONS \
                 " -fgm2end -o %b.make %i \n" \
             "%:exit()"

/* MAKEALL invoke GM2M if the user requested -fmakeall, -fmakeall0 is
   interanlly added by gm2m and thus recursive calls using -fmakeall
   from make are dismissed.  */

#define MAKEALL "%{fmakeall:%{!fmakeall0:" GM2M "}}"

/* GM2 invoke cc1gm2 placing assembler output into OUTPUT given
   source file, INPUT.  */

#define GM2(INPUT,OUTPUT) CC1GM2 " -o " OUTPUT " " INPUT

/* M2LINK compile main module (providing absense of -fonlylink)
   and link all project dependent modules.  */

#define M2LINK \
  "%{!S:%{!gm2gcc:%{!fonlylink:" GM2("%{fcpp:%g.mod;:%i}","%d%g.s") " \n\
                               " AS("%g.s","%b.o") " } \n\
                   %{!fuselist:" GM2L("%{fcpp:%g.mod;:%i}"," -o %g.l") " \n\
                               " GM2LORDER("%g.l","%g.lst") " \n\
                  " GM2LGEN("%{fuselist:%b.lst;:%g.lst}",\
			    "%{g:%b_m2.cpp;:%g.cpp}","%b.o") "}}}"

/* MODULA_LINK_SUPPORT only invoke link subprocesses if no -c option.  */

#define MODULA_LINK_SUPPORT  "%{!c:" MAKEALL M2LINK "}"
