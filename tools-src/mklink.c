/* Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
 *               2009, 2010
 *               Free Software Foundation, Inc.
 * This file is part of GNU Modula-2.
 *
 * GNU Modula-2 is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 * 
 * GNU Modula-2 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with GNU Modula-2; see the file COPYING.  If not, write to the
 * Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA. 
 */

/*
 * Gaius Mulley <gaius@glam.ac.uk> constructed this file.
 *
 * Title      : mklink
 * Description: creates startup code and link command line given a list of files.
 * Start date : 9/6/93
 *
 *
 * $Header: /sources/gm2/gm2/tools-src/mklink.c,v 1.10 2013/02/11 14:45:21 gaius Exp $
 *
 * $Log: mklink.c,v $
 * Revision 1.10  2013/02/11 14:45:21  gaius
 * * gm2/Make-lang.in:  build rules changed to enable gm2 to be built
 *   with gcc-4.7.1.
 * * gm2/NEWS:  minor version number change.
 * * gm2/config-lang.in:  removed gcc-3.x.y version checking.
 * * gm2/configure:  rebuilt.
 * * gm2/configure.in:  updated as gccgm2.c is no longer used.
 * * gm2/gccgm2.c:  (removed).  The contents of this 12000 line file
 *   have been replaced by the modular equivalent in gm2/gm2-gcc/*.[ch]
 * * gm2/gm2-common.h:  (removed).
 * * gm2/gm2-common.c:  (removed).
 * * gm2/gm2-lang.c:  changed to use the gm2/gm2-gcc structure and gcc-4.7.1.
 * * gm2/gm2-lang.h:  changed to use the gm2/gm2-gcc structure and gcc-4.7.1.
 * * gm2/gm2-tree.def:  removed THROW_EXPR, TRY_BLOCK, HANDLER, EXPR_STMT
 *   definitions.
 * * gm2/gm2-tree.h:  commented out the language tree code.
 * * gm2/gm2.texi:  modified to reflect library command line switch changes
 *   between 1.0.1 and 1.0.4.  Improved the assembly language example
 *   together with an explanation.  Added alignment information.
 *   Updated the Solaris building instructions.
 * * gm2/gm2config.h.in:  (updated to reflect gcc-4.7.1).
 * * gm2/gm2spec.c:    (updated to reflect gcc-4.7.1).
 * * gm2/lang-options.h:  changed -fverbose-unbounded to
 *   -Wverbose-unbounded.
 * * gm2/lang.opt:  changed so that libraries dialects can be ordered.
 * * gm2/m2.flex:  updated to reflect gcc-4.7.1 and the new gm2/gm2-gcc
 *   structure.
 * * gm2/m2pp.c:  updated to reflect gcc-4.7.1 and the new gm2/gm2-gcc
 *   structure.
 * * gm2/p2crc:  added many more AvoidNames to avoid conflicts with C
 *   header files.
 * * gm2/version.c:  updated to 1.0.5.
 * * gm2/bnf/gm2l.bnf:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/bnf/gm2m.bnf:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/bnf/m2-2.bnf:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/bnf/m2-3.bnf:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/bnf/m2-c.bnf:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/bnf/m2-h.bnf:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/bnf/m2.bnf:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/el/g-mode.el:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/examples/gravity/README:  updated package versions required.
 * * gm2/examples/gravity/twoDsim.def:  updated dates.
 * * gm2/examples/gravity/twoDsim.mod:  updated dates and improved debugging.
 * * gm2/examples/gravity/doc/collision.ms:  reorganised document.
 * * gm2/examples/hello/Makefile.in:  modified build rules.
 * * gm2/examples/hello/hello.mod:  added blank line.
 * * gm2/examples/map/AdvMap.def:  removed BITSET import.
 * * gm2/gm2-compiler/Indexing.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2ALU.def:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2ALU.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Base.def:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Base.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Batch.def:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Batch.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Bitset.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2CaseList.def:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2CaseList.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Code.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Comp.def:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Comp.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Error.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2GCCDeclare.def:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2GCCDeclare.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2GenGCC.def:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2GenGCC.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2LexBuf.def:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2LexBuf.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2MetaError.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Options.def:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Options.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Preprocess.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Printf.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Quads.def:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Quads.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Range.def:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Range.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Reserved.def:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2Reserved.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2System.def:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/M2System.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/P2SymBuild.def:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/P2SymBuild.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/PCSymBuild.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/Sets.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/SymbolConversion.def:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/SymbolConversion.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/SymbolKey.def:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/SymbolKey.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/SymbolTable.def:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/SymbolTable.mod:  updated to reflect gcc-4.7.1 and the
 *   new gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/gccgm2.def:  updated to reflect gcc-4.7.1 and the new
 *   gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/gm2.mod:  updated to reflect gcc-4.7.1 and the new
 *   gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/m2flex.def:  updated to reflect gcc-4.7.1 and the new
 *   gm2/gm2-gcc structure.
 * * gm2/gm2-compiler/ppg.mod:  updated to reflect gcc-4.7.1 and the new
 *   gm2/gm2-gcc structure.
 * * gm2/gm2-libs/Builtins.def:  added alloca_trace for debugging.
 * * gm2/gm2-libs/Builtins.mod:  implemented alloca_trace for debugging.
 * * gm2/gm2-libs/Indexing.mod:  removed BITSET from imports.
 * * gm2/gm2-libs/M2RTS.def:  removed BITSET from imports.
 * * gm2/gm2-libs/SYSTEM.def:  removed BITSET from export list.
 * * gm2/gm2-libs/config-host:  rebuilt.
 * * gm2/gm2-libs/config-host.in:  updated version to 1.0.5.
 * * gm2/gm2-libs/config-target:  rebuilt.
 * * gm2/gm2-libs/config-target.in:  updated version to 1.0.5.
 * * gm2/gm2-libs-boot/SYSTEM.def:  removed BITSET from export list.
 * * gm2/gm2-libs-coroutines/SYSTEM.def:  removed BITSET from export list.
 * * gm2/gm2-libs-iso/README.texi:  updated the list of implemented ISO
 *   Modules and GNU Modules.
 * * gm2/gm2-libs-iso/SYSTEM.def:  export TBITSIZE.
 * * gm2/gm2-libs-iso/ServerSocket.mod:  added missing, RETURN TRUE.
 * * gm2/gm2-libs-iso/StdChans.mod:  changed to include 'raw' in the std
 *   file descriptors.
 * * gm2/gm2-libs-min/SYSTEM.def:  removed BITSET from export list.
 * * gm2/gm2-libs-pim/BitBlockOps.mod:  removed BITSET from import list.
 * * gm2/gm2-libs-pim/BitByteOps.mod:  removed BITSET from import list.
 * * gm2/gm2-libs-pim/BitWordOps.mod:  removed BITSET from import list.
 * * gm2/gm2-libs-pim/Random.def:  fixed RandomBytes definition.
 * * gm2/gm2-libs-pim/Random.mod:  fixed RandomBytes definition.
 * * gm2/init/gm2linit:  updated to reflect gcc-4.7.1 and the new gm2/gm2-gcc
 *   structure.
 * * gm2/init/gm2minit:  updated to reflect gcc-4.7.1 and the new gm2/gm2-gcc
 *   structure.
 * * gm2/p2c/p2c.h:  modified prototypes to use plain C.
 * * gm2/p2c/p2c-src/src/citmods.c:  use TOUPPER rather than toupper,
 *   use TOLOWER rather than tolower.
 * * gm2/p2c/p2c-src/src/decl.c:  test for WORD and use unsigned int.
 * * gm2/p2c/p2c-src/src/expr.c:  use TOLOWER, ISSPACE, ISDIGIT, ISALPHA
 *   rather than their lower case counterparts.
 * * gm2/p2c/p2c-src/src/funcs.c:  use TOUPPER rather than the lower
 *   case counterpart.
 * * gm2/p2c/p2c-src/src/lex.c:  register the uppercase functions:
 *   TOLOWER, TOUPPER, ISALPHA, ISDIGIT.
 * * gm2/p2c/p2c-src/src/p2clib.c:  use ISSPACE and TOUPPER rather than
 *   their lower case counterparts.
 * * gm2/tools-src/def2texi.py:  many changes to explicitly differentiate
 *   between the build and src directories.
 * * gm2/tools-src/mklink.c:  modified so that the default library is NULL and
 *   new --lib specifies the library.
 *   Also alter long options to use -- rather than -.
 * * gm2/ulm-lib-gm2/std/Archive.def:  remove BITSET from import.
 * * gm2/ulm-lib-gm2/std/M2RTS.mod:  remove BITSET from import.
 * * gm2/ulm-lib-gm2/std/MathLib.mod:  remove BITSET from import.
 * * gm2/ulm-lib-gm2/std/Plot.mod:  remove BITSET from import.
 * * gm2/ulm-lib-gm2/sys/SYSTEM.def:  remove BITSET from import.
 * * gm2/ulm-lib-gm2/sys/SysIoctl.def:  remove BITSET from import.
 * * gm2/ulm-lib-gm2/sys/SysStat.def:  remove BITSET from import.
 * * gm2/ulm-lib-gm2/sys/SysStat.mod:  remove BITSET from import.
 * * gm2/www/tools/texi2tr/html/download.html:  modified url to reference
 *   gm2-1.0.4.
 * * gm2/www/tools/texi2tr/html/release.html:  modified url to reference
 *   gm2-1.0.4.
 *
 * Revision 1.9  2010/10/03 19:01:19  gaius
 * * gm2/Make-lang.in:  tidied up Copyright dates on
 * * gm2/Makefile.in:  the following files.
 * * gm2/config-lang.in:
 * * gm2/gccgm2.c:
 * * gm2/gm2-common.c:
 * * gm2/gm2-common.h:
 * * gm2/gm2-lang.c:
 * * gm2/gm2-lang.h:
 * * gm2/gm2-tree.def:
 * * gm2/gm2-tree.h:
 * * gm2/gm2builtins.c:
 * * gm2/gm2config.h.in:
 * * gm2/gm2except.c:
 * * gm2/gm2spec.c:
 * * gm2/gm2version.h:
 * * gm2/lang-options.h:
 * * gm2/lang-specs.h:
 * * gm2/m2pp.c:
 * * gm2/m2pp.h:
 * * gm2/examples/callingC/Makefile.in:
 * * gm2/examples/callingC/hello.mod:
 * * gm2/examples/callingC/libprintf.def:
 * * gm2/examples/cplusplus/cppcatchm2/Makefile.in:
 * * gm2/examples/cplusplus/cppcatchm2/cpp.def:
 * * gm2/examples/cplusplus/cppcatchm2/m2.def:
 * * gm2/examples/cplusplus/cppcatchm2/m2.mod:
 * * gm2/examples/cplusplus/m2catchcpp/Makefile.in:
 * * gm2/examples/cplusplus/m2catchcpp/cpp.def:
 * * gm2/examples/cplusplus/m2catchcpp/m2.mod:
 * * gm2/examples/cpp/Makefile.in:
 * * gm2/examples/cpp/hello.mod:
 * * gm2/examples/cppDef/a.def:
 * * gm2/examples/cppDef/b.mod:
 * * gm2/examples/executive/testexecutive.mod:
 * * gm2/examples/executive/testtime.mod:
 * * gm2/examples/executive/winexec.mod:
 * * gm2/examples/gravity/Makefile.in:
 * * gm2/examples/gravity/deviceGnuPic.def:
 * * gm2/examples/gravity/deviceGnuPic.mod:
 * * gm2/examples/gravity/gsl.def:
 * * gm2/examples/gravity/roots.def:
 * * gm2/examples/gravity/roots.mod:
 * * gm2/examples/gravity/test.mod:
 * * gm2/examples/gravity/testsim.py:
 * * gm2/examples/gravity/twoDsim.def:
 * * gm2/examples/gravity/twoDsim.mod:
 * * gm2/examples/hello/Makefile.in:
 * * gm2/examples/hello/hello.mod:
 * * gm2/examples/iso/files/rot13.mod:
 * * gm2/examples/iso/hello/hello.mod:
 * * gm2/examples/iso/socket/client.mod:
 * * gm2/examples/map/AdvMap.def:
 * * gm2/examples/map/AdvMap.mod:
 * * gm2/examples/map/BoxMap.def:
 * * gm2/examples/map/BoxMap.mod:
 * * gm2/examples/map/Chance.def:
 * * gm2/examples/map/Chance.mod:
 * * gm2/examples/map/Find.def:
 * * gm2/examples/map/Find.mod:
 * * gm2/examples/map/Geometry.def:
 * * gm2/examples/map/Geometry.mod:
 * * gm2/examples/map/MakeBoxes.def:
 * * gm2/examples/map/MakeBoxes.mod:
 * * gm2/examples/map/Makefile.in:
 * * gm2/examples/map/Map.mod:
 * * gm2/examples/map/RoomMap.def:
 * * gm2/examples/map/RoomMap.mod:
 * * gm2/examples/map/Semantic.mod:
 * * gm2/examples/map/StoreCoord.def:
 * * gm2/examples/map/StoreCoord.mod:
 * * gm2/examples/map/StoreCoords.def:
 * * gm2/examples/map/StoreCoords.mod:
 * * gm2/examples/map/WriteMap.def:
 * * gm2/examples/map/WriteMap.mod:
 * * gm2/examples/map/testch2.mod:
 * * gm2/examples/map/testchan.mod:
 * * gm2/examples/map/testcoor.mod:
 * * gm2/examples/map/old-src/GenMap.mod:
 * * gm2/examples/map/old-src/MakeMap.mod:
 * * gm2/examples/map/old-src/MonMap.def:
 * * gm2/examples/map/old-src/MonMap.mod:
 * * gm2/examples/map/old-src/testboxes.mod:
 * * gm2/examples/min/minhello.mod:
 * * gm2/examples/ncurses/ColorText.def:
 * * gm2/examples/ncurses/ColorText.mod:
 * * gm2/examples/ncurses/Makefile.in:
 * * gm2/examples/ncurses/WindowDevice.def:
 * * gm2/examples/ncurses/WindowDevice.mod:
 * * gm2/examples/ncurses/ncurses.def:
 * * gm2/examples/ncurses/shortc.c:
 * * gm2/examples/ncurses/shortc.def:
 * * gm2/examples/ncurses/test.c:
 * * gm2/examples/ncurses/test.mod:
 * * gm2/examples/ncurses/testcolor.mod:
 * * gm2/examples/ncurses/testmove.mod:
 * * gm2/examples/ncurses/testshort.mod:
 * * gm2/examples/ncurses/testwin.mod:
 * * gm2/examples/pthread/fullpth.def:
 * * gm2/examples/pthread/libcextra.def:
 * * gm2/examples/pthread/libcwrap.c:
 * * gm2/examples/pthread/libcwrap.def:
 * * gm2/examples/pthread/modified_pth.h:
 * * gm2/examples/pthread/testselect.mod:
 * * gm2/examples/server/server.mod:
 * * gm2/examples/svga/DisplayBuffer.def:
 * * gm2/examples/svga/DisplayBuffer.mod:
 * * gm2/examples/svga/Matrix3D.def:
 * * gm2/examples/svga/Matrix3D.mod:
 * * gm2/examples/svga/Transform.def:
 * * gm2/examples/svga/Transform.mod:
 * * gm2/examples/svga/testbox.mod:
 * * gm2/examples/svga/vga.def:
 * * gm2/examples/swig/exceptions/Makefile.in:
 * * gm2/examples/swig/exceptions/tiny.def:
 * * gm2/examples/swig/exceptions/tiny.mod:
 * * gm2/examples/swig/full-strlib/testequ.py:
 * * gm2/examples/swig/strlen/MyStrLib.def:
 * * gm2/examples/swig/strlen/MyStrLib.mod:
 * * gm2/examples/swig/strlib/Makefile.in:
 * * gm2/examples/swig/strlib/MyStrLib.def:
 * * gm2/examples/swig/strlib/MyStrLib.mod:
 * * gm2/examples/swig/tiny/Makefile.in:
 * * gm2/examples/swig/tiny/tiny.def:
 * * gm2/examples/swig/tiny/tiny.mod:
 * * gm2/gm2-compiler/CLexBuf.def:
 * * gm2/gm2-compiler/CLexBuf.mod:
 * * gm2/gm2-compiler/FifoQueue.def:
 * * gm2/gm2-compiler/FifoQueue.mod:
 * * gm2/gm2-compiler/Lists.def:
 * * gm2/gm2-compiler/Lists.mod:
 * * gm2/gm2-compiler/M2ALU.def:
 * * gm2/gm2-compiler/M2ALU.mod:
 * * gm2/gm2-compiler/M2AsmUtil.def:
 * * gm2/gm2-compiler/M2AsmUtil.mod:
 * * gm2/gm2-compiler/M2Base.def:
 * * gm2/gm2-compiler/M2Base.mod:
 * * gm2/gm2-compiler/M2BasicBlock.def:
 * * gm2/gm2-compiler/M2BasicBlock.mod:
 * * gm2/gm2-compiler/M2Batch.def:
 * * gm2/gm2-compiler/M2Batch.mod:
 * * gm2/gm2-compiler/M2Bitset.def:
 * * gm2/gm2-compiler/M2Bitset.mod:
 * * gm2/gm2-compiler/M2CaseList.def:
 * * gm2/gm2-compiler/M2CaseList.mod:
 * * gm2/gm2-compiler/M2Code.def:
 * * gm2/gm2-compiler/M2Code.mod:
 * * gm2/gm2-compiler/M2Comp.def:
 * * gm2/gm2-compiler/M2Comp.mod:
 * * gm2/gm2-compiler/M2Configure.def:
 * * gm2/gm2-compiler/M2Constants.def:
 * * gm2/gm2-compiler/M2Constants.mod:
 * * gm2/gm2-compiler/M2Debug.def:
 * * gm2/gm2-compiler/M2Debug.mod:
 * * gm2/gm2-compiler/M2Defaults.def:
 * * gm2/gm2-compiler/M2Defaults.mod:
 * * gm2/gm2-compiler/M2Depth.def:
 * * gm2/gm2-compiler/M2Depth.mod:
 * * gm2/gm2-compiler/M2Entity.def:
 * * gm2/gm2-compiler/M2Entity.mod:
 * * gm2/gm2-compiler/M2Error.def:
 * * gm2/gm2-compiler/M2Error.mod:
 * * gm2/gm2-compiler/M2EvalSym.def:
 * * gm2/gm2-compiler/M2FileName.def:
 * * gm2/gm2-compiler/M2FileName.mod:
 * * gm2/gm2-compiler/M2GCCDeclare.def:
 * * gm2/gm2-compiler/M2GCCDeclare.mod:
 * * gm2/gm2-compiler/M2GenGCC.def:
 * * gm2/gm2-compiler/M2GenGCC.mod:
 * * gm2/gm2-compiler/M2Inline.def:
 * * gm2/gm2-compiler/M2Inline.mod:
 * * gm2/gm2-compiler/M2Lex.def:
 * * gm2/gm2-compiler/M2Lex.mod:
 * * gm2/gm2-compiler/M2LexBuf.def:
 * * gm2/gm2-compiler/M2LexBuf.mod:
 * * gm2/gm2-compiler/M2MetaError.def:
 * * gm2/gm2-compiler/M2MetaError.mod:
 * * gm2/gm2-compiler/M2Optimize.def:
 * * gm2/gm2-compiler/M2Optimize.mod:
 * * gm2/gm2-compiler/M2Options.def:
 * * gm2/gm2-compiler/M2Options.mod:
 * * gm2/gm2-compiler/M2Pass.def:
 * * gm2/gm2-compiler/M2Pass.mod:
 * * gm2/gm2-compiler/M2Preprocess.def:
 * * gm2/gm2-compiler/M2Preprocess.mod:
 * * gm2/gm2-compiler/M2Printf.def:
 * * gm2/gm2-compiler/M2Printf.mod:
 * * gm2/gm2-compiler/M2Quads.def:
 * * gm2/gm2-compiler/M2Quads.mod:
 * * gm2/gm2-compiler/M2Quiet.def:
 * * gm2/gm2-compiler/M2Quiet.mod:
 * * gm2/gm2-compiler/M2Range.def:
 * * gm2/gm2-compiler/M2Range.mod:
 * * gm2/gm2-compiler/M2Reserved.def:
 * * gm2/gm2-compiler/M2Reserved.mod:
 * * gm2/gm2-compiler/M2Scope.def:
 * * gm2/gm2-compiler/M2Scope.mod:
 * * gm2/gm2-compiler/M2Search.def:
 * * gm2/gm2-compiler/M2Search.mod:
 * * gm2/gm2-compiler/M2Size.def:
 * * gm2/gm2-compiler/M2Size.mod:
 * * gm2/gm2-compiler/M2StackAddress.def:
 * * gm2/gm2-compiler/M2StackAddress.mod:
 * * gm2/gm2-compiler/M2StackWord.def:
 * * gm2/gm2-compiler/M2StackWord.mod:
 * * gm2/gm2-compiler/M2Students.def:
 * * gm2/gm2-compiler/M2Students.mod:
 * * gm2/gm2-compiler/M2SubExp.def:
 * * gm2/gm2-compiler/M2SubExp.mod:
 * * gm2/gm2-compiler/M2Swig.def:
 * * gm2/gm2-compiler/M2Swig.mod:
 * * gm2/gm2-compiler/M2System.def:
 * * gm2/gm2-compiler/M2System.mod:
 * * gm2/gm2-compiler/M2Version.def:
 * * gm2/gm2-compiler/NameKey.def:
 * * gm2/gm2-compiler/NameKey.mod:
 * * gm2/gm2-compiler/P1SymBuild.def:
 * * gm2/gm2-compiler/P1SymBuild.mod:
 * * gm2/gm2-compiler/P1SyntaxCheck.def:
 * * gm2/gm2-compiler/P2Build.def:
 * * gm2/gm2-compiler/P2SymBuild.def:
 * * gm2/gm2-compiler/P2SymBuild.mod:
 * * gm2/gm2-compiler/P3Build.def:
 * * gm2/gm2-compiler/P3SymBuild.def:
 * * gm2/gm2-compiler/P3SymBuild.mod:
 * * gm2/gm2-compiler/PCBuild.def:
 * * gm2/gm2-compiler/PCSymBuild.def:
 * * gm2/gm2-compiler/PCSymBuild.mod:
 * * gm2/gm2-compiler/PHBuild.def:
 * * gm2/gm2-compiler/Sets.def:
 * * gm2/gm2-compiler/Sets.mod:
 * * gm2/gm2-compiler/SymbolConversion.def:
 * * gm2/gm2-compiler/SymbolConversion.mod:
 * * gm2/gm2-compiler/SymbolKey.def:
 * * gm2/gm2-compiler/SymbolKey.mod:
 * * gm2/gm2-compiler/SymbolTable.def:
 * * gm2/gm2-compiler/SymbolTable.mod:
 * * gm2/gm2-compiler/bnflex.def:
 * * gm2/gm2-compiler/bnflex.mod:
 * * gm2/gm2-compiler/cflex.def:
 * * gm2/gm2-compiler/gccgm2.def:
 * * gm2/gm2-compiler/gm2.mod:
 * * gm2/gm2-compiler/gm2builtins.def:
 * * gm2/gm2-compiler/gm2except.def:
 * * gm2/gm2-compiler/gm2lcc.mod:
 * * gm2/gm2-compiler/gm2lgen.mod:
 * * gm2/gm2-compiler/gm2lorder.mod:
 * * gm2/gm2-compiler/m2flex.def:
 * * gm2/gm2-compiler/ppg.mod:
 * * gm2/gm2-harness/Makefile.in:
 * * gm2/gm2-libiberty/choosetemp.def:
 * * gm2/gm2-libiberty/pexecute.def:
 * * gm2/gm2-libs/ASCII.def:
 * * gm2/gm2-libs/Args.def:
 * * gm2/gm2-libs/Args.mod:
 * * gm2/gm2-libs/Assertion.def:
 * * gm2/gm2-libs/Assertion.mod:
 * * gm2/gm2-libs/Break.mod:
 * * gm2/gm2-libs/Builtins.def:
 * * gm2/gm2-libs/Builtins.mod:
 * * gm2/gm2-libs/COROUTINES.def:
 * * gm2/gm2-libs/COROUTINES.mod:
 * * gm2/gm2-libs/CmdArgs.def:
 * * gm2/gm2-libs/CmdArgs.mod:
 * * gm2/gm2-libs/Debug.def:
 * * gm2/gm2-libs/Debug.mod:
 * * gm2/gm2-libs/DynamicStrings.def:
 * * gm2/gm2-libs/DynamicStrings.mod:
 * * gm2/gm2-libs/Environment.def:
 * * gm2/gm2-libs/Environment.mod:
 * * gm2/gm2-libs/FIO.def:
 * * gm2/gm2-libs/FIO.mod:
 * * gm2/gm2-libs/FormatStrings.def:
 * * gm2/gm2-libs/FormatStrings.mod:
 * * gm2/gm2-libs/FpuIO.def:
 * * gm2/gm2-libs/FpuIO.mod:
 * * gm2/gm2-libs/IO.def:
 * * gm2/gm2-libs/IO.mod:
 * * gm2/gm2-libs/Indexing.def:
 * * gm2/gm2-libs/Indexing.mod:
 * * gm2/gm2-libs/LMathLib0.def:
 * * gm2/gm2-libs/LMathLib0.mod:
 * * gm2/gm2-libs/LegacyReal.def:
 * * gm2/gm2-libs/M2EXCEPTION.mod:
 * * gm2/gm2-libs/M2RTS.def:
 * * gm2/gm2-libs/M2RTS.mod:
 * * gm2/gm2-libs/MathLib0.def:
 * * gm2/gm2-libs/MathLib0.mod:
 * * gm2/gm2-libs/MemUtils.def:
 * * gm2/gm2-libs/MemUtils.mod:
 * * gm2/gm2-libs/NumberIO.def:
 * * gm2/gm2-libs/NumberIO.mod:
 * * gm2/gm2-libs/PushBackInput.def:
 * * gm2/gm2-libs/PushBackInput.mod:
 * * gm2/gm2-libs/RTExceptions.def:
 * * gm2/gm2-libs/RTExceptions.mod:
 * * gm2/gm2-libs/RTint.def:
 * * gm2/gm2-libs/RTint.mod:
 * * gm2/gm2-libs/SArgs.def:
 * * gm2/gm2-libs/SArgs.mod:
 * * gm2/gm2-libs/SEnvironment.def:
 * * gm2/gm2-libs/SEnvironment.mod:
 * * gm2/gm2-libs/SFIO.def:
 * * gm2/gm2-libs/SFIO.mod:
 * * gm2/gm2-libs/SMathLib0.def:
 * * gm2/gm2-libs/SMathLib0.mod:
 * * gm2/gm2-libs/SYSTEM.def:
 * * gm2/gm2-libs/SYSTEM.mod:
 * * gm2/gm2-libs/Scan.def:
 * * gm2/gm2-libs/Scan.mod:
 * * gm2/gm2-libs/Selective.def:
 * * gm2/gm2-libs/StdIO.def:
 * * gm2/gm2-libs/StdIO.mod:
 * * gm2/gm2-libs/Storage.def:
 * * gm2/gm2-libs/Storage.mod:
 * * gm2/gm2-libs/StrCase.def:
 * * gm2/gm2-libs/StrCase.mod:
 * * gm2/gm2-libs/StrIO.def:
 * * gm2/gm2-libs/StrIO.mod:
 * * gm2/gm2-libs/StrLib.def:
 * * gm2/gm2-libs/StrLib.mod:
 * * gm2/gm2-libs/StringConvert.def:
 * * gm2/gm2-libs/StringConvert.mod:
 * * gm2/gm2-libs/SysExceptions.def:
 * * gm2/gm2-libs/SysStorage.def:
 * * gm2/gm2-libs/SysStorage.mod:
 * * gm2/gm2-libs/TimeString.def:
 * * gm2/gm2-libs/TimeString.mod:
 * * gm2/gm2-libs/UnixArgs.def:
 * * gm2/gm2-libs/cbuiltin.def:
 * * gm2/gm2-libs/configure.in:
 * * gm2/gm2-libs/cxxabi.def:
 * * gm2/gm2-libs/dtoa.def:
 * * gm2/gm2-libs/errno.def:
 * * gm2/gm2-libs/gm2-libs-host.h.in:
 * * gm2/gm2-libs/ldtoa.def:
 * * gm2/gm2-libs/libc.def:
 * * gm2/gm2-libs/libm.def:
 * * gm2/gm2-libs/sckt.def:
 * * gm2/gm2-libs/termios.def:
 * * gm2/gm2-libs/wrapc.def:
 * * gm2/gm2-libs-boot/SYSTEM.def:
 * * gm2/gm2-libs-ch/Selective.c:
 * * gm2/gm2-libs-ch/StdIO.c:
 * * gm2/gm2-libs-ch/Storage.c:
 * * gm2/gm2-libs-ch/SysExceptions.c:
 * * gm2/gm2-libs-ch/UnixArgs.c:
 * * gm2/gm2-libs-ch/choosetemp.c:
 * * gm2/gm2-libs-ch/dtoa.c:
 * * gm2/gm2-libs-ch/errno.c:
 * * gm2/gm2-libs-ch/ldtoa.c:
 * * gm2/gm2-libs-ch/libc.c:
 * * gm2/gm2-libs-ch/sckt.c:
 * * gm2/gm2-libs-ch/target.c:
 * * gm2/gm2-libs-ch/termios.c:
 * * gm2/gm2-libs-ch/wrapc.c:
 * * gm2/gm2-libs-ch/xlibc.c:
 * * gm2/gm2-libs-coroutines/Debug.def:
 * * gm2/gm2-libs-coroutines/Debug.mod:
 * * gm2/gm2-libs-coroutines/Executive.def:
 * * gm2/gm2-libs-coroutines/Executive.mod:
 * * gm2/gm2-libs-coroutines/KeyBoardLEDs.c:
 * * gm2/gm2-libs-coroutines/KeyBoardLEDs.def:
 * * gm2/gm2-libs-coroutines/SYSTEM.def:
 * * gm2/gm2-libs-coroutines/SYSTEM.mod:
 * * gm2/gm2-libs-coroutines/TimerHandler.def:
 * * gm2/gm2-libs-coroutines/TimerHandler.mod:
 * * gm2/gm2-libs-iso/COROUTINES.mod:
 * * gm2/gm2-libs-iso/ChanConsts.h:
 * * gm2/gm2-libs-iso/ChanConsts.mod:
 * * gm2/gm2-libs-iso/CharClass.mod:
 * * gm2/gm2-libs-iso/ClientSocket.def:
 * * gm2/gm2-libs-iso/ClientSocket.mod:
 * * gm2/gm2-libs-iso/ComplexMath.mod:
 * * gm2/gm2-libs-iso/ConvStringLong.def:
 * * gm2/gm2-libs-iso/ConvStringLong.mod:
 * * gm2/gm2-libs-iso/ConvStringReal.def:
 * * gm2/gm2-libs-iso/ConvStringReal.mod:
 * * gm2/gm2-libs-iso/ConvTypes.mod:
 * * gm2/gm2-libs-iso/EXCEPTIONS.mod:
 * * gm2/gm2-libs-iso/ErrnoCategory.c:
 * * gm2/gm2-libs-iso/ErrnoCategory.def:
 * * gm2/gm2-libs-iso/GeneralUserExceptions.mod:
 * * gm2/gm2-libs-iso/IOChan.mod:
 * * gm2/gm2-libs-iso/IOLink.mod:
 * * gm2/gm2-libs-iso/IOResult.mod:
 * * gm2/gm2-libs-iso/LongComplexMath.mod:
 * * gm2/gm2-libs-iso/LongConv.mod:
 * * gm2/gm2-libs-iso/LongMath.mod:
 * * gm2/gm2-libs-iso/LongStr.mod:
 * * gm2/gm2-libs-iso/LowLong.mod:
 * * gm2/gm2-libs-iso/LowReal.mod:
 * * gm2/gm2-libs-iso/LowShort.def:
 * * gm2/gm2-libs-iso/LowShort.mod:
 * * gm2/gm2-libs-iso/M2EXCEPTION.mod:
 * * gm2/gm2-libs-iso/M2RTS.def:
 * * gm2/gm2-libs-iso/M2RTS.mod:
 * * gm2/gm2-libs-iso/Processes.mod:
 * * gm2/gm2-libs-iso/ProgramArgs.mod:
 * * gm2/gm2-libs-iso/RTdata.def:
 * * gm2/gm2-libs-iso/RTdata.mod:
 * * gm2/gm2-libs-iso/RTentity.def:
 * * gm2/gm2-libs-iso/RTentity.mod:
 * * gm2/gm2-libs-iso/RTfio.def:
 * * gm2/gm2-libs-iso/RTfio.mod:
 * * gm2/gm2-libs-iso/RTgen.def:
 * * gm2/gm2-libs-iso/RTgen.mod:
 * * gm2/gm2-libs-iso/RTgenif.def:
 * * gm2/gm2-libs-iso/RTgenif.mod:
 * * gm2/gm2-libs-iso/RTio.def:
 * * gm2/gm2-libs-iso/RTio.mod:
 * * gm2/gm2-libs-iso/RawIO.mod:
 * * gm2/gm2-libs-iso/RealConv.mod:
 * * gm2/gm2-libs-iso/RealMath.mod:
 * * gm2/gm2-libs-iso/RealStr.mod:
 * * gm2/gm2-libs-iso/RndFile.mod:
 * * gm2/gm2-libs-iso/SIOResult.mod:
 * * gm2/gm2-libs-iso/SLongIO.mod:
 * * gm2/gm2-libs-iso/SRawIO.mod:
 * * gm2/gm2-libs-iso/SRealIO.mod:
 * * gm2/gm2-libs-iso/STextIO.mod:
 * * gm2/gm2-libs-iso/SWholeIO.mod:
 * * gm2/gm2-libs-iso/SYSTEM.mod:
 * * gm2/gm2-libs-iso/Semaphores.mod:
 * * gm2/gm2-libs-iso/SeqFile.mod:
 * * gm2/gm2-libs-iso/ServerSocket.def:
 * * gm2/gm2-libs-iso/ServerSocket.mod:
 * * gm2/gm2-libs-iso/ShortComplexMath.def:
 * * gm2/gm2-libs-iso/ShortComplexMath.mod:
 * * gm2/gm2-libs-iso/SimpleCipher.def:
 * * gm2/gm2-libs-iso/SimpleCipher.mod:
 * * gm2/gm2-libs-iso/StdChans.mod:
 * * gm2/gm2-libs-iso/Storage.mod:
 * * gm2/gm2-libs-iso/StreamFile.mod:
 * * gm2/gm2-libs-iso/StringChan.def:
 * * gm2/gm2-libs-iso/StringChan.mod:
 * * gm2/gm2-libs-iso/Strings.mod:
 * * gm2/gm2-libs-iso/SysClock.mod:
 * * gm2/gm2-libs-iso/TERMINATION.mod:
 * * gm2/gm2-libs-iso/TermFile.mod:
 * * gm2/gm2-libs-iso/TextIO.mod:
 * * gm2/gm2-libs-iso/WholeConv.mod:
 * * gm2/gm2-libs-iso/WholeIO.mod:
 * * gm2/gm2-libs-iso/WholeStr.mod:
 * * gm2/gm2-libs-iso/pth.def:
 * * gm2/gm2-libs-iso/wrapsock.c:
 * * gm2/gm2-libs-iso/wrapsock.def:
 * * gm2/gm2-libs-iso/wraptime.c:
 * * gm2/gm2-libs-iso/wraptime.def:
 * * gm2/gm2-libs-min/M2RTS.def:
 * * gm2/gm2-libs-min/M2RTS.mod:
 * * gm2/gm2-libs-min/SYSTEM.def:
 * * gm2/gm2-libs-min/SYSTEM.mod:
 * * gm2/gm2-libs-min/libc.c:
 * * gm2/gm2-libs-min/libc.def:
 * * gm2/gm2-libs-pim/BitBlockOps.def:
 * * gm2/gm2-libs-pim/BitBlockOps.mod:
 * * gm2/gm2-libs-pim/BitByteOps.def:
 * * gm2/gm2-libs-pim/BitByteOps.mod:
 * * gm2/gm2-libs-pim/BitWordOps.def:
 * * gm2/gm2-libs-pim/BitWordOps.mod:
 * * gm2/gm2-libs-pim/BlockOps.def:
 * * gm2/gm2-libs-pim/BlockOps.mod:
 * * gm2/gm2-libs-pim/Break.c:
 * * gm2/gm2-libs-pim/Break.def:
 * * gm2/gm2-libs-pim/CardinalIO.def:
 * * gm2/gm2-libs-pim/CardinalIO.mod:
 * * gm2/gm2-libs-pim/Conversions.def:
 * * gm2/gm2-libs-pim/Conversions.mod:
 * * gm2/gm2-libs-pim/DebugTrace.def:
 * * gm2/gm2-libs-pim/Delay.def:
 * * gm2/gm2-libs-pim/Delay.mod:
 * * gm2/gm2-libs-pim/Display.def:
 * * gm2/gm2-libs-pim/Display.mod:
 * * gm2/gm2-libs-pim/ErrorCode.def:
 * * gm2/gm2-libs-pim/ErrorCode.mod:
 * * gm2/gm2-libs-pim/FileSystem.def:
 * * gm2/gm2-libs-pim/FileSystem.mod:
 * * gm2/gm2-libs-pim/FloatingUtilities.def:
 * * gm2/gm2-libs-pim/FloatingUtilities.mod:
 * * gm2/gm2-libs-pim/InOut.def:
 * * gm2/gm2-libs-pim/InOut.mod:
 * * gm2/gm2-libs-pim/Keyboard.def:
 * * gm2/gm2-libs-pim/Keyboard.mod:
 * * gm2/gm2-libs-pim/LongIO.def:
 * * gm2/gm2-libs-pim/LongIO.mod:
 * * gm2/gm2-libs-pim/Random.def:
 * * gm2/gm2-libs-pim/Random.mod:
 * * gm2/gm2-libs-pim/RealConversions.def:
 * * gm2/gm2-libs-pim/RealConversions.mod:
 * * gm2/gm2-libs-pim/RealInOut.def:
 * * gm2/gm2-libs-pim/RealInOut.mod:
 * * gm2/gm2-libs-pim/Strings.def:
 * * gm2/gm2-libs-pim/Strings.mod:
 * * gm2/gm2-libs-pim/Termbase.def:
 * * gm2/gm2-libs-pim/Termbase.mod:
 * * gm2/gm2-libs-pim/Terminal.def:
 * * gm2/gm2-libs-pim/Terminal.mod:
 * * gm2/gm2-libs-pim/TimeDate.def:
 * * gm2/gm2-libs-pim/TimeDate.mod:
 * * gm2/man/Makefile.in:
 * * gm2/p2c/Makefile.in:
 * * gm2/p2c/p2c.h:
 * * gm2/p2c/p2c-src/Makefile.in:
 * * gm2/p2c/p2c-src/auto-host.h.in:
 * * gm2/p2c/p2c-src/configure.in:
 * * gm2/p2c/p2c-src/include/ansidecl.h:
 * * gm2/p2c/p2c-src/include/config.h:
 * * gm2/p2c/p2c-src/include/system.h:
 * * gm2/p2c/p2c-src/src/Makefile.in:
 * * gm2/p2c/p2c-src/src/citmods.c:
 * * gm2/p2c/p2c-src/src/comment.c:
 * * gm2/p2c/p2c-src/src/decl.c:
 * * gm2/p2c/p2c-src/src/dir.c:
 * * gm2/p2c/p2c-src/src/expr.c:
 * * gm2/p2c/p2c-src/src/funcs.c:
 * * gm2/p2c/p2c-src/src/hpmods.c:
 * * gm2/p2c/p2c-src/src/lex.c:
 * * gm2/p2c/p2c-src/src/loc.p2clib.c:
 * * gm2/p2c/p2c-src/src/makeproto.c:
 * * gm2/p2c/p2c-src/src/out.c:
 * * gm2/p2c/p2c-src/src/p2c-config.h:
 * * gm2/p2c/p2c-src/src/p2c.h:
 * * gm2/p2c/p2c-src/src/p2clib.c:
 * * gm2/p2c/p2c-src/src/parse.c:
 * * gm2/p2c/p2c-src/src/pexpr.c:
 * * gm2/p2c/p2c-src/src/stuff.c:
 * * gm2/p2c/p2c-src/src/trans.c:
 * * gm2/p2c/p2c-src/src/trans.h:
 * * gm2/patches/gcc/4.1.2/08.gaius_ipa_type_escape.c:
 * * gm2/tools-src/array2index.py:
 * * gm2/tools-src/def2texi.py:
 * * gm2/tools-src/gensum.py:
 * * gm2/tools-src/mklink.c:
 * * gm2/ulm-lib-gm2/processes/CoExpressions.def:
 * * gm2/ulm-lib-gm2/processes/CoExpressions.mod:
 * * gm2/ulm-lib-gm2/processes/Processes.def:
 * * gm2/ulm-lib-gm2/processes/Processes.mod:
 * * gm2/ulm-lib-gm2/std/ASCII.def:
 * * gm2/ulm-lib-gm2/std/ASCII.mod:
 * * gm2/ulm-lib-gm2/std/Archive.def:
 * * gm2/ulm-lib-gm2/std/Archive.mod:
 * * gm2/ulm-lib-gm2/std/Arguments.def:
 * * gm2/ulm-lib-gm2/std/Arguments.mod:
 * * gm2/ulm-lib-gm2/std/Calendar.def:
 * * gm2/ulm-lib-gm2/std/Calendar.mod:
 * * gm2/ulm-lib-gm2/std/CallShell.def:
 * * gm2/ulm-lib-gm2/std/CallShell.mod:
 * * gm2/ulm-lib-gm2/std/Clock.def:
 * * gm2/ulm-lib-gm2/std/Clock.mod:
 * * gm2/ulm-lib-gm2/std/Conversions.def:
 * * gm2/ulm-lib-gm2/std/Conversions.mod:
 * * gm2/ulm-lib-gm2/std/Directories.def:
 * * gm2/ulm-lib-gm2/std/Directories.mod:
 * * gm2/ulm-lib-gm2/std/Environment.def:
 * * gm2/ulm-lib-gm2/std/Environment.mod:
 * * gm2/ulm-lib-gm2/std/EtcGroup.def:
 * * gm2/ulm-lib-gm2/std/EtcGroup.mod:
 * * gm2/ulm-lib-gm2/std/Files.def:
 * * gm2/ulm-lib-gm2/std/Files.mod:
 * * gm2/ulm-lib-gm2/std/FtdIO.def:
 * * gm2/ulm-lib-gm2/std/FtdIO.mod:
 * * gm2/ulm-lib-gm2/std/Functions.def:
 * * gm2/ulm-lib-gm2/std/Functions.mod:
 * * gm2/ulm-lib-gm2/std/GetPass.def:
 * * gm2/ulm-lib-gm2/std/GetPass.mod:
 * * gm2/ulm-lib-gm2/std/InOut.def:
 * * gm2/ulm-lib-gm2/std/InOut.mod:
 * * gm2/ulm-lib-gm2/std/M2EXCEPTION.mod:
 * * gm2/ulm-lib-gm2/std/M2RTS.mod:
 * * gm2/ulm-lib-gm2/std/MathLib.def:
 * * gm2/ulm-lib-gm2/std/MathLib.mod:
 * * gm2/ulm-lib-gm2/std/Passwd.def:
 * * gm2/ulm-lib-gm2/std/Passwd.mod:
 * * gm2/ulm-lib-gm2/std/PipeIO.def:
 * * gm2/ulm-lib-gm2/std/PipeIO.mod:
 * * gm2/ulm-lib-gm2/std/Plot.def:
 * * gm2/ulm-lib-gm2/std/Plot.mod:
 * * gm2/ulm-lib-gm2/std/RTErrors.def:
 * * gm2/ulm-lib-gm2/std/RTErrors.mod:
 * * gm2/ulm-lib-gm2/std/RTExceptions.mod:
 * * gm2/ulm-lib-gm2/std/RandomGenerator.def:
 * * gm2/ulm-lib-gm2/std/RandomGenerator.mod:
 * * gm2/ulm-lib-gm2/std/ReadIntCard.def:
 * * gm2/ulm-lib-gm2/std/ReadIntCard.mod:
 * * gm2/ulm-lib-gm2/std/RealConv.def:
 * * gm2/ulm-lib-gm2/std/RealConv.mod:
 * * gm2/ulm-lib-gm2/std/RealInOut.def:
 * * gm2/ulm-lib-gm2/std/RealInOut.mod:
 * * gm2/ulm-lib-gm2/std/ScanPwfile.def:
 * * gm2/ulm-lib-gm2/std/ScanPwfile.mod:
 * * gm2/ulm-lib-gm2/std/StdFuncs.def:
 * * gm2/ulm-lib-gm2/std/StdFuncs.mod:
 * * gm2/ulm-lib-gm2/std/StdIO.def:
 * * gm2/ulm-lib-gm2/std/StdIO.mod:
 * * gm2/ulm-lib-gm2/std/Storage.def:
 * * gm2/ulm-lib-gm2/std/Storage.mod:
 * * gm2/ulm-lib-gm2/std/StrSpec.def:
 * * gm2/ulm-lib-gm2/std/StrSpec.mod:
 * * gm2/ulm-lib-gm2/std/StrToNum.def:
 * * gm2/ulm-lib-gm2/std/StrToNum.mod:
 * * gm2/ulm-lib-gm2/std/StrToReal.def:
 * * gm2/ulm-lib-gm2/std/StrToReal.mod:
 * * gm2/ulm-lib-gm2/std/Strings.def:
 * * gm2/ulm-lib-gm2/std/Strings.mod:
 * * gm2/ulm-lib-gm2/std/SysConf.def:
 * * gm2/ulm-lib-gm2/std/SysConf.mod:
 * * gm2/ulm-lib-gm2/std/SysPerror.def:
 * * gm2/ulm-lib-gm2/std/SysPerror.mod:
 * * gm2/ulm-lib-gm2/std/Terminal.def:
 * * gm2/ulm-lib-gm2/std/Terminal.mod:
 * * gm2/ulm-lib-gm2/std/TimeIO.def:
 * * gm2/ulm-lib-gm2/std/TimeIO.mod:
 * * gm2/ulm-lib-gm2/sys/Errno.def:
 * * gm2/ulm-lib-gm2/sys/Errno.mod:
 * * gm2/ulm-lib-gm2/sys/SYSTEM.def:
 * * gm2/ulm-lib-gm2/sys/Sys.def:
 * * gm2/ulm-lib-gm2/sys/Sys.mod:
 * * gm2/ulm-lib-gm2/sys/SysAccess.def:
 * * gm2/ulm-lib-gm2/sys/SysAccess.mod:
 * * gm2/ulm-lib-gm2/sys/SysAlarm.def:
 * * gm2/ulm-lib-gm2/sys/SysAlarm.mod:
 * * gm2/ulm-lib-gm2/sys/SysBreak.def:
 * * gm2/ulm-lib-gm2/sys/SysBreak.mod:
 * * gm2/ulm-lib-gm2/sys/SysClose.def:
 * * gm2/ulm-lib-gm2/sys/SysClose.mod:
 * * gm2/ulm-lib-gm2/sys/SysCreat.def:
 * * gm2/ulm-lib-gm2/sys/SysCreat.mod:
 * * gm2/ulm-lib-gm2/sys/SysDup.def:
 * * gm2/ulm-lib-gm2/sys/SysDup.mod:
 * * gm2/ulm-lib-gm2/sys/SysExec.def:
 * * gm2/ulm-lib-gm2/sys/SysExec.mod:
 * * gm2/ulm-lib-gm2/sys/SysExit.def:
 * * gm2/ulm-lib-gm2/sys/SysExit.mod:
 * * gm2/ulm-lib-gm2/sys/SysFcntl.def:
 * * gm2/ulm-lib-gm2/sys/SysFcntl.mod:
 * * gm2/ulm-lib-gm2/sys/SysFork.def:
 * * gm2/ulm-lib-gm2/sys/SysFork.mod:
 * * gm2/ulm-lib-gm2/sys/SysGetpid.def:
 * * gm2/ulm-lib-gm2/sys/SysGetpid.mod:
 * * gm2/ulm-lib-gm2/sys/SysGetuid.def:
 * * gm2/ulm-lib-gm2/sys/SysGetuid.mod:
 * * gm2/ulm-lib-gm2/sys/SysIoctl.def:
 * * gm2/ulm-lib-gm2/sys/SysIoctl.mod:
 * * gm2/ulm-lib-gm2/sys/SysKill.def:
 * * gm2/ulm-lib-gm2/sys/SysKill.mod:
 * * gm2/ulm-lib-gm2/sys/SysLink.def:
 * * gm2/ulm-lib-gm2/sys/SysLink.mod:
 * * gm2/ulm-lib-gm2/sys/SysLocations.def:
 * * gm2/ulm-lib-gm2/sys/SysLocations.mod:
 * * gm2/ulm-lib-gm2/sys/SysLseek.def:
 * * gm2/ulm-lib-gm2/sys/SysLseek.mod:
 * * gm2/ulm-lib-gm2/sys/SysOpen.def:
 * * gm2/ulm-lib-gm2/sys/SysOpen.mod:
 * * gm2/ulm-lib-gm2/sys/SysPanic.def:
 * * gm2/ulm-lib-gm2/sys/SysPanic.mod:
 * * gm2/ulm-lib-gm2/sys/SysPause.def:
 * * gm2/ulm-lib-gm2/sys/SysPause.mod:
 * * gm2/ulm-lib-gm2/sys/SysPipe.def:
 * * gm2/ulm-lib-gm2/sys/SysPipe.mod:
 * * gm2/ulm-lib-gm2/sys/SysRead.def:
 * * gm2/ulm-lib-gm2/sys/SysRead.mod:
 * * gm2/ulm-lib-gm2/sys/SysSetuid.def:
 * * gm2/ulm-lib-gm2/sys/SysSetuid.mod:
 * * gm2/ulm-lib-gm2/sys/SysSignal.def:
 * * gm2/ulm-lib-gm2/sys/SysSignal.mod:
 * * gm2/ulm-lib-gm2/sys/SysStat.def:
 * * gm2/ulm-lib-gm2/sys/SysStat.mod:
 * * gm2/ulm-lib-gm2/sys/SysTermIO.def:
 * * gm2/ulm-lib-gm2/sys/SysTermIO.mod:
 * * gm2/ulm-lib-gm2/sys/SysTime.def:
 * * gm2/ulm-lib-gm2/sys/SysTime.mod:
 * * gm2/ulm-lib-gm2/sys/SysUnlink.def:
 * * gm2/ulm-lib-gm2/sys/SysUnlink.mod:
 * * gm2/ulm-lib-gm2/sys/SysWait.def:
 * * gm2/ulm-lib-gm2/sys/SysWait.mod:
 * * gm2/ulm-lib-gm2/sys/SysWrite.def:
 * * gm2/ulm-lib-gm2/sys/SysWrite.mod:
 * * gm2/ulm-lib-gm2/sys/SystemTypes.def:
 * * gm2/ulm-lib-gm2/sys/SystemTypes.mod:
 * * gm2/ulm-lib-gm2/sys/UnixString.def:
 * * gm2/ulm-lib-gm2/sys/UnixString.mod:
 * * gm2/ulm-lib-gm2/sys/test.mod:
 * * gm2/www/Makefile.in:
 *
 * Revision 1.8  2010/09/29 05:32:08  gaius
 * * gm2/Make-lang.in:  changed license to v3 of GPL and LGPL for
 *   all the following files.
 *
 * Revision 1.7  2010/09/21 10:02:37  gaius
 * * gm2/www/index.ms:  updated information for the 0.98 release.
 * * gm2/gm2-compiler/M2Quads.mod:  fixed bug reported by
 *   DragiÅ¡a DuriÄ (dragisa-duric/testcase16/pass/TermIO.mod)
 *   and also DragiÅ¡a DuriÄ
 *   (dragisa-duric/testcase15/pass/testcase15.mod).
 * * gm2/gm2-compiler/P2SymBuild.def:  (New procedure) SkipConst.
 * * gm2/gm2-compiler/P2SymBuild.mod:  (New procedure) SkipConst
 *   implemented.
 * * gm2/gm2-compiler/PCSymBuild.mod:  call FixupConstExpr if a
 *   constant is assigned to a procedure.
 * * gm2/Make-lang.in:  fixed dates for the following files.
 * * gm2/Makefile.in:
 * * gm2/configure.in:
 * * gm2/gm2-common.c:
 * * gm2/gm2-common.h:
 * * gm2/gm2-lang.c:
 * * gm2/gm2-lang.h:
 * * gm2/gm2-tree.def:
 * * gm2/gm2-tree.h:
 * * gm2/gm2builtins.c:
 * * gm2/gm2config.h.in:
 * * gm2/gm2except.c:
 * * gm2/gm2spec.c:
 * * gm2/gm2version.h:
 * * gm2/lang-options.h:
 * * gm2/m2pp.c:
 * * gm2/m2pp.h:
 * * gm2/bnf/gm2l.bnf:
 * * gm2/bnf/gm2m.bnf:
 * * gm2/bnf/h2def.bnf:
 * * gm2/bnf/m2-2.bnf:
 * * gm2/bnf/m2-3.bnf:
 * * gm2/bnf/m2-h.bnf:
 * * gm2/bnf/m2.bnf:
 * * gm2/examples/callingC/Makefile.in:
 * * gm2/examples/callingC/hello.mod:
 * * gm2/examples/callingC/libprintf.def:
 * * gm2/examples/cplusplus/cppcatchm2/cpp.def:
 * * gm2/examples/cplusplus/cppcatchm2/m2.def:
 * * gm2/examples/cplusplus/cppcatchm2/m2.mod:
 * * gm2/examples/cplusplus/m2catchcpp/cpp.def:
 * * gm2/examples/cplusplus/m2catchcpp/m2.mod:
 * * gm2/examples/cpp/hello.mod:
 * * gm2/examples/cppDef/a.def:
 * * gm2/examples/cppDef/a.mod:
 * * gm2/examples/cppDef/b.mod:
 * * gm2/examples/executive/testexecutive.mod:
 * * gm2/examples/executive/testtime.mod:
 * * gm2/examples/executive/winexec.mod:
 * * gm2/examples/gravity/deviceGnuPic.def:
 * * gm2/examples/gravity/deviceGnuPic.mod:
 * * gm2/examples/gravity/gsl.def:
 * * gm2/examples/gravity/test.mod:
 * * gm2/examples/gravity/twoDsim.def:
 * * gm2/examples/hello/Makefile.in:
 * * gm2/examples/hello/hello.mod:
 * * gm2/examples/iso/files/rot13.mod:
 * * gm2/examples/iso/hello/hello.mod:
 * * gm2/examples/iso/socket/client.mod:
 * * gm2/examples/map/AdvMap.def:
 * * gm2/examples/map/AdvMap.mod:
 * * gm2/examples/map/BoxMap.def:
 * * gm2/examples/map/BoxMap.mod:
 * * gm2/examples/map/Chance.def:
 * * gm2/examples/map/Chance.mod:
 * * gm2/examples/map/Find.def:
 * * gm2/examples/map/Find.mod:
 * * gm2/examples/map/Geometry.def:
 * * gm2/examples/map/Geometry.mod:
 * * gm2/examples/map/MakeBoxes.def:
 * * gm2/examples/map/MakeBoxes.mod:
 * * gm2/examples/map/Makefile.in:
 * * gm2/examples/map/Map.mod:
 * * gm2/examples/map/RoomMap.def:
 * * gm2/examples/map/RoomMap.mod:
 * * gm2/examples/map/Semantic.mod:
 * * gm2/examples/map/StoreCoord.def:
 * * gm2/examples/map/StoreCoord.mod:
 * * gm2/examples/map/StoreCoords.def:
 * * gm2/examples/map/StoreCoords.mod:
 * * gm2/examples/map/WriteMap.def:
 * * gm2/examples/map/WriteMap.mod:
 * * gm2/examples/map/testch2.mod:
 * * gm2/examples/map/testchan.mod:
 * * gm2/examples/map/testcoor.mod:
 * * gm2/examples/map/old-src/GenMap.mod:
 * * gm2/examples/map/old-src/MakeMap.mod:
 * * gm2/examples/map/old-src/MonMap.def:
 * * gm2/examples/map/old-src/MonMap.mod:
 * * gm2/examples/map/old-src/testboxes.mod:
 * * gm2/examples/min/minhello.mod:
 * * gm2/examples/ncurses/ColorText.def:
 * * gm2/examples/ncurses/ColorText.mod:
 * * gm2/examples/ncurses/Makefile.in:
 * * gm2/examples/ncurses/WindowDevice.def:
 * * gm2/examples/ncurses/WindowDevice.mod:
 * * gm2/examples/ncurses/ncurses.def:
 * * gm2/examples/ncurses/shortc.c:
 * * gm2/examples/ncurses/shortc.def:
 * * gm2/examples/ncurses/test.c:
 * * gm2/examples/ncurses/test.mod:
 * * gm2/examples/ncurses/testcolor.mod:
 * * gm2/examples/ncurses/testmove.mod:
 * * gm2/examples/ncurses/testshort.mod:
 * * gm2/examples/ncurses/testwin.mod:
 * * gm2/examples/pthread/fullpth.def:
 * * gm2/examples/pthread/libcextra.def:
 * * gm2/examples/pthread/libcwrap.c:
 * * gm2/examples/pthread/libcwrap.def:
 * * gm2/examples/pthread/testselect.mod:
 * * gm2/examples/server/server.mod:
 * * gm2/examples/svga/DisplayBuffer.def:
 * * gm2/examples/svga/DisplayBuffer.mod:
 * * gm2/examples/svga/Matrix3D.def:
 * * gm2/examples/svga/Matrix3D.mod:
 * * gm2/examples/svga/Transform.def:
 * * gm2/examples/svga/Transform.mod:
 * * gm2/examples/svga/testbox.mod:
 * * gm2/examples/svga/vga.def:
 * * gm2/examples/swig/exceptions/tiny.def:
 * * gm2/examples/swig/exceptions/tiny.mod:
 * * gm2/examples/swig/strlen/MyStrLib.def:
 * * gm2/examples/swig/strlen/MyStrLib.mod:
 * * gm2/examples/swig/strlib/MyStrLib.def:
 * * gm2/examples/swig/strlib/MyStrLib.mod:
 * * gm2/examples/swig/tiny/tiny.def:
 * * gm2/examples/swig/tiny/tiny.mod:
 * * gm2/gm2-compiler/CLexBuf.def:
 * * gm2/gm2-compiler/CLexBuf.mod:
 * * gm2/gm2-compiler/FifoQueue.def:
 * * gm2/gm2-compiler/FifoQueue.mod:
 * * gm2/gm2-compiler/Lists.def:
 * * gm2/gm2-compiler/Lists.mod:
 * * gm2/gm2-compiler/M2ALU.def:
 * * gm2/gm2-compiler/M2ALU.mod:
 * * gm2/gm2-compiler/M2AsmUtil.def:
 * * gm2/gm2-compiler/M2AsmUtil.mod:
 * * gm2/gm2-compiler/M2Base.def:
 * * gm2/gm2-compiler/M2Base.mod:
 * * gm2/gm2-compiler/M2BasicBlock.def:
 * * gm2/gm2-compiler/M2BasicBlock.mod:
 * * gm2/gm2-compiler/M2Batch.def:
 * * gm2/gm2-compiler/M2Batch.mod:
 * * gm2/gm2-compiler/M2Bitset.def:
 * * gm2/gm2-compiler/M2Bitset.mod:
 * * gm2/gm2-compiler/M2CaseList.def:
 * * gm2/gm2-compiler/M2CaseList.mod:
 * * gm2/gm2-compiler/M2Code.def:
 * * gm2/gm2-compiler/M2Code.mod:
 * * gm2/gm2-compiler/M2Comp.def:
 * * gm2/gm2-compiler/M2Comp.mod:
 * * gm2/gm2-compiler/M2Configure.def:
 * * gm2/gm2-compiler/M2Configure.mod:
 * * gm2/gm2-compiler/M2Constants.def:
 * * gm2/gm2-compiler/M2Constants.mod:
 * * gm2/gm2-compiler/M2Debug.def:
 * * gm2/gm2-compiler/M2Debug.mod:
 * * gm2/gm2-compiler/M2Defaults.def:
 * * gm2/gm2-compiler/M2Defaults.mod:
 * * gm2/gm2-compiler/M2Depth.def:
 * * gm2/gm2-compiler/M2Depth.mod:
 * * gm2/gm2-compiler/M2Entity.def:
 * * gm2/gm2-compiler/M2Entity.mod:
 * * gm2/gm2-compiler/M2Error.def:
 * * gm2/gm2-compiler/M2Error.mod:
 * * gm2/gm2-compiler/M2EvalSym.def:
 * * gm2/gm2-compiler/M2FileName.def:
 * * gm2/gm2-compiler/M2FileName.mod:
 * * gm2/gm2-compiler/M2GCCDeclare.def:
 * * gm2/gm2-compiler/M2GCCDeclare.mod:
 * * gm2/gm2-compiler/M2GenGCC.def:
 * * gm2/gm2-compiler/M2Inline.def:
 * * gm2/gm2-compiler/M2Inline.mod:
 * * gm2/gm2-compiler/M2Lex.def:
 * * gm2/gm2-compiler/M2Lex.mod:
 * * gm2/gm2-compiler/M2LexBuf.def:
 * * gm2/gm2-compiler/M2LexBuf.mod:
 * * gm2/gm2-compiler/M2MetaError.def:
 * * gm2/gm2-compiler/M2MetaError.mod:
 * * gm2/gm2-compiler/M2Optimize.def:
 * * gm2/gm2-compiler/M2Optimize.mod:
 * * gm2/gm2-compiler/M2Options.def:
 * * gm2/gm2-compiler/M2Options.mod:
 * * gm2/gm2-compiler/M2Pass.mod:
 * * gm2/gm2-compiler/M2Preprocess.def:
 * * gm2/gm2-compiler/M2Preprocess.mod:
 * * gm2/gm2-compiler/M2Printf.def:
 * * gm2/gm2-compiler/M2Printf.mod:
 * * gm2/gm2-compiler/M2Quads.def:
 * * gm2/gm2-compiler/M2Quads.mod:
 * * gm2/gm2-compiler/M2Quiet.def:
 * * gm2/gm2-compiler/M2Quiet.mod:
 * * gm2/gm2-compiler/M2Range.def:
 * * gm2/gm2-compiler/M2Range.mod:
 * * gm2/gm2-compiler/M2Reserved.def:
 * * gm2/gm2-compiler/M2Reserved.mod:
 * * gm2/gm2-compiler/M2Scope.def:
 * * gm2/gm2-compiler/M2Scope.mod:
 * * gm2/gm2-compiler/M2Search.def:
 * * gm2/gm2-compiler/M2Search.mod:
 * * gm2/gm2-compiler/M2Size.def:
 * * gm2/gm2-compiler/M2Size.mod:
 * * gm2/gm2-compiler/M2StackAddress.def:
 * * gm2/gm2-compiler/M2StackAddress.mod:
 * * gm2/gm2-compiler/M2StackWord.def:
 * * gm2/gm2-compiler/M2StackWord.mod:
 * * gm2/gm2-compiler/M2Students.def:
 * * gm2/gm2-compiler/M2Students.mod:
 * * gm2/gm2-compiler/M2SubExp.def:
 * * gm2/gm2-compiler/M2SubExp.mod:
 * * gm2/gm2-compiler/M2Swig.def:
 * * gm2/gm2-compiler/M2Swig.mod:
 * * gm2/gm2-compiler/M2System.def:
 * * gm2/gm2-compiler/M2Version.def:
 * * gm2/gm2-compiler/NameKey.def:
 * * gm2/gm2-compiler/NameKey.mod:
 * * gm2/gm2-compiler/P1SymBuild.def:
 * * gm2/gm2-compiler/P1SymBuild.mod:
 * * gm2/gm2-compiler/P1SyntaxCheck.def:
 * * gm2/gm2-compiler/P2Build.def:
 * * gm2/gm2-compiler/P2SymBuild.def:
 * * gm2/gm2-compiler/P2SymBuild.mod:
 * * gm2/gm2-compiler/P3Build.def:
 * * gm2/gm2-compiler/P3SymBuild.def:
 * * gm2/gm2-compiler/P3SymBuild.mod:
 * * gm2/gm2-compiler/PCBuild.def:
 * * gm2/gm2-compiler/PCSymBuild.def:
 * * gm2/gm2-compiler/PCSymBuild.mod:
 * * gm2/gm2-compiler/PHBuild.def:
 * * gm2/gm2-compiler/Sets.def:
 * * gm2/gm2-compiler/Sets.mod:
 * * gm2/gm2-compiler/SymbolConversion.def:
 * * gm2/gm2-compiler/SymbolConversion.mod:
 * * gm2/gm2-compiler/SymbolKey.def:
 * * gm2/gm2-compiler/SymbolKey.mod:
 * * gm2/gm2-compiler/SymbolTable.def:
 * * gm2/gm2-compiler/SymbolTable.mod:
 * * gm2/gm2-compiler/bnflex.def:
 * * gm2/gm2-compiler/bnflex.mod:
 * * gm2/gm2-compiler/cflex.def:
 * * gm2/gm2-compiler/gccgm2.def:
 * * gm2/gm2-compiler/gm2.mod:
 * * gm2/gm2-compiler/gm2builtins.def:
 * * gm2/gm2-compiler/gm2except.def:
 * * gm2/gm2-compiler/gm2lcc.mod:
 * * gm2/gm2-compiler/gm2lgen.mod:
 * * gm2/gm2-compiler/gm2lorder.mod:
 * * gm2/gm2-compiler/m2flex.def:
 * * gm2/gm2-compiler/ppg.mod:
 * * gm2/gm2-libiberty/choosetemp.def:
 * * gm2/gm2-libiberty/pexecute.def:
 * * gm2/gm2-libs/ASCII.def:
 * * gm2/gm2-libs/ASCII.mod:
 * * gm2/gm2-libs/Args.def:
 * * gm2/gm2-libs/Args.mod:
 * * gm2/gm2-libs/Assertion.def:
 * * gm2/gm2-libs/Assertion.mod:
 * * gm2/gm2-libs/Break.def:
 * * gm2/gm2-libs/Break.mod:
 * * gm2/gm2-libs/Builtins.def:
 * * gm2/gm2-libs/COROUTINES.def:
 * * gm2/gm2-libs/COROUTINES.mod:
 * * gm2/gm2-libs/CmdArgs.def:
 * * gm2/gm2-libs/CmdArgs.mod:
 * * gm2/gm2-libs/Debug.def:
 * * gm2/gm2-libs/Debug.mod:
 * * gm2/gm2-libs/DynamicStrings.def:
 * * gm2/gm2-libs/DynamicStrings.mod:
 * * gm2/gm2-libs/Environment.def:
 * * gm2/gm2-libs/Environment.mod:
 * * gm2/gm2-libs/FIO.def:
 * * gm2/gm2-libs/FIO.mod:
 * * gm2/gm2-libs/FormatStrings.def:
 * * gm2/gm2-libs/FormatStrings.mod:
 * * gm2/gm2-libs/FpuIO.def:
 * * gm2/gm2-libs/FpuIO.mod:
 * * gm2/gm2-libs/Indexing.def:
 * * gm2/gm2-libs/Indexing.mod:
 * * gm2/gm2-libs/LMathLib0.def:
 * * gm2/gm2-libs/LMathLib0.mod:
 * * gm2/gm2-libs/LegacyReal.def:
 * * gm2/gm2-libs/LegacyReal.mod:
 * * gm2/gm2-libs/M2EXCEPTION.mod:
 * * gm2/gm2-libs/M2RTS.def:
 * * gm2/gm2-libs/M2RTS.mod:
 * * gm2/gm2-libs/MathLib0.def:
 * * gm2/gm2-libs/MathLib0.mod:
 * * gm2/gm2-libs/MemUtils.def:
 * * gm2/gm2-libs/MemUtils.mod:
 * * gm2/gm2-libs/NumberIO.def:
 * * gm2/gm2-libs/NumberIO.mod:
 * * gm2/gm2-libs/PushBackInput.def:
 * * gm2/gm2-libs/PushBackInput.mod:
 * * gm2/gm2-libs/RTExceptions.def:
 * * gm2/gm2-libs/RTExceptions.mod:
 * * gm2/gm2-libs/RTint.def:
 * * gm2/gm2-libs/RTint.mod:
 * * gm2/gm2-libs/SArgs.def:
 * * gm2/gm2-libs/SArgs.mod:
 * * gm2/gm2-libs/SEnvironment.def:
 * * gm2/gm2-libs/SEnvironment.mod:
 * * gm2/gm2-libs/SFIO.def:
 * * gm2/gm2-libs/SFIO.mod:
 * * gm2/gm2-libs/SMathLib0.def:
 * * gm2/gm2-libs/SMathLib0.mod:
 * * gm2/gm2-libs/SYSTEM.def:
 * * gm2/gm2-libs/SYSTEM.mod:
 * * gm2/gm2-libs/Scan.def:
 * * gm2/gm2-libs/Scan.mod:
 * * gm2/gm2-libs/Selective.def:
 * * gm2/gm2-libs/StdIO.def:
 * * gm2/gm2-libs/StdIO.mod:
 * * gm2/gm2-libs/Storage.def:
 * * gm2/gm2-libs/Storage.mod:
 * * gm2/gm2-libs/StrCase.def:
 * * gm2/gm2-libs/StrCase.mod:
 * * gm2/gm2-libs/StrIO.def:
 * * gm2/gm2-libs/StrIO.mod:
 * * gm2/gm2-libs/StrLib.def:
 * * gm2/gm2-libs/StrLib.mod:
 * * gm2/gm2-libs/SysExceptions.def:
 * * gm2/gm2-libs/SysStorage.def:
 * * gm2/gm2-libs/SysStorage.mod:
 * * gm2/gm2-libs/TimeString.def:
 * * gm2/gm2-libs/TimeString.mod:
 * * gm2/gm2-libs/UnixArgs.def:
 * * gm2/gm2-libs/cxxabi.def:
 * * gm2/gm2-libs/dtoa.def:
 * * gm2/gm2-libs/errno.def:
 * * gm2/gm2-libs/gm2-libs-host.h.in:
 * * gm2/gm2-libs/ldtoa.def:
 * * gm2/gm2-libs/libc.def:
 * * gm2/gm2-libs/libm.def:
 * * gm2/gm2-libs/sckt.def:
 * * gm2/gm2-libs/termios.def:
 * * gm2/gm2-libs/wrapc.def:
 * * gm2/gm2-libs-boot/SYSTEM.def:
 * * gm2/gm2-libs-ch/Selective.c:
 * * gm2/gm2-libs-ch/StdIO.c:
 * * gm2/gm2-libs-ch/Storage.c:
 * * gm2/gm2-libs-ch/SysExceptions.c:
 * * gm2/gm2-libs-ch/UnixArgs.c:
 * * gm2/gm2-libs-ch/choosetemp.c:
 * * gm2/gm2-libs-ch/dtoa.c:
 * * gm2/gm2-libs-ch/errno.c:
 * * gm2/gm2-libs-ch/ldtoa.c:
 * * gm2/gm2-libs-ch/libc.c:
 * * gm2/gm2-libs-ch/sckt.c:
 * * gm2/gm2-libs-ch/wrapc.c:
 * * gm2/gm2-libs-ch/xlibc.c:
 * * gm2/gm2-libs-coroutines/Debug.def:
 * * gm2/gm2-libs-coroutines/Debug.mod:
 * * gm2/gm2-libs-coroutines/Executive.def:
 * * gm2/gm2-libs-coroutines/Executive.mod:
 * * gm2/gm2-libs-coroutines/KeyBoardLEDs.c:
 * * gm2/gm2-libs-coroutines/KeyBoardLEDs.def:
 * * gm2/gm2-libs-coroutines/SYSTEM.def:
 * * gm2/gm2-libs-coroutines/SYSTEM.mod:
 * * gm2/gm2-libs-coroutines/TimerHandler.def:
 * * gm2/gm2-libs-coroutines/TimerHandler.mod:
 * * gm2/gm2-libs-iso/ChanConsts.h:
 * * gm2/gm2-libs-iso/ChanConsts.mod:
 * * gm2/gm2-libs-iso/CharClass.mod:
 * * gm2/gm2-libs-iso/ClientSocket.def:
 * * gm2/gm2-libs-iso/ClientSocket.mod:
 * * gm2/gm2-libs-iso/ComplexMath.mod:
 * * gm2/gm2-libs-iso/ConvStringLong.def:
 * * gm2/gm2-libs-iso/ConvStringLong.mod:
 * * gm2/gm2-libs-iso/ConvStringReal.def:
 * * gm2/gm2-libs-iso/ConvStringReal.mod:
 * * gm2/gm2-libs-iso/ConvTypes.mod:
 * * gm2/gm2-libs-iso/EXCEPTIONS.mod:
 * * gm2/gm2-libs-iso/ErrnoCategory.c:
 * * gm2/gm2-libs-iso/ErrnoCategory.def:
 * * gm2/gm2-libs-iso/IOChan.mod:
 * * gm2/gm2-libs-iso/IOConsts.mod:
 * * gm2/gm2-libs-iso/IOLink.mod:
 * * gm2/gm2-libs-iso/IOResult.mod:
 * * gm2/gm2-libs-iso/LongComplexMath.mod:
 * * gm2/gm2-libs-iso/LongConv.mod:
 * * gm2/gm2-libs-iso/LongMath.mod:
 * * gm2/gm2-libs-iso/LongStr.mod:
 * * gm2/gm2-libs-iso/M2RTS.def:
 * * gm2/gm2-libs-iso/M2RTS.mod:
 * * gm2/gm2-libs-iso/ProgramArgs.mod:
 * * gm2/gm2-libs-iso/RTdata.def:
 * * gm2/gm2-libs-iso/RTdata.mod:
 * * gm2/gm2-libs-iso/RTentity.def:
 * * gm2/gm2-libs-iso/RTentity.mod:
 * * gm2/gm2-libs-iso/RTfio.def:
 * * gm2/gm2-libs-iso/RTfio.mod:
 * * gm2/gm2-libs-iso/RTgen.def:
 * * gm2/gm2-libs-iso/RTgen.mod:
 * * gm2/gm2-libs-iso/RTgenif.def:
 * * gm2/gm2-libs-iso/RTgenif.mod:
 * * gm2/gm2-libs-iso/RTio.def:
 * * gm2/gm2-libs-iso/RTio.mod:
 * * gm2/gm2-libs-iso/RawIO.mod:
 * * gm2/gm2-libs-iso/RealConv.mod:
 * * gm2/gm2-libs-iso/RealMath.mod:
 * * gm2/gm2-libs-iso/RealStr.mod:
 * * gm2/gm2-libs-iso/RndFile.mod:
 * * gm2/gm2-libs-iso/SIOResult.mod:
 * * gm2/gm2-libs-iso/SLongIO.mod:
 * * gm2/gm2-libs-iso/SRawIO.mod:
 * * gm2/gm2-libs-iso/SRealIO.mod:
 * * gm2/gm2-libs-iso/STextIO.mod:
 * * gm2/gm2-libs-iso/SWholeIO.mod:
 * * gm2/gm2-libs-iso/SYSTEM.mod:
 * * gm2/gm2-libs-iso/SeqFile.mod:
 * * gm2/gm2-libs-iso/ServerSocket.def:
 * * gm2/gm2-libs-iso/ServerSocket.mod:
 * * gm2/gm2-libs-iso/ShortComplexMath.mod:
 * * gm2/gm2-libs-iso/SimpleCipher.def:
 * * gm2/gm2-libs-iso/SimpleCipher.mod:
 * * gm2/gm2-libs-iso/StdChans.mod:
 * * gm2/gm2-libs-iso/Storage.mod:
 * * gm2/gm2-libs-iso/StreamFile.mod:
 * * gm2/gm2-libs-iso/StringChan.def:
 * * gm2/gm2-libs-iso/StringChan.mod:
 * * gm2/gm2-libs-iso/Strings.mod:
 * * gm2/gm2-libs-iso/SysClock.mod:
 * * gm2/gm2-libs-iso/TERMINATION.mod:
 * * gm2/gm2-libs-iso/TermFile.mod:
 * * gm2/gm2-libs-iso/TextIO.mod:
 * * gm2/gm2-libs-iso/WholeConv.mod:
 * * gm2/gm2-libs-iso/WholeIO.mod:
 * * gm2/gm2-libs-iso/WholeStr.mod:
 * * gm2/gm2-libs-iso/wrapsock.c:
 * * gm2/gm2-libs-iso/wrapsock.def:
 * * gm2/gm2-libs-iso/wraptime.c:
 * * gm2/gm2-libs-iso/wraptime.def:
 * * gm2/gm2-libs-min/M2RTS.def:
 * * gm2/gm2-libs-min/M2RTS.mod:
 * * gm2/gm2-libs-min/SYSTEM.def:
 * * gm2/gm2-libs-min/SYSTEM.mod:
 * * gm2/gm2-libs-min/libc.def:
 * * gm2/gm2-libs-pim/BitBlockOps.def:
 * * gm2/gm2-libs-pim/BitBlockOps.mod:
 * * gm2/gm2-libs-pim/BitByteOps.def:
 * * gm2/gm2-libs-pim/BitByteOps.mod:
 * * gm2/gm2-libs-pim/BitWordOps.def:
 * * gm2/gm2-libs-pim/BitWordOps.mod:
 * * gm2/gm2-libs-pim/BlockOps.def:
 * * gm2/gm2-libs-pim/BlockOps.mod:
 * * gm2/gm2-libs-pim/Break.c:
 * * gm2/gm2-libs-pim/Break.def:
 * * gm2/gm2-libs-pim/CardinalIO.def:
 * * gm2/gm2-libs-pim/CardinalIO.mod:
 * * gm2/gm2-libs-pim/Conversions.def:
 * * gm2/gm2-libs-pim/Conversions.mod:
 * * gm2/gm2-libs-pim/DebugPMD.def:
 * * gm2/gm2-libs-pim/DebugPMD.mod:
 * * gm2/gm2-libs-pim/DebugTrace.def:
 * * gm2/gm2-libs-pim/DebugTrace.mod:
 * * gm2/gm2-libs-pim/Delay.def:
 * * gm2/gm2-libs-pim/Delay.mod:
 * * gm2/gm2-libs-pim/Display.def:
 * * gm2/gm2-libs-pim/Display.mod:
 * * gm2/gm2-libs-pim/ErrorCode.def:
 * * gm2/gm2-libs-pim/ErrorCode.mod:
 * * gm2/gm2-libs-pim/FileSystem.def:
 * * gm2/gm2-libs-pim/FileSystem.mod:
 * * gm2/gm2-libs-pim/FloatingUtilities.def:
 * * gm2/gm2-libs-pim/FloatingUtilities.mod:
 * * gm2/gm2-libs-pim/InOut.def:
 * * gm2/gm2-libs-pim/InOut.mod:
 * * gm2/gm2-libs-pim/Keyboard.def:
 * * gm2/gm2-libs-pim/Keyboard.mod:
 * * gm2/gm2-libs-pim/LongIO.def:
 * * gm2/gm2-libs-pim/LongIO.mod:
 * * gm2/gm2-libs-pim/Random.def:
 * * gm2/gm2-libs-pim/Random.mod:
 * * gm2/gm2-libs-pim/RealConversions.mod:
 * * gm2/gm2-libs-pim/RealInOut.def:
 * * gm2/gm2-libs-pim/RealInOut.mod:
 * * gm2/gm2-libs-pim/Strings.def:
 * * gm2/gm2-libs-pim/Strings.mod:
 * * gm2/gm2-libs-pim/Termbase.def:
 * * gm2/gm2-libs-pim/Termbase.mod:
 * * gm2/gm2-libs-pim/Terminal.def:
 * * gm2/gm2-libs-pim/Terminal.mod:
 * * gm2/gm2-libs-pim/TimeDate.def:
 * * gm2/gm2-libs-pim/TimeDate.mod:
 * * gm2/man/Makefile.in:
 * * gm2/p2c/Makefile.in:
 * * gm2/p2c/p2c.h:
 * * gm2/p2c/p2c-src/Makefile.in:
 * * gm2/p2c/p2c-src/auto-host.h.in:
 * * gm2/p2c/p2c-src/include/config.h:
 * * gm2/p2c/p2c-src/include/system.h:
 * * gm2/p2c/p2c-src/src/Makefile.in:
 * * gm2/p2c/p2c-src/src/citmods.c:
 * * gm2/p2c/p2c-src/src/comment.c:
 * * gm2/p2c/p2c-src/src/decl.c:
 * * gm2/p2c/p2c-src/src/dir.c:
 * * gm2/p2c/p2c-src/src/expr.c:
 * * gm2/p2c/p2c-src/src/funcs.c:
 * * gm2/p2c/p2c-src/src/hpmods.c:
 * * gm2/p2c/p2c-src/src/lex.c:
 * * gm2/p2c/p2c-src/src/loc.p2clib.c:
 * * gm2/p2c/p2c-src/src/makeproto.c:
 * * gm2/p2c/p2c-src/src/out.c:
 * * gm2/p2c/p2c-src/src/p2c-config.h:
 * * gm2/p2c/p2c-src/src/p2c.h:
 * * gm2/p2c/p2c-src/src/p2clib.c:
 * * gm2/p2c/p2c-src/src/parse.c:
 * * gm2/p2c/p2c-src/src/pexpr.c:
 * * gm2/p2c/p2c-src/src/stuff.c:
 * * gm2/p2c/p2c-src/src/trans.c:
 * * gm2/p2c/p2c-src/src/trans.h:
 * * gm2/tools-src/def2texi.py:
 * * gm2/tools-src/mklink.c:
 * * gm2/ulm-lib-gm2/processes/CoExpressions.def:
 * * gm2/ulm-lib-gm2/processes/CoExpressions.mod:
 * * gm2/ulm-lib-gm2/processes/Processes.def:
 * * gm2/ulm-lib-gm2/processes/Processes.mod:
 * * gm2/ulm-lib-gm2/std/ASCII.def:
 * * gm2/ulm-lib-gm2/std/ASCII.mod:
 * * gm2/ulm-lib-gm2/std/Archive.def:
 * * gm2/ulm-lib-gm2/std/Archive.mod:
 * * gm2/ulm-lib-gm2/std/Arguments.def:
 * * gm2/ulm-lib-gm2/std/Arguments.mod:
 * * gm2/ulm-lib-gm2/std/Calendar.def:
 * * gm2/ulm-lib-gm2/std/Calendar.mod:
 * * gm2/ulm-lib-gm2/std/CallShell.def:
 * * gm2/ulm-lib-gm2/std/CallShell.mod:
 * * gm2/ulm-lib-gm2/std/Clock.def:
 * * gm2/ulm-lib-gm2/std/Clock.mod:
 * * gm2/ulm-lib-gm2/std/Conversions.def:
 * * gm2/ulm-lib-gm2/std/Conversions.mod:
 * * gm2/ulm-lib-gm2/std/Directories.def:
 * * gm2/ulm-lib-gm2/std/Directories.mod:
 * * gm2/ulm-lib-gm2/std/Environment.def:
 * * gm2/ulm-lib-gm2/std/Environment.mod:
 * * gm2/ulm-lib-gm2/std/EtcGroup.def:
 * * gm2/ulm-lib-gm2/std/EtcGroup.mod:
 * * gm2/ulm-lib-gm2/std/Files.def:
 * * gm2/ulm-lib-gm2/std/Files.mod:
 * * gm2/ulm-lib-gm2/std/FtdIO.def:
 * * gm2/ulm-lib-gm2/std/FtdIO.mod:
 * * gm2/ulm-lib-gm2/std/Functions.def:
 * * gm2/ulm-lib-gm2/std/Functions.mod:
 * * gm2/ulm-lib-gm2/std/GetPass.def:
 * * gm2/ulm-lib-gm2/std/GetPass.mod:
 * * gm2/ulm-lib-gm2/std/InOut.def:
 * * gm2/ulm-lib-gm2/std/InOut.mod:
 * * gm2/ulm-lib-gm2/std/M2EXCEPTION.mod:
 * * gm2/ulm-lib-gm2/std/M2RTS.mod:
 * * gm2/ulm-lib-gm2/std/MathLib.def:
 * * gm2/ulm-lib-gm2/std/MathLib.mod:
 * * gm2/ulm-lib-gm2/std/Passwd.def:
 * * gm2/ulm-lib-gm2/std/Passwd.mod:
 * * gm2/ulm-lib-gm2/std/PipeIO.def:
 * * gm2/ulm-lib-gm2/std/PipeIO.mod:
 * * gm2/ulm-lib-gm2/std/Plot.def:
 * * gm2/ulm-lib-gm2/std/Plot.mod:
 * * gm2/ulm-lib-gm2/std/RTErrors.def:
 * * gm2/ulm-lib-gm2/std/RTErrors.mod:
 * * gm2/ulm-lib-gm2/std/RTExceptions.mod:
 * * gm2/ulm-lib-gm2/std/RandomGenerator.def:
 * * gm2/ulm-lib-gm2/std/RandomGenerator.mod:
 * * gm2/ulm-lib-gm2/std/ReadIntCard.def:
 * * gm2/ulm-lib-gm2/std/ReadIntCard.mod:
 * * gm2/ulm-lib-gm2/std/RealConv.def:
 * * gm2/ulm-lib-gm2/std/RealConv.mod:
 * * gm2/ulm-lib-gm2/std/RealInOut.def:
 * * gm2/ulm-lib-gm2/std/RealInOut.mod:
 * * gm2/ulm-lib-gm2/std/ScanPwfile.def:
 * * gm2/ulm-lib-gm2/std/ScanPwfile.mod:
 * * gm2/ulm-lib-gm2/std/StdFuncs.def:
 * * gm2/ulm-lib-gm2/std/StdFuncs.mod:
 * * gm2/ulm-lib-gm2/std/StdIO.def:
 * * gm2/ulm-lib-gm2/std/StdIO.mod:
 * * gm2/ulm-lib-gm2/std/Storage.def:
 * * gm2/ulm-lib-gm2/std/Storage.mod:
 * * gm2/ulm-lib-gm2/std/StrSpec.def:
 * * gm2/ulm-lib-gm2/std/StrSpec.mod:
 * * gm2/ulm-lib-gm2/std/StrToNum.def:
 * * gm2/ulm-lib-gm2/std/StrToNum.mod:
 * * gm2/ulm-lib-gm2/std/StrToReal.def:
 * * gm2/ulm-lib-gm2/std/StrToReal.mod:
 * * gm2/ulm-lib-gm2/std/Strings.def:
 * * gm2/ulm-lib-gm2/std/Strings.mod:
 * * gm2/ulm-lib-gm2/std/SysConf.def:
 * * gm2/ulm-lib-gm2/std/SysConf.mod:
 * * gm2/ulm-lib-gm2/std/SysPerror.def:
 * * gm2/ulm-lib-gm2/std/SysPerror.mod:
 * * gm2/ulm-lib-gm2/std/Terminal.def:
 * * gm2/ulm-lib-gm2/std/Terminal.mod:
 * * gm2/ulm-lib-gm2/std/TimeIO.def:
 * * gm2/ulm-lib-gm2/std/TimeIO.mod:
 * * gm2/ulm-lib-gm2/sys/Errno.def:
 * * gm2/ulm-lib-gm2/sys/Errno.mod:
 * * gm2/ulm-lib-gm2/sys/SYSTEM.def:
 * * gm2/ulm-lib-gm2/sys/Sys.def:
 * * gm2/ulm-lib-gm2/sys/Sys.mod:
 * * gm2/ulm-lib-gm2/sys/SysAccess.def:
 * * gm2/ulm-lib-gm2/sys/SysAccess.mod:
 * * gm2/ulm-lib-gm2/sys/SysAlarm.def:
 * * gm2/ulm-lib-gm2/sys/SysAlarm.mod:
 * * gm2/ulm-lib-gm2/sys/SysBreak.def:
 * * gm2/ulm-lib-gm2/sys/SysBreak.mod:
 * * gm2/ulm-lib-gm2/sys/SysClose.def:
 * * gm2/ulm-lib-gm2/sys/SysClose.mod:
 * * gm2/ulm-lib-gm2/sys/SysCreat.def:
 * * gm2/ulm-lib-gm2/sys/SysCreat.mod:
 * * gm2/ulm-lib-gm2/sys/SysDup.def:
 * * gm2/ulm-lib-gm2/sys/SysDup.mod:
 * * gm2/ulm-lib-gm2/sys/SysExec.def:
 * * gm2/ulm-lib-gm2/sys/SysExec.mod:
 * * gm2/ulm-lib-gm2/sys/SysExit.def:
 * * gm2/ulm-lib-gm2/sys/SysExit.mod:
 * * gm2/ulm-lib-gm2/sys/SysFcntl.def:
 * * gm2/ulm-lib-gm2/sys/SysFcntl.mod:
 * * gm2/ulm-lib-gm2/sys/SysFork.def:
 * * gm2/ulm-lib-gm2/sys/SysFork.mod:
 * * gm2/ulm-lib-gm2/sys/SysGetpid.def:
 * * gm2/ulm-lib-gm2/sys/SysGetpid.mod:
 * * gm2/ulm-lib-gm2/sys/SysGetuid.def:
 * * gm2/ulm-lib-gm2/sys/SysGetuid.mod:
 * * gm2/ulm-lib-gm2/sys/SysIoctl.def:
 * * gm2/ulm-lib-gm2/sys/SysIoctl.mod:
 * * gm2/ulm-lib-gm2/sys/SysKill.def:
 * * gm2/ulm-lib-gm2/sys/SysKill.mod:
 * * gm2/ulm-lib-gm2/sys/SysLink.def:
 * * gm2/ulm-lib-gm2/sys/SysLink.mod:
 * * gm2/ulm-lib-gm2/sys/SysLocations.def:
 * * gm2/ulm-lib-gm2/sys/SysLocations.mod:
 * * gm2/ulm-lib-gm2/sys/SysLseek.def:
 * * gm2/ulm-lib-gm2/sys/SysLseek.mod:
 * * gm2/ulm-lib-gm2/sys/SysOpen.def:
 * * gm2/ulm-lib-gm2/sys/SysOpen.mod:
 * * gm2/ulm-lib-gm2/sys/SysPanic.def:
 * * gm2/ulm-lib-gm2/sys/SysPanic.mod:
 * * gm2/ulm-lib-gm2/sys/SysPause.def:
 * * gm2/ulm-lib-gm2/sys/SysPause.mod:
 * * gm2/ulm-lib-gm2/sys/SysPipe.def:
 * * gm2/ulm-lib-gm2/sys/SysPipe.mod:
 * * gm2/ulm-lib-gm2/sys/SysRead.def:
 * * gm2/ulm-lib-gm2/sys/SysRead.mod:
 * * gm2/ulm-lib-gm2/sys/SysSetuid.def:
 * * gm2/ulm-lib-gm2/sys/SysSetuid.mod:
 * * gm2/ulm-lib-gm2/sys/SysSignal.def:
 * * gm2/ulm-lib-gm2/sys/SysSignal.mod:
 * * gm2/ulm-lib-gm2/sys/SysStat.def:
 * * gm2/ulm-lib-gm2/sys/SysStat.mod:
 * * gm2/ulm-lib-gm2/sys/SysTermIO.def:
 * * gm2/ulm-lib-gm2/sys/SysTermIO.mod:
 * * gm2/ulm-lib-gm2/sys/SysTime.def:
 * * gm2/ulm-lib-gm2/sys/SysTime.mod:
 * * gm2/ulm-lib-gm2/sys/SysUnlink.def:
 * * gm2/ulm-lib-gm2/sys/SysUnlink.mod:
 * * gm2/ulm-lib-gm2/sys/SysWait.def:
 * * gm2/ulm-lib-gm2/sys/SysWait.mod:
 * * gm2/ulm-lib-gm2/sys/SysWrite.def:
 * * gm2/ulm-lib-gm2/sys/SysWrite.mod:
 * * gm2/ulm-lib-gm2/sys/SystemTypes.def:
 * * gm2/ulm-lib-gm2/sys/SystemTypes.mod:
 * * gm2/ulm-lib-gm2/sys/UnixString.def:
 * * gm2/ulm-lib-gm2/sys/UnixString.mod:
 * * gm2/ulm-lib-gm2/sys/test.mod:
 * * gm2/www/Makefile.in:
 * * gm2/www/index.ms:
 *
 * Revision 1.6  2010/02/18 16:44:13  gaius
 * fixed options
 *
 * Revision 1.5  2006/09/19 20:08:34  gaius
 * *** empty log message ***
 *
 * Revision 1.4  2006/01/11 00:04:44  gaius
 * added 2006 to all Copyright dates
 *
 * Revision 1.3  2005/11/21 21:50:38  gaius
 * * fixed many Copyright dates and GPL, LGPL and FDL license
 *   issues.
 * * modified gm2/ulm-lib-gm2/std/Storage.mod to use malloc and
 *   free. This in turn fixes a runtime regression test (hello world)
 *   now works with the Ulm libraries.
 * * fixed gm2/gm2.texi to include FDL notice and also fixed all
 *   included texi files in the same way.
 * * added GPL, Modula-2 and Copyright notices to all gm2/tools-src
 *   files.
 *
 * Revision 1.2  2003/04/29 15:27:28  gaius
 * many changes made which relate to the introduction of ISO SYSTEM:
 *
 * * examples/pthreads cleaned up
 * * introduced ISO SYSTEM. Limitations, TSIZE only takes one parameter,
 *   and SHIFT, ROTATE are not implemented yet.
 * * renamed gm2-libs/Strings to gm2-libs/DynamicStrings to avoid name
 *   clash with gm2-iso/Strings
 * * p2c modified to understand DEFINITION MODULE FOR "C"
 * * gm2-libs/libc.def modified to use DEFINITION MODULE FOR "C"
 * * gm2-iso/libc.def removed
 * * linking references to libc (in gm2/init/*init) removed
 * * gm2/tools-src/def2texi finished
 * * gm2/gm2-libs.texi built via gm2/tools-src/def2texi
 * * gm2/gm2.texi now contains library definition modules and index.
 * * added -Wiso switch to gm2 driver
 *
 * Revision 1.1  2002/04/17 14:03:44  gaius
 * added build files
 *
 * Revision 1.5  2000/10/26 00:04:12  anoncvs
 * intemediate checkin still working on getting gm2 working with egcs-20001016.
 * Altered the linking programs, mklink.c and gm2lgen.mod so that they generate
 * external prototypes for initialization function.
 *
 * Revision 1.4  2000/09/16 21:22:43  gaius
 * 2000-09-14	Matthias Kurz <mk@baerlap.north.de>
 *
 * 	* Many makefile portability corrections (tests for symbol links).
 * 	  Made makeversion more portable via guessing email address.
 * 	  Added setenv emulation in libc.c.
 * 	  Inserted missing Close(fo) to gm2lgen.mod.GenMain().
 * 	  Improved README,
 * 	  Added QUIAT to the makefiles.
 * 	  Cast EOF to (char) inside mkfor.c mklink.c.
 *
 * Revision 1.3  2000/08/24 17:37:11  gaius
 * fixed the linking and install rules
 *
 * Revision 1.2  1999/12/03 17:06:24  gaius
 * interim check of the sources. The gcc backend is underway - loads
 * of bugs and only partially done. Stage1 build and will compile
 * "tiny" fragments only.
 *
 * Revision 1.1.1.1  1996/05/28 10:13:09  gaius
 * Modula-2 compiler sources imported
 *
 *
 */

#define TRUE           (1==1)
#define FALSE          (1==0)
#define MAX_FILE_NAME  4096
#define MAXSTACK       100
#define STDIN            0
#define STDOUT           1
#define ENDOFILE       ((char) -1)
#define ERROR(X)       (fprintf(stderr, "%s:%d error %s\n", __FILE__, __LINE__, X) && \
			(fflush(stderr)))
#define DEBUG(X)       ((Debug) && (fprintf(stderr, "%s\n", X) && (fflush(stderr))))


#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>


typedef struct functlist {
  char             *functname;
  struct functlist *next;
} functList;


/* Prototypes */

static void ParseFileLinkCommand (void);
static void ParseFileStartup (void);
static void ParseFile (char *Name);
static void ParseComments (void);
static void CopyUntilEof (void) ;
static void CopyUntilEol (void) ;
static int  IsSym (char *s) ;
static int  SymIs (char *s) ;
static int  FindString (char *String) ;
static void GetNL (void) ;
static char GetChar (void) ;
static void ResetBuffer (void) ;
static int  GetSingleChar (char *ch) ;
static int  InRange (int Element, unsigned int Min, unsigned int Max) ;
static char PutChar (char ch) ;
static int  IsSpace (char ch) ;
static void SkipSpaces (void) ;
static void SkipText (void) ;
static void SilentSkipSpaces (void);
static void SilentSkipText (void);
static void PushBack (char *s) ;
static int  IsDigit (char ch) ;
static void GetName (char *Name) ;
static void OpenOutputFile (void) ;
static void CloseFile (void);
static void FindSource (char *Name);
static void CopyUntilEolInto (char *Buffer);
static void FindObject (char *Name);
static int  IsExists (char *Name);

/* Global variables */ 

static char      *NameOfMain        = "main";
static int        StackPtr          = 0;
static char       Stack[MAXSTACK];
static int        CurrentFile       = STDIN;
static int        OutputFile;
static int        CopyStdout;
static int        LinkCommandLine   = FALSE;
static int        ProfilePCommand   = FALSE;
static int        ProfilePGCommand  = FALSE;
static int        ExitNeeded        = TRUE;
static char      *libraries         = NULL;
static functList *head              = NULL;
static functList *tail              = NULL;


/*
 *  addLibrary - adds libname to the list of libraries to be linked.
 */

static void addLibrary (char *libname)
{
  if (libraries == NULL)
    libraries = strdup (libname);
  else {
    char *old = libraries;
    char *new = (char *) malloc (strlen (libname) + strlen(libraries) + 1 + 1);
    strcpy (new, libraries);
    strcat (new, " ");
    strcat (new, libname);
    libraries = new;
    free (old);
  }
}


main (int argc, char *argv[])
{
    int i;

    if (argc >= 3) {
	if (strcmp(argv[1], "-l") == 0) {
	    LinkCommandLine = TRUE;
	} else if (strcmp(argv[1], "-s") == 0) {
	    LinkCommandLine = FALSE;
	} else {
	    fprintf(stderr, "Usage: mklink (-l|-s) [--pg|-p] [--lib library] [--main name] [--exit] <modulelistfile>\n");
	    fprintf(stderr, "       must supply -l or -s option\n");
	    exit(1);
	}
	ProfilePCommand = FALSE;
	ProfilePGCommand = FALSE;
	i = 2;
	while (i<argc-1) {
	  if (strcmp(argv[i], "--pg") == 0) {
	    ProfilePGCommand = TRUE;
	  } else if (strcmp(argv[i], "-p") == 0) {
	    ProfilePCommand = TRUE;
	  } else if (strcmp(argv[i], "--exit") == 0) {
	    ExitNeeded = FALSE;
	  } else if (strcmp(argv[i], "--lib") == 0) {
	    i++;
	    addLibrary(argv[i]);
	  } else if (strcmp(argv[i], "--main") == 0) {
	    i++;
	    NameOfMain = argv[i];
	  }
	  i++;
	}
	ParseFile(argv[i]);
    } else {
        fprintf(stderr, "Usage: mklink (-l|-s) [--pg|-p] [--lib library] [--main name] [--exit] <modulelistfile>\n");
	exit(1);
    }
    exit(0);
}


/*
   ParseFile - parses the input file and generates the output file.
*/

static void ParseFile (char *Name)
{
    FindSource(Name);
    OpenOutputFile();
    if (LinkCommandLine) {
	ParseFileLinkCommand();
    } else {
	ParseFileStartup();
    }
    CloseFile();
}


/*
   ParseFileLinkCommand - generates the link command.
*/

static void ParseFileLinkCommand (void)
{
  char name[MAX_FILE_NAME];
  char *s;

  s = getenv("CC");
  if (s == NULL)
    printf("gcc -g");
  else
    printf("%s -g", s);

  if (ProfilePGCommand)
    printf(" -pg");
  else if (ProfilePCommand)
    printf(" -p");

  while (PutChar(GetChar()) != (char)EOF) {
    CopyUntilEolInto(name);
#if defined(XENIX)
    name[10] = (char)0;  /* truncate object file name */
#endif
    if ((strlen(name) > 0) && (name[0] != '#'))
      FindObject(name);
  }
  printf(" %s\n", libraries);
}


/*
   FindObject - searches the M2PATH variable to find the object file.
                If it finds the object file it prints it to stdout
		otherwise it writes an error on stderr.
*/

static void FindObject (char *Name)
{
    char m2search[4096];
    char m2path  [4096];
    char name    [4096];
    char exist   [4096];
    int  s, p;

    if (getenv("M2PATH") == NULL) {
	strcpy(m2path, ".");
    } else {
	strcpy(m2path, getenv("M2PATH"));
    }
    sprintf(name, "%s.o", Name);
    p = 0;
    while (m2path[p] != (char)0) {
	s = 0;
	while ((m2path[p] != (char)0) && (m2path[p] != ' ')) {
	    m2search[s] = m2path[p];
	    s++;
	    p++;
	}
	if (m2path[p] == ' ') {
	    p++;
	}
	m2search[s] = (char)0;
	sprintf(exist, "%s/%s", m2search, name);
	if (IsExists(exist)) {
	    printf(" %s", exist);
	    return;
	}
    }
    fprintf(stderr, "cannot find %s\n", name);
}


/*
   IsExists - returns true if a file, Name, exists. It returns
              false otherwise.
*/

static int IsExists (char *Name)
{
    struct stat buf;

    return( stat(Name, &buf) == 0 );
}


/*
   add_function - adds a name to the list of functions, in order.
 */

void add_function (char *name)
{
  functList *p = (functList *)malloc(sizeof(functList));
  p->functname = (char *)malloc(strlen(name)+1);
  strcpy(p->functname, name);

  if (head == NULL) {
    head = p;
    tail = p;
    p->next = NULL;
  } else {
    tail->next = p;
    tail       = p;
    tail->next = NULL;
  }    
}


/*
   ParseFileStartup - generates the startup code.
*/

static void ParseFileStartup (void)
{
    char name[MAX_FILE_NAME];
    functList *p;

    while (PutChar(GetChar()) != (char)EOF) {
	CopyUntilEolInto(name);
	if ((strlen(name) > 0) && (strcmp(name, "mod_init") != 0) &&
            (name[0] != '#')) {
            add_function(name);
	}
    }
    p = head;

    while (p != NULL) {
      printf("extern void _M2_%s_init(int argc, char *argv[]);\n", p->functname);
      p = p->next;
    }
    printf("extern void exit(int);\n");

    p = head;
    printf("\n\nint %s(int argc, char *argv[])\n", NameOfMain);
    printf("{\n");
    while (p != NULL) {
      printf("   _M2_%s_init(argc, argv);\n", p->functname);
      p = p->next;
    }
    if (ExitNeeded) {
      printf("   exit(0);\n");
    }
    printf("   return(0);\n");
    printf("\n}\n");
}


/*
   OpenOutputFile - shut down stdout and open the new mod_init.c
*/

static void OpenOutputFile (void)
{
    char FileName[MAX_FILE_NAME];

    CopyStdout = dup(STDOUT);
    if (close(STDOUT) != 0) {
	ERROR("Unable to close stdout"); exit(1);
    }
    if (LinkCommandLine) {
	sprintf(FileName, "linkcommand");
	system("/bin/rm -f linkcommand");
    } else {
	sprintf(FileName, "mod_init.c");
	system("/bin/rm -f mod_init.c");
    }
    OutputFile = open(FileName, O_CREAT | O_RDWR, 0666);
    if (OutputFile != STDOUT) {
	ERROR("Expected that the file descriptor should be 1");
    }
}


/*
   CloseFile - flush and close the temporary par file.
*/

static void CloseFile (void)
{
    fflush(stdout);
    if (close(STDOUT) != 0) {
	ERROR("Unable to close our output file"); exit(1);
    }
    if (dup(CopyStdout) != STDOUT) {
	ERROR("Expected that dup should use Stdout");
    }
    if (close(CopyStdout) != 0) {
	ERROR("Unable to close CopyStdout"); exit(1);
    }
}


/*
   CopyUntilEof - copies from the current input marker
                  until ENDOFILE is reached.
*/

static void CopyUntilEof (void)
{
    char ch;

    while ((ch=GetChar()) != ENDOFILE) {
	putchar(ch);
    }
}


/*
   CopyUntilEol - copies from the current input marker
                  until '\n' is reached.
*/

static void CopyUntilEol (void)
{
    char ch;

    while (((ch=GetChar()) != '\n') && (ch != (char)EOF)) {
	putchar(ch);
    }
    if (ch == '\n') {
	putchar(ch);
    }
}


/*
   CopyUntilEolInto - copies from the current input marker
                      until '\n' is reached into a Buffer.
*/

static void CopyUntilEolInto (char *Buffer)
{
    char ch;
    int  i=0;

    while (((ch=GetChar()) != '\n') && (ch != (char)EOF)) {
	Buffer[i] = ch;
	i++;
    }
    if ((ch == '\n') || (ch == (char)EOF)) {
	Buffer[i] = (char)0;
    }
}


/*
   IsSym - returns true if string, s, was found in the input stream.
           The input stream is uneffected.
*/

static int IsSym (char *s)
{
    int i=0;

    while ((s[i] != (char)0) && (s[i] == PutChar(GetChar()))) {
	GetChar();
	i++;
    }
    if (s[i]==(char)0) {
	PushBack(s);
	/* found s in input string */
	return( TRUE );
    } else {
	/* push back the characters we have scanned */
	if (i>0) {
	    do {
		i--;
		PutChar(s[i]);
	    } while (i>0);
	}
	return( FALSE );
    }
}


/*
   SymIs - returns true if string, s, was found in the input stream.
           The token s is consumed from the input stream.
*/

static int SymIs (char *s)
{
    int i=0;

    while ((s[i] != (char)0) && (s[i] == PutChar(GetChar()))) {
	GetChar();
	i++;
    }
    if (s[i]==(char)0) {
	/* found s in input string */
	return( TRUE );
    } else {
	/* push back the characters we have scanned */
	if (i>0) {
	    do {
		i--;
		PutChar(s[i]);
	    } while (i>0);
	}
	return( FALSE );
    }
}


/*
   FindString - keeps on reading input until a string,
                String, is matched.
		If end of file is reached then FALSE is returned,
                otherwise TRUE is returned.
*/

static int FindString (char *String)
{
    int StringIndex=0;
    int Found      =FALSE;
    int eof        =FALSE;
    char ch;

    while ((! Found) && (!eof)) {
	if (String[StringIndex] == (char)0) {
	    /* must have found string */
	    Found = TRUE;
	} else {
	    ch = GetChar();
	    eof = (ch == ENDOFILE);
	    if (ch == String[StringIndex]) {
		StringIndex++;
	    } else {
		StringIndex = 0;
	    }
	}
    }
    return( Found );
}


/*
   GetNL - keeps on reading input from until
           a new line is found.
*/

static void GetNL (void)
{
    char ch;

    while ((ch=GetChar()) != '\n') {
	putchar(ch);
    }
    putchar('\n');
}


/*
   GetChar - returns the current character in input.
*/

static char GetChar (void)
{
    char ch;

    if (StackPtr > 0) {
	StackPtr--;
	return(Stack[StackPtr]);
    } else {
	if (GetSingleChar(&ch)) {
	    return( ch );
	} else {
	    return( ENDOFILE );
	}
    }
}


#define MAXBUF  0x1000
static int  Pointer=0;
static int  AmountRead=0;
static char Buffer[MAXBUF];


/*
   ResetBuffer - resets the buffer information to an initial state.
*/

static void ResetBuffer (void)
{
    StackPtr = 0;
    Pointer = 0;
    AmountRead = 0;
}


/*
   GetSingleChar - gets a single character from input.
                   TRUE is returned upon success.
*/

static int GetSingleChar (char *ch)
{
    if (Pointer == AmountRead) {
	AmountRead = read(CurrentFile, &Buffer, MAXBUF);
	if (AmountRead < 0) {
	    AmountRead = 0;
	}
	Pointer = 0;
    }
    if (Pointer == AmountRead) {
	*ch = ENDOFILE;
	return( FALSE );
    } else {
	*ch = Buffer[Pointer];
	Pointer++;
	return( TRUE );
    }

}

    
/*
   InRange - returns true if Element is within the range Min..Max.
*/

static int InRange (int Element, unsigned int Min, unsigned int Max)
{
    return( (Element >= Min) && (Element <= Max));
}

static int stop () {}

/*
   PutChar - pushes a character back onto input.
             This character is also returned.
*/

static char PutChar (char ch)
{
    if (StackPtr < MAXSTACK) {
	if (ch=='\n') {
	    stop();
	}
	Stack[StackPtr] = ch;
	StackPtr++;
    } else {
	ERROR("Stack overflow in PutChar");
    }
    return(ch);
}


/*
   IsSpace - returns true if character, ch, is a space.
*/

static int IsSpace (char ch)
{
    return( (ch == ' ') || (ch == '\t') );
}


/*
   SkipSpaces - eats up spaces in input.
*/

static void SkipSpaces (void)
{
    while (IsSpace(PutChar(GetChar()))) {
	putchar(GetChar());
    }
}


/*
   SilentSkipSpaces - eats up spaces in input.
*/

static void SilentSkipSpaces (void)
{
    char ch;

    while (IsSpace(PutChar(GetChar()))) {
	ch = GetChar();  /* throw away character */
    }
}


/*
   SkipText - skips ascii text, it does not skip white spaces.
*/

static void SkipText (void)
{
    while (! IsSpace(PutChar(GetChar()))) {
	putchar(GetChar());
    }
}


/*
   SilentSkipText - skips ascii text, it does not skip white spaces.
*/

static void SilentSkipText (void)
{
    char ch;

    while (! IsSpace(PutChar(GetChar()))) {
	ch = GetChar();  /* throw away character */
    }
}


/*
   PushBack - pushes a string, backwards onto the input stack.
*/

static void PushBack (char *s)
{
    int i;

    i = strlen(s);
    while (i > 0) {
	i--;
	PutChar(s[i]);
    }
}

/*
   IsDigit - returns true if a character, ch, is a decimal digit.
*/

static int IsDigit (char ch)
{
    return( ((ch >= '0') && (ch <= '9')) );
}


/*
   GetName - returns the next name found.
*/

static void GetName (char *Name)
{
    int i;
    char ch;

    SkipSpaces();
    ch = GetChar();
    i = 0;
    while (! IsSpace(ch)) {
	Name[i] = ch;
	i++;
	ch = GetChar();
    }
    Name[i]= '\0';
}


/*
   FindSource - open source file on StdIn.
*/

static void FindSource (char *Name)
{
    if (close(STDIN) != 0) {
	ERROR("close on STDIN failed");
    }
    CurrentFile = open(Name, O_RDONLY);
    if (CurrentFile < 0) {
	perror("failed to open file");
	exit(1);
    }
    if (CurrentFile != STDIN) {
	ERROR("Expecting file descriptor value of 1");
    }
}
