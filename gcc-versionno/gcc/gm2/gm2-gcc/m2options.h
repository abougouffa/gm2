/* Copyright (C) 2012
 * Free Software Foundation, Inc.
 *
 *  Gaius Mulley <gaius@glam.ac.uk> constructed this file.
 */

/*
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
along with GNU Modula-2; see the file COPYING.  If not, write to the
Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.
*/

#if !defined(m2options_h)

#  define m2options_h
#  if defined(m2options_c)
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN 
#      endif
#  else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#  endif

#include "input.h"


EXTERN void M2Options_SetMakeIncludePath (const char *arg);
EXTERN void M2Options_SetSearchPath (const char *arg);
EXTERN void M2Options_setdefextension (const char *arg);
EXTERN void M2Options_setmodextension (const char *arg);

EXTERN void M2Options_SetISO (int value);
EXTERN void M2Options_SetPIM (int value);
EXTERN void M2Options_SetPIM2 (int value);
EXTERN void M2Options_SetPIM3 (int value);
EXTERN void M2Options_SetPIM4 (int value);

EXTERN int M2Options_GetISO (void);
EXTERN int M2Options_GetPIM (void);
EXTERN int M2Options_GetPIM2 (void);
EXTERN int M2Options_GetPIM3 (void);
EXTERN int M2Options_GetPIM4 (void);
EXTERN int M2Options_GetPositiveModFloor (void);

EXTERN void M2Options_SetPositiveModFloor (int value);
EXTERN void M2Options_SetNilCheck (int value);
EXTERN void M2Options_SetWholeDiv (int value);
EXTERN void M2Options_SetIndex (int value);
EXTERN void M2Options_SetRange (int value);
EXTERN void M2Options_SetReturnCheck (int value);
EXTERN void M2Options_SetCaseCheck (int value);
EXTERN void M2Options_SetCheckAll (int value);
EXTERN void M2Options_SetExceptions (int value);
EXTERN void M2Options_SetStudents (int value);
EXTERN void M2Options_SetPedantic (int value);
EXTERN void M2Options_SetPedanticParamNames (int value);
EXTERN void M2Options_SetPedanticCast (int value);
EXTERN void M2Options_SetExtendedOpaque (int value);
EXTERN void M2Options_SetVerboseUnbounded (int value);
EXTERN void M2Options_SetXCode (int value);
EXTERN void M2Options_SetCompilerDebugging (int value);
EXTERN void M2Options_SetQuadDebugging (int value);
EXTERN void M2Options_SetDebugTraceQuad (int value);
EXTERN void M2Options_SetDebugTraceAPI (int value);
EXTERN void M2Options_SetSources (int value);
EXTERN void M2Options_SetUnboundedByReference (int value);
EXTERN void M2Options_SetDumpSystemExports (int value);
EXTERN void M2Options_SetOptimizing (int value);
EXTERN void M2Options_SetQuiet (int value);
EXTERN void M2Options_SetCC1Quiet (int value);
EXTERN void M2Options_SetCpp (int value);
EXTERN void M2Options_SetSwig (int value);
EXTERN void M2Options_SetForcedLocation (location_t location);
EXTERN location_t M2Options_OverrideLocation (location_t location);
EXTERN void M2Options_SetStatistics (int on);
EXTERN void M2Options_CppProg (const char *program);
EXTERN void M2Options_CppArg (const char *opt, const char *arg, int joined);
EXTERN void M2Options_SetWholeProgram (int value);
EXTERN void M2Options_FinaliseOptions (void);

EXTERN void M2Options_DisplayVersion (int mustExit);

#undef EXTERN
#endif
