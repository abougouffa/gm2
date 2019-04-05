/* Copyright (C) 2019 Free Software Foundation, Inc.

This file is part of GNU Modula-2.

GNU Modula-2 is software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING.  If not,
see <https://www.gnu.org/licenses/>.  */


#if !defined (_CmdArgs_H)
#   define _CmdArgs_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_CmdArgs_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   GetArg - returns the nth argument from the command line, CmdLine
            the success of the operation is returned.
*/

EXTERN unsigned int CmdArgs_GetArg (char *CmdLine_, unsigned int _CmdLine_high, unsigned int n, char *Argi, unsigned int _Argi_high);

/*
   Narg - returns the number of arguments available from
          command line, CmdLine.
*/

EXTERN unsigned int CmdArgs_Narg (char *CmdLine_, unsigned int _CmdLine_high);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
