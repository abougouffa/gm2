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


#if !defined (_wrapc_H)
#   define _wrapc_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_wrapc_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   strtime - returns the C string for the equivalent C asctime
             function.
*/

EXTERN void * wrapc_strtime (void);

/*
   filesize - assigns the size of a file, f, into low, high and
              returns zero if successful.
*/

EXTERN int wrapc_filesize (int f, unsigned int *low, unsigned int *high);

/*
   fileinode - return the inode associated with file, f.
*/

EXTERN int wrapc_fileinode (int f, unsigned int *low, unsigned int *high);

/*
   filemtime - returns the mtime of a file, f.
*/

EXTERN int wrapc_filemtime (int f);

/*
   getrand - returns a random number between 0..n-1
*/

EXTERN int wrapc_getrand (int n);

/*
   getusername - returns a C string describing the current user.
*/

EXTERN void * wrapc_getusername (void);

/*
   getnameuidgid - fills in the, uid, and, gid, which represents
                   user, name.
*/

EXTERN void wrapc_getnameuidgid (void * name, int *uid, int *gid);
EXTERN int wrapc_signbit (double r);
EXTERN int wrapc_signbitf (float s);
EXTERN int wrapc_signbitl (long double l);

/*
   isfinite - provide non builtin alternative to the gcc builtin isfinite.
              Returns 1 if x is finite and 0 if it is not.
*/

EXTERN int wrapc_isfinite (double x);

/*
   isfinitef - provide non builtin alternative to the gcc builtin isfinite.
               Returns 1 if x is finite and 0 if it is not.
*/

EXTERN int wrapc_isfinitef (float x);

/*
   isfinitel - provide non builtin alternative to the gcc builtin isfinite.
               Returns 1 if x is finite and 0 if it is not.
*/

EXTERN int wrapc_isfinitel (long double x);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
