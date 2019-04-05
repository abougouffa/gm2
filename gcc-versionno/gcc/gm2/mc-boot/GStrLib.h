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


#if !defined (_StrLib_H)
#   define _StrLib_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_StrLib_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   StrConCat - combines a and b into c.
*/

EXTERN void StrLib_StrConCat (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high, char *c, unsigned int _c_high);

/*
   StrLess - returns TRUE if string, a, alphabetically occurs before
             string, b.
*/

EXTERN unsigned int StrLib_StrLess (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high);

/*
   StrEqual - performs a = b on two strings.
*/

EXTERN unsigned int StrLib_StrEqual (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high);

/*
   StrLen - returns the length of string, a.
*/

EXTERN unsigned int StrLib_StrLen (char *a_, unsigned int _a_high);

/*
   StrCopy - effectively performs b := a with two strings.
*/

EXTERN void StrLib_StrCopy (char *a_, unsigned int _a_high, char *b, unsigned int _b_high);

/*
   IsSubString - returns true if b is a subcomponent of a.
*/

EXTERN unsigned int StrLib_IsSubString (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high);

/*
   StrRemoveWhitePrefix - copies string, into string, b, excluding any white
                          space infront of a.
*/

EXTERN void StrLib_StrRemoveWhitePrefix (char *a_, unsigned int _a_high, char *b, unsigned int _b_high);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
