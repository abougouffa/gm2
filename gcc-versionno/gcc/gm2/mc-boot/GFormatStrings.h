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


#if !defined (_FormatStrings_H)
#   define _FormatStrings_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"
#   include "GDynamicStrings.h"

#   if defined (_FormatStrings_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   Sprintf0 - returns a String containing, s, after it has had its
              escape sequences translated.
*/

EXTERN DynamicStrings_String FormatStrings_Sprintf0 (DynamicStrings_String s);

/*
   Sprintf1 - returns a String containing, s, together with encapsulated
              entity, w. It only formats the first %s or %d with n.
*/

EXTERN DynamicStrings_String FormatStrings_Sprintf1 (DynamicStrings_String s, unsigned char *w_, unsigned int _w_high);

/*
   Sprintf2 - returns a string, s, which has been formatted.
*/

EXTERN DynamicStrings_String FormatStrings_Sprintf2 (DynamicStrings_String s, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);

/*
   Sprintf3 - returns a string, s, which has been formatted.
*/

EXTERN DynamicStrings_String FormatStrings_Sprintf3 (DynamicStrings_String s, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);

/*
   Sprintf4 - returns a string, s, which has been formatted.
*/

EXTERN DynamicStrings_String FormatStrings_Sprintf4 (DynamicStrings_String s, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high, unsigned char *w4_, unsigned int _w4_high);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
