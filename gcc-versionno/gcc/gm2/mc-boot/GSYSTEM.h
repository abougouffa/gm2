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


#if !defined (_SYSTEM_H)
#   define _SYSTEM_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_SYSTEM_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

#   define SYSTEM_BITSPERBYTE 8
#   define SYSTEM_BYTESPERWORD 4

/*
   ShiftVal - is a runtime procedure whose job is to implement
              the SHIFT procedure of ISO SYSTEM. GNU Modula-2 will
              inline a SHIFT of a single WORD sized set and will only
              call this routine for larger sets.
*/

EXTERN void SYSTEM_ShiftVal (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, int ShiftCount);

/*
   ShiftLeft - performs the shift left for a multi word set.
               This procedure might be called by the back end of
               GNU Modula-2 depending whether amount is known at compile
               time.
*/

EXTERN void SYSTEM_ShiftLeft (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, unsigned int ShiftCount);

/*
   ShiftRight - performs the shift left for a multi word set.
                This procedure might be called by the back end of
                GNU Modula-2 depending whether amount is known at compile
                time.
*/

EXTERN void SYSTEM_ShiftRight (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, unsigned int ShiftCount);

/*
   RotateVal - is a runtime procedure whose job is to implement
               the ROTATE procedure of ISO SYSTEM. GNU Modula-2 will
               inline a ROTATE of a single WORD (or less)
               sized set and will only call this routine for larger sets.
*/

EXTERN void SYSTEM_RotateVal (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, int RotateCount);

/*
   RotateLeft - performs the rotate left for a multi word set.
                This procedure might be called by the back end of
                GNU Modula-2 depending whether amount is known at compile
                time.
*/

EXTERN void SYSTEM_RotateLeft (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, unsigned int RotateCount);

/*
   RotateRight - performs the rotate right for a multi word set.
                 This procedure might be called by the back end of
                 GNU Modula-2 depending whether amount is known at compile
                 time.
*/

EXTERN void SYSTEM_RotateRight (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, unsigned int RotateCount);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
