/* Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013
 *               2014, 2015
 *               Free Software Foundation, Inc. */
/* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. */
#ifndef P2C_H
#define P2C_H

/*
 * This file is now part of GNU Modula-2 and is not restricted by
 * the licence used by GNU Modula-2 or "p2c". See below:
 */

/* Copyright (C) 1989, 1990, 1991, 1992, 1993, 1994, 1995,
 *               1996, 1997, 1998, 1999, 2000, 2001, 2002,
 *               2003, 2004, 2005, 2006, 2007, 2008, 2009,
 *               2010, 2011, 2012, 2013, 2014, 2015.
 * Free Software Foundation.
 *
 * Written by Dave Gillespie, daveg@csvax.cs.caltech.edu.  Version 1.20.
 * This file may be copied, modified, etc. in any way.  It is not restricted
 * by the licence agreement accompanying p2c itself.
 */

#if defined(BUILD_GM2_LIBS)
#   if defined(BUILD_GM2_LIBS_TARGET)
#       include "gm2-libs-target.h"
#   else
#       include "gm2-libs-host.h"
#       include "ansidecl.h"
#   endif
#else
#   include "auto-host.h"
#   include "ansidecl.h"
#   include "p2c-src/src/p2c-config.h"
#   if !defined(USE_MALLOC)
#      include "gm2-gcc/gcc-consolidation.h"
#   endif
#endif

#if !defined(USE_MALLOC)
#   include "hwint.h"
#   include "system.h"
#endif

#  define Signed    signed
#  define Void      void      /* Void f() = procedure */
#  define Const     const
#  define Volatile  volatile

#  define PP(x)     x         /* function prototype */
#  define PV()      (void)    /* null function prototype */

#define Register    register  /* Register variables */
#define Char        char      /* Characters (not bytes) */

#ifndef Static
# define Static     static    /* Private global funcs and vars */
#endif

#ifndef Local
# define Local      static    /* Nested functions */
#endif

typedef Signed   char schar;
typedef unsigned char uchar;
typedef int      BOOLEAN;
typedef void *   SYSTEM_ADDRESS;

#ifndef TRUE
# define TRUE    1
# define FALSE   0
#endif


typedef void *Anyptr;

#if !defined(IN_GCC)
/* if we are IN_GCC we must not use malloc as it is poisoned */
#  ifdef __GNUC__
/* #    define Malloc(n)   ((malloc(n)) ?: (Anyptr)_OutMem()) */
#    define Malloc(n)   (malloc(n))
#  else
extern Anyptr __MallocTemp__;
#    define Malloc(n)  ((__MallocTemp__ = malloc(n)) ? __MallocTemp__ : (Anyptr)_OutMem())
#  endif
#  undef abort
#  define FreeR(p)    (free((Anyptr)(p)))    /* used if arg is an rvalue */
#  define Free(p)     (free((Anyptr)(p)), (p)=NULL)
#endif

#    define Malloc(n)   (ggc_internal_cleared_alloc (n))
#    define Free(p)     (ggc_free((Anyptr)(p)), (p)=NULL)

typedef struct {
    Anyptr proc, link;
} _PROCEDURE;

#ifndef _FNSIZE
# define _FNSIZE  120
#endif

extern int _OutMem (void);
extern int _CaseCheck (void);
extern int _NilCheck (void);
extern int _Escape (int);
extern int _EscIO (int);
extern void M2RTS_HALT (int);

#endif    /* P2C_H */


/* End. */


