#ifndef P2C_H
#define P2C_H

/*
 * This file is now part of GNU Modula-2 and is not restricted by
 * the licence used by GNU Modula-2 or "p2c". See below:
 */

/* Copyright (C) 1989, 1990, 1991, 1992, 1993, 1994, 1995,
 * 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
 * Free Software Foundation.
 *
 * Written by Dave Gillespie, daveg@csvax.cs.caltech.edu.  Version 1.20.
 * This file may be copied, modified, etc. in any way.  It is not restricted
 * by the licence agreement accompanying p2c itself.
 */

#if defined(BUILD_GM2_LIBS)
#   include "gm2-libs-host.h"
#   include "ansidecl.h"
#else
#   include "auto-host.h"
#   include "ansidecl.h"
#   include "p2c-src/src/p2c-config.h"
#endif

#  include "system.h"

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
#  define FreeR(p)    (free((Anyptr)(p)))    /* used if arg is an rvalue */
#  define Free(p)     (free((Anyptr)(p)), (p)=NULL)
#endif

typedef struct {
    Anyptr proc, link;
} _PROCEDURE;

#ifndef _FNSIZE
# define _FNSIZE  120
#endif



extern int      _OutMem     PV();
extern int      _CaseCheck  PV();
extern int      _NilCheck   PV();
extern int	_Escape     PP( (int) );
extern int	_EscIO      PP( (int) );


#endif    /* P2C_H */


/* End. */

