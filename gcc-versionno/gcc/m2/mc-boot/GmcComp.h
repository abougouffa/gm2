/* 
    Title      : mcComp
    Author     : Gaius Mulley
    System     : GNU Modula-2
    Date       : Tue Nov 17 16:22:34 2015
    Revision   : $Version$
    Description: provides a procedure which coordinates all passes of mc.
  */


#if !defined (_mcComp_H)
#   define _mcComp_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GDynamicStrings.h"

#   if defined (_mcComp_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   compile - translate file, s, using a 6 pass technique.
*/

EXTERN void mcComp_compile (DynamicStrings_String s);

/*
   getPassNo - return the pass no.
*/

EXTERN unsigned int mcComp_getPassNo (void);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
