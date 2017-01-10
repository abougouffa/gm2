/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcflex.def.  */


#if !defined (_mcflex_H)
#   define _mcflex_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_mcflex_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   closeSource - provided for semantic sugar
*/

EXTERN void mcflex_closeSource (void);
EXTERN unsigned int mcflex_openSource (void * s);

/*
   getToken - returns the ADDRESS of the next token.
*/

EXTERN void * mcflex_getToken (void);

/*
   getLineNo - returns the current line number.
*/

EXTERN unsigned int mcflex_getLineNo (void);

/*
   getColumnNo - returns the column where the current token starts.
*/

EXTERN unsigned int mcflex_getColumnNo (void);

/*
   mcError - displays the error message, s, after the code line and pointer
             to the erroneous token.
*/

EXTERN void mcflex_mcError (void * s);

/*
   getTotalLines - returns the total number of lines parsed.
*/

EXTERN unsigned int mcflex_getTotalLines (void);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
