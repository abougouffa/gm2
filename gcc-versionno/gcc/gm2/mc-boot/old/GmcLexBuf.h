/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcLexBuf.def.  */


#if !defined (_mcLexBuf_H)
#   define _mcLexBuf_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"
#   include "GmcReserved.h"
#   include "GDynamicStrings.h"

#   if defined (_mcLexBuf_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN mcReserved_toktype mcLexBuf_currenttoken;
EXTERN void * mcLexBuf_currentstring;
EXTERN unsigned int mcLexBuf_currentcolumn;
EXTERN int mcLexBuf_currentinteger;
EXTERN unsigned int mcLexBuf_openSource (DynamicStrings_String s);
EXTERN void mcLexBuf_closeSource (void);
EXTERN void mcLexBuf_reInitialize (void);
EXTERN void mcLexBuf_resetForNewPass (void);
EXTERN void mcLexBuf_getToken (void);
EXTERN void mcLexBuf_insertToken (mcReserved_toktype token);
EXTERN void mcLexBuf_insertTokenAndRewind (mcReserved_toktype token);
EXTERN unsigned int mcLexBuf_getPreviousTokenLineNo (void);
EXTERN unsigned int mcLexBuf_getLineNo (void);
EXTERN unsigned int mcLexBuf_getTokenNo (void);
EXTERN unsigned int mcLexBuf_tokenToLineNo (unsigned int tokenNo, unsigned int depth);
EXTERN unsigned int mcLexBuf_getColumnNo (void);
EXTERN unsigned int mcLexBuf_tokenToColumnNo (unsigned int tokenNo, unsigned int depth);
EXTERN DynamicStrings_String mcLexBuf_findFileNameFromToken (unsigned int tokenNo, unsigned int depth);
EXTERN DynamicStrings_String mcLexBuf_getFileName (void);
EXTERN void mcLexBuf_addTok (mcReserved_toktype t);
EXTERN void mcLexBuf_addTokCharStar (mcReserved_toktype t, void * s);
EXTERN void mcLexBuf_addTokInteger (mcReserved_toktype t, int i);
EXTERN void mcLexBuf_setFile (void * filename);
EXTERN void mcLexBuf_pushFile (void * filename);
EXTERN void mcLexBuf_popFile (void * filename);

#   undef EXTERN
#endif
