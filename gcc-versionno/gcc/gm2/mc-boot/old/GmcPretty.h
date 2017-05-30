/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcPretty.def.  */


#if !defined (_mcPretty_H)
#   define _mcPretty_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GDynamicStrings.h"

#   if defined (_mcPretty_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

#if !defined (mcPretty_pretty_D)
#  define mcPretty_pretty_D
   typedef void *mcPretty_pretty;
#endif

typedef struct mcPretty_writeProc_p mcPretty_writeProc;

typedef struct mcPretty_writeLnProc_p mcPretty_writeLnProc;

typedef void (*mcPretty_writeProc_t) (char);
struct mcPretty_writeProc_p { mcPretty_writeProc_t proc; };

typedef void (*mcPretty_writeLnProc_t) (void);
struct mcPretty_writeLnProc_p { mcPretty_writeLnProc_t proc; };

EXTERN mcPretty_pretty mcPretty_initPretty (mcPretty_writeProc w, mcPretty_writeLnProc l);
EXTERN mcPretty_pretty mcPretty_dupPretty (mcPretty_pretty p);
EXTERN void mcPretty_killPretty (mcPretty_pretty *p);
EXTERN mcPretty_pretty mcPretty_pushPretty (mcPretty_pretty p);
EXTERN mcPretty_pretty mcPretty_popPretty (mcPretty_pretty p);
EXTERN unsigned int mcPretty_getindent (mcPretty_pretty p);
EXTERN void mcPretty_setindent (mcPretty_pretty p, unsigned int n);
EXTERN unsigned int mcPretty_getcurpos (mcPretty_pretty s);
EXTERN unsigned int mcPretty_getseekpos (mcPretty_pretty s);
EXTERN unsigned int mcPretty_getcurline (mcPretty_pretty s);
EXTERN void mcPretty_setNeedSpace (mcPretty_pretty s);
EXTERN void mcPretty_noSpace (mcPretty_pretty s);
EXTERN void mcPretty_print (mcPretty_pretty p, char *a_, unsigned int _a_high);
EXTERN void mcPretty_prints (mcPretty_pretty p, DynamicStrings_String s);
EXTERN void mcPretty_raw (mcPretty_pretty p, DynamicStrings_String s);

#   undef EXTERN
#endif
