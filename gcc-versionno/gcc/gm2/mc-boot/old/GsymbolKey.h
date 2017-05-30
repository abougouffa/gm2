/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/symbolKey.def.  */


#if !defined (_symbolKey_H)
#   define _symbolKey_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#include <stddef.h>
#   include "GSYSTEM.h"
#   include "GnameKey.h"

#   if defined (_symbolKey_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

#   define symbolKey_NulKey NULL
#if !defined (symbolKey_symbolTree_D)
#  define symbolKey_symbolTree_D
   typedef void *symbolKey_symbolTree;
#endif

typedef struct symbolKey_isSymbol_p symbolKey_isSymbol;

typedef struct symbolKey_performOperation_p symbolKey_performOperation;

typedef unsigned int (*symbolKey_isSymbol_t) (void *);
struct symbolKey_isSymbol_p { symbolKey_isSymbol_t proc; };

typedef void (*symbolKey_performOperation_t) (void *);
struct symbolKey_performOperation_p { symbolKey_performOperation_t proc; };

EXTERN symbolKey_symbolTree symbolKey_initTree (void);
EXTERN void symbolKey_killTree (symbolKey_symbolTree *t);
EXTERN void * symbolKey_getSymKey (symbolKey_symbolTree t, nameKey_Name name);
EXTERN void symbolKey_putSymKey (symbolKey_symbolTree t, nameKey_Name name, void * key);
EXTERN void symbolKey_delSymKey (symbolKey_symbolTree t, nameKey_Name name);
EXTERN unsigned int symbolKey_isEmptyTree (symbolKey_symbolTree t);
EXTERN unsigned int symbolKey_doesTreeContainAny (symbolKey_symbolTree t, symbolKey_isSymbol p);
EXTERN void symbolKey_foreachNodeDo (symbolKey_symbolTree t, symbolKey_performOperation p);

#   undef EXTERN
#endif
