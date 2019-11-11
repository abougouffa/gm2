

#if !defined (_symbolKey_H)
#   define _symbolKey_H

#   ifdef __cplusplus
extern "C" {
#   endif
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
#      define EXTERN extern
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


/*
   initTree - initializes a symbolTree pointed to by t.
*/

EXTERN symbolKey_symbolTree symbolKey_initTree (void);

/*
   killTree - destroys the symbolTree pointed to by t.
*/

EXTERN void symbolKey_killTree (symbolKey_symbolTree *t);

/*
   getSymKey - searches the symbolTree t for an entry name. If
               found then the key is returned otherwise NulKey
               is returned.
*/

EXTERN void * symbolKey_getSymKey (symbolKey_symbolTree t, nameKey_Name name);

/*
   putSymKey - puts an symbol entry, name, in the symbolTree t.
               SymKey is the value stored with name.
*/

EXTERN void symbolKey_putSymKey (symbolKey_symbolTree t, nameKey_Name name, void * key);

/*
   delSymKey - deletes a symbol entry name in the symbolTree, t.
*/

EXTERN void symbolKey_delSymKey (symbolKey_symbolTree t, nameKey_Name name);

/*
   isEmptyTree - returns true if symbolTree, t, is empty.
*/

EXTERN unsigned int symbolKey_isEmptyTree (symbolKey_symbolTree t);

/*
   doesTreeContainAny - returns true if symbolTree, t, contains any
                        symbols which in turn return true when procedure,
                        p, is called with a symbol as its parameter.
*/

EXTERN unsigned int symbolKey_doesTreeContainAny (symbolKey_symbolTree t, symbolKey_isSymbol p);

/*
   foreachNodeDo - for each node in symbolTree, t, a procedure, p,
                   is called with the node symbol as its parameter.
                   It traverse the tree in order.
*/

EXTERN void symbolKey_foreachNodeDo (symbolKey_symbolTree t, symbolKey_performOperation p);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
