/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/alists.def.  */


#if !defined (_alists_H)
#   define _alists_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_alists_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

#if !defined (alists_alist_D)
#  define alists_alist_D
   typedef void *alists_alist;
#endif

typedef struct alists_performOperation_p alists_performOperation;

typedef void (*alists_performOperation_t) (void *);
struct alists_performOperation_p { alists_performOperation_t proc; };

EXTERN alists_alist alists_initList (void);
EXTERN void alists_killList (alists_alist *l);
EXTERN void alists_putItemIntoList (alists_alist l, void * c);
EXTERN void * alists_getItemFromList (alists_alist l, unsigned int n);
EXTERN unsigned int alists_getIndexOfList (alists_alist l, void * c);
EXTERN unsigned int alists_noOfItemsInList (alists_alist l);
EXTERN void alists_includeItemIntoList (alists_alist l, void * c);
EXTERN void alists_removeItemFromList (alists_alist l, void * c);
EXTERN unsigned int alists_isItemInList (alists_alist l, void * c);
EXTERN void alists_foreachItemInListDo (alists_alist l, alists_performOperation p);
EXTERN alists_alist alists_duplicateList (alists_alist l);

#   undef EXTERN
#endif
