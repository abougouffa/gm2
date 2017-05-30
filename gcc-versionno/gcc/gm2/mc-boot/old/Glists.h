/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/lists.def.  */


#if !defined (_lists_H)
#   define _lists_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"
#   include "GsymbolKey.h"

#   if defined (_lists_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

#if !defined (lists_list_D)
#  define lists_list_D
   typedef void *lists_list;
#endif

EXTERN lists_list lists_initList (void);
EXTERN void lists_killList (lists_list *l);
EXTERN void lists_putItemIntoList (lists_list l, void * c);
EXTERN void * lists_getItemFromList (lists_list l, unsigned int n);
EXTERN unsigned int lists_getIndexOfList (lists_list l, void * c);
EXTERN unsigned int lists_noOfItemsInList (lists_list l);
EXTERN void lists_includeItemIntoList (lists_list l, void * c);
EXTERN void lists_removeItemFromList (lists_list l, void * c);
EXTERN unsigned int lists_isItemInList (lists_list l, void * c);
EXTERN void lists_foreachItemInListDo (lists_list l, symbolKey_performOperation p);
EXTERN lists_list lists_duplicateList (lists_list l);

#   undef EXTERN
#endif
