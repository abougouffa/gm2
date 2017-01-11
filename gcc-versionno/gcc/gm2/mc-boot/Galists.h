/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/alists.def.  */


#if !defined (_alists_H)
#   define _alists_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_alists_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

#if !defined (alists_alist_D)
#  define alists_alist_D
   typedef void *alists_alist;
#endif

typedef struct alists_performOperation_p alists_performOperation;

typedef void (*alists_performOperation_t) (void *);
struct alists_performOperation_p { alists_performOperation_t proc; };


/*
   initList - creates a new alist, l.
*/

EXTERN alists_alist alists_initList (void);

/*
   killList - deletes the complete alist, l.
*/

EXTERN void alists_killList (alists_alist *l);

/*
   putItemIntoList - places an ADDRESS, c, into alist, l.
*/

EXTERN void alists_putItemIntoList (alists_alist l, void * c);

/*
   getItemFromList - retrieves the nth ADDRESS from alist, l.
*/

EXTERN void * alists_getItemFromList (alists_alist l, unsigned int n);

/*
   getIndexOfList - returns the index for ADDRESS, c, in alist, l.
                    If more than one CARDINAL, c, exists the index
                    for the first is returned.
*/

EXTERN unsigned int alists_getIndexOfList (alists_alist l, void * c);

/*
   noOfItemsInList - returns the number of items in alist, l.
*/

EXTERN unsigned int alists_noOfItemsInList (alists_alist l);

/*
   includeItemIntoList - adds an ADDRESS, c, into a alist providing
                         the value does not already exist.
*/

EXTERN void alists_includeItemIntoList (alists_alist l, void * c);

/*
   removeItemFromList - removes an ADDRESS, c, from a alist.
                        It assumes that this value only appears once.
*/

EXTERN void alists_removeItemFromList (alists_alist l, void * c);

/*
   isItemInList - returns true if a ADDRESS, c, was found in alist, l.
*/

EXTERN unsigned int alists_isItemInList (alists_alist l, void * c);

/*
   foreachItemInListDo - calls procedure, P, foreach item in alist, l.
*/

EXTERN void alists_foreachItemInListDo (alists_alist l, alists_performOperation p);

/*
   duplicateList - returns a duplicate alist derived from, l.
*/

EXTERN alists_alist alists_duplicateList (alists_alist l);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
