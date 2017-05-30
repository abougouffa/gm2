/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/lists.def.  */


#if !defined (_lists_H)
#   define _lists_H

#   ifdef __cplusplus
extern "C" {
#   endif
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
#      define EXTERN extern
#   endif

#if !defined (lists_list_D)
#  define lists_list_D
   typedef void *lists_list;
#endif


/*
   initList - creates a new list, l.
*/

EXTERN lists_list lists_initList (void);

/*
   killList - deletes the complete list, l.
*/

EXTERN void lists_killList (lists_list *l);

/*
   putItemIntoList - places an ADDRESS, c, into list, l.
*/

EXTERN void lists_putItemIntoList (lists_list l, void * c);

/*
   getItemFromList - retrieves the nth ADDRESS from list, l.
*/

EXTERN void * lists_getItemFromList (lists_list l, unsigned int n);

/*
   getIndexOfList - returns the index for ADDRESS, c, in list, l.
                    If more than one CARDINAL, c, exists the index
                    for the first is returned.
*/

EXTERN unsigned int lists_getIndexOfList (lists_list l, void * c);

/*
   noOfItemsInList - returns the number of items in list, l.
*/

EXTERN unsigned int lists_noOfItemsInList (lists_list l);

/*
   includeItemIntoList - adds an ADDRESS, c, into a list providing
                         the value does not already exist.
*/

EXTERN void lists_includeItemIntoList (lists_list l, void * c);

/*
   removeItemFromList - removes an ADDRESS, c, from a list.
                        It assumes that this value only appears once.
*/

EXTERN void lists_removeItemFromList (lists_list l, void * c);

/*
   isItemInList - returns true if a ADDRESS, c, was found in list, l.
*/

EXTERN unsigned int lists_isItemInList (lists_list l, void * c);

/*
   foreachItemInListDo - calls procedure, P, foreach item in list, l.
*/

EXTERN void lists_foreachItemInListDo (lists_list l, symbolKey_performOperation p);

/*
   duplicateList - returns a duplicate list derived from, l.
*/

EXTERN lists_list lists_duplicateList (lists_list l);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
