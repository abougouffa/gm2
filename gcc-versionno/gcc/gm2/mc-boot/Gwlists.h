/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/wlists.def.  */


#if !defined (_wlists_H)
#   define _wlists_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_wlists_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

#if !defined (wlists_wlist_D)
#  define wlists_wlist_D
   typedef void *wlists_wlist;
#endif

typedef struct wlists_performOperation_p wlists_performOperation;

typedef void (*wlists_performOperation_t) (unsigned int);
struct wlists_performOperation_p { wlists_performOperation_t proc; };

EXTERN wlists_wlist wlists_initList (void);
EXTERN void wlists_killList (wlists_wlist *l);
EXTERN void wlists_putItemIntoList (wlists_wlist l, unsigned int c);
EXTERN unsigned int wlists_getItemFromList (wlists_wlist l, unsigned int n);
EXTERN unsigned int wlists_getIndexOfList (wlists_wlist l, unsigned int c);
EXTERN unsigned int wlists_noOfItemsInList (wlists_wlist l);
EXTERN void wlists_includeItemIntoList (wlists_wlist l, unsigned int c);
EXTERN void wlists_removeItemFromList (wlists_wlist l, unsigned int c);
EXTERN void wlists_replaceItemInList (wlists_wlist l, unsigned int n, unsigned int w);
EXTERN unsigned int wlists_isItemInList (wlists_wlist l, unsigned int c);
EXTERN void wlists_foreachItemInListDo (wlists_wlist l, wlists_performOperation p);
EXTERN wlists_wlist wlists_duplicateList (wlists_wlist l);

#   undef EXTERN
#endif
