/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/nameKey.def.  */


#if !defined (_nameKey_H)
#   define _nameKey_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_nameKey_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

#   define nameKey_NulName 0
typedef unsigned int nameKey_Name;

EXTERN nameKey_Name nameKey_makeKey (char *a_, unsigned int _a_high);
EXTERN nameKey_Name nameKey_makekey (void * a);
EXTERN void nameKey_getKey (nameKey_Name key, char *a, unsigned int _a_high);
EXTERN unsigned int nameKey_lengthKey (nameKey_Name key);
EXTERN unsigned int nameKey_isKey (char *a_, unsigned int _a_high);
EXTERN void nameKey_writeKey (nameKey_Name key);
EXTERN unsigned int nameKey_isSameExcludingCase (nameKey_Name key1, nameKey_Name key2);
EXTERN void * nameKey_keyToCharStar (nameKey_Name key);

#   undef EXTERN
#endif
