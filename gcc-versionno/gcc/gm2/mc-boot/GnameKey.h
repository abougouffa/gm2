/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/nameKey.def.  */


#if !defined (_nameKey_H)
#   define _nameKey_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_nameKey_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

#   define nameKey_NulName 0
typedef unsigned int nameKey_Name;


/*
   makeKey - returns the Key of the symbol, a. If a is not in the
             name table then it is added, otherwise the Key of a is returned
             directly. Note that the name table has no scope - it merely
             presents a more convienient way of expressing strings. By a Key.
             These keys last for the duration of compilation.
*/

EXTERN nameKey_Name nameKey_makeKey (char *a_, unsigned int _a_high);

/*
   makekey - returns the Key of the symbol, a. If a is not in the
             name table then it is added, otherwise the Key of a is returned
             directly. Note that the name table has no scope - it merely
             presents a more convienient way of expressing strings. By a Key.
             These keys last for the duration of compilation.
*/

EXTERN nameKey_Name nameKey_makekey (void * a);

/*
   getKey - returns the name, a, of the key, key.
*/

EXTERN void nameKey_getKey (nameKey_Name key, char *a, unsigned int _a_high);

/*
   lengthKey - returns the length of a Key.
*/

EXTERN unsigned int nameKey_lengthKey (nameKey_Name key);

/*
   isKey - returns TRUE if string, a, is currently a key.
*/

EXTERN unsigned int nameKey_isKey (char *a_, unsigned int _a_high);

/*
   writeKey - Display the symbol represented by Key.
*/

EXTERN void nameKey_writeKey (nameKey_Name key);

/*
   isSameExcludingCase - returns TRUE if key1 and key2 are
                         the same. It is case insensitive.
*/

EXTERN unsigned int nameKey_isSameExcludingCase (nameKey_Name key1, nameKey_Name key2);

/*
   keyToCharStar - returns the C char * string equivalent for, key.
*/

EXTERN void * nameKey_keyToCharStar (nameKey_Name key);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif