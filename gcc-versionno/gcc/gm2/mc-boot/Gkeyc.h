/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/keyc.def.  */


#if !defined (_keyc_H)
#   define _keyc_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GmcPretty.h"
#   include "GDynamicStrings.h"
#   include "Gdecl.h"
#   include "GnameKey.h"

#   if defined (_keyc_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   useStorage - indicate we have used storage.
*/

EXTERN void keyc_useStorage (void);

/*
   useFree - indicate we have used free.
*/

EXTERN void keyc_useFree (void);

/*
   useMalloc - indicate we have used malloc.
*/

EXTERN void keyc_useMalloc (void);

/*
   useProc - indicate we have used proc.
*/

EXTERN void keyc_useProc (void);

/*
   useTrue - indicate we have used TRUE.
*/

EXTERN void keyc_useTrue (void);

/*
   useFalse - indicate we have used FALSE.
*/

EXTERN void keyc_useFalse (void);

/*
   useNull - indicate we have used NULL.
*/

EXTERN void keyc_useNull (void);

/*
   useMemcpy - indicate we have used memcpy.
*/

EXTERN void keyc_useMemcpy (void);

/*
   useIntMin - indicate we have used INT_MIN.
*/

EXTERN void keyc_useIntMin (void);

/*
   useUIntMin - indicate we have used UINT_MIN.
*/

EXTERN void keyc_useUIntMin (void);

/*
   useLongMin - indicate we have used LONG_MIN.
*/

EXTERN void keyc_useLongMin (void);

/*
   useULongMin - indicate we have used ULONG_MIN.
*/

EXTERN void keyc_useULongMin (void);

/*
   useCharMin - indicate we have used CHAR_MIN.
*/

EXTERN void keyc_useCharMin (void);

/*
   useUCharMin - indicate we have used UCHAR_MIN.
*/

EXTERN void keyc_useUCharMin (void);

/*
   useIntMax - indicate we have used INT_MAX.
*/

EXTERN void keyc_useIntMax (void);

/*
   useUIntMax - indicate we have used UINT_MAX.
*/

EXTERN void keyc_useUIntMax (void);

/*
   useLongMax - indicate we have used LONG_MAX.
*/

EXTERN void keyc_useLongMax (void);

/*
   useULongMax - indicate we have used ULONG_MAX.
*/

EXTERN void keyc_useULongMax (void);

/*
   useCharMax - indicate we have used CHAR_MAX.
*/

EXTERN void keyc_useCharMax (void);

/*
   useUCharMax - indicate we have used UChar_MAX.
*/

EXTERN void keyc_useUCharMax (void);

/*
   useLabs - indicate we have used labs.
*/

EXTERN void keyc_useLabs (void);

/*
   useAbs - indicate we have used abs.
*/

EXTERN void keyc_useAbs (void);

/*
   useFabs - indicate we have used fabs.
*/

EXTERN void keyc_useFabs (void);

/*
   useFabsl - indicate we have used fabsl.
*/

EXTERN void keyc_useFabsl (void);

/*
   useException - use the exceptions module, mcrts.
*/

EXTERN void keyc_useException (void);

/*
   useComplex - use the complex data type.
*/

EXTERN void keyc_useComplex (void);

/*
   useM2RTS - indicate we have used M2RTS in the converted code.
*/

EXTERN void keyc_useM2RTS (void);

/*
   useStrlen - indicate we have used strlen in the converted code.
*/

EXTERN void keyc_useStrlen (void);

/*
   useCtype - indicate we have used the toupper function.
*/

EXTERN void keyc_useCtype (void);

/*
   genDefs - generate definitions or includes for all
             macros and prototypes used.
*/

EXTERN void keyc_genDefs (mcPretty_pretty p);

/*
   enterScope - enter a scope defined by, n.
*/

EXTERN void keyc_enterScope (decl_node n);

/*
   leaveScope - leave the scope defined by, n.
*/

EXTERN void keyc_leaveScope (decl_node n);

/*
   cname - attempts to declare a symbol with name, n, in the
           current scope.  If there is no conflict with the
           target language then NIL is returned, otherwise
           a mangled name is returned as a String.
           If scopes is FALSE then only the keywords and
           macros are detected for a clash (all scoping
           is ignored).
*/

EXTERN DynamicStrings_String keyc_cname (nameKey_Name n, unsigned int scopes);

/*
   cnamen - attempts to declare a symbol with name, n, in the
            current scope.  If there is no conflict with the
            target language then NIL is returned, otherwise
            a mangled name is returned as a Name
            If scopes is FALSE then only the keywords and
            macros are detected for a clash (all scoping
            is ignored).
*/

EXTERN nameKey_Name keyc_cnamen (nameKey_Name n, unsigned int scopes);

/*
   cp - include C++ keywords and standard declarations to avoid.
*/

EXTERN void keyc_cp (void);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif