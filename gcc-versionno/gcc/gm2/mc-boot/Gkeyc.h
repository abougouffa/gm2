/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/keyc.def.  */


#if !defined (_keyc_H)
#   define _keyc_H

#ifdef __cplusplus
extern "C" {
#endif
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

EXTERN void keyc_useStorage (void);
EXTERN void keyc_useFree (void);
EXTERN void keyc_useMalloc (void);
EXTERN void keyc_useProc (void);
EXTERN void keyc_useTrue (void);
EXTERN void keyc_useFalse (void);
EXTERN void keyc_useNull (void);
EXTERN void keyc_useMemcpy (void);
EXTERN void keyc_useIntMin (void);
EXTERN void keyc_useUIntMin (void);
EXTERN void keyc_useLongMin (void);
EXTERN void keyc_useULongMin (void);
EXTERN void keyc_useCharMin (void);
EXTERN void keyc_useUCharMin (void);
EXTERN void keyc_useIntMax (void);
EXTERN void keyc_useUIntMax (void);
EXTERN void keyc_useLongMax (void);
EXTERN void keyc_useULongMax (void);
EXTERN void keyc_useCharMax (void);
EXTERN void keyc_useUCharMax (void);
EXTERN void keyc_useLabs (void);
EXTERN void keyc_useAbs (void);
EXTERN void keyc_useFabs (void);
EXTERN void keyc_useFabsl (void);
EXTERN void keyc_useException (void);
EXTERN void keyc_genDefs (mcPretty_pretty p);
EXTERN void keyc_enterScope (decl_node n);
EXTERN void keyc_leaveScope (decl_node n);
EXTERN DynamicStrings_String keyc_cname (nameKey_Name n, unsigned int scopes);
EXTERN nameKey_Name keyc_cnamen (nameKey_Name n, unsigned int scopes);
EXTERN void keyc_cp (void);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
