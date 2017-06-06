/* automatically created by mc from ../../gcc-versionno/gcc/gm2/mc/mcp1.def.  */


#if !defined (_mcp1_H)
#   define _mcp1_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_mcp1_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   CompilationUnit - returns TRUE if the input was correct enough to parse
                     in future passes.
*/

EXTERN unsigned int mcp1_CompilationUnit (void);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
