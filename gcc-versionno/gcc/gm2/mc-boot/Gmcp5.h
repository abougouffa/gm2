/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcp5.def.  */


#if !defined (_mcp5_H)
#   define _mcp5_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_mcp5_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   CompilationUnit - returns TRUE if the input was correct enough to parse
                     in future passes.
*/

EXTERN unsigned int mcp5_CompilationUnit (void);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
