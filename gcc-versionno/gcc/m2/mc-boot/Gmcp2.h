/* 
   CompilationUnit - returns TRUE if the input was correct enough to parse
                     in future passes.
  */


#if !defined (_mcp2_H)
#   define _mcp2_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_mcp2_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   CompilationUnit - returns TRUE if the input was correct enough to parse
                     in future passes.
*/

EXTERN unsigned int mcp2_CompilationUnit (void);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
