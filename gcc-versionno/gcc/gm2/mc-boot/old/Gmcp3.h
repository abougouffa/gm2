/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcp3.def.  */


#if !defined (_mcp3_H)
#   define _mcp3_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_mcp3_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN unsigned int mcp3_CompilationUnit (void);

#   undef EXTERN
#endif