/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/CmdArgs.def.  */


#if !defined (_CmdArgs_H)
#   define _CmdArgs_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_CmdArgs_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN unsigned int CmdArgs_GetArg (char *CmdLine_, unsigned int _CmdLine_high, unsigned int n, char *Argi, unsigned int _Argi_high);
EXTERN unsigned int CmdArgs_Narg (char *CmdLine_, unsigned int _CmdLine_high);

#   undef EXTERN
#endif