/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/Debug.def.  */


#if !defined (_Debug_H)
#   define _Debug_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_Debug_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN void Debug_Halt (char *Message_, unsigned int _Message_high, unsigned int LineNo, char *Module_, unsigned int _Module_high);
EXTERN void Debug_DebugString (char *a_, unsigned int _a_high);

#   undef EXTERN
#endif