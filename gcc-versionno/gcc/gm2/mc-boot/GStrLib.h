/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/StrLib.def.  */


#if !defined (_StrLib_H)
#   define _StrLib_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_StrLib_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN void StrLib_StrConCat (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high, char *c, unsigned int _c_high);
EXTERN unsigned int StrLib_StrLess (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high);
EXTERN unsigned int StrLib_StrEqual (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high);
EXTERN unsigned int StrLib_StrLen (char *a_, unsigned int _a_high);
EXTERN void StrLib_StrCopy (char *a_, unsigned int _a_high, char *b, unsigned int _b_high);
EXTERN unsigned int StrLib_IsSubString (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high);
EXTERN void StrLib_StrRemoveWhitePrefix (char *a_, unsigned int _a_high, char *b, unsigned int _b_high);

#   undef EXTERN
#endif
