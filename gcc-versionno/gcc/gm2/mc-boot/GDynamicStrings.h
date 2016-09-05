/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/DynamicStrings.def.  */


#if !defined (_DynamicStrings_H)
#   define _DynamicStrings_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_DynamicStrings_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

#if !defined (DynamicStrings_String_D)
#  define DynamicStrings_String_D
   typedef void *DynamicStrings_String;
#endif

EXTERN DynamicStrings_String DynamicStrings_InitString (char *a_, unsigned int _a_high);
EXTERN DynamicStrings_String DynamicStrings_KillString (DynamicStrings_String s);
EXTERN void DynamicStrings_Fin (DynamicStrings_String s);
EXTERN DynamicStrings_String DynamicStrings_InitStringCharStar (void * a);
EXTERN DynamicStrings_String DynamicStrings_InitStringChar (char ch);
EXTERN DynamicStrings_String DynamicStrings_Mark (DynamicStrings_String s);
EXTERN unsigned int DynamicStrings_Length (DynamicStrings_String s);
EXTERN DynamicStrings_String DynamicStrings_ConCat (DynamicStrings_String a, DynamicStrings_String b);
EXTERN DynamicStrings_String DynamicStrings_ConCatChar (DynamicStrings_String a, char ch);
EXTERN DynamicStrings_String DynamicStrings_Assign (DynamicStrings_String a, DynamicStrings_String b);
EXTERN DynamicStrings_String DynamicStrings_Dup (DynamicStrings_String s);
EXTERN DynamicStrings_String DynamicStrings_Add (DynamicStrings_String a, DynamicStrings_String b);
EXTERN unsigned int DynamicStrings_Equal (DynamicStrings_String a, DynamicStrings_String b);
EXTERN unsigned int DynamicStrings_EqualCharStar (DynamicStrings_String s, void * a);
EXTERN unsigned int DynamicStrings_EqualArray (DynamicStrings_String s, char *a_, unsigned int _a_high);
EXTERN DynamicStrings_String DynamicStrings_Mult (DynamicStrings_String s, unsigned int n);
EXTERN DynamicStrings_String DynamicStrings_Slice (DynamicStrings_String s, int low, int high);
EXTERN int DynamicStrings_Index (DynamicStrings_String s, char ch, unsigned int o);
EXTERN int DynamicStrings_RIndex (DynamicStrings_String s, char ch, unsigned int o);
EXTERN DynamicStrings_String DynamicStrings_RemoveComment (DynamicStrings_String s, char comment);
EXTERN DynamicStrings_String DynamicStrings_RemoveWhitePrefix (DynamicStrings_String s);
EXTERN DynamicStrings_String DynamicStrings_RemoveWhitePostfix (DynamicStrings_String s);
EXTERN DynamicStrings_String DynamicStrings_ToUpper (DynamicStrings_String s);
EXTERN DynamicStrings_String DynamicStrings_ToLower (DynamicStrings_String s);
EXTERN void DynamicStrings_CopyOut (char *a, unsigned int _a_high, DynamicStrings_String s);
EXTERN char DynamicStrings_char (DynamicStrings_String s, int i);
EXTERN void * DynamicStrings_string (DynamicStrings_String s);
EXTERN DynamicStrings_String DynamicStrings_InitStringDB (char *a_, unsigned int _a_high, char *file_, unsigned int _file_high, unsigned int line);
EXTERN DynamicStrings_String DynamicStrings_InitStringCharStarDB (void * a, char *file_, unsigned int _file_high, unsigned int line);
EXTERN DynamicStrings_String DynamicStrings_InitStringCharDB (char ch, char *file_, unsigned int _file_high, unsigned int line);
EXTERN DynamicStrings_String DynamicStrings_MultDB (DynamicStrings_String s, unsigned int n, char *file_, unsigned int _file_high, unsigned int line);
EXTERN DynamicStrings_String DynamicStrings_DupDB (DynamicStrings_String s, char *file_, unsigned int _file_high, unsigned int line);
EXTERN DynamicStrings_String DynamicStrings_SliceDB (DynamicStrings_String s, int low, int high, char *file_, unsigned int _file_high, unsigned int line);
EXTERN void DynamicStrings_PushAllocation (void);
EXTERN void DynamicStrings_PopAllocation (unsigned int halt);
EXTERN DynamicStrings_String DynamicStrings_PopAllocationExemption (unsigned int halt, DynamicStrings_String e);

#   undef EXTERN
#endif
