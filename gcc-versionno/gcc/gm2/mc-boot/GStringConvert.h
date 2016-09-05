/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/StringConvert.def.  */


#if !defined (_StringConvert_H)
#   define _StringConvert_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GDynamicStrings.h"

#   if defined (_StringConvert_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN DynamicStrings_String StringConvert_IntegerToString (int i, unsigned int width, char padding, unsigned int sign, unsigned int base, unsigned int lower);
EXTERN DynamicStrings_String StringConvert_CardinalToString (unsigned int c, unsigned int width, char padding, unsigned int base, unsigned int lower);
EXTERN int StringConvert_StringToInteger (DynamicStrings_String s, unsigned int base, unsigned int *found);
EXTERN unsigned int StringConvert_StringToCardinal (DynamicStrings_String s, unsigned int base, unsigned int *found);
EXTERN DynamicStrings_String StringConvert_LongIntegerToString (long int i, unsigned int width, char padding, unsigned int sign, unsigned int base, unsigned int lower);
EXTERN long int StringConvert_StringToLongInteger (DynamicStrings_String s, unsigned int base, unsigned int *found);
EXTERN DynamicStrings_String StringConvert_LongCardinalToString (long unsigned int c, unsigned int width, char padding, unsigned int base, unsigned int lower);
EXTERN long unsigned int StringConvert_StringToLongCardinal (DynamicStrings_String s, unsigned int base, unsigned int *found);
EXTERN DynamicStrings_String StringConvert_ShortCardinalToString (short unsigned int c, unsigned int width, char padding, unsigned int base, unsigned int lower);
EXTERN short unsigned int StringConvert_StringToShortCardinal (DynamicStrings_String s, unsigned int base, unsigned int *found);
EXTERN int StringConvert_stoi (DynamicStrings_String s);
EXTERN DynamicStrings_String StringConvert_itos (int i, unsigned int width, char padding, unsigned int sign);
EXTERN DynamicStrings_String StringConvert_ctos (unsigned int c, unsigned int width, char padding);
EXTERN unsigned int StringConvert_stoc (DynamicStrings_String s);
EXTERN int StringConvert_hstoi (DynamicStrings_String s);
EXTERN int StringConvert_ostoi (DynamicStrings_String s);
EXTERN int StringConvert_bstoi (DynamicStrings_String s);
EXTERN unsigned int StringConvert_hstoc (DynamicStrings_String s);
EXTERN unsigned int StringConvert_ostoc (DynamicStrings_String s);
EXTERN unsigned int StringConvert_bstoc (DynamicStrings_String s);
EXTERN long double StringConvert_StringToLongreal (DynamicStrings_String s, unsigned int *found);
EXTERN DynamicStrings_String StringConvert_LongrealToString (long double x, unsigned int TotalWidth, unsigned int FractionWidth);
EXTERN double StringConvert_stor (DynamicStrings_String s);
EXTERN long double StringConvert_stolr (DynamicStrings_String s);
EXTERN DynamicStrings_String StringConvert_ToSigFig (DynamicStrings_String s, unsigned int n);
EXTERN DynamicStrings_String StringConvert_ToDecimalPlaces (DynamicStrings_String s, unsigned int n);

#   undef EXTERN
#endif
