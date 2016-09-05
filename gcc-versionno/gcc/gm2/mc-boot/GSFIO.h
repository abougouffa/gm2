/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/SFIO.def.  */


#if !defined (_SFIO_H)
#   define _SFIO_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GDynamicStrings.h"
#   include "GFIO.h"

#   if defined (_SFIO_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN unsigned int SFIO_Exists (DynamicStrings_String fname);
EXTERN FIO_File SFIO_OpenToRead (DynamicStrings_String fname);
EXTERN FIO_File SFIO_OpenToWrite (DynamicStrings_String fname);
EXTERN FIO_File SFIO_OpenForRandom (DynamicStrings_String fname, unsigned int towrite, unsigned int newfile);
EXTERN DynamicStrings_String SFIO_WriteS (FIO_File file, DynamicStrings_String s);
EXTERN DynamicStrings_String SFIO_ReadS (FIO_File file);

#   undef EXTERN
#endif
