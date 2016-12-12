/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/FIO.def.  */


#if !defined (_FIO_H)
#   define _FIO_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_FIO_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

typedef unsigned int FIO_File;

EXTERN FIO_File FIO_StdIn;
EXTERN FIO_File FIO_StdOut;
EXTERN FIO_File FIO_StdErr;
EXTERN unsigned int FIO_IsNoError (FIO_File f);
EXTERN unsigned int FIO_IsActive (FIO_File f);
EXTERN unsigned int FIO_Exists (char *fname_, unsigned int _fname_high);
EXTERN FIO_File FIO_OpenToRead (char *fname_, unsigned int _fname_high);
EXTERN FIO_File FIO_OpenToWrite (char *fname_, unsigned int _fname_high);
EXTERN FIO_File FIO_OpenForRandom (char *fname_, unsigned int _fname_high, unsigned int towrite, unsigned int newfile);
EXTERN void FIO_Close (FIO_File f);
EXTERN unsigned int FIO_exists (void * fname, unsigned int flength);
EXTERN FIO_File FIO_openToRead (void * fname, unsigned int flength);
EXTERN FIO_File FIO_openToWrite (void * fname, unsigned int flength);
EXTERN FIO_File FIO_openForRandom (void * fname, unsigned int flength, unsigned int towrite, unsigned int newfile);
EXTERN void FIO_FlushBuffer (FIO_File f);
EXTERN unsigned int FIO_ReadNBytes (FIO_File f, unsigned int nBytes, void * a);
EXTERN void FIO_ReadAny (FIO_File f, unsigned char *a, unsigned int _a_high);
EXTERN unsigned int FIO_WriteNBytes (FIO_File f, unsigned int nBytes, void * a);
EXTERN void FIO_WriteAny (FIO_File f, unsigned char *a, unsigned int _a_high);
EXTERN void FIO_WriteChar (FIO_File f, char ch);
EXTERN unsigned int FIO_EOF (FIO_File f);
EXTERN unsigned int FIO_EOLN (FIO_File f);
EXTERN unsigned int FIO_WasEOLN (FIO_File f);
EXTERN char FIO_ReadChar (FIO_File f);
EXTERN void FIO_UnReadChar (FIO_File f, char ch);
EXTERN void FIO_WriteLine (FIO_File f);
EXTERN void FIO_WriteString (FIO_File f, char *a_, unsigned int _a_high);
EXTERN void FIO_ReadString (FIO_File f, char *a, unsigned int _a_high);
EXTERN void FIO_WriteCardinal (FIO_File f, unsigned int c);
EXTERN unsigned int FIO_ReadCardinal (FIO_File f);
EXTERN int FIO_GetUnixFileDescriptor (FIO_File f);
EXTERN void FIO_SetPositionFromBeginning (FIO_File f, long int pos);
EXTERN void FIO_SetPositionFromEnd (FIO_File f, long int pos);
EXTERN long int FIO_FindPosition (FIO_File f);
EXTERN void FIO_GetFileName (FIO_File f, char *a, unsigned int _a_high);
EXTERN void * FIO_getFileName (FIO_File f);
EXTERN unsigned int FIO_getFileNameLength (FIO_File f);
EXTERN void FIO_FlushOutErr (void);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
