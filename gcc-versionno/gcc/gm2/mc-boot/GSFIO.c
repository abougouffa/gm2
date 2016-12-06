/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/SFIO.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#include <stddef.h>
#define _SFIO_H
#define _SFIO_C

#   include "GASCII.h"
#   include "GDynamicStrings.h"
#   include "GFIO.h"

unsigned int SFIO_Exists (DynamicStrings_String fname);
FIO_File SFIO_OpenToRead (DynamicStrings_String fname);
FIO_File SFIO_OpenToWrite (DynamicStrings_String fname);
FIO_File SFIO_OpenForRandom (DynamicStrings_String fname, unsigned int towrite, unsigned int newfile);
DynamicStrings_String SFIO_WriteS (FIO_File file, DynamicStrings_String s);
DynamicStrings_String SFIO_ReadS (FIO_File file);

unsigned int SFIO_Exists (DynamicStrings_String fname)
{
  return FIO_exists (DynamicStrings_string (fname), DynamicStrings_Length (fname));
}

FIO_File SFIO_OpenToRead (DynamicStrings_String fname)
{
  return FIO_openToRead (DynamicStrings_string (fname), DynamicStrings_Length (fname));
}

FIO_File SFIO_OpenToWrite (DynamicStrings_String fname)
{
  return FIO_openToWrite (DynamicStrings_string (fname), DynamicStrings_Length (fname));
}

FIO_File SFIO_OpenForRandom (DynamicStrings_String fname, unsigned int towrite, unsigned int newfile)
{
  return FIO_openForRandom (DynamicStrings_string (fname), DynamicStrings_Length (fname), towrite, newfile);
}

DynamicStrings_String SFIO_WriteS (FIO_File file, DynamicStrings_String s)
{
  unsigned int nBytes;

  if (s != NULL)
    nBytes = FIO_WriteNBytes (file, DynamicStrings_Length (s), DynamicStrings_string (s));
  return s;
}

DynamicStrings_String SFIO_ReadS (FIO_File file)
{
  DynamicStrings_String s;
  unsigned int c;

  s = DynamicStrings_InitString ((char *) "", 0);
  while (((! (FIO_EOLN (file))) && (! (FIO_EOF (file)))) && (FIO_IsNoError (file)))
    s = DynamicStrings_ConCatChar (s, FIO_ReadChar (file));
  if (FIO_EOLN (file))
    if ((FIO_ReadChar (file)) == ASCII_nul)
      {}  /* empty.  */
  return s;
}

void _M2_SFIO_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_SFIO_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
