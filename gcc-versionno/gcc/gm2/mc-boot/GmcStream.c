/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcStream.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   if !defined (TRUE)
#      define TRUE (1==1)
#   endif

#   if !defined (FALSE)
#      define FALSE (1==0)
#   endif

#include <stddef.h>
#   include "GStorage.h"
#define _mcStream_H
#define _mcStream_C

#   include "GFIO.h"
#   include "Glibc.h"
#   include "GIndexing.h"
#   include "GDynamicStrings.h"
#   include "GFormatStrings.h"
#   include "GSYSTEM.h"
#   include "GStorage.h"
#   include "Galists.h"
#   include "GSFIO.h"
#   include "GM2RTS.h"

#   define maxBuffer 4096
typedef FIO_File *ptrToFile;

static alists_alist listOfFiles;
static Indexing_Index frag;
static FIO_File destFile;
static unsigned int seenDest;

/*
   openFrag - create and open fragment, id, and return the file.
              The file should not be closed by the user.
*/

FIO_File mcStream_openFrag (unsigned int id);

/*
   setDest - informs the stream module and all fragments must be copied
             info, f.
*/

void mcStream_setDest (FIO_File f);

/*
   combine - closes all fragments and then writes them in
             order to the destination file.  The dest file
             is returned.
*/

FIO_File mcStream_combine (void);

/*
   removeLater -
*/

static DynamicStrings_String removeLater (DynamicStrings_String filename);

/*
   removeNow - removes a single file, s.
*/

static void removeNow (DynamicStrings_String s);

/*
   removeFiles -
*/

static void removeFiles (void);

/*
   createTemporaryFile -
*/

static FIO_File createTemporaryFile (unsigned int id);

/*
   copy - copies contents of f to the destination file.
*/

static void copy (ptrToFile p);


/*
   removeLater -
*/

static DynamicStrings_String removeLater (DynamicStrings_String filename)
{
  alists_includeItemIntoList (listOfFiles, (void *) filename);
  return filename;
}


/*
   removeNow - removes a single file, s.
*/

static void removeNow (DynamicStrings_String s)
{
  if ((libc_unlink (DynamicStrings_string (s))) != 0)
    {}  /* empty.  */
}


/*
   removeFiles -
*/

static void removeFiles (void)
{
  alists_foreachItemInListDo (listOfFiles, (alists_performOperation) {(alists_performOperation_t) removeNow});
}


/*
   createTemporaryFile -
*/

static FIO_File createTemporaryFile (unsigned int id)
{
  DynamicStrings_String s;
  FIO_File f;
  int p;

  s = DynamicStrings_InitString ((char *) "/tmp/frag-%d-%d.frag", 20);
  p = libc_getpid ();
  s = removeLater (FormatStrings_Sprintf2 (s, (unsigned char *) &p, (sizeof (p)-1), (unsigned char *) &id, (sizeof (id)-1)));
  f = SFIO_OpenToWrite (s);
  return f;
}


/*
   copy - copies contents of f to the destination file.
*/

static void copy (ptrToFile p)
{
  typedef struct _T1_a _T1;

  struct _T1_a { char array[maxBuffer+1]; };
  _T1 buffer;
  unsigned int b;
  DynamicStrings_String s;
  FIO_File f;

  if (p != NULL)
    {
      f = (*p);
      s = DynamicStrings_InitStringCharStar (FIO_getFileName (f));
      FIO_Close (f);
      f = SFIO_OpenToRead (s);
      while (! (FIO_EOF (f)))
        {
          b = FIO_ReadNBytes (f, maxBuffer, &buffer);
          b = FIO_WriteNBytes (destFile, b, &buffer);
        }
      FIO_Close (f);
    }
}


/*
   openFrag - create and open fragment, id, and return the file.
              The file should not be closed by the user.
*/

FIO_File mcStream_openFrag (unsigned int id)
{
  FIO_File f;
  ptrToFile p;

  f = createTemporaryFile (id);
  Storage_ALLOCATE ((void **) &p, sizeof (FIO_File));
  (*p) = f;
  Indexing_PutIndice (frag, id, (void *) p);
  return f;
}


/*
   setDest - informs the stream module and all fragments must be copied
             info, f.
*/

void mcStream_setDest (FIO_File f)
{
  seenDest = TRUE;
  destFile = f;
}


/*
   combine - closes all fragments and then writes them in
             order to the destination file.  The dest file
             is returned.
*/

FIO_File mcStream_combine (void)
{
  if (! seenDest)
    M2RTS_HALT (0);
  Indexing_ForeachIndiceInIndexDo (frag, (Indexing_IndexProcedure) {(Indexing_IndexProcedure_t) copy});
  removeFiles ();
  return destFile;
}

void _M2_mcStream_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  listOfFiles = alists_initList ();
  seenDest = FALSE;
  frag = Indexing_InitIndex (1);
}

void _M2_mcStream_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}