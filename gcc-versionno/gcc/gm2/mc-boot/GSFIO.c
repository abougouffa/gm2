/* Copyright (C) 2019 Free Software Foundation, Inc.

This file is part of GNU Modula-2.

GNU Modula-2 is software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING.  If not,
see <https://www.gnu.org/licenses/>.  */

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


/*
   Exists - returns TRUE if a file named, fname exists for reading.
*/

unsigned int SFIO_Exists (DynamicStrings_String fname);

/*
   OpenToRead - attempts to open a file, fname, for reading and
                it returns this file.
                The success of this operation can be checked by
                calling IsNoError.
*/

FIO_File SFIO_OpenToRead (DynamicStrings_String fname);

/*
   OpenToWrite - attempts to open a file, fname, for write and
                 it returns this file.
                 The success of this operation can be checked by
                 calling IsNoError.
*/

FIO_File SFIO_OpenToWrite (DynamicStrings_String fname);

/*
   OpenForRandom - attempts to open a file, fname, for random access
                   read or write and it returns this file.
                   The success of this operation can be checked by
                   calling IsNoError.
                   towrite, determines whether the file should be
                   opened for writing or reading.
                   if towrite is TRUE or whether the previous file should
                   be left alone, allowing this descriptor to seek
                   and modify an existing file.
*/

FIO_File SFIO_OpenForRandom (DynamicStrings_String fname, unsigned int towrite, unsigned int newfile);

/*
   WriteS - writes a string, s, to, file. It returns the String, s.
*/

DynamicStrings_String SFIO_WriteS (FIO_File file, DynamicStrings_String s);

/*
   ReadS - reads and returns a string from, file.
           It stops reading the string at the end of line or end of file.
           It consumes the newline at the end of line but does not place
           this into the returned string.
*/

DynamicStrings_String SFIO_ReadS (FIO_File file);


/*
   Exists - returns TRUE if a file named, fname exists for reading.
*/

unsigned int SFIO_Exists (DynamicStrings_String fname)
{
  return FIO_exists (DynamicStrings_string (fname), DynamicStrings_Length (fname));
}


/*
   OpenToRead - attempts to open a file, fname, for reading and
                it returns this file.
                The success of this operation can be checked by
                calling IsNoError.
*/

FIO_File SFIO_OpenToRead (DynamicStrings_String fname)
{
  return FIO_openToRead (DynamicStrings_string (fname), DynamicStrings_Length (fname));
}


/*
   OpenToWrite - attempts to open a file, fname, for write and
                 it returns this file.
                 The success of this operation can be checked by
                 calling IsNoError.
*/

FIO_File SFIO_OpenToWrite (DynamicStrings_String fname)
{
  return FIO_openToWrite (DynamicStrings_string (fname), DynamicStrings_Length (fname));
}


/*
   OpenForRandom - attempts to open a file, fname, for random access
                   read or write and it returns this file.
                   The success of this operation can be checked by
                   calling IsNoError.
                   towrite, determines whether the file should be
                   opened for writing or reading.
                   if towrite is TRUE or whether the previous file should
                   be left alone, allowing this descriptor to seek
                   and modify an existing file.
*/

FIO_File SFIO_OpenForRandom (DynamicStrings_String fname, unsigned int towrite, unsigned int newfile)
{
  return FIO_openForRandom (DynamicStrings_string (fname), DynamicStrings_Length (fname), towrite, newfile);
}


/*
   WriteS - writes a string, s, to, file. It returns the String, s.
*/

DynamicStrings_String SFIO_WriteS (FIO_File file, DynamicStrings_String s)
{
  unsigned int nBytes;

  if (s != NULL)
    nBytes = FIO_WriteNBytes (file, DynamicStrings_Length (s), DynamicStrings_string (s));
  return s;
}


/*
   ReadS - reads and returns a string from, file.
           It stops reading the string at the end of line or end of file.
           It consumes the newline at the end of line but does not place
           this into the returned string.
*/

DynamicStrings_String SFIO_ReadS (FIO_File file)
{
  DynamicStrings_String s;
  unsigned int c;

  s = DynamicStrings_InitString ((char *) "", 0);
  while (((! (FIO_EOLN (file))) && (! (FIO_EOF (file)))) && (FIO_IsNoError (file)))
    s = DynamicStrings_ConCatChar (s, FIO_ReadChar (file));
  if (FIO_EOLN (file))
    /* consume nl  */
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
