/* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#define _MemUtils_H
#define _MemUtils_C

#   include "GSYSTEM.h"


/*
   MemCopy - copys a region of memory to the required destination.
*/

void MemUtils_MemCopy (void * from, unsigned int length, void * to);

/*
   MemZero - sets a region of memory: a..a+length to zero.
*/

void MemUtils_MemZero (void * a, unsigned int length);


/*
   MemCopy - copys a region of memory to the required destination.
*/

void MemUtils_MemCopy (void * from, unsigned int length, void * to)
{
  unsigned int * pwb;
  unsigned int * pwa;
  unsigned char * pbb;
  unsigned char * pba;

  while (length >= sizeof (unsigned int ))
    {
      pwa = from;
      pwb = to;
      (*pwb) = (*pwa);
      from += sizeof (unsigned int );
      to += sizeof (unsigned int );
      length -= sizeof (unsigned int );
    }
  while (length > 0)
    {
      pba = from;
      pbb = to;
      (*pbb) = (*pba);
      from += sizeof (unsigned char );
      to += sizeof (unsigned char );
      length -= sizeof (unsigned char );
    }
}


/*
   MemZero - sets a region of memory: a..a+length to zero.
*/

void MemUtils_MemZero (void * a, unsigned int length)
{
  unsigned int * pwa;
  unsigned char * pba;

  pwa = a;
  while (length >= sizeof (unsigned int ))
    {
      (*pwa) = (unsigned int ) (0);
      pwa += sizeof (unsigned int );
      length -= sizeof (unsigned int );
    }
  pba = (void *) (pwa);
  while (length >= sizeof (unsigned char ))
    {
      (*pba) = (unsigned char ) (0);
      pba += sizeof (unsigned char );
      length -= sizeof (unsigned char );
    }
}

void _M2_MemUtils_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_MemUtils_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
