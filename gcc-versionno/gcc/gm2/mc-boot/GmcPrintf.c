/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcPrintf.mod.  */

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
#include <string.h>
#include <limits.h>
#define _mcPrintf_H
#define _mcPrintf_C

#   include "GSFIO.h"
#   include "GFIO.h"
#   include "GDynamicStrings.h"
#   include "GStrLib.h"
#   include "GFormatStrings.h"
#   include "GnameKey.h"

void mcPrintf_printf0 (char *a_, unsigned int _a_high);
void mcPrintf_printf1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);
void mcPrintf_printf2 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);
void mcPrintf_printf3 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);
void mcPrintf_printf4 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high, unsigned char *w4_, unsigned int _w4_high);
void mcPrintf_fprintf0 (FIO_File file, char *a_, unsigned int _a_high);
void mcPrintf_fprintf1 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);
void mcPrintf_fprintf2 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);
void mcPrintf_fprintf3 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);
void mcPrintf_fprintf4 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high, unsigned char *w4_, unsigned int _w4_high);
static unsigned int isDigit (char ch);
static void cast (unsigned char *a, unsigned int _a_high, unsigned char *b_, unsigned int _b_high);
static unsigned int TranslateNameToCharStar (char *a, unsigned int _a_high, unsigned int n);

static unsigned int isDigit (char ch)
{
  return (ch >= '0') && (ch <= '9');
}

static void cast (unsigned char *a, unsigned int _a_high, unsigned char *b_, unsigned int _b_high)
{
  unsigned int i;
  unsigned char b[_b_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (b, b_, _b_high+1);

  if ((_a_high) == (_b_high))
    for (i=0; i<=_a_high; i++)
      a[i] = b[i];
  else
    M2RTS_HALT (0);
}

static unsigned int TranslateNameToCharStar (char *a, unsigned int _a_high, unsigned int n)
{
  unsigned int argno;
  unsigned int i;
  unsigned int h;

  argno = 1;
  i = 0;
  h = StrLib_StrLen ((char *) a, _a_high);
  while (i < h)
    {
      if ((a[i] == '%') && ((i+1) < h))
        {
          if ((a[i+1] == 'a') && (argno == n))
            {
              a[i+1] = 's';
              return TRUE;
            }
          argno += 1;
          if (argno > n)
            return FALSE;
        }
      i += 1;
    }
  return FALSE;
}

void mcPrintf_printf0 (char *a_, unsigned int _a_high)
{
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  mcPrintf_fprintf0 (FIO_StdOut, (char *) a, _a_high);
}

void mcPrintf_printf1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high)
{
  char a[_a_high+1];
  unsigned char w[_w_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w, w_, _w_high+1);

  mcPrintf_fprintf1 (FIO_StdOut, (char *) a, _a_high, (unsigned char *) w, _w_high);
}

void mcPrintf_printf2 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high)
{
  char a[_a_high+1];
  unsigned char w1[_w1_high+1];
  unsigned char w2[_w2_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w1, w1_, _w1_high+1);
  memcpy (w2, w2_, _w2_high+1);

  mcPrintf_fprintf2 (FIO_StdOut, (char *) a, _a_high, (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high);
}

void mcPrintf_printf3 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high)
{
  char a[_a_high+1];
  unsigned char w1[_w1_high+1];
  unsigned char w2[_w2_high+1];
  unsigned char w3[_w3_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w1, w1_, _w1_high+1);
  memcpy (w2, w2_, _w2_high+1);
  memcpy (w3, w3_, _w3_high+1);

  mcPrintf_fprintf3 (FIO_StdOut, (char *) a, _a_high, (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high, (unsigned char *) w3, _w3_high);
}

void mcPrintf_printf4 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high, unsigned char *w4_, unsigned int _w4_high)
{
  char a[_a_high+1];
  unsigned char w1[_w1_high+1];
  unsigned char w2[_w2_high+1];
  unsigned char w3[_w3_high+1];
  unsigned char w4[_w4_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w1, w1_, _w1_high+1);
  memcpy (w2, w2_, _w2_high+1);
  memcpy (w3, w3_, _w3_high+1);
  memcpy (w4, w4_, _w4_high+1);

  mcPrintf_fprintf4 (FIO_StdOut, (char *) a, _a_high, (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high, (unsigned char *) w3, _w3_high, (unsigned char *) w4, _w4_high);
}

void mcPrintf_fprintf0 (FIO_File file, char *a_, unsigned int _a_high)
{
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  if ((DynamicStrings_KillString (SFIO_WriteS (file, FormatStrings_Sprintf0 (DynamicStrings_InitString ((char *) a, _a_high))))) == NULL)
    ;  /* empty.  */
}

void mcPrintf_fprintf1 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high)
{
  DynamicStrings_String s;
  DynamicStrings_String t;
  nameKey_Name n;
  char a[_a_high+1];
  unsigned char w[_w_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w, w_, _w_high+1);

  if (TranslateNameToCharStar ((char *) a, _a_high, 1))
    {
      cast ((unsigned char *) &n, sizeof (n), (unsigned char *) w, _w_high);
      s = DynamicStrings_Mark (DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
      t = DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high));
      s = FormatStrings_Sprintf1 (t, (unsigned char *) &s, sizeof (s));
    }
  else
    {
      t = DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high));
      s = FormatStrings_Sprintf1 (t, (unsigned char *) w, _w_high);
    }
  if ((DynamicStrings_KillString (SFIO_WriteS (file, s))) == NULL)
    ;  /* empty.  */
}

void mcPrintf_fprintf2 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high)
{
  nameKey_Name n;
  DynamicStrings_String s;
  DynamicStrings_String s1;
  DynamicStrings_String s2;
  unsigned int b;
  char a[_a_high+1];
  unsigned char w1[_w1_high+1];
  unsigned char w2[_w2_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w1, w1_, _w1_high+1);
  memcpy (w2, w2_, _w2_high+1);

  b = (unsigned int) 0;
  if (TranslateNameToCharStar ((char *) a, _a_high, 1))
    {
      cast ((unsigned char *) &n, sizeof (n), (unsigned char *) w1, _w1_high);
      s1 = DynamicStrings_Mark (DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
      b |= (1 << (1 ));
    }
  if (TranslateNameToCharStar ((char *) a, _a_high, 2))
    {
      cast ((unsigned char *) &n, sizeof (n), (unsigned char *) w2, _w2_high);
      s2 = DynamicStrings_Mark (DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
      b |= (1 << (2 ));
    }
  switch (b)
    {
      case (unsigned int) 0:
        s = FormatStrings_Sprintf2 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high);
        break;

      case (unsigned int) ((1 << (1))):
        s = FormatStrings_Sprintf2 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, sizeof (s1), (unsigned char *) w2, _w2_high);
        break;

      case (unsigned int) ((1 << (2))):
        s = FormatStrings_Sprintf2 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) &s2, sizeof (s2));
        break;

      case (unsigned int) ((1 << (1)) | (1 << (2))):
        s = FormatStrings_Sprintf2 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, sizeof (s1), (unsigned char *) &s2, sizeof (s2));
        break;


      default:
        M2RTS_HALT (0);
        break;
    }
  if ((DynamicStrings_KillString (SFIO_WriteS (file, s))) == NULL)
    ;  /* empty.  */
}

void mcPrintf_fprintf3 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high)
{
  nameKey_Name n;
  DynamicStrings_String s;
  DynamicStrings_String s1;
  DynamicStrings_String s2;
  DynamicStrings_String s3;
  unsigned int b;
  char a[_a_high+1];
  unsigned char w1[_w1_high+1];
  unsigned char w2[_w2_high+1];
  unsigned char w3[_w3_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w1, w1_, _w1_high+1);
  memcpy (w2, w2_, _w2_high+1);
  memcpy (w3, w3_, _w3_high+1);

  b = (unsigned int) 0;
  if (TranslateNameToCharStar ((char *) a, _a_high, 1))
    {
      cast ((unsigned char *) &n, sizeof (n), (unsigned char *) w1, _w1_high);
      s1 = DynamicStrings_Mark (DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
      b |= (1 << (1 ));
    }
  if (TranslateNameToCharStar ((char *) a, _a_high, 2))
    {
      cast ((unsigned char *) &n, sizeof (n), (unsigned char *) w2, _w2_high);
      s2 = DynamicStrings_Mark (DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
      b |= (1 << (2 ));
    }
  if (TranslateNameToCharStar ((char *) a, _a_high, 3))
    {
      cast ((unsigned char *) &n, sizeof (n), (unsigned char *) w3, _w3_high);
      s3 = DynamicStrings_Mark (DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
      b |= (1 << (3 ));
    }
  switch (b)
    {
      case (unsigned int) 0:
        s = FormatStrings_Sprintf3 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high, (unsigned char *) w3, _w3_high);
        break;

      case (unsigned int) ((1 << (1))):
        s = FormatStrings_Sprintf3 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, sizeof (s1), (unsigned char *) w2, _w2_high, (unsigned char *) w3, _w3_high);
        break;

      case (unsigned int) ((1 << (2))):
        s = FormatStrings_Sprintf3 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) &s2, sizeof (s2), (unsigned char *) w3, _w3_high);
        break;

      case (unsigned int) ((1 << (1)) | (1 << (2))):
        s = FormatStrings_Sprintf3 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, sizeof (s1), (unsigned char *) &s2, sizeof (s2), (unsigned char *) w3, _w3_high);
        break;

      case (unsigned int) ((1 << (3))):
        s = FormatStrings_Sprintf3 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high, (unsigned char *) &s3, sizeof (s3));
        break;

      case (unsigned int) ((1 << (1)) | (1 << (3))):
        s = FormatStrings_Sprintf3 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, sizeof (s1), (unsigned char *) w2, _w2_high, (unsigned char *) &s3, sizeof (s3));
        break;

      case (unsigned int) ((1 << (2)) | (1 << (3))):
        s = FormatStrings_Sprintf3 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) &s2, sizeof (s2), (unsigned char *) &s3, sizeof (s3));
        break;

      case (unsigned int) ((1 << (1)) | (1 << (2)) | (1 << (3))):
        s = FormatStrings_Sprintf3 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, sizeof (s1), (unsigned char *) &s2, sizeof (s2), (unsigned char *) &s3, sizeof (s3));
        break;


      default:
        M2RTS_HALT (0);
        break;
    }
  if ((DynamicStrings_KillString (SFIO_WriteS (file, s))) == NULL)
    ;  /* empty.  */
}

void mcPrintf_fprintf4 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high, unsigned char *w4_, unsigned int _w4_high)
{
  nameKey_Name n;
  DynamicStrings_String s;
  DynamicStrings_String s1;
  DynamicStrings_String s2;
  DynamicStrings_String s3;
  DynamicStrings_String s4;
  unsigned int b;
  char a[_a_high+1];
  unsigned char w1[_w1_high+1];
  unsigned char w2[_w2_high+1];
  unsigned char w3[_w3_high+1];
  unsigned char w4[_w4_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w1, w1_, _w1_high+1);
  memcpy (w2, w2_, _w2_high+1);
  memcpy (w3, w3_, _w3_high+1);
  memcpy (w4, w4_, _w4_high+1);

  b = (unsigned int) 0;
  if (TranslateNameToCharStar ((char *) a, _a_high, 1))
    {
      cast ((unsigned char *) &n, sizeof (n), (unsigned char *) w1, _w1_high);
      s1 = DynamicStrings_Mark (DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
      b |= (1 << (1 ));
    }
  if (TranslateNameToCharStar ((char *) a, _a_high, 2))
    {
      cast ((unsigned char *) &n, sizeof (n), (unsigned char *) w2, _w2_high);
      s2 = DynamicStrings_Mark (DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
      b |= (1 << (2 ));
    }
  if (TranslateNameToCharStar ((char *) a, _a_high, 3))
    {
      cast ((unsigned char *) &n, sizeof (n), (unsigned char *) w3, _w3_high);
      s3 = DynamicStrings_Mark (DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
      b |= (1 << (3 ));
    }
  if (TranslateNameToCharStar ((char *) a, _a_high, 4))
    {
      cast ((unsigned char *) &n, sizeof (n), (unsigned char *) w4, _w4_high);
      s4 = DynamicStrings_Mark (DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
      b |= (1 << (4 ));
    }
  switch (b)
    {
      case (unsigned int) 0:
        s = FormatStrings_Sprintf4 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high, (unsigned char *) w3, _w3_high, (unsigned char *) w4, _w4_high);
        break;

      case (unsigned int) ((1 << (1))):
        s = FormatStrings_Sprintf4 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, sizeof (s1), (unsigned char *) w2, _w2_high, (unsigned char *) w3, _w3_high, (unsigned char *) w4, _w4_high);
        break;

      case (unsigned int) ((1 << (2))):
        s = FormatStrings_Sprintf4 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) &s2, sizeof (s2), (unsigned char *) w3, _w3_high, (unsigned char *) w4, _w4_high);
        break;

      case (unsigned int) ((1 << (1)) | (1 << (2))):
        s = FormatStrings_Sprintf4 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, sizeof (s1), (unsigned char *) &s2, sizeof (s2), (unsigned char *) w3, _w3_high, (unsigned char *) w4, _w4_high);
        break;

      case (unsigned int) ((1 << (3))):
        s = FormatStrings_Sprintf4 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high, (unsigned char *) &s3, sizeof (s3), (unsigned char *) w4, _w4_high);
        break;

      case (unsigned int) ((1 << (1)) | (1 << (3))):
        s = FormatStrings_Sprintf4 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, sizeof (s1), (unsigned char *) w2, _w2_high, (unsigned char *) &s3, sizeof (s3), (unsigned char *) w4, _w4_high);
        break;

      case (unsigned int) ((1 << (2)) | (1 << (3))):
        s = FormatStrings_Sprintf4 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) &s2, sizeof (s2), (unsigned char *) &s3, sizeof (s3), (unsigned char *) w4, _w4_high);
        break;

      case (unsigned int) ((1 << (1)) | (1 << (2)) | (1 << (3))):
        s = FormatStrings_Sprintf4 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, sizeof (s1), (unsigned char *) &s2, sizeof (s2), (unsigned char *) &s3, sizeof (s3), (unsigned char *) w4, _w4_high);
        break;

      case (unsigned int) ((1 << (4))):
        s = FormatStrings_Sprintf4 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high, (unsigned char *) w3, _w3_high, (unsigned char *) &s4, sizeof (s4));
        break;

      case (unsigned int) ((1 << (1)) | (1 << (4))):
        s = FormatStrings_Sprintf4 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, sizeof (s1), (unsigned char *) w2, _w2_high, (unsigned char *) w3, _w3_high, (unsigned char *) &s4, sizeof (s4));
        break;

      case (unsigned int) ((1 << (2)) | (1 << (4))):
        s = FormatStrings_Sprintf4 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) &s2, sizeof (s2), (unsigned char *) w3, _w3_high, (unsigned char *) &s4, sizeof (s4));
        break;

      case (unsigned int) ((1 << (1)) | (1 << (2)) | (1 << (4))):
        s = FormatStrings_Sprintf4 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, sizeof (s1), (unsigned char *) &s2, sizeof (s2), (unsigned char *) w3, _w3_high, (unsigned char *) &s4, sizeof (s4));
        break;

      case (unsigned int) ((1 << (3)) | (1 << (4))):
        s = FormatStrings_Sprintf4 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high, (unsigned char *) &s3, sizeof (s3), (unsigned char *) &s4, sizeof (s4));
        break;

      case (unsigned int) ((1 << (1)) | (1 << (3)) | (1 << (4))):
        s = FormatStrings_Sprintf4 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, sizeof (s1), (unsigned char *) w2, _w2_high, (unsigned char *) &s3, sizeof (s3), (unsigned char *) &s4, sizeof (s4));
        break;

      case (unsigned int) ((1 << (2)) | (1 << (3)) | (1 << (4))):
        s = FormatStrings_Sprintf4 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) &s2, sizeof (s2), (unsigned char *) &s3, sizeof (s3), (unsigned char *) &s4, sizeof (s4));
        break;

      case (unsigned int) ((1 << (1)) | (1 << (2)) | (1 << (3)) | (1 << (4))):
        s = FormatStrings_Sprintf4 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, sizeof (s1), (unsigned char *) &s2, sizeof (s2), (unsigned char *) &s3, sizeof (s3), (unsigned char *) &s4, sizeof (s4));
        break;


      default:
        M2RTS_HALT (0);
        break;
    }
  if ((DynamicStrings_KillString (SFIO_WriteS (file, s))) == NULL)
    ;  /* empty.  */
}

void _M2_mcPrintf_init (int argc, char *argv[])
{
}

void _M2_mcPrintf_finish (int argc, char *argv[])
{
}
