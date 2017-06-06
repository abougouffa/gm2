/* automatically created by mc from ../../gcc-versionno/gcc/gm2/mc/mcQuiet.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#include <string.h>
#include <limits.h>
#define _mcQuiet_H
#define _mcQuiet_C

#   include "GmcOptions.h"
#   include "GmcPrintf.h"

void mcQuiet_qprintf0 (char *a_, unsigned int _a_high);
void mcQuiet_qprintf1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);
void mcQuiet_qprintf2 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);
void mcQuiet_qprintf3 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);
void mcQuiet_qprintf4 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high, unsigned char *w4_, unsigned int _w4_high);

void mcQuiet_qprintf0 (char *a_, unsigned int _a_high)
{
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  if (! (mcOptions_getQuiet ()))
    mcPrintf_printf0 ((char *) a, _a_high);
}

void mcQuiet_qprintf1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high)
{
  char a[_a_high+1];
  unsigned char w[_w_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w, w_, _w_high+1);

  if (! (mcOptions_getQuiet ()))
    mcPrintf_printf1 ((char *) a, _a_high, (unsigned char *) w, _w_high);
}

void mcQuiet_qprintf2 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high)
{
  char a[_a_high+1];
  unsigned char w1[_w1_high+1];
  unsigned char w2[_w2_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w1, w1_, _w1_high+1);
  memcpy (w2, w2_, _w2_high+1);

  if (! (mcOptions_getQuiet ()))
    mcPrintf_printf2 ((char *) a, _a_high, (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high);
}

void mcQuiet_qprintf3 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high)
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

  if (! (mcOptions_getQuiet ()))
    mcPrintf_printf3 ((char *) a, _a_high, (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high, (unsigned char *) w3, _w3_high);
}

void mcQuiet_qprintf4 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high, unsigned char *w4_, unsigned int _w4_high)
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

  if (! (mcOptions_getQuiet ()))
    mcPrintf_printf4 ((char *) a, _a_high, (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high, (unsigned char *) w3, _w3_high, (unsigned char *) w4, _w4_high);
}

void _M2_mcQuiet_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_mcQuiet_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
