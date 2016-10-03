/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcMetaError.mod.  */

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
#define _mcMetaError_H
#define _mcMetaError_C

#   include "GnameKey.h"
#   include "GStrLib.h"
#   include "GmcLexBuf.h"
#   include "GmcError.h"
#   include "GFIO.h"
#   include "GSFIO.h"
#   include "GStringConvert.h"
#   include "Gvarargs.h"
#   include "GDynamicStrings.h"
#   include "Gdecl.h"

typedef enum {newerror, newwarning, chained} errorType;

void mcMetaError_metaError1 (char *m_, unsigned int _m_high, unsigned char *s_, unsigned int _s_high);
void mcMetaError_metaError2 (char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
void mcMetaError_metaError3 (char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
void mcMetaError_metaError4 (char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
void mcMetaError_metaErrors1 (char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s_, unsigned int _s_high);
void mcMetaError_metaErrors2 (char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
void mcMetaError_metaErrors3 (char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
void mcMetaError_metaErrors4 (char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
void mcMetaError_metaErrorT1 (unsigned int tok, char *m_, unsigned int _m_high, unsigned char *s_, unsigned int _s_high);
void mcMetaError_metaErrorT2 (unsigned int tok, char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
void mcMetaError_metaErrorT3 (unsigned int tok, char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
void mcMetaError_metaErrorT4 (unsigned int tok, char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
void mcMetaError_metaErrorsT1 (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s_, unsigned int _s_high);
void mcMetaError_metaErrorsT2 (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
void mcMetaError_metaErrorsT3 (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
void mcMetaError_metaErrorsT4 (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
void mcMetaError_metaErrorString1 (DynamicStrings_String m, unsigned char *s_, unsigned int _s_high);
void mcMetaError_metaErrorString2 (DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
void mcMetaError_metaErrorString3 (DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
void mcMetaError_metaErrorString4 (DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
void mcMetaError_metaErrorStringT1 (unsigned int tok, DynamicStrings_String m, unsigned char *s_, unsigned int _s_high);
void mcMetaError_metaErrorStringT2 (unsigned int tok, DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
void mcMetaError_metaErrorStringT3 (unsigned int tok, DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
void mcMetaError_metaErrorStringT4 (unsigned int tok, DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
static void internalFormat (DynamicStrings_String s, int i, char *m_, unsigned int _m_high);
static DynamicStrings_String x (DynamicStrings_String a, DynamicStrings_String b);
static unsigned int isWhite (char ch);
static void then (mcError_error *e, errorType *t, DynamicStrings_String *r, DynamicStrings_String s, varargs_vararg sym, int *i, int l, DynamicStrings_String o, unsigned int positive);
static DynamicStrings_String doNumber (unsigned int bol, varargs_vararg sym, DynamicStrings_String o, unsigned int *quotes);
static DynamicStrings_String doCount (unsigned int bol, varargs_vararg sym, DynamicStrings_String o, unsigned int *quotes);
static DynamicStrings_String doAscii (unsigned int bol, varargs_vararg sym, DynamicStrings_String o);
static DynamicStrings_String doName (unsigned int bol, varargs_vararg sym, DynamicStrings_String o, unsigned int *quotes);
static DynamicStrings_String doQualified (unsigned int bol, varargs_vararg sym, DynamicStrings_String o);
static DynamicStrings_String doType (unsigned int bol, varargs_vararg *sym, DynamicStrings_String o);
static DynamicStrings_String doSkipType (unsigned int bol, varargs_vararg *sym, DynamicStrings_String o);
static DynamicStrings_String doKey (unsigned int bol, varargs_vararg sym, DynamicStrings_String o);
static mcError_error doError (mcError_error e, errorType t, unsigned int tok);
static mcError_error doDeclaredDef (mcError_error e, errorType t, unsigned int bol, varargs_vararg sym);
static mcError_error doDeclaredMod (mcError_error e, errorType t, unsigned int bol, varargs_vararg sym);
static mcError_error doUsed (mcError_error e, errorType t, unsigned int bol, varargs_vararg sym);
static DynamicStrings_String ConCatWord (DynamicStrings_String a, DynamicStrings_String b);
static DynamicStrings_String symDesc (decl_node n, DynamicStrings_String o);
static DynamicStrings_String doDesc (unsigned int bol, varargs_vararg sym, DynamicStrings_String o, unsigned int *quotes);
static DynamicStrings_String addQuoted (DynamicStrings_String r, DynamicStrings_String o, unsigned int quotes);
static void op (mcError_error *e, errorType *t, DynamicStrings_String *r, DynamicStrings_String s, varargs_vararg sym, int *i, int l, unsigned int bol, unsigned int positive);
static void percenttoken (mcError_error *e, errorType t, DynamicStrings_String *r, DynamicStrings_String s, varargs_vararg sym, int *i, int l, unsigned int positive);
static void percent (DynamicStrings_String *r, DynamicStrings_String s, varargs_vararg sym, int *i, int l);
static void lbra (mcError_error *e, errorType *t, DynamicStrings_String *r, DynamicStrings_String s, varargs_vararg sym, int *i, int l);
static void stop (void);
static void ebnf (mcError_error *e, errorType *t, DynamicStrings_String *r, DynamicStrings_String s, varargs_vararg sym, int *i, int l);
static DynamicStrings_String doFormat (mcError_error *e, errorType *t, DynamicStrings_String s, varargs_vararg sym);
static void wrapErrors (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, varargs_vararg sym);

static void internalFormat (DynamicStrings_String s, int i, char *m_, unsigned int _m_high)
{
  mcError_error e;
  char m[_m_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m, m_, _m_high+1);

  e = mcError_newError (mcLexBuf_getTokenNo ());
  s = SFIO_WriteS (FIO_StdOut, s);
  FIO_WriteLine (FIO_StdOut);
  s = DynamicStrings_KillString (s);
  if (i > 0)
    i -= 1;
  s = DynamicStrings_Mult (DynamicStrings_InitString ((char *) " ", 1), (unsigned int ) i);
  s = DynamicStrings_ConCatChar (s, '^');
  s = SFIO_WriteS (FIO_StdOut, s);
  FIO_WriteLine (FIO_StdOut);
  mcError_internalError ((char *) m, _m_high, (char *) "../../gcc-5.2.0/gcc/gm2/mc/mcMetaError.mod", 42, 98);
}

static DynamicStrings_String x (DynamicStrings_String a, DynamicStrings_String b)
{
  if (a != b)
    mcError_internalError ((char *) "different string returned", 25, (char *) "../../gcc-5.2.0/gcc/gm2/mc/mcMetaError.mod", 42, 110);
  return a;
}

static unsigned int isWhite (char ch)
{
  return ch == ' ';
}

static void then (mcError_error *e, errorType *t, DynamicStrings_String *r, DynamicStrings_String s, varargs_vararg sym, int *i, int l, DynamicStrings_String o, unsigned int positive)
{
  if ((DynamicStrings_char (s, (*i))) == ':')
    {
      (*i) += 1;
      ebnf (e, t, r, s, sym, i, l);
      if (((*i) < l) && ((DynamicStrings_char (s, (*i))) != '}'))
        internalFormat (s, (*i), (char *) "expecting to see }", 18);
    }
}

static DynamicStrings_String doNumber (unsigned int bol, varargs_vararg sym, DynamicStrings_String o, unsigned int *quotes)
{
  unsigned int c;

  if ((DynamicStrings_Length (o)) > 0)
    return o;
  else
    {
      (*quotes) = FALSE;
      varargs_next (sym, bol);
      varargs_arg (sym, (unsigned char *) &c, (sizeof (c)-1));
      return DynamicStrings_ConCat (o, StringConvert_ctos (c, 0, ' '));
    }
}

static DynamicStrings_String doCount (unsigned int bol, varargs_vararg sym, DynamicStrings_String o, unsigned int *quotes)
{
  unsigned int c;

  if ((DynamicStrings_Length (o)) > 0)
    return o;
  else
    {
      (*quotes) = FALSE;
      varargs_next (sym, bol);
      varargs_arg (sym, (unsigned char *) &c, (sizeof (c)-1));
      o = DynamicStrings_ConCat (o, StringConvert_ctos (c, 0, ' '));
      if (((c % 100) >= 11) && ((c % 100) <= 13))
        o = DynamicStrings_ConCat (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "th", 2)));

      else {
        switch (c % 10)
          {
            case 1:
              o = DynamicStrings_ConCat (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "st", 2)));
              break;

            case 2:
              o = DynamicStrings_ConCat (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "nd", 2)));
              break;

            case 3:
              o = DynamicStrings_ConCat (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "rd", 2)));
              break;


            default:
              o = DynamicStrings_ConCat (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "th", 2)));
              break;
          }
      }
      return o;
    }
}

static DynamicStrings_String doAscii (unsigned int bol, varargs_vararg sym, DynamicStrings_String o)
{
  decl_node n;

  varargs_next (sym, bol);
  varargs_arg (sym, (unsigned char *) &n, (sizeof (n)-1));
  if (((DynamicStrings_Length (o)) > 0) || (decl_isTemporary (n)))
    return o;
  else
    return DynamicStrings_ConCat (o, DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (n))));
}

static DynamicStrings_String doName (unsigned int bol, varargs_vararg sym, DynamicStrings_String o, unsigned int *quotes)
{
  decl_node n;

  varargs_next (sym, bol);
  varargs_arg (sym, (unsigned char *) &n, (sizeof (n)-1));
  if (((DynamicStrings_Length (o)) > 0) || (decl_isTemporary (n)))
    return o;
  else
    if (decl_isZtype (n))
      {
        (*quotes) = FALSE;
        return DynamicStrings_ConCat (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "the ZType", 9)));
      }
    else if (decl_isRtype (n))
      {
        (*quotes) = FALSE;
        return DynamicStrings_ConCat (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "the RType", 9)));
      }
    else if ((decl_getSymName (n)) != nameKey_NulName)
      return DynamicStrings_ConCat (o, DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (n))));
    else
      return o;
}

static DynamicStrings_String doQualified (unsigned int bol, varargs_vararg sym, DynamicStrings_String o)
{
  decl_node s;
  decl_node n;
  varargs_vararg mod;

  varargs_next (sym, bol);
  varargs_arg (sym, (unsigned char *) &n, (sizeof (n)-1));
  if (((DynamicStrings_Length (o)) > 0) || (decl_isTemporary (n)))
    return o;
  else
    {
      s = decl_getScope (n);
      mod = varargs_start1 ((unsigned char *) &s, (sizeof (s)-1));
      if ((decl_isDef (s)) && (decl_isExported (n)))
        {
          o = x (o, doAscii (0, mod, o));
          o = x (o, DynamicStrings_ConCatChar (o, '.'));
          o = x (o, DynamicStrings_ConCat (o, DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (n)))));
        }
      else
        o = x (o, doAscii (bol, sym, o));
      varargs_end (&mod);
      return o;
    }
}

static DynamicStrings_String doType (unsigned int bol, varargs_vararg *sym, DynamicStrings_String o)
{
  decl_node n;

  varargs_next ((*sym), bol);
  varargs_arg ((*sym), (unsigned char *) &n, (sizeof (n)-1));
  if (((DynamicStrings_Length (o)) > 0) || ((decl_getType (n)) == NULL))
    return o;
  else
    {
      n = decl_skipType (decl_getType (n));
      varargs_next ((*sym), bol);
      varargs_replace ((*sym), (unsigned char *) &n, (sizeof (n)-1));
      return x (o, doAscii (bol, (*sym), o));
    }
}

static DynamicStrings_String doSkipType (unsigned int bol, varargs_vararg *sym, DynamicStrings_String o)
{
  decl_node n;

  varargs_next ((*sym), bol);
  varargs_arg ((*sym), (unsigned char *) &n, (sizeof (n)-1));
  if ((DynamicStrings_Length (o)) > 0)
    return o;
  else
    {
      n = decl_skipType (decl_getType (n));
      varargs_next ((*sym), bol);
      varargs_replace ((*sym), (unsigned char *) &n, (sizeof (n)-1));
      if ((decl_getSymName (n)) == nameKey_NulName)
        return o;
      else
        return x (o, doAscii (bol, (*sym), o));
    }
}

static DynamicStrings_String doKey (unsigned int bol, varargs_vararg sym, DynamicStrings_String o)
{
  nameKey_Name n;

  if ((DynamicStrings_Length (o)) > 0)
    return o;
  else
    {
      varargs_next (sym, bol);
      varargs_arg (sym, (unsigned char *) &n, (sizeof (n)-1));
      return DynamicStrings_ConCat (o, DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
    }
}

static mcError_error doError (mcError_error e, errorType t, unsigned int tok)
{
  switch (t)
    {
      case chained:
        if (e == NULL)
          mcError_internalError ((char *) "should not be chaining an error onto an empty error note", 56, (char *) "../../gcc-5.2.0/gcc/gm2/mc/mcMetaError.mod", 42, 356);
        else
          e = mcError_chainError (tok, e);
        break;

      case newerror:
        if (e == NULL)
          e = mcError_newError (tok);
        break;

      case newwarning:
        if (e == NULL)
          e = mcError_newWarning (tok);
        break;


      default:
        mcError_internalError ((char *) "unexpected enumeration value", 28, (char *) "../../gcc-5.2.0/gcc/gm2/mc/mcMetaError.mod", 42, 370);
        break;
    }
  return e;
}

static mcError_error doDeclaredDef (mcError_error e, errorType t, unsigned int bol, varargs_vararg sym)
{
  decl_node n;

  if (bol <= (varargs_nargs (sym)))
    {
      varargs_next (sym, bol);
      varargs_arg (sym, (unsigned char *) &n, (sizeof (n)-1));
      e = doError (e, t, decl_getDeclaredDef (n));
    }
  return e;
}

static mcError_error doDeclaredMod (mcError_error e, errorType t, unsigned int bol, varargs_vararg sym)
{
  decl_node n;

  if (bol <= (varargs_nargs (sym)))
    {
      varargs_next (sym, bol);
      varargs_arg (sym, (unsigned char *) &n, (sizeof (n)-1));
      e = doError (e, t, decl_getDeclaredMod (n));
    }
  return e;
}

static mcError_error doUsed (mcError_error e, errorType t, unsigned int bol, varargs_vararg sym)
{
  decl_node n;

  if (bol <= (varargs_nargs (sym)))
    {
      varargs_next (sym, bol);
      varargs_arg (sym, (unsigned char *) &n, (sizeof (n)-1));
      e = doError (e, t, decl_getFirstUsed (n));
    }
  return e;
}

static DynamicStrings_String ConCatWord (DynamicStrings_String a, DynamicStrings_String b)
{
  if (((DynamicStrings_Length (a)) == 1) && ((DynamicStrings_char (a, 0)) == 'a'))
    a = x (a, DynamicStrings_ConCatChar (a, 'n'));
  else if ((((DynamicStrings_Length (a)) > 1) && ((DynamicStrings_char (a, -1)) == 'a')) && (isWhite (DynamicStrings_char (a, -2))))
    a = x (a, DynamicStrings_ConCatChar (a, 'n'));
  if (((DynamicStrings_Length (a)) > 0) && (! (isWhite (DynamicStrings_char (a, -1)))))
    a = x (a, DynamicStrings_ConCatChar (a, ' '));
  return x (a, DynamicStrings_ConCat (a, b));
}

static DynamicStrings_String symDesc (decl_node n, DynamicStrings_String o)
{
  if (decl_isLiteral (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "literal", 7)));
  else if (decl_isConstSet (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "constant set", 12)));
  else if (decl_isConst (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "constant", 8)));
  else if (decl_isArray (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "array", 5)));
  else if (decl_isVar (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "variable", 8)));
  else if (decl_isEnumeration (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "enumeration type", 16)));
  else if (decl_isEnumerationField (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "enumeration field", 17)));
  else if (decl_isUnbounded (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "unbounded parameter", 19)));
  else if (decl_isProcType (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "procedure type", 14)));
  else if (decl_isProcedure (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "procedure", 9)));
  else if (decl_isPointer (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "pointer", 7)));
  else if (decl_isParameter (n))
    if (decl_isVarParam (n))
      return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "var parameter", 13)));
    else
      return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "parameter", 9)));
  else if (decl_isType (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "type", 4)));
  else if (decl_isRecord (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "record", 6)));
  else if (decl_isRecordField (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "record field", 12)));
  else if (decl_isVarient (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "varient record", 14)));
  else if (decl_isModule (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "module", 6)));
  else if (decl_isDef (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "definition module", 17)));
  else if (decl_isImp (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "implementation module", 21)));
  else if (decl_isSet (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "set", 3)));
  else if (decl_isSubrange (n))
    return ConCatWord (o, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "subrange", 8)));
  else
    return o;
}

static DynamicStrings_String doDesc (unsigned int bol, varargs_vararg sym, DynamicStrings_String o, unsigned int *quotes)
{
  decl_node n;

  if ((DynamicStrings_Length (o)) == 0)
    {
      varargs_next (sym, bol);
      varargs_arg (sym, (unsigned char *) &n, (sizeof (n)-1));
      o = symDesc (n, o);
      if ((DynamicStrings_Length (o)) > 0)
        (*quotes) = FALSE;
    }
  return o;
}

static DynamicStrings_String addQuoted (DynamicStrings_String r, DynamicStrings_String o, unsigned int quotes)
{
  if ((DynamicStrings_Length (o)) > 0)
    {
      if (! (isWhite (DynamicStrings_char (r, -1))))
        r = x (r, DynamicStrings_ConCatChar (r, ' '));
      if (quotes)
        r = x (r, DynamicStrings_ConCatChar (r, '\''));
      r = x (r, DynamicStrings_ConCat (r, o));
      if (quotes)
        r = x (r, DynamicStrings_ConCatChar (r, '\''));
    }
  return r;
}

static void op (mcError_error *e, errorType *t, DynamicStrings_String *r, DynamicStrings_String s, varargs_vararg sym, int *i, int l, unsigned int bol, unsigned int positive)
{
  DynamicStrings_String o;
  varargs_vararg c;
  unsigned int quotes;

  c = varargs_copy (sym);
  o = DynamicStrings_InitString ((char *) "", 0);
  quotes = TRUE;
  while (((*i) < l) && ((DynamicStrings_char (s, (*i))) != '}'))
    {
      switch (DynamicStrings_char (s, (*i)))
        {
          case 'a':
            o = x (o, doName (bol, sym, o, &quotes));
            break;

          case 'q':
            o = x (o, doQualified (bol, sym, o));
            break;

          case 't':
            o = x (o, doType (bol, &sym, o));
            break;

          case 'd':
            o = x (o, doDesc (bol, sym, o, &quotes));
            break;

          case 'n':
            o = x (o, doNumber (bol, sym, o, &quotes));
            break;

          case 'N':
            o = x (o, doCount (bol, sym, o, &quotes));
            break;

          case 's':
            o = x (o, doSkipType (bol, &sym, o));
            break;

          case 'k':
            o = x (o, doKey (bol, sym, o));
            break;

          case 'D':
            (*e) = doDeclaredDef ((*e), (*t), bol, sym);
            break;

          case 'M':
            (*e) = doDeclaredMod ((*e), (*t), bol, sym);
            break;

          case 'U':
            (*e) = doUsed ((*e), (*t), bol, sym);
            break;

          case 'E':
            (*t) = newerror;
            break;

          case 'W':
            (*t) = newwarning;
            break;

          case ':':
            varargs_end (&sym);
            sym = varargs_copy (c);
            then (e, t, r, s, sym, i, l, o, positive);
            o = DynamicStrings_KillString (o);
            o = DynamicStrings_InitString ((char *) "", 0);
            if (((*i) < l) && ((DynamicStrings_char (s, (*i))) != '}'))
              internalFormat (s, (*i), (char *) "expecting to see }", 18);
            (*i) -= 1;
            break;


          default:
            internalFormat (s, (*i), (char *) "expecting one of [aqtdnNsDUEW:]", 31);
            break;
        }
      (*i) += 1;
    }
  (*r) = x ((*r), addQuoted ((*r), o, quotes));
  o = DynamicStrings_KillString (o);
}

static void percenttoken (mcError_error *e, errorType t, DynamicStrings_String *r, DynamicStrings_String s, varargs_vararg sym, int *i, int l, unsigned int positive)
{
  if ((DynamicStrings_char (s, (*i))) == '%')
    {
      (*i) += 1;
      switch (DynamicStrings_char (s, (*i)))
        {
          case '1':
            (*i) += 1;
            op (e, &t, r, s, sym, i, l, 0, positive);
            break;

          case '2':
            (*i) += 1;
            op (e, &t, r, s, sym, i, l, 1, positive);
            break;

          case '3':
            (*i) += 1;
            op (e, &t, r, s, sym, i, l, 2, positive);
            break;

          case '4':
            (*i) += 1;
            op (e, &t, r, s, sym, i, l, 3, positive);
            break;


          default:
            internalFormat (s, (*i), (char *) "expecting one of [123]", 22);
            break;
        }
      if (((*i) < l) && ((DynamicStrings_char (s, (*i))) != '}'))
        internalFormat (s, (*i), (char *) "expecting to see }", 18);
    }
}

static void percent (DynamicStrings_String *r, DynamicStrings_String s, varargs_vararg sym, int *i, int l)
{
  if ((DynamicStrings_char (s, (*i))) == '%')
    {
      (*i) += 1;
      if ((*i) < l)
        {
          (*r) = x ((*r), DynamicStrings_ConCatChar ((*r), DynamicStrings_char (s, (*i))));
          (*i) += 1;
        }
    }
}

static void lbra (mcError_error *e, errorType *t, DynamicStrings_String *r, DynamicStrings_String s, varargs_vararg sym, int *i, int l)
{
  unsigned int positive;

  if ((DynamicStrings_char (s, (*i))) == '{')
    {
      positive = TRUE;
      (*i) += 1;
      if ((DynamicStrings_char (s, (*i))) == '!')
        {
          positive = FALSE;
          (*i) += 1;
        }
      if ((DynamicStrings_char (s, (*i))) != '%')
        internalFormat (s, (*i), (char *) "expecting to see %", 18);
      percenttoken (e, (*t), r, s, sym, i, l, positive);
      if (((*i) < l) && ((DynamicStrings_char (s, (*i))) != '}'))
        internalFormat (s, (*i), (char *) "expecting to see }", 18);
    }
}

static void stop (void)
{
}

static void ebnf (mcError_error *e, errorType *t, DynamicStrings_String *r, DynamicStrings_String s, varargs_vararg sym, int *i, int l)
{
  while ((*i) < l)
    {
      switch (DynamicStrings_char (s, (*i)))
        {
          case '%':
            percent (r, s, sym, i, l);
            break;

          case '{':
            lbra (e, t, r, s, sym, i, l);
            if (((*i) < l) && ((DynamicStrings_char (s, (*i))) != '}'))
              internalFormat (s, (*i), (char *) "expecting to see }", 18);
            break;

          case '}':
            return;
            break;


          default:
            if ((((isWhite (DynamicStrings_char (s, (*i)))) && ((DynamicStrings_Length ((*r))) > 0)) && (! (isWhite (DynamicStrings_char ((*r), -1))))) || (! (isWhite (DynamicStrings_char (s, (*i))))))
              (*r) = x ((*r), DynamicStrings_ConCatChar ((*r), DynamicStrings_char (s, (*i))));
            break;
        }
      (*i) += 1;
    }
}

static DynamicStrings_String doFormat (mcError_error *e, errorType *t, DynamicStrings_String s, varargs_vararg sym)
{
  DynamicStrings_String r;
  int i;
  int l;

  r = DynamicStrings_InitString ((char *) "", 0);
  i = 0;
  l = DynamicStrings_Length (s);
  ebnf (e, t, &r, s, sym, &i, l);
  s = DynamicStrings_KillString (s);
  return r;
}

static void wrapErrors (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, varargs_vararg sym)
{
  mcError_error e;
  mcError_error f;
  DynamicStrings_String str;
  errorType t;
  char m1[_m1_high+1];
  char m2[_m2_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m1, m1_, _m1_high+1);
  memcpy (m2, m2_, _m2_high+1);

  e = NULL;
  t = newerror;
  str = doFormat (&e, &t, DynamicStrings_InitString ((char *) m1, _m1_high), sym);
  e = doError (e, t, tok);
  mcError_errorString (e, str);
  f = e;
  t = chained;
  str = doFormat (&f, &t, DynamicStrings_InitString ((char *) m2, _m2_high), sym);
  if (e == f)
    {
      t = chained;
      f = doError (e, t, tok);
    }
  mcError_errorString (f, str);
}

void mcMetaError_metaError1 (char *m_, unsigned int _m_high, unsigned char *s_, unsigned int _s_high)
{
  char m[_m_high+1];
  unsigned char s[_s_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m, m_, _m_high+1);
  memcpy (s, s_, _s_high+1);

  mcMetaError_metaErrorT1 (mcLexBuf_getTokenNo (), (char *) m, _m_high, (unsigned char *) s, _s_high);
}

void mcMetaError_metaError2 (char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high)
{
  char m[_m_high+1];
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m, m_, _m_high+1);
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);

  mcMetaError_metaErrorT2 (mcLexBuf_getTokenNo (), (char *) m, _m_high, (unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high);
}

void mcMetaError_metaError3 (char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high)
{
  char m[_m_high+1];
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];
  unsigned char s3[_s3_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m, m_, _m_high+1);
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);
  memcpy (s3, s3_, _s3_high+1);

  mcMetaError_metaErrorT3 (mcLexBuf_getTokenNo (), (char *) m, _m_high, (unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high, (unsigned char *) s3, _s3_high);
}

void mcMetaError_metaError4 (char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high)
{
  char m[_m_high+1];
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];
  unsigned char s3[_s3_high+1];
  unsigned char s4[_s4_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m, m_, _m_high+1);
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);
  memcpy (s3, s3_, _s3_high+1);
  memcpy (s4, s4_, _s4_high+1);

  mcMetaError_metaErrorT4 (mcLexBuf_getTokenNo (), (char *) m, _m_high, (unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high, (unsigned char *) s3, _s3_high, (unsigned char *) s4, _s4_high);
}

void mcMetaError_metaErrors1 (char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s_, unsigned int _s_high)
{
  char m1[_m1_high+1];
  char m2[_m2_high+1];
  unsigned char s[_s_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m1, m1_, _m1_high+1);
  memcpy (m2, m2_, _m2_high+1);
  memcpy (s, s_, _s_high+1);

  mcMetaError_metaErrorsT1 (mcLexBuf_getTokenNo (), (char *) m1, _m1_high, (char *) m2, _m2_high, (unsigned char *) s, _s_high);
}

void mcMetaError_metaErrors2 (char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high)
{
  char m1[_m1_high+1];
  char m2[_m2_high+1];
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m1, m1_, _m1_high+1);
  memcpy (m2, m2_, _m2_high+1);
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);

  mcMetaError_metaErrorsT2 (mcLexBuf_getTokenNo (), (char *) m1, _m1_high, (char *) m2, _m2_high, (unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high);
}

void mcMetaError_metaErrors3 (char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high)
{
  char m1[_m1_high+1];
  char m2[_m2_high+1];
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];
  unsigned char s3[_s3_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m1, m1_, _m1_high+1);
  memcpy (m2, m2_, _m2_high+1);
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);
  memcpy (s3, s3_, _s3_high+1);

  mcMetaError_metaErrorsT3 (mcLexBuf_getTokenNo (), (char *) m1, _m1_high, (char *) m2, _m2_high, (unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high, (unsigned char *) s3, _s3_high);
}

void mcMetaError_metaErrors4 (char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high)
{
  char m1[_m1_high+1];
  char m2[_m2_high+1];
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];
  unsigned char s3[_s3_high+1];
  unsigned char s4[_s4_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m1, m1_, _m1_high+1);
  memcpy (m2, m2_, _m2_high+1);
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);
  memcpy (s3, s3_, _s3_high+1);
  memcpy (s4, s4_, _s4_high+1);

  mcMetaError_metaErrorsT4 (mcLexBuf_getTokenNo (), (char *) m1, _m1_high, (char *) m2, _m2_high, (unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high, (unsigned char *) s3, _s3_high, (unsigned char *) s4, _s4_high);
}

void mcMetaError_metaErrorT1 (unsigned int tok, char *m_, unsigned int _m_high, unsigned char *s_, unsigned int _s_high)
{
  char m[_m_high+1];
  unsigned char s[_s_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m, m_, _m_high+1);
  memcpy (s, s_, _s_high+1);

  mcMetaError_metaErrorStringT1 (tok, DynamicStrings_InitString ((char *) m, _m_high), (unsigned char *) s, _s_high);
}

void mcMetaError_metaErrorT2 (unsigned int tok, char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high)
{
  char m[_m_high+1];
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m, m_, _m_high+1);
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);

  mcMetaError_metaErrorStringT2 (tok, DynamicStrings_InitString ((char *) m, _m_high), (unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high);
}

void mcMetaError_metaErrorT3 (unsigned int tok, char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high)
{
  char m[_m_high+1];
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];
  unsigned char s3[_s3_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m, m_, _m_high+1);
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);
  memcpy (s3, s3_, _s3_high+1);

  mcMetaError_metaErrorStringT3 (tok, DynamicStrings_InitString ((char *) m, _m_high), (unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high, (unsigned char *) s3, _s3_high);
}

void mcMetaError_metaErrorT4 (unsigned int tok, char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high)
{
  char m[_m_high+1];
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];
  unsigned char s3[_s3_high+1];
  unsigned char s4[_s4_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m, m_, _m_high+1);
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);
  memcpy (s3, s3_, _s3_high+1);
  memcpy (s4, s4_, _s4_high+1);

  mcMetaError_metaErrorStringT4 (tok, DynamicStrings_InitString ((char *) m, _m_high), (unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high, (unsigned char *) s3, _s3_high, (unsigned char *) s4, _s4_high);
}

void mcMetaError_metaErrorsT1 (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s_, unsigned int _s_high)
{
  varargs_vararg sym;
  char m1[_m1_high+1];
  char m2[_m2_high+1];
  unsigned char s[_s_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m1, m1_, _m1_high+1);
  memcpy (m2, m2_, _m2_high+1);
  memcpy (s, s_, _s_high+1);

  sym = varargs_start1 ((unsigned char *) s, _s_high);
  wrapErrors (tok, (char *) m1, _m1_high, (char *) m2, _m2_high, sym);
  varargs_end (&sym);
}

void mcMetaError_metaErrorsT2 (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high)
{
  varargs_vararg sym;
  char m1[_m1_high+1];
  char m2[_m2_high+1];
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m1, m1_, _m1_high+1);
  memcpy (m2, m2_, _m2_high+1);
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);

  sym = varargs_start2 ((unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high);
  wrapErrors (tok, (char *) m1, _m1_high, (char *) m2, _m2_high, sym);
  varargs_end (&sym);
}

void mcMetaError_metaErrorsT3 (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high)
{
  varargs_vararg sym;
  char m1[_m1_high+1];
  char m2[_m2_high+1];
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];
  unsigned char s3[_s3_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m1, m1_, _m1_high+1);
  memcpy (m2, m2_, _m2_high+1);
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);
  memcpy (s3, s3_, _s3_high+1);

  sym = varargs_start3 ((unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high, (unsigned char *) s3, _s3_high);
  wrapErrors (tok, (char *) m1, _m1_high, (char *) m2, _m2_high, sym);
  varargs_end (&sym);
}

void mcMetaError_metaErrorsT4 (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high)
{
  varargs_vararg sym;
  char m1[_m1_high+1];
  char m2[_m2_high+1];
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];
  unsigned char s3[_s3_high+1];
  unsigned char s4[_s4_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (m1, m1_, _m1_high+1);
  memcpy (m2, m2_, _m2_high+1);
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);
  memcpy (s3, s3_, _s3_high+1);
  memcpy (s4, s4_, _s4_high+1);

  sym = varargs_start4 ((unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high, (unsigned char *) s3, _s3_high, (unsigned char *) s4, _s4_high);
  wrapErrors (tok, (char *) m1, _m1_high, (char *) m2, _m2_high, sym);
  varargs_end (&sym);
}

void mcMetaError_metaErrorString1 (DynamicStrings_String m, unsigned char *s_, unsigned int _s_high)
{
  unsigned char s[_s_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (s, s_, _s_high+1);

  mcMetaError_metaErrorStringT1 (mcLexBuf_getTokenNo (), m, (unsigned char *) s, _s_high);
}

void mcMetaError_metaErrorString2 (DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high)
{
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);

  mcMetaError_metaErrorStringT2 (mcLexBuf_getTokenNo (), m, (unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high);
}

void mcMetaError_metaErrorString3 (DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high)
{
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];
  unsigned char s3[_s3_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);
  memcpy (s3, s3_, _s3_high+1);

  mcMetaError_metaErrorStringT3 (mcLexBuf_getTokenNo (), m, (unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high, (unsigned char *) s3, _s3_high);
}

void mcMetaError_metaErrorString4 (DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high)
{
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];
  unsigned char s3[_s3_high+1];
  unsigned char s4[_s4_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);
  memcpy (s3, s3_, _s3_high+1);
  memcpy (s4, s4_, _s4_high+1);

  mcMetaError_metaErrorStringT4 (mcLexBuf_getTokenNo (), m, (unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high, (unsigned char *) s3, _s3_high, (unsigned char *) s4, _s4_high);
}

void mcMetaError_metaErrorStringT1 (unsigned int tok, DynamicStrings_String m, unsigned char *s_, unsigned int _s_high)
{
  DynamicStrings_String str;
  mcError_error e;
  varargs_vararg sym;
  errorType t;
  unsigned char s[_s_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (s, s_, _s_high+1);

  e = NULL;
  sym = varargs_start1 ((unsigned char *) s, _s_high);
  t = newerror;
  str = doFormat (&e, &t, m, sym);
  e = doError (e, t, tok);
  mcError_errorString (e, str);
  varargs_end (&sym);
}

void mcMetaError_metaErrorStringT2 (unsigned int tok, DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high)
{
  DynamicStrings_String str;
  mcError_error e;
  varargs_vararg sym;
  errorType t;
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);

  e = NULL;
  sym = varargs_start2 ((unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high);
  t = newerror;
  str = doFormat (&e, &t, m, sym);
  e = doError (e, t, tok);
  mcError_errorString (e, str);
  varargs_end (&sym);
}

void mcMetaError_metaErrorStringT3 (unsigned int tok, DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high)
{
  DynamicStrings_String str;
  mcError_error e;
  varargs_vararg sym;
  errorType t;
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];
  unsigned char s3[_s3_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);
  memcpy (s3, s3_, _s3_high+1);

  e = NULL;
  sym = varargs_start3 ((unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high, (unsigned char *) s3, _s3_high);
  t = newerror;
  str = doFormat (&e, &t, m, sym);
  e = doError (e, t, tok);
  mcError_errorString (e, str);
  varargs_end (&sym);
}

void mcMetaError_metaErrorStringT4 (unsigned int tok, DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high)
{
  DynamicStrings_String str;
  mcError_error e;
  varargs_vararg sym;
  errorType t;
  unsigned char s1[_s1_high+1];
  unsigned char s2[_s2_high+1];
  unsigned char s3[_s3_high+1];
  unsigned char s4[_s4_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (s1, s1_, _s1_high+1);
  memcpy (s2, s2_, _s2_high+1);
  memcpy (s3, s3_, _s3_high+1);
  memcpy (s4, s4_, _s4_high+1);

  e = NULL;
  sym = varargs_start4 ((unsigned char *) s1, _s1_high, (unsigned char *) s2, _s2_high, (unsigned char *) s3, _s3_high, (unsigned char *) s4, _s4_high);
  t = newerror;
  str = doFormat (&e, &t, m, sym);
  e = doError (e, t, tok);
  mcError_errorString (e, str);
  varargs_end (&sym);
}

void _M2_mcMetaError_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_mcMetaError_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
