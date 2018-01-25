/* automatically created by mc from ../../gcc-versionno/gcc/gm2/mc/mcComment.mod.  */

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
#   include "Gmcrts.h"
#define _mcComment_H
#define _mcComment_C

#   include "GDynamicStrings.h"
#   include "GStorage.h"
#   include "GnameKey.h"
#   include "GmcDebug.h"
#   include "GASCII.h"
#   include "Glibc.h"

typedef struct _T1_r _T1;

typedef enum {unknown, procedureHeading, inBody, afterStatement} commentType;

typedef _T1 *mcComment_commentDesc;

struct _T1_r {
               commentType type;
               DynamicStrings_String content;
               nameKey_Name procName;
               unsigned int used;
             };


/*
   initComment - the start of a new comment has been seen by the lexical analyser.
                 A new comment block is created and all addText contents are placed
                 in this block.  onlySpaces indicates whether we have only seen
                 spaces on this line.
*/

mcComment_commentDesc mcComment_initComment (unsigned int onlySpaces);

/*
   addText - cs is a C string (null terminated) which contains comment text.
             This is appended to the comment, cd.
*/

void mcComment_addText (mcComment_commentDesc cd, void * cs);

/*
   getContent - returns the content of comment, cd.
*/

DynamicStrings_String mcComment_getContent (mcComment_commentDesc cd);

/*
   getCommentCharStar - returns the C string content of comment, cd.
*/

void * mcComment_getCommentCharStar (mcComment_commentDesc cd);

/*
   setProcedureComment - changes the type of comment, cd, to a
                         procedure heading comment,
                         providing it has the procname as the first word.
*/

void mcComment_setProcedureComment (mcComment_commentDesc cd, nameKey_Name procname);

/*
   getProcedureComment - returns the current procedure comment if available.
*/

DynamicStrings_String mcComment_getProcedureComment (mcComment_commentDesc cd);

/*
   getAfterStatementComment - returns the current statement after comment if available.
*/

DynamicStrings_String mcComment_getAfterStatementComment (mcComment_commentDesc cd);

/*
   getInbodyStatementComment - returns the current statement after comment if available.
*/

DynamicStrings_String mcComment_getInbodyStatementComment (mcComment_commentDesc cd);

/*
   isProcedureComment - returns TRUE if, cd, is a procedure comment.
*/

unsigned int mcComment_isProcedureComment (mcComment_commentDesc cd);

/*
   isBodyComment - returns TRUE if, cd, is a body comment.
*/

unsigned int mcComment_isBodyComment (mcComment_commentDesc cd);

/*
   isAfterComment - returns TRUE if, cd, is an after comment.
*/

unsigned int mcComment_isAfterComment (mcComment_commentDesc cd);

/*
   Min - returns the lower of, a, and, b.
*/

static unsigned int Min (unsigned int a, unsigned int b);

/*
   RemoveNewlines -
*/

static DynamicStrings_String RemoveNewlines (DynamicStrings_String s);

/*
   seenProcedure - returns TRUE if the name, procName, appears as the first word
                   in the comment.
*/

static unsigned int seenProcedure (mcComment_commentDesc cd, nameKey_Name procName);

/*
   dumpComment -
*/

static void dumpComment (mcComment_commentDesc cd);


/*
   Min - returns the lower of, a, and, b.
*/

static unsigned int Min (unsigned int a, unsigned int b)
{
  if (a < b)
    return a;
  else
    return b;
}


/*
   RemoveNewlines -
*/

static DynamicStrings_String RemoveNewlines (DynamicStrings_String s)
{
  while ((DynamicStrings_Length (s)) > 0)
    if ((DynamicStrings_char (s, 0)) == ASCII_nl)
      s = DynamicStrings_RemoveWhitePrefix (DynamicStrings_Slice (s, 1, 0));
    else
      return DynamicStrings_RemoveWhitePrefix (s);
  return s;
}


/*
   seenProcedure - returns TRUE if the name, procName, appears as the first word
                   in the comment.
*/

static unsigned int seenProcedure (mcComment_commentDesc cd, nameKey_Name procName)
{
  DynamicStrings_String s;
  void * a;
  unsigned int i;
  unsigned int h;
  unsigned int res;

  a = nameKey_keyToCharStar (procName);
  s = RemoveNewlines (cd->content);
  s = DynamicStrings_Slice (DynamicStrings_Mark (s), 0, (int) Min (DynamicStrings_Length (s), nameKey_lengthKey (procName)));
  res = DynamicStrings_EqualCharStar (s, a);
  s = DynamicStrings_KillString (s);
  return res;
}


/*
   dumpComment -
*/

static void dumpComment (mcComment_commentDesc cd)
{
  libc_printf ((char *) "comment : ", 10);
  switch (cd->type)
    {
      case unknown:
        libc_printf ((char *) "unknown", 7);
        break;

      case procedureHeading:
        libc_printf ((char *) "procedureheading", 16);
        break;

      case inBody:
        libc_printf ((char *) "inbody", 6);
        break;

      case afterStatement:
        libc_printf ((char *) "afterstatement", 14);
        break;


      default:
        CaseException ("../../gcc-versionno/gcc/gm2/mc/mcComment.def", 2, 1);
    }
  if (cd->used)
    libc_printf ((char *) " used", 5);
  else
    libc_printf ((char *) " unused", 7);
  libc_printf ((char *) " contents = %s\\n", 16, DynamicStrings_string (cd->content));
}


/*
   initComment - the start of a new comment has been seen by the lexical analyser.
                 A new comment block is created and all addText contents are placed
                 in this block.  onlySpaces indicates whether we have only seen
                 spaces on this line.
*/

mcComment_commentDesc mcComment_initComment (unsigned int onlySpaces)
{
  mcComment_commentDesc cd;

  Storage_ALLOCATE ((void **) &cd, sizeof (_T1));
  mcDebug_assert (cd != NULL);
  if (onlySpaces)
    cd->type = inBody;
  else
    cd->type = afterStatement;
  cd->content = DynamicStrings_InitString ((char *) "", 0);
  cd->procName = nameKey_NulName;
  cd->used = FALSE;
  return cd;
}


/*
   addText - cs is a C string (null terminated) which contains comment text.
             This is appended to the comment, cd.
*/

void mcComment_addText (mcComment_commentDesc cd, void * cs)
{
  if (cd != NULL)
    cd->content = DynamicStrings_ConCat (cd->content, DynamicStrings_InitStringCharStar (cs));
}


/*
   getContent - returns the content of comment, cd.
*/

DynamicStrings_String mcComment_getContent (mcComment_commentDesc cd)
{
  if (cd != NULL)
    return cd->content;
  return NULL;
}


/*
   getCommentCharStar - returns the C string content of comment, cd.
*/

void * mcComment_getCommentCharStar (mcComment_commentDesc cd)
{
  DynamicStrings_String s;

  s = mcComment_getContent (cd);
  if (s == NULL)
    return NULL;
  else
    return DynamicStrings_string (s);
}


/*
   setProcedureComment - changes the type of comment, cd, to a
                         procedure heading comment,
                         providing it has the procname as the first word.
*/

void mcComment_setProcedureComment (mcComment_commentDesc cd, nameKey_Name procname)
{
  if (cd != NULL)
    if (seenProcedure (cd, procname))
      {
        cd->type = procedureHeading;
        cd->procName = procname;
      }
}


/*
   getProcedureComment - returns the current procedure comment if available.
*/

DynamicStrings_String mcComment_getProcedureComment (mcComment_commentDesc cd)
{
  if ((cd->type == procedureHeading) && ! cd->used)
    {
      cd->used = TRUE;
      return cd->content;
    }
  return NULL;
}


/*
   getAfterStatementComment - returns the current statement after comment if available.
*/

DynamicStrings_String mcComment_getAfterStatementComment (mcComment_commentDesc cd)
{
  if ((cd->type == afterStatement) && ! cd->used)
    {
      cd->used = TRUE;
      return cd->content;
    }
  return NULL;
}


/*
   getInbodyStatementComment - returns the current statement after comment if available.
*/

DynamicStrings_String mcComment_getInbodyStatementComment (mcComment_commentDesc cd)
{
  if ((cd->type == inBody) && ! cd->used)
    {
      cd->used = TRUE;
      return cd->content;
    }
  return NULL;
}


/*
   isProcedureComment - returns TRUE if, cd, is a procedure comment.
*/

unsigned int mcComment_isProcedureComment (mcComment_commentDesc cd)
{
  return (cd != NULL) && (cd->type == procedureHeading);
}


/*
   isBodyComment - returns TRUE if, cd, is a body comment.
*/

unsigned int mcComment_isBodyComment (mcComment_commentDesc cd)
{
  return (cd != NULL) && (cd->type == inBody);
}


/*
   isAfterComment - returns TRUE if, cd, is an after comment.
*/

unsigned int mcComment_isAfterComment (mcComment_commentDesc cd)
{
  return (cd != NULL) && (cd->type == afterStatement);
}

void _M2_mcComment_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_mcComment_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
