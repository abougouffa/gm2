/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcComment.mod.  */

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
#define _mcComment_H
#define _mcComment_C

#   include "GIndexing.h"
#   include "GDynamicStrings.h"
#   include "GStorage.h"
#   include "GnameKey.h"
#   include "GmcDebug.h"
#   include "GASCII.h"

typedef struct _T1_r _T1;

typedef _T1 *commentDescriptor;

typedef enum {unknown, procedureHeading, inBody, afterStatement} commentType;

struct _T1_r {
               commentType type;
               DynamicStrings_String content;
               nameKey_Name procName;
             };

static unsigned int incomment;
static unsigned int currentComment;
static unsigned int maxComment;
static Indexing_Index comments;

/*
   beginComment - the start of a new comment has been seen by the lexical analyser.
                  A new comment block is created and all addText contents are placed
                  in this block.
*/

void mcComment_beginComment (void);

/*
   endComment - the end of the comment has been seen by the lexical analyser.
*/

void mcComment_endComment (void);

/*
   addText - cs is a C string (null terminated) which contains comment text.
             This is appended to the current comment.
*/

void mcComment_addText (void * cs);

/*
   getComment - returns the current comment.
*/

DynamicStrings_String mcComment_getComment (void);

/*
   getCommentCharStar - returns the current comment.
*/

void * mcComment_getCommentCharStar (void);

/*
   setProcedureComment - changes the type of the current comment to a procedure heading comment,
                         providing it has the procname as the first word.
*/

void mcComment_setProcedureComment (nameKey_Name procname);

/*
   getProcedureComment - returns the current procedure comment if available.
*/

DynamicStrings_String mcComment_getProcedureComment (void);

/*
   newPass - resets the comment count so that we can collect the comments in order again.
*/

void mcComment_newPass (void);

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

static unsigned int seenProcedure (commentDescriptor cd, nameKey_Name procName);

/*
   init -
*/

static void init (void);


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

static unsigned int seenProcedure (commentDescriptor cd, nameKey_Name procName)
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
   init -
*/

static void init (void)
{
  incomment = FALSE;
  maxComment = 0;
  currentComment = 0;
  comments = Indexing_InitIndex (1);
}


/*
   beginComment - the start of a new comment has been seen by the lexical analyser.
                  A new comment block is created and all addText contents are placed
                  in this block.
*/

void mcComment_beginComment (void)
{
  commentDescriptor cd;

  if (! incomment)
    {
      Storage_ALLOCATE ((void **) &cd, sizeof (_T1));
      mcDebug_assert (cd != NULL);
      maxComment += 1;
      currentComment = maxComment;
      cd->type = unknown;
      cd->content = DynamicStrings_InitString ((char *) "", 0);
      cd->procName = nameKey_NulName;
      Indexing_PutIndice (comments, maxComment, (void *) cd);
      incomment = TRUE;
    }
}


/*
   endComment - the end of the comment has been seen by the lexical analyser.
*/

void mcComment_endComment (void)
{
  incomment = FALSE;
}


/*
   addText - cs is a C string (null terminated) which contains comment text.
             This is appended to the current comment.
*/

void mcComment_addText (void * cs)
{
  commentDescriptor cd;

  mcDebug_assert (incomment);
  cd = Indexing_GetIndice (comments, maxComment);
  mcDebug_assert (cd != NULL);
  cd->content = DynamicStrings_ConCat (cd->content, DynamicStrings_InitStringCharStar (cs));
}


/*
   getComment - returns the current comment.
*/

DynamicStrings_String mcComment_getComment (void)
{
  commentDescriptor cd;

  if (currentComment <= maxComment)
    {
      cd = Indexing_GetIndice (comments, currentComment);
      mcDebug_assert (cd != NULL);
      return cd->content;
    }
  return NULL;
}


/*
   getCommentCharStar - returns the current comment.
*/

void * mcComment_getCommentCharStar (void)
{
  DynamicStrings_String s;

  s = mcComment_getComment ();
  if (s == NULL)
    return NULL;
  else
    return DynamicStrings_string (s);
}


/*
   setProcedureComment - changes the type of the current comment to a procedure heading comment,
                         providing it has the procname as the first word.
*/

void mcComment_setProcedureComment (nameKey_Name procname)
{
  commentDescriptor cd;

  if ((currentComment > 0) && (currentComment <= maxComment))
    {
      cd = Indexing_GetIndice (comments, currentComment);
      mcDebug_assert (cd != NULL);
      if (seenProcedure (cd, procname))
        {
          cd->type = procedureHeading;
          cd->procName = procname;
        }
    }
}


/*
   getProcedureComment - returns the current procedure comment if available.
*/

DynamicStrings_String mcComment_getProcedureComment (void)
{
  commentDescriptor cd;

  if (Indexing_InBounds (comments, currentComment))
    {
      cd = Indexing_GetIndice (comments, currentComment);
      mcDebug_assert (cd != NULL);
      if (cd->type == procedureHeading)
        return cd->content;
    }
  return NULL;
}


/*
   newPass - resets the comment count so that we can collect the comments in order again.
*/

void mcComment_newPass (void)
{
  currentComment = 0;
}

void _M2_mcComment_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  init ();
}

void _M2_mcComment_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
