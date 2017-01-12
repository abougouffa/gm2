/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcComment.def.  */


#if !defined (_mcComment_H)
#   define _mcComment_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"
#   include "GDynamicStrings.h"
#   include "GnameKey.h"

#   if defined (_mcComment_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   beginComment - the start of a new comment has been seen by the lexical analyser.
                  A new comment block is created and all addText contents are placed
                  in this block.
*/

EXTERN void mcComment_beginComment (void);

/*
   endComment - the end of the comment has been seen by the lexical analyser.
*/

EXTERN void mcComment_endComment (void);

/*
   addText - cs is a C string (null terminated) which contains comment text.
             This is appended to the current comment.
*/

EXTERN void mcComment_addText (void * cs);

/*
   getComment - returns the current comment.
*/

EXTERN DynamicStrings_String mcComment_getComment (void);

/*
   getCommentCharStar - returns the current comment.
*/

EXTERN void * mcComment_getCommentCharStar (void);

/*
   setProcedureComment - changes the type of the current comment to a procedure heading comment,
                         providing it has the procname as the first word.
*/

EXTERN void mcComment_setProcedureComment (nameKey_Name procname);

/*
   getProcedureComment - returns the current procedure comment if available.
*/

EXTERN DynamicStrings_String mcComment_getProcedureComment (void);

/*
   newPass - resets the comment count so that we can collect the comments in order again.
*/

EXTERN void mcComment_newPass (void);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
