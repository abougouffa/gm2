/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcLexBuf.def.  */


#if !defined (_mcLexBuf_H)
#   define _mcLexBuf_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"
#   include "GmcReserved.h"
#   include "GDynamicStrings.h"

#   if defined (_mcLexBuf_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

EXTERN mcReserved_toktype mcLexBuf_currenttoken;
EXTERN void * mcLexBuf_currentstring;
EXTERN unsigned int mcLexBuf_currentcolumn;
EXTERN int mcLexBuf_currentinteger;

/*
   openSource - Attempts to open the source file, s.
                The success of the operation is returned.
*/

EXTERN unsigned int mcLexBuf_openSource (DynamicStrings_String s);

/*
   closeSource - closes the current open file.
*/

EXTERN void mcLexBuf_closeSource (void);

/*
   reInitialize - re-initialize the all the data structures.
*/

EXTERN void mcLexBuf_reInitialize (void);

/*
   resetForNewPass - reset the buffer pointers to the beginning ready for
                     a new pass
*/

EXTERN void mcLexBuf_resetForNewPass (void);

/*
   getToken - gets the next token into currenttoken.
*/

EXTERN void mcLexBuf_getToken (void);

/*
   insertToken - inserts a symbol, token, infront of the current token
                 ready for the next pass.
*/

EXTERN void mcLexBuf_insertToken (mcReserved_toktype token);

/*
   insertTokenAndRewind - inserts a symbol, token, infront of the current token
                          and then moves the token stream back onto the inserted token.
*/

EXTERN void mcLexBuf_insertTokenAndRewind (mcReserved_toktype token);

/*
   getPreviousTokenLineNo - returns the line number of the previous token.
*/

EXTERN unsigned int mcLexBuf_getPreviousTokenLineNo (void);

/*
   getLineNo - returns the current line number where the symbol occurs in
               the source file.
*/

EXTERN unsigned int mcLexBuf_getLineNo (void);

/*
   getTokenNo - returns the current token number.
*/

EXTERN unsigned int mcLexBuf_getTokenNo (void);

/*
   tokenToLineNo - returns the line number of the current file for the
                   TokenNo. The depth refers to the include depth.
                   A depth of 0 is the current file, depth of 1 is the file
                   which included the current file. Zero is returned if the
                   depth exceeds the file nesting level.
*/

EXTERN unsigned int mcLexBuf_tokenToLineNo (unsigned int tokenNo, unsigned int depth);

/*
   getColumnNo - returns the current column where the symbol occurs in
                 the source file.
*/

EXTERN unsigned int mcLexBuf_getColumnNo (void);

/*
   tokenToColumnNo - returns the column number of the current file for the
                     TokenNo. The depth refers to the include depth.
                     A depth of 0 is the current file, depth of 1 is the file
                     which included the current file. Zero is returned if the
                     depth exceeds the file nesting level.
*/

EXTERN unsigned int mcLexBuf_tokenToColumnNo (unsigned int tokenNo, unsigned int depth);

/*
   findFileNameFromToken - returns the complete FileName for the appropriate
                           source file yields the token number, TokenNo.
                           The, Depth, indicates the include level: 0..n
                           Level 0 is the current. NIL is returned if n+1
                           is requested.
*/

EXTERN DynamicStrings_String mcLexBuf_findFileNameFromToken (unsigned int tokenNo, unsigned int depth);

/*
   getFileName - returns a String defining the current file.
*/

EXTERN DynamicStrings_String mcLexBuf_getFileName (void);

/*
   addTok - adds a token to the buffer.
*/

EXTERN void mcLexBuf_addTok (mcReserved_toktype t);

/*
   addTokCharStar - adds a token to the buffer and an additional string, s.
                    A copy of string, s, is made.
*/

EXTERN void mcLexBuf_addTokCharStar (mcReserved_toktype t, void * s);

/*
   addTokInteger - adds a token and an integer to the buffer.
*/

EXTERN void mcLexBuf_addTokInteger (mcReserved_toktype t, int i);

/*
   setFile - sets the current filename to, filename.
*/

EXTERN void mcLexBuf_setFile (void * filename);

/*
   pushFile - indicates that, filename, has just been included.
*/

EXTERN void mcLexBuf_pushFile (void * filename);

/*
   popFile - indicates that we are returning to, filename, having finished
             an include.
*/

EXTERN void mcLexBuf_popFile (void * filename);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif