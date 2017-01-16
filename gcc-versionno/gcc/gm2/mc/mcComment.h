
#ifndef mcCommentH
#define mcCommentH

/*
 *  addText - the text cs is appended to the current comment.
 */

extern mcComment_addText (char *cs);


/*
 *
 *  beginComment - the start of a new comment has been seen by the lexical analyser.
 *                 A new comment block is created and all addText contents are placed
 *                 in this block.
 */

extern void mcComment_beginComment (void);


/*
 *  getCommentCharStar - returns the current comment.
 */

extern void *mcComment_getCommentCharStar (void);


/*
 *  endComment - the end of the comment has been seen by the lexical analyser.
 */

extern void mcComment_endComment (void);


#endif
