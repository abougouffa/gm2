
#ifndef mcCommentH
#define mcCommentH

/*
 *  addText - the text cs is appended to the current comment.
 */

extern void mcComment_addText (void *cd, char *cs);


/*
 *  initComment - the start of a new comment has been seen by the lexical analyser.
 *                A new comment block is created and all addText contents are placed
 *                in this block.  onlySpaces indicates whether we have only seen
 *                spaces on this line.  The new comment descriptor is returned.
 *		 If onlySpaces is TRUE then an inbody comment is created.
 *		 If onlySpaces is FALSE then an after statement comment is created.
 */

extern void *mcComment_initComment (unsigned int onlySpaces);


#endif
