%{
/* Copyright (C) 2001 Free Software Foundation, Inc.
   This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include <p2c/p2c.h>

#include "M2Reserved.h"
#include "M2LexBuf.h"

  /*
   *  m2.lex - provides a lexical analyser for GNU Modula-2
   */

  struct lineInfo {
    char            *linebuf;          /* line contents */
    int              linelen;          /* length */
    int              tokenpos;         /* start position of token within line */
    int              toklen;           /* a copy of yylen (length of token) */
    int              nextpos;          /* position after token */
    int              actualline;       /* line number of this line */
    int              inuse;            /* do we need to keep this line info? */
    struct lineInfo *next;
  };

  static int              lineno      =1;   /* a running count of the file line number */
  static char            *filename    =NULL;
  static int              commentLevel=0;
  static struct lineInfo *currentLine=NULL;

        void m2lex_M2Error(char *);
static  void pushLine     (void);
static  void popLine      (void);
static  void finishedLine (void);
static  void resetpos     (void);
static  void consumeLine  (void);
static  void updatepos    (void);
static  void skippos      (void);
static  void poperrorskip (char *);
static  void endOfComment (void);
static  void handleDate   (void);
static  void handleLine   (void);
static  void handleFile   (void);

#if !defined(TRUE)
#    define TRUE  (1==1)
#endif
#if !defined(FALSE)
#    define FALSE (1==0)
#endif

#define YY_DECL void yylex (void)


#if !defined(GM2)
static void *xmalloc (unsigned int size)
{
  void *a=(void *)malloc(size);

  if (a == NULL) {
    m2lex_M2Error("memory exhausted");
    exit(1);
  }
  return a;
}
#endif
%}

%x COMMENT COMMENT1 LINE0 LINE1 LINE2

%%

"(*"                       { updatepos();
                             commentLevel=1; pushLine(); skippos(); 
			     BEGIN COMMENT; }
<COMMENT>"*)"              { endOfComment(); }
<COMMENT>"(*"              { commentLevel++; pushLine(); updatepos(); skippos(); }
<COMMENT>"<*"              { if (commentLevel == 1) {
                               updatepos();
                               pushLine();
                               skippos();
                               BEGIN COMMENT1;
                             } else
                               updatepos(); skippos();
                           }
<COMMENT>\n.*              { consumeLine(); }
<COMMENT>.                 { updatepos(); skippos(); }
<COMMENT1>.                { updatepos(); skippos(); }
<COMMENT1>ATTRIBUTE_UNUSED { updatepos(); return; }
<COMMENT1>"*>"             { updatepos(); skippos(); finishedLine(); BEGIN COMMENT; }
<COMMENT1>\n.*             { consumeLine(); }
<COMMENT1>"*)"             { poperrorskip("unterminated source code directive, missing *>");
                             endOfComment(); }
<COMMENT1><<EOF>>          { poperrorskip("unterminated source code directive, missing *>"); BEGIN COMMENT; }
<COMMENT><<EOF>>           { poperrorskip("unterminated comment found at the end of the file, missing *)"); BEGIN INITIAL; }

^\#.*                      { consumeLine(); /* printf("found: %s\n", currentLine->linebuf); */ BEGIN LINE0; }
\n\#.*                     { consumeLine(); /* printf("found: %s\n", currentLine->linebuf); */ BEGIN LINE0; }
<LINE0>\#[ \t]*            { updatepos(); }
<LINE0>[0-9]+[ \t]*\"      { updatepos(); lineno=atoi(yytext)-1; BEGIN LINE1; }
<LINE0>\n                  { m2lex_M2Error("missing initial quote after #line directive"); resetpos(); BEGIN INITIAL; }
<LINE0>[^\n]
<LINE1>[^\"\n]+            { m2lex_M2Error("missing final quote after #line directive"); resetpos(); BEGIN INITIAL; }
<LINE1>.*\"                { updatepos();
                             if (filename != NULL) {
			       free(filename);
	                     }
                             filename = (char *)strdup(yytext);
                             filename[yyleng-1] = (char)0;  /* remove trailing quote */
                             BEGIN LINE2;
                           }
<LINE2>[ \t]*              { updatepos(); }
<LINE2>\n                  { M2LexBuf_SetFile(filename); updatepos(); BEGIN INITIAL; }
<LINE2>2[ \t]*\n           { M2LexBuf_PopFile(filename); updatepos(); BEGIN INITIAL; }
<LINE2>1[ \t]*\n           { M2LexBuf_PushFile(filename); updatepos(); BEGIN INITIAL; }

\n[^\#].*                  { consumeLine(); /* printf("found: %s\n", currentLine->linebuf); */ }
\n                         { consumeLine(); /* printf("found: %s\n", currentLine->linebuf); */ }

\"[^\"\n]*\"               { updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_stringtok, yytext); return; }
\"[^\"\n]*$                { updatepos();
                             m2lex_M2Error("missing terminating quote, \"");
                             resetpos(); return;
                           }

'[^'\n]*'                  { updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_stringtok, yytext); return; }
'[^'\n]*$                  { updatepos();
                             m2lex_M2Error("missing terminating quote, '");
                             resetpos(); return;
                           }

<<EOF>>                    { updatepos(); M2LexBuf_AddTok(M2Reserved_eoftok); return; }
\+                         { updatepos(); M2LexBuf_AddTok(M2Reserved_plustok); return; }
-                          { updatepos(); M2LexBuf_AddTok(M2Reserved_minustok); return; }
"*"                        { updatepos(); M2LexBuf_AddTok(M2Reserved_timestok); return; }
\/                         { updatepos(); M2LexBuf_AddTok(M2Reserved_dividetok); return; }
:=                         { updatepos(); M2LexBuf_AddTok(M2Reserved_becomestok); return; }
\&                         { updatepos(); M2LexBuf_AddTok(M2Reserved_ambersandtok); return; }
\.                         { updatepos(); M2LexBuf_AddTok(M2Reserved_periodtok); return; }
\,                         { updatepos(); M2LexBuf_AddTok(M2Reserved_commatok); return; }
\;                         { updatepos(); M2LexBuf_AddTok(M2Reserved_semicolontok); return; }
\(                         { updatepos(); M2LexBuf_AddTok(M2Reserved_lparatok); return; }
\)                         { updatepos(); M2LexBuf_AddTok(M2Reserved_rparatok); return; }
\[                         { updatepos(); M2LexBuf_AddTok(M2Reserved_lsbratok); return; }
\]                         { updatepos(); M2LexBuf_AddTok(M2Reserved_rsbratok); return; }
\^                         { updatepos(); M2LexBuf_AddTok(M2Reserved_uparrowtok); return; }
\{                         { updatepos(); M2LexBuf_AddTok(M2Reserved_lcbratok); return; }
\}                         { updatepos(); M2LexBuf_AddTok(M2Reserved_rcbratok); return; }
\'                         { updatepos(); M2LexBuf_AddTok(M2Reserved_singlequotetok); return; }
\=                         { updatepos(); M2LexBuf_AddTok(M2Reserved_equaltok); return; }
\#                         { updatepos(); M2LexBuf_AddTok(M2Reserved_hashtok); return; }
\<                         { updatepos(); M2LexBuf_AddTok(M2Reserved_lesstok); return; }
\>                         { updatepos(); M2LexBuf_AddTok(M2Reserved_greatertok); return; }
\<\>                       { updatepos(); M2LexBuf_AddTok(M2Reserved_lessgreatertok); return; }
\<\=                       { updatepos(); M2LexBuf_AddTok(M2Reserved_lessequaltok); return; }
\>\=                       { updatepos(); M2LexBuf_AddTok(M2Reserved_greaterequaltok); return; }
\.\.                       { updatepos(); M2LexBuf_AddTok(M2Reserved_periodperiodtok); return; }
\.\.\.                     { updatepos(); M2LexBuf_AddTok(M2Reserved_periodperiodperiodtok); return; }
\:                         { updatepos(); M2LexBuf_AddTok(M2Reserved_colontok); return; }
\"                         { updatepos(); M2LexBuf_AddTok(M2Reserved_doublequotestok); return; }
\|                         { updatepos(); M2LexBuf_AddTok(M2Reserved_bartok); return; }
AND                        { updatepos(); M2LexBuf_AddTok(M2Reserved_andtok); return; }
ARRAY                      { updatepos(); M2LexBuf_AddTok(M2Reserved_arraytok); return; }
BEGIN                      { updatepos(); M2LexBuf_AddTok(M2Reserved_begintok); return; }
BY                         { updatepos(); M2LexBuf_AddTok(M2Reserved_bytok); return; }
CASE                       { updatepos(); M2LexBuf_AddTok(M2Reserved_casetok); return; }
CONST                      { updatepos(); M2LexBuf_AddTok(M2Reserved_consttok); return; }
DEFINITION                 { updatepos(); M2LexBuf_AddTok(M2Reserved_definitiontok); return; }
DIV                        { updatepos(); M2LexBuf_AddTok(M2Reserved_divtok); return; }
DO                         { updatepos(); M2LexBuf_AddTok(M2Reserved_dotok); return; }
ELSE                       { updatepos(); M2LexBuf_AddTok(M2Reserved_elsetok); return; }
ELSIF                      { updatepos(); M2LexBuf_AddTok(M2Reserved_elsiftok); return; }
END                        { updatepos(); M2LexBuf_AddTok(M2Reserved_endtok); return; }
EXIT                       { updatepos(); M2LexBuf_AddTok(M2Reserved_exittok); return; }
EXPORT                     { updatepos(); M2LexBuf_AddTok(M2Reserved_exporttok); return; }
FOR                        { updatepos(); M2LexBuf_AddTok(M2Reserved_fortok); return; }
FROM                       { updatepos(); M2LexBuf_AddTok(M2Reserved_fromtok); return; }
IF                         { updatepos(); M2LexBuf_AddTok(M2Reserved_iftok); return; }
IMPLEMENTATION             { updatepos(); M2LexBuf_AddTok(M2Reserved_implementationtok); return; }
IMPORT                     { updatepos(); M2LexBuf_AddTok(M2Reserved_importtok); return; }
IN                         { updatepos(); M2LexBuf_AddTok(M2Reserved_intok); return; }
LOOP                       { updatepos(); M2LexBuf_AddTok(M2Reserved_looptok); return; }
MOD                        { updatepos(); M2LexBuf_AddTok(M2Reserved_modtok); return; }
MODULE                     { updatepos(); M2LexBuf_AddTok(M2Reserved_moduletok); return; }
NOT                        { updatepos(); M2LexBuf_AddTok(M2Reserved_nottok); return; }
OF                         { updatepos(); M2LexBuf_AddTok(M2Reserved_oftok); return; }
OR                         { updatepos(); M2LexBuf_AddTok(M2Reserved_ortok); return; }
POINTER                    { updatepos(); M2LexBuf_AddTok(M2Reserved_pointertok); return; }
PROCEDURE                  { updatepos(); M2LexBuf_AddTok(M2Reserved_proceduretok); return; }
QUALIFIED                  { updatepos(); M2LexBuf_AddTok(M2Reserved_qualifiedtok); return; }
UNQUALIFIED                { updatepos(); M2LexBuf_AddTok(M2Reserved_unqualifiedtok); return; }
RECORD                     { updatepos(); M2LexBuf_AddTok(M2Reserved_recordtok); return; }
REPEAT                     { updatepos(); M2LexBuf_AddTok(M2Reserved_repeattok); return; }
RETURN                     { updatepos(); M2LexBuf_AddTok(M2Reserved_returntok); return; }
SET                        { updatepos(); M2LexBuf_AddTok(M2Reserved_settok); return; }
THEN                       { updatepos(); M2LexBuf_AddTok(M2Reserved_thentok); return; }
TO                         { updatepos(); M2LexBuf_AddTok(M2Reserved_totok); return; }
TYPE                       { updatepos(); M2LexBuf_AddTok(M2Reserved_typetok); return; }
UNTIL                      { updatepos(); M2LexBuf_AddTok(M2Reserved_untiltok); return; }
VAR                        { updatepos(); M2LexBuf_AddTok(M2Reserved_vartok); return; }
WHILE                      { updatepos(); M2LexBuf_AddTok(M2Reserved_whiletok); return; }
WITH                       { updatepos(); M2LexBuf_AddTok(M2Reserved_withtok); return; }
ASM                        { updatepos(); M2LexBuf_AddTok(M2Reserved_asmtok); return; }
VOLATILE                   { updatepos(); M2LexBuf_AddTok(M2Reserved_volatiletok); return; }
\_\_DATE\_\_               { updatepos(); handleDate(); return; }
\_\_LINE\_\_               { updatepos(); handleLine(); return; }
\_\_FILE\_\_               { updatepos(); handleFile(); return; }

(([0-9]*\.[0-9]+)(E[+-]?[0-9]+)?) { updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_realtok, yytext); return; }
[a-zA-Z_][a-zA-Z0-9_]*     { updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_identtok, yytext); return; }
[0-9]+                     { updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_integertok, yytext); return; }
[0-9]+B                    { updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_integertok, yytext); return; }
[0-9]+C                    { updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_integertok, yytext); return; }
[0-9A-F]+H                 { updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_integertok, yytext); return; }
[\t ]+                     { currentLine->tokenpos += yyleng;  /* ignore whitespace */; }
.                          { updatepos(); m2lex_M2Error("unrecognised symbol"); skippos(); }

%%

/* have removed the -? from the beginning of the real/integer constant literal rules */

/*
 *  hand built routines
 */

/*
 *  handleFile - handles the __FILE__ construct by wraping it in double quotes and putting
 *               it into the token buffer as a string.
 */

static void handleFile (void)
{
  char *s = (char *)xmalloc(strlen(filename)+2+1);

  strcpy(s, "\"");
  strcat(s, filename);
  strcat(s, "\"");
  M2LexBuf_AddTokCharStar(M2Reserved_stringtok, s);
  free(s);
}

/*
 *  handleLine - handles the __LINE__ construct by passing an integer to
 *               the token buffer.
 */

static void handleLine (void)
{
  M2LexBuf_AddTokInteger(M2Reserved_integertok, lineno);
}

/*
 *  handleDate - handles the __DATE__ construct by passing the date
 *               as a string to the token buffer.
 */

static void handleDate (void)
{
  time_t  clock = time((long *)0);
  char   *sdate = ctime(&clock);
  char   *s     = (char *)xmalloc(strlen(sdate)+2+1);
  char   *p     = index(sdate, '\n');

  if (p != NULL) {
    *p = (char) 0;
  }
  strcpy(s, "\"");
  strcat(s, sdate);
  strcat(s, "\"");
  M2LexBuf_AddTokCharStar(M2Reserved_stringtok, s);
  free(s);
}

/*
 *  endOfComment - handles the end of comment
 */

static void endOfComment (void)
{
  commentLevel--;
  updatepos();
  skippos();
  if (commentLevel==0) {
    BEGIN INITIAL;
    finishedLine();
  } else
    popLine();
}

/*
 *  m2lex_M2Error - displays the error message, s, after the code line and pointer
 *                  to the erroneous token.
 */

void m2lex_M2Error (char *s)
{
  if (currentLine->linebuf != NULL) {
    int i=1;

    printf("%s:%d:%s\n", filename, currentLine->actualline, currentLine->linebuf);
    printf("%s:%d:%*s", filename, currentLine->actualline, 1+currentLine->tokenpos, "^");
    while (i<currentLine->toklen) {
      putchar('^');
      i++;
    }
    putchar('\n');
  }
  printf("%s:%d:%s\n", filename, currentLine->actualline, s);
}

static void poperrorskip (char *s)
{
  int nextpos =currentLine->nextpos;
  int tokenpos=currentLine->tokenpos;

  popLine();
  m2lex_M2Error(s);
  if (currentLine != NULL) {
    currentLine->nextpos  = nextpos;
    currentLine->tokenpos = tokenpos;
  }
}

#if 0
/*
 *  setFileName - assigns filename to s.
 */

void setFileName (char *s)
{
  if (filename != NULL) {
    free(filename);
  }
  filename = (char *)strdup(s);
}
#endif

/*
 *  consumeLine - reads a line into a buffer, it then pushes back the whole
 *                line except the initial \n.
 */

static void consumeLine (void)
{
  if (currentLine->linelen<yyleng) {
    if (currentLine->linebuf != NULL) {
      free(currentLine->linebuf);
    }
    currentLine->linebuf = (char *)malloc(yyleng);
    if (currentLine->linebuf == NULL)
      perror("malloc");
    currentLine->linelen = yyleng;
  }
  strcpy(currentLine->linebuf, yytext+1);  /* copy all except the initial \n */
  lineno++;
  currentLine->actualline = lineno;
  currentLine->tokenpos=0;
  currentLine->nextpos=0;
  yyless(1);                  /* push back all but the \n */
}

/*
 *  updatepos - updates the current token position.
 *              Should be used when a rule matches a token.
 */

static void updatepos (void)
{
  currentLine->nextpos = currentLine->tokenpos+yyleng;
  currentLine->toklen  = yyleng;
}

/*
 *  skippos - skips over this token. This function should be called
 *            if we are not returning and thus not calling getToken.
 */

static void skippos (void)
{
  currentLine->tokenpos = currentLine->nextpos;
}

/*
 *  initLine - initializes a currentLine
 */

static void initLine (void)
{
  currentLine = (struct lineInfo *)malloc(sizeof(struct lineInfo));

  if (currentLine == NULL)
    perror("malloc");
  currentLine->linebuf    = NULL;
  currentLine->linelen    = 0;
  currentLine->tokenpos   = 0;
  currentLine->toklen     = 0;
  currentLine->nextpos    = 0;
  currentLine->actualline = lineno;
  currentLine->inuse      = TRUE;
  currentLine->next       = NULL;
}

/*
 *  pushLine - pushes a new line structure.
 */

static void pushLine (void)
{
  if (currentLine == NULL) {
    initLine();
  } else if (currentLine->inuse) {
      struct lineInfo *l = (struct lineInfo *)malloc(sizeof(struct lineInfo));

      if (l == NULL)
	perror("malloc");
      if (currentLine->linebuf == NULL) {
	l->linebuf  = NULL;
	l->linelen  = 0;
      } else {
	l->linebuf    = (char *)strdup(currentLine->linebuf);
	l->linelen    = strlen(l->linebuf)+1;
      }
      l->tokenpos   = currentLine->tokenpos;
      l->toklen     = currentLine->toklen;
      l->nextpos    = currentLine->nextpos;
      l->actualline = currentLine->actualline;
      l->next       = currentLine;
      currentLine   = l;
  }
  currentLine->inuse = TRUE;
}

/*
 *  popLine - pops a line structure.
 */

static void popLine (void)
{
  if (currentLine != NULL) {
    struct lineInfo *l = currentLine;

    if (currentLine->linebuf != NULL)
      free(currentLine->linebuf);
    currentLine = l->next;
    free(l);
  }
}

/*
 *  resetpos - resets the position of the next token to the start of the line.
 */

static void resetpos (void)
{
  if (currentLine != NULL)
    currentLine->nextpos = 0;
}

/*
 *  finishedLine - indicates that the current line does not need to be preserved when a pushLine
 *                 occurs.
 */

static void finishedLine (void)
{
  currentLine->inuse = FALSE;
}

/*
 *  m2lex_GetToken - returns a new token.
 */

char *m2lex_GetToken (void)
{
  if (currentLine == NULL)
    initLine();
  currentLine->tokenpos = currentLine->nextpos;
  yylex();
  return( yytext );
}

/*
 *  CloseSource - provided for semantic sugar
 */

void m2lex_CloseSource (void)
{
}

/*
 *  OpenSource - returns TRUE if file, s, can be opened and
 *               all tokens are taken from this file.
 */

int m2lex_OpenSource (char *s)
{
  FILE *f = fopen(s, "r");

#if 0
  /* remove any pending line buffers from the last time we opened a file */
  while (currentLine != NULL)
    popLine();
#endif

  if (f == NULL)
    return( FALSE );
  else {
    yy_delete_buffer(YY_CURRENT_BUFFER);
    yy_switch_to_buffer(yy_create_buffer(f, YY_BUF_SIZE));
    if (filename)
      free(filename);
    filename = strdup(s);
    lineno   =1;
    return( TRUE );
  }
}

/*
 *  m2lex_GetLineNo - returns the current line number.
 */

int m2lex_GetLineNo (void)
{
  if (currentLine != NULL)
    return currentLine->actualline;
  else
    return 0;
}

/*
 *  yywrap is called when end of file is seen. We push an eof token
 *         and tell the lexical analysis to stop.
 */

int yywrap (void)
{
  updatepos(); M2LexBuf_AddTok(M2Reserved_eoftok); return 1;
}

void _M2_m2lex_init () {}
