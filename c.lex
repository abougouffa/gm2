%{
/* Copyright (C) 2003 Free Software Foundation, Inc.
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
#include "CLexBuf.h"

  /*
   *  c.lex - provides a lexical analyser for C used by h2def
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
  static struct lineInfo *currentLine =NULL;
  static int              hash        =FALSE;
  static int              parsingOn   =TRUE;
  static int              level       =0;

        void clex_CError (const char *);
static  void pushLine     (void);
static  void popLine      (void);
static  void finishedLine (void);
static  void resetpos     (void);
static  void consumeLine  (int n);
static  void updatepos    (void);
static  void skippos      (void);
static  void skipline     (void);
static  void poperrorskip (const char *);
static  void checkEndHash (void);
static  void handleNewline(int hashSeen, int n);

#if !defined(TRUE)
#    define TRUE  (1==1)
#endif
#if !defined(FALSE)
#    define FALSE (1==0)
#endif

#define YY_DECL   void yylex (void)


#define YY_INPUT(buf,result,max_size) \
{\
   int c = fgetc(yyin);\
\
   while ((char)c == '\\') {\
      c = fgetc(yyin);\
      if (c == '\n')\
         c = fgetc(yyin);\
      else {\
         ungetc(c, yyin);\
         c = '\\';\
      }\
   }\
   result = (c == EOF) ? YY_NULL : (buf[0] = c, 1); \
}

%}

%x COMMENT LINE0 LINE1 LINE2 SUPPRESS

%%

"/*"                       { updatepos();
                             pushLine(); skippos(); 
			     BEGIN COMMENT; }
<COMMENT>"*/"              { updatepos(); skippos(); BEGIN INITIAL; finishedLine(); }
<COMMENT>\n.*              { consumeLine(1); }
<COMMENT>.                 { updatepos(); skippos(); }
<COMMENT><<EOF>>           { updatepos(); clex_CError("end of file found inside a comment"); exit(0); return; }

<SUPPRESS>\n#[ \t]*if      { level++; }
<SUPPRESS>\n#[ \t]*ifdef   { level++; }
<SUPPRESS>\n#[ \t]*endif   { level--;
                             if (level == 0) {
			        CLexBuf_AddTok(CLexBuf_endiftok);
                                BEGIN INITIAL;
				return;
                             }
                           }
<SUPPRESS>\n#[ \t]*else    { if (level == 1) {
			        CLexBuf_AddTok(CLexBuf_elsetok);
                                BEGIN INITIAL;
				return;
                             }
                           }
<SUPPRESS>\n               { }
<SUPPRESS>.*               { }
<SUPPRESS><<EOF>>          { CLexBuf_AddTok(CLexBuf_eoftok); return; }

\n\#.*                     { handleNewline(TRUE, 1); }
\n.*                       { handleNewline(FALSE, 1); }
^#.*                       { consumeLine(0); hash=TRUE; BEGIN LINE0; }
<LINE0>\#                  { updatepos(); CLexBuf_AddTok(CLexBuf_starthashtok); return; }
<LINE0>[ \t]*              { currentLine->tokenpos += yyleng; return; }
<LINE0>define              { updatepos(); CLexBuf_AddTok(CLexBuf_definetok); BEGIN INITIAL; return; }
<LINE0>undef               { updatepos(); CLexBuf_AddTok(CLexBuf_undeftok); BEGIN INITIAL; return; }
<LINE0>include             { updatepos(); CLexBuf_AddTok(CLexBuf_includetok); BEGIN INITIAL; return; }
<LINE0>if                  { updatepos(); CLexBuf_AddTok(CLexBuf_iftok); BEGIN INITIAL; return; }
<LINE0>else                { updatepos(); CLexBuf_AddTok(CLexBuf_elsetok); BEGIN INITIAL; return; }
<LINE0>endif               { updatepos(); CLexBuf_AddTok(CLexBuf_endiftok); BEGIN INITIAL; return; }
<LINE0>ifdef               { updatepos(); CLexBuf_AddTok(CLexBuf_ifdeftok); BEGIN INITIAL; return; }
<LINE0>ifndef              { updatepos(); CLexBuf_AddTok(CLexBuf_ifndeftok); BEGIN INITIAL; return; }
<LINE0>[0-9]+[ \t]*\"      { updatepos(); lineno=atoi(yytext)-1; BEGIN LINE1; }
<LINE0>\n                  { clex_CError("missing initial quote after #line directive"); resetpos(); BEGIN INITIAL; }
<LINE0>[^\n]
<LINE0><<EOF>>             { updatepos(); CLexBuf_AddTok(CLexBuf_eoftok); return; }
<LINE1>[^\"\n]+            { clex_CError("missing final quote after #line directive"); resetpos(); BEGIN INITIAL; }
<LINE1>.*\"                { updatepos();
                             filename = (char *)xrealloc(filename, yyleng+1);
			     strcpy(filename, yytext);
                             filename[yyleng-1] = (char)0;  /* remove trailing quote */
                             BEGIN LINE2;
                           }
<LINE1><<EOF>>             { updatepos(); CLexBuf_AddTok(CLexBuf_eoftok); return; }
<LINE2>[ \t]*              { updatepos(); }
<LINE2>\n                  { /* CLexBuf_SetFile(filename); */ updatepos(); BEGIN INITIAL; }
<LINE2>2[ \t]*\n           { /* CLexBuf_PopFile(filename); */ updatepos(); BEGIN INITIAL; }
<LINE2>1[ \t]*\n           { /* CLexBuf_PushFile(filename); */ updatepos(); BEGIN INITIAL; }
<LINE2><<EOF>>             { updatepos(); CLexBuf_AddTok(CLexBuf_eoftok); return; }

\"[^\"\n]*\"               { updatepos(); CLexBuf_AddTokCharStar(CLexBuf_conststringtok, yytext); return; }
\"[^\"\n]*$                { updatepos();
                             clex_CError("missing terminating quote, \"");
                             resetpos(); return;
                           }

'[^'\n]*'                  { updatepos(); CLexBuf_AddTokCharStar(CLexBuf_constchartok, yytext); return; }
'[^'\n]*$                  { updatepos();
                             clex_CError("missing terminating quote, '");
                             resetpos(); return;
                           }

\#.*                       { updatepos(); consumeLine(1); BEGIN LINE0; }
"*"                        { updatepos(); CLexBuf_AddTok(CLexBuf_startok); return; }
->                         { updatepos(); CLexBuf_AddTok(CLexBuf_arrowtok); return; }
struct                     { updatepos(); CLexBuf_AddTok(CLexBuf_structtok); return; }
\[                         { updatepos(); CLexBuf_AddTok(CLexBuf_lsbratok); return; }
\]                         { updatepos(); CLexBuf_AddTok(CLexBuf_rsbratok); return; }
\{                         { updatepos(); CLexBuf_AddTok(CLexBuf_lcbratok); return; }
\}                         { updatepos(); CLexBuf_AddTok(CLexBuf_rcbratok); return; }
\(                         { updatepos(); CLexBuf_AddTok(CLexBuf_lparatok); return; }
\)                         { updatepos(); CLexBuf_AddTok(CLexBuf_rparatok); return; }
\;                         { updatepos(); CLexBuf_AddTok(CLexBuf_semicolontok); return; }
\.\.\.                     { updatepos(); CLexBuf_AddTok(CLexBuf_periodperiodperiodtok); return; }
\<                         { updatepos(); CLexBuf_AddTok(CLexBuf_lesstok); return; }
\>                         { updatepos(); CLexBuf_AddTok(CLexBuf_gretok); return; }
\,                         { updatepos(); CLexBuf_AddTok(CLexBuf_commatok); return; }
\.                         { updatepos(); CLexBuf_AddTok(CLexBuf_periodtok); return; }
\/                         { updatepos(); CLexBuf_AddTok(CLexBuf_divtok); return; }
\|\|                       { updatepos(); CLexBuf_AddTok(CLexBuf_ortok); return; }
\&\&                       { updatepos(); CLexBuf_AddTok(CLexBuf_andtok); return; }
\|                         { updatepos(); CLexBuf_AddTok(CLexBuf_bartok); return; }
\&                         { updatepos(); CLexBuf_AddTok(CLexBuf_ambersandtok); return; }
"<<"                       { updatepos(); CLexBuf_AddTok(CLexBuf_shiftlefttok); return; }
">>"                       { updatepos(); CLexBuf_AddTok(CLexBuf_shiftrighttok); return; }
"/"                        { updatepos(); CLexBuf_AddTok(CLexBuf_divtok); return; }
"%"                        { updatepos(); CLexBuf_AddTok(CLexBuf_modtok); return; }
sizeof                     { updatepos(); CLexBuf_AddTok(CLexBuf_sizeoftok); return; }
defined                    { updatepos(); CLexBuf_AddTok(CLexBuf_definedtok); return; }
\^                         { updatepos(); CLexBuf_AddTok(CLexBuf_hattok); return; }
\!                         { updatepos(); CLexBuf_AddTok(CLexBuf_nottok); return; }
"=="                       { updatepos(); CLexBuf_AddTok(CLexBuf_equaltok); return; }
"!="                       { updatepos(); CLexBuf_AddTok(CLexBuf_notequaltok); return; }
">="                       { updatepos(); CLexBuf_AddTok(CLexBuf_greequaltok); return; }
"<="                       { updatepos(); CLexBuf_AddTok(CLexBuf_lessequaltok); return; }
\+                         { updatepos(); CLexBuf_AddTok(CLexBuf_plustok); return; }
\-                         { updatepos(); CLexBuf_AddTok(CLexBuf_minustok); return; }
\~                         { updatepos(); CLexBuf_AddTok(CLexBuf_tildetok); return; }
long                       { updatepos(); CLexBuf_AddTok(CLexBuf_longtok); return; }
int                        { updatepos(); CLexBuf_AddTok(CLexBuf_inttok); return; }
char                       { updatepos(); CLexBuf_AddTok(CLexBuf_chartok); return; }
enum                       { updatepos(); CLexBuf_AddTok(CLexBuf_enumtok); return; }
typedef                    { updatepos(); CLexBuf_AddTok(CLexBuf_typedeftok); return; }
float                      { updatepos(); CLexBuf_AddTok(CLexBuf_floattok); return; }
double                     { updatepos(); CLexBuf_AddTok(CLexBuf_doubletok); return; }
unsigned                   { updatepos(); CLexBuf_AddTok(CLexBuf_unsignedtok); return; }
const                      { updatepos(); CLexBuf_AddTok(CLexBuf_consttok); return; }
if                         { updatepos(); CLexBuf_AddTok(CLexBuf_codetok); return; }
else                       { updatepos(); CLexBuf_AddTok(CLexBuf_codetok); return; }
while                      { updatepos(); CLexBuf_AddTok(CLexBuf_codetok); return; }
for                        { updatepos(); CLexBuf_AddTok(CLexBuf_codetok); return; }
do                         { updatepos(); CLexBuf_AddTok(CLexBuf_codetok); return; }
break                      { updatepos(); CLexBuf_AddTok(CLexBuf_codetok); return; }
case                       { updatepos(); CLexBuf_AddTok(CLexBuf_codetok); return; }
switch                     { updatepos(); CLexBuf_AddTok(CLexBuf_codetok); return; }
extern                     { updatepos(); CLexBuf_AddTok(CLexBuf_externtok); return; }
static                     { updatepos(); CLexBuf_AddTok(CLexBuf_statictok); return; }
auto                       { updatepos(); CLexBuf_AddTok(CLexBuf_autotok); return; }
register                   { updatepos(); CLexBuf_AddTok(CLexBuf_registertok); return; }
void                       { updatepos(); CLexBuf_AddTok(CLexBuf_voidtok); return; }
short                      { updatepos(); CLexBuf_AddTok(CLexBuf_shorttok); return; }
signed                     { updatepos(); CLexBuf_AddTok(CLexBuf_signedtok); return; }
union                      { updatepos(); CLexBuf_AddTok(CLexBuf_uniontok); return; }
\:                         { updatepos(); CLexBuf_AddTok(CLexBuf_colontok); return; }
\=                         { updatepos(); CLexBuf_AddTok(CLexBuf_becomestok); return; }
volatile                   { updatepos(); CLexBuf_AddTok(CLexBuf_volatiletok); return; }

-?(([0-9]*\.[0-9]+)([eE][-+]?[0-9]+)?) { updatepos(); CLexBuf_AddTokCharStar(CLexBuf_realtok, yytext); return; }
[a-zA-Z_][a-zA-Z0-9_]*     { updatepos(); CLexBuf_AddTokCharStar(CLexBuf_identtok, yytext); return; }
0[0-9]+                    { updatepos(); CLexBuf_AddTokCharStar(CLexBuf_octintegertok, yytext); return; }
[0-9]+                     { updatepos(); CLexBuf_AddTokCharStar(CLexBuf_integertok, yytext); return; }
0x[0-9A-Fa-f]+             { updatepos(); CLexBuf_AddTokCharStar(CLexBuf_hexintegertok, yytext); return; }
[\t ]+                     { currentLine->tokenpos += yyleng;  /* ignore whitespace */; }
<<EOF>>                    { updatepos(); CLexBuf_AddTok(CLexBuf_eoftok); return; }
.                          { updatepos(); clex_CError("unrecognised symbol"); skippos(); }

%%

/*
 *  handleNewline - 
 */

static void handleNewline (int hashSeen, int n)
{
  checkEndHash();
  consumeLine(n);
  hash=hashSeen;
  if (hashSeen)
    BEGIN LINE0;
  else
    BEGIN INITIAL;
}

/*
 *  checkEndHash - adds an endhash token if we are processing a cpp #... and have reached
 *                 a newline
 */

static void checkEndHash (void)
{
  if (hash)
    CLexBuf_AddTok(CLexBuf_endhashtok);
  hash = FALSE;
}

/*
 *  ParsingOn - if t is FALSE then the lexical analysis will
 *              consume all lines except when a line is one of
 *              '#endif' or '#else' or '#if' or '#ifdef'
 */

void clex_ParsingOn (int t)
{
  parsingOn = t;
  if (! parsingOn) {
    level = 1;
    BEGIN SUPPRESS;
  }
}

/*
 *  clex_CError - displays the error message, s, after the code line and pointer
 *                 to the erroneous token.
 */

void clex_CError (const char *s)
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

static void poperrorskip (const char *s)
{
  int nextpos =currentLine->nextpos;
  int tokenpos=currentLine->tokenpos;

  popLine();
  clex_CError(s);
  if (currentLine != NULL) {
    currentLine->nextpos  = nextpos;
    currentLine->tokenpos = tokenpos;
  }
}

/*
 *  consumeLine - reads a line into a buffer, it then pushes back the whole
 *                line except the initial n characters.
 */

static void consumeLine (int n)
{
  int i;

  if (currentLine->linelen<yyleng) {
    currentLine->linebuf = (char *)xrealloc(currentLine->linebuf, yyleng);
    currentLine->linelen = yyleng;
  }
  strcpy(currentLine->linebuf, yytext+n);  /* copy all except the initial n */
  lineno++;
  currentLine->actualline = lineno;
  currentLine->tokenpos=0;
  currentLine->nextpos=0;
  if (parsingOn || (currentLine->linebuf[0] == '#'))
    yyless(n);                  /* push back all but the n */
    
  /* translate \t onto ' ' */
  for (i=0; i<yyleng; i++)
    if (currentLine->linebuf[i] == '\t')
      currentLine->linebuf[i] = ' ';
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
 *  skipline - skips over this line.
 */

static void skipline (void)
{
  currentLine->nextpos  = currentLine->linelen;
  currentLine->tokenpos = currentLine->linelen;
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
  currentLine = (struct lineInfo *)xmalloc(sizeof(struct lineInfo));

  if (currentLine == NULL)
    perror("xmalloc");
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
      struct lineInfo *l = (struct lineInfo *)xmalloc(sizeof(struct lineInfo));

      if (currentLine->linebuf == NULL) {
	l->linebuf  = NULL;
	l->linelen  = 0;
      } else {
	l->linebuf    = (char *)xstrdup(currentLine->linebuf);
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
 *  clex_GetToken - returns a new token.
 */

char *clex_GetToken (void)
{
  if (currentLine == NULL)
    initLine();
  currentLine->tokenpos = currentLine->nextpos;
  yylex();
#if 0
  clex_CError("testing token");
#endif
  return yytext;
}

/*
 *  CloseSource - provided for semantic sugar
 */

void clex_CloseSource (void)
{
}

/*
 *  OpenSource - returns TRUE if file, s, can be opened and
 *               all tokens are taken from this file.
 */

int clex_OpenSource (char *s)
{
  FILE *f = fopen(s, "r");

  if (f == NULL)
    return( FALSE );
  else {
    yy_delete_buffer(YY_CURRENT_BUFFER);
    yy_switch_to_buffer(yy_create_buffer(f, YY_BUF_SIZE));
    filename = xstrdup(s);
    lineno   =1;
    return TRUE;
  }
}

/*
 *  clex_GetLineNo - returns the current line number.
 */

int clex_GetLineNo (void)
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
  updatepos(); CLexBuf_AddTok(CLexBuf_eoftok); return 1;
}

void _M2_clex_init () {}

#if 0
main()
{
  char *t;

  if (clex_OpenSource("/usr/include/vga.h")) {
    do {
      t=clex_GetToken();

      if (t != NULL)
	clex_CError(t); printf("\n");
    } while (t != NULL);
    clex_CloseSource();
  }
}
#endif
