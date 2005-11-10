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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include <p2c/p2c.h>
#include "GCLexBuf.h"

#define MAX_INCLUDE_DEPTH 50

  /*
   *  c.lex - provides a lexical analyser for C used by h2def
   */

  struct lineInfo {
    char            *filename;         /* current files */
    char            *linebuf;          /* line contents */
    int              linelen;          /* length */
    int              tokenpos;         /* start position of token within line */
    int              toklen;           /* a copy of yylen (length of token) */
    int              nextpos;          /* position after token */
    int              actualline;       /* line number of this line */
    YY_BUFFER_STATE  lex_buffer;       /* the flex buffer for this file */
    struct lineInfo *next;
  };

  struct typeDef {
    char           *name;
    struct typeDef *next;
  };

  static struct lineInfo *currentLine =NULL;
  static int              hash        =FALSE;
  static int              parsingOn   =TRUE;
  static int              level       =0;
  static char            *searchPath  =NULL;
  static int              includeNo   =0;
  static struct typeDef  *listOfTypes =NULL;


        void cflex_ParsingOn (int t);
	char *cflex_GetToken (void);
	void cflex_CloseSource (void);
	int cflex_OpenSource (char *s);
	int cflex_GetLineNo (void);
	void cflex_SetSearchPath (char *newPath);
	void cflex_AddTypeDef (char *a);
        void cflex_CError (const char *);
static  void pushLine     (void);
static  void popLine      (void);
static  void resetpos     (void);
static  void consumeLine  (int n);
static  void updatepos    (void);
static  void skippos      (void);
static  void checkEndHash (void);
static  void handleNewline(int hashSeen, int n);
static  int  handleEof    (void);
static  char *findFile    (char *, int);
static  int  isTypeDef    (char *a);

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

%x COMMENT LINE0 LINE1 LINE2 SUPPRESS INCLUDE DEFINE

%%

"/*"                       { updatepos();
                             skippos(); 
			     BEGIN COMMENT; }
<COMMENT>"*/"              { updatepos(); skippos(); BEGIN INITIAL; }
<COMMENT>\n.*              { consumeLine(1); }
<COMMENT>.                 { updatepos(); skippos(); }
<COMMENT><<EOF>>           { handleEof();
                             if (includeNo == 0)
			       cflex_CError("end of file found inside a comment"); exit(0);
                           }

<SUPPRESS>\n#[ \t]*if      { level++; }
<SUPPRESS>\n#[ \t]*ifdef   { level++; }
<SUPPRESS>\n#[ \t]*endif   { level--;
                             if (level == 0) {
                                CLexBuf_AddTok(CLexBuf_starthashtok);
			        CLexBuf_AddTok(CLexBuf_endiftok);
                                CLexBuf_AddTok(CLexBuf_endhashtok);
				hash = FALSE;
                                BEGIN INITIAL;
				return;
                             }
                           }
<SUPPRESS>\n#[ \t]*else    { if (level == 1) {
                                CLexBuf_AddTok(CLexBuf_starthashtok);
			        CLexBuf_AddTok(CLexBuf_elsetok);
                                CLexBuf_AddTok(CLexBuf_endhashtok);
				hash = FALSE;
                                BEGIN INITIAL;
				return;
                             }
                           }
<SUPPRESS>\n               { }
<SUPPRESS>.*               { }
<SUPPRESS><<EOF>>          {
                             if (includeNo == 0) {
                               CLexBuf_AddTok(CLexBuf_eoftok);
			       return;
			     }
                           }

\n\#.*                     { handleNewline(TRUE, 1); }
\n.*                       { handleNewline(FALSE, 1); }
^#.*                       { consumeLine(0); hash=TRUE; BEGIN LINE0; }
<LINE0>\#                  { updatepos(); CLexBuf_AddTok(CLexBuf_starthashtok); return; }
<LINE0>[ \t]*              { currentLine->tokenpos += yyleng; return; }
<LINE0>define              { updatepos(); CLexBuf_AddTok(CLexBuf_definetok); BEGIN DEFINE; return; }
<LINE0>undef               { updatepos(); CLexBuf_AddTok(CLexBuf_undeftok); BEGIN INITIAL; return; }
<LINE0>include             { updatepos(); CLexBuf_AddTok(CLexBuf_includetok); BEGIN INCLUDE; return; }
<LINE0>if                  { updatepos(); CLexBuf_AddTok(CLexBuf_iftok); BEGIN INITIAL; return; }
<LINE0>else                { updatepos(); CLexBuf_AddTok(CLexBuf_elsetok); BEGIN INITIAL; return; }
<LINE0>endif               { updatepos(); CLexBuf_AddTok(CLexBuf_endiftok); BEGIN INITIAL; return; }
<LINE0>ifdef               { updatepos(); CLexBuf_AddTok(CLexBuf_ifdeftok); BEGIN INITIAL; return; }
<LINE0>ifndef              { updatepos(); CLexBuf_AddTok(CLexBuf_ifndeftok); BEGIN INITIAL; return; }
<LINE0>[0-9]+[ \t]*\"      { updatepos(); currentLine->actualline=atoi(yytext)-1; BEGIN LINE1; }
<LINE0>\n                  { cflex_CError("missing initial quote after #line directive"); resetpos(); BEGIN INITIAL; }
<LINE0>[^\n]
<LINE0><<EOF>>             { if (! handleEof()) return; }
<LINE1>[^\"\n]+            { cflex_CError("missing final quote after #line directive"); resetpos(); BEGIN INITIAL; }
<LINE1>.*\"                { updatepos();
                             /* filename = (char *)xrealloc(filename, yyleng+1);
			     strcpy(filename, yytext);
                             filename[yyleng-1] = (char)0; */  /* remove trailing quote */
                             BEGIN LINE2;
                           }
<LINE1><<EOF>>             { if (! handleEof()) return; }
<LINE2>[ \t]*              { updatepos(); }
<LINE2>\n                  { /* CLexBuf_SetFile(filename); */ updatepos(); BEGIN INITIAL; }
<LINE2>2[ \t]*\n           { /* CLexBuf_PopFile(filename); */ updatepos(); BEGIN INITIAL; }
<LINE2>1[ \t]*\n           { /* CLexBuf_PushFile(filename); */ updatepos(); BEGIN INITIAL; }
<LINE2><<EOF>>             { if (! handleEof()) return; }

<INCLUDE>[ \\t]*           { updatepos(); } /* eat the whitespace */
<INCLUDE>[^ \t\n]+         { /* got the include file name */
                              if (includeNo >= MAX_INCLUDE_DEPTH) {
				fprintf( stderr, "too many nested include statements" );
				exit(1);
			      }
                              includeNo++;
                              pushLine();
                              currentLine->actualline = 0;
			      currentLine->lex_buffer = YY_CURRENT_BUFFER;
			      if (strlen (yytext)>1) {
				char *fileName   = xstrdup (yytext+1);
				int   localFirst = (fileName[strlen(fileName)-1] == '"');
				char *actualPath;

				fileName[strlen(fileName)-1] = (char)0;
				/* printf("searching for %s\n", fileName); */
				actualPath = findFile (fileName, localFirst);
				if (actualPath == NULL) {
				  cflex_CError ("include file not found");
				  popLine();
				} else {
				  yyin = fopen (actualPath, "r");
				  if (yyin) {
				    yy_switch_to_buffer (yy_create_buffer (yyin, YY_BUF_SIZE) );
				    currentLine->filename = actualPath;
				  } else {
				    perror("failed to open include file");
				    popLine();
				  }
				}
			      }
			      BEGIN(INITIAL);
			      checkEndHash();
                           }
<DEFINE>[ \\t]*            { updatepos(); }
<DEFINE>[^ \t\n]+          { updatepos(); CLexBuf_AddTokCharStar(CLexBuf_identtok, yytext); BEGIN(INITIAL); return; }

\"[^\"\n]*\"               { updatepos(); CLexBuf_AddTokCharStar (CLexBuf_conststringtok, yytext); return; }
\"[^\"\n]*$                { updatepos();
                             cflex_CError("missing terminating quote, \"");
                             resetpos(); return;
                           }

'[^'\n]*'                  { updatepos(); CLexBuf_AddTokCharStar(CLexBuf_constchartok, yytext); return; }
'[^'\n]*$                  { updatepos();
                             cflex_CError("missing terminating quote, '");
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
[a-zA-Z_][a-zA-Z0-9_]*     { updatepos();
                             if (isTypeDef (yytext))
                                CLexBuf_AddTokCharStar(CLexBuf_typetok, yytext);
                             else
                                CLexBuf_AddTokCharStar(CLexBuf_identtok, yytext);
                             return;
                           }
0[0-9]+                    { updatepos(); CLexBuf_AddTokCharStar(CLexBuf_octintegertok, yytext); return; }
[0-9]+                     { updatepos(); CLexBuf_AddTokCharStar(CLexBuf_integertok, yytext); return; }
[0-9]+L                    { /* long int */ updatepos(); CLexBuf_AddTokCharStar(CLexBuf_integertok, yytext); return; }
0x[0-9A-Fa-f]+             { updatepos(); CLexBuf_AddTokCharStar(CLexBuf_hexintegertok, yytext); return; }
[\t\r ]+                   { currentLine->tokenpos += yyleng;  /* ignore whitespace */; }
<<EOF>>                    { if (! handleEof()) return; }
.                          { updatepos(); cflex_CError("unrecognised symbol"); skippos(); }

%%

/*
 *  handleEof - unwinds the last nested include file or present,
 *              otherwise it adds an eof token and returns.
 */

static int handleEof (void)
{
  includeNo--;
  if (includeNo < 0) {
    if (includeNo == -1) {
       updatepos();
       CLexBuf_AddTok(CLexBuf_eoftok);
       return TRUE;
    }
  } else {
    yy_delete_buffer (YY_CURRENT_BUFFER);
    yy_switch_to_buffer (currentLine->lex_buffer);
    popLine();
  }
  return FALSE;
}

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

void cflex_ParsingOn (int t)
{
  parsingOn = t;
  if (! parsingOn) {
    level = 1;
    BEGIN SUPPRESS;
    CLexBuf_FlushTokens();
  }
}

/*
 *  cflex_CError - displays the error message, s, after the code line and pointer
 *                 to the erroneous token.
 */

void cflex_CError (const char *s)
{
  struct lineInfo *l;

  if (currentLine->linebuf != NULL) {
    int i=1;

    printf("%s:%d:%s\n", currentLine->filename, currentLine->actualline, currentLine->linebuf);
    printf("%s:%d:%*s", currentLine->filename, currentLine->actualline, 1+currentLine->tokenpos, "^");
    while (i<currentLine->toklen) {
      putchar('^');
      i++;
    }
    putchar('\n');
  }
  printf("%s:%d:%s\n", currentLine->filename, currentLine->actualline, s);
  l = currentLine->next;
  while (l != NULL) {
    printf("%s:%d:was included\n", l->filename, l->actualline);
    l = l->next;
  }
}

#if 0
static void poperrorskip (const char *s)
{
  int nextpos =currentLine->nextpos;
  int tokenpos=currentLine->tokenpos;

  popLine();
  cflex_CError(s);
  if (currentLine != NULL) {
    currentLine->nextpos  = nextpos;
    currentLine->tokenpos = tokenpos;
  }
}

/*
 *  skipline - skips over this line.
 */

static void skipline (void)
{
  currentLine->nextpos  = currentLine->linelen;
  currentLine->tokenpos = currentLine->linelen;
}
#endif

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
  currentLine->actualline++;
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
  currentLine->actualline = 0;
  currentLine->next       = NULL;
  currentLine->filename   = NULL;
}

/*
 *  pushLine - pushes a new line structure.
 */

static void pushLine (void)
{
  if (currentLine == NULL)
    initLine();
  else {
    struct lineInfo *l = (struct lineInfo *)xmalloc(sizeof(struct lineInfo));

    if (currentLine->linebuf == NULL) {
      l->linebuf  = NULL;
      l->linelen  = 0;
    } else {
      l->linebuf    = (char *)xstrdup (currentLine->linebuf);
      l->linelen    = strlen(l->linebuf)+1;
    }
    l->tokenpos   = currentLine->tokenpos;
    l->toklen     = currentLine->toklen;
    l->nextpos    = currentLine->nextpos;
    l->actualline = currentLine->actualline;
    l->filename   = currentLine->filename;
    l->next       = currentLine;
    currentLine   = l;
  }
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
 *  cflex_GetToken - returns a new token.
 */

char *cflex_GetToken (void)
{
  if (currentLine == NULL)
    initLine();
  currentLine->tokenpos = currentLine->nextpos;
  yylex();
#if 0
  cflex_CError("testing token");
#endif
  return yytext;
}

/*
 *  CloseSource - provided for semantic sugar
 */

void cflex_CloseSource (void)
{
}

/*
 *  OpenSource - returns TRUE if file, s, can be opened and
 *               all tokens are taken from this file.
 */

int cflex_OpenSource (char *s)
{
  FILE *f = fopen(s, "r");

  if (f == NULL)
    return( FALSE );
  else {
    yy_delete_buffer(YY_CURRENT_BUFFER);
    yy_switch_to_buffer(yy_create_buffer(f, YY_BUF_SIZE));
    initLine();
    currentLine->filename = xstrdup (s);
    return TRUE;
  }
}

/*
 *  cflex_GetLineNo - returns the current line number.
 */

int cflex_GetLineNo (void)
{
  if (currentLine != NULL)
    return currentLine->actualline;
  else
    return 0;
}

/*
 *  SetSearchPath - reassigns the search path to newPath.
 */

void cflex_SetSearchPath (char *newPath)
{
  if (searchPath != NULL)
    free(searchPath);
  searchPath = xstrdup (newPath);
}

/*
 *  fileExists - returns TRUE if, filename, exists.
 */

static int fileExists (char *fileName)
{
  FILE *f = fopen(fileName, "r");

  if (f == NULL)
    return FALSE;
  fclose (f);
  return TRUE;
}

/*
 *  findFile - returns the path which contains, filename.
 *             NULL is returned if, filename, cannot be found.
 */

static char *findFile (char *fileName, int localFirst)
{
  char *start = searchPath;

  if (localFirst)
    if (fileExists (fileName))
      return fileName;

  while (start != NULL) {
    char *end = index (start, ':');
    
    if (end == NULL) {
      char *try = (char *)xmalloc (strlen(start) + 2 + strlen (fileName));
      strcpy (try, start);
      strcat(try, "/");
      strcat(try, fileName);
      if (fileExists (try))
	return try;
      free (try);
      
      start = NULL;
    }
    else {
      char *try = (char *)xmalloc (end-start + 2 + strlen (fileName));
      strncpy (try, start, end-start);
      try[end-start] = (char)0;
      strcat(try, "/");
      strcat(try, fileName);
      if (fileExists (try))
	return try;
      free (try);
      start = end+1;
    }
  }

  if (! localFirst)
    if (fileExists (fileName))
      return fileName;

  return NULL;
}

/*
 *  addTypeDef - adds the string, a, to the list of typedefs.
 */

static void addTypeDef (char *a)
{
  struct typeDef *t = (struct typeDef*)xmalloc (sizeof (struct typeDef));

  t->next = listOfTypes;
  t->name = xstrdup (a);
  listOfTypes = t;
}

/*
 *  isTypeDef - returns TRUE if, a, is already defined as a typedef.
 */

static int isTypeDef (char *a)
{
  struct typeDef *t = listOfTypes;

  while (t != NULL) {
    if (strcmp (a, t->name) == 0)
      return TRUE;
    t = t->next;
  }
  return FALSE;
}

/*
 *  AddTypeDef - adds the string, a, to the list of typedefs.
 */

void cflex_AddTypeDef (char *a)
{
  if (! isTypeDef (a))
    addTypeDef (a);
}

/*
 *  yywrap is called when end of file is seen. We push an eof token
 *         and tell the lexical analysis to stop.
 */

int yywrap (void)
{
  /* handleEof(); */
  return 1;
}


void _M2_cflex_init () {}

#if 0
main()
{
  char *t;

  if (cflex_OpenSource("/usr/include/vga.h")) {
    do {
      t=cflex_GetToken();

      if (t != NULL)
	cflex_CError(t); printf("\n");
    } while (t != NULL);
    cflex_CloseSource();
  }
}
#endif
