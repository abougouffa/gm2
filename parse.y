/* Grammer file for compiler for toy language.  */

%{
#include "config.h"
#include "tree.h"

extern void build_function	PROTO((tree, tree));
extern tree build_function_decl	PROTO((char, int));
extern tree get_arg_decl	PROTO((int));
%}

%union {
tree exp;			/* Tree node representing value. */
int ival;			/* Integer value for constant or arg */
char character;			/* Name of function */
}

%token <ival> NUM		/* Decimal constant. */
%token <character> NAME		/* Function name.  */

%left '-' '+'
%left '*' '/'

%type <exp> exp fndef

%%

input: /* empty */
  | input function
  | error ;

function: fndef ':' exp ';' { build_function ($1, $3); } ;

fndef: NAME '(' NUM ')' { $$ = build_function_decl ($1, $3); } ;

exp:  NUM	    { $$ = build_int_2 ($1, $1 >= 0 ? 0 : -1); }
     | '$' NUM      { $$ = get_arg_decl ($2); }
     | exp '+' exp  { $$ = build (PLUS_EXPR, integer_type_node, $1, $3); }
     | exp '-' exp  { $$ = build (MINUS_EXPR, integer_type_node, $1, $3); }
     | exp '*' exp  { $$ = build (MULT_EXPR, integer_type_node, $1, $3); }
     | exp '/' exp  { $$ = build (TRUNC_DIV_EXPR, integer_type_node, $1, $3); }
     | '(' exp ')'  { $$ = $2; }
     | error        { $$ = error_mark_node; } ;

%%

#include <stdio.h>
#include <ctype.h>

extern FILE *finput;

int
yylex ()
{
  int c;

  /* Ignore whitespace, get first character.  */
  while (isspace (c = getc (finput)))
    ;

  /* If a digit, get the number.  */
  if (isdigit (c))
    {
      ungetc (c, finput);
      fscanf (finput, "%i", &yylval.ival);
      return NUM;
    }

  /* Otherwise return the letter to use as the function name.  */
  else if (isalpha (c))
    {
      yylval.character = c;
      return NAME;
    }

  return c;
}
