/* Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006
 * Free Software Foundation, Inc.
 * This file is part of GNU Modula-2.
 *
 * GNU Modula-2 is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * GNU Modula-2 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with GNU Modula-2; see the file COPYING.  If not, write to the
 * Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA. 
 */

/*
 * Gaius Mulley <gaius@glam.ac.uk> constructed this file.
 *
 * Title      : mklink
 * Description: creates startup code and link command line given a list of files.
 * Start date : 9/6/93
 *
 *
 * $Header: /sources/gm2/gm2/tools-src/mklink.c,v 1.6 2010/02/18 16:44:13 gaius Exp $
 *
 * $Log: mklink.c,v $
 * Revision 1.6  2010/02/18 16:44:13  gaius
 * fixed options
 *
 * Revision 1.5  2006/09/19 20:08:34  gaius
 * *** empty log message ***
 *
 * Revision 1.4  2006/01/11 00:04:44  gaius
 * added 2006 to all Copyright dates
 *
 * Revision 1.3  2005/11/21 21:50:38  gaius
 * * fixed many Copyright dates and GPL, LGPL and FDL license
 *   issues.
 * * modified gm2/ulm-lib-gm2/std/Storage.mod to use malloc and
 *   free. This in turn fixes a runtime regression test (hello world)
 *   now works with the Ulm libraries.
 * * fixed gm2/gm2.texi to include FDL notice and also fixed all
 *   included texi files in the same way.
 * * added GPL, Modula-2 and Copyright notices to all gm2/tools-src
 *   files.
 *
 * Revision 1.2  2003/04/29 15:27:28  gaius
 * many changes made which relate to the introduction of ISO SYSTEM:
 *
 * * examples/pthreads cleaned up
 * * introduced ISO SYSTEM. Limitations, TSIZE only takes one parameter,
 *   and SHIFT, ROTATE are not implemented yet.
 * * renamed gm2-libs/Strings to gm2-libs/DynamicStrings to avoid name
 *   clash with gm2-iso/Strings
 * * p2c modified to understand DEFINITION MODULE FOR "C"
 * * gm2-libs/libc.def modified to use DEFINITION MODULE FOR "C"
 * * gm2-iso/libc.def removed
 * * linking references to libc (in gm2/init/*init) removed
 * * gm2/tools-src/def2texi finished
 * * gm2/gm2-libs.texi built via gm2/tools-src/def2texi
 * * gm2/gm2.texi now contains library definition modules and index.
 * * added -Wiso switch to gm2 driver
 *
 * Revision 1.1  2002/04/17 14:03:44  gaius
 * added build files
 *
 * Revision 1.5  2000/10/26 00:04:12  anoncvs
 * intemediate checkin still working on getting gm2 working with egcs-20001016.
 * Altered the linking programs, mklink.c and gm2lgen.mod so that they generate
 * external prototypes for initialization function.
 *
 * Revision 1.4  2000/09/16 21:22:43  gaius
 * 2000-09-14	Matthias Kurz <mk@baerlap.north.de>
 *
 * 	* Many makefile portability corrections (tests for symbol links).
 * 	  Made makeversion more portable via guessing email address.
 * 	  Added setenv emulation in libc.c.
 * 	  Inserted missing Close(fo) to gm2lgen.mod.GenMain().
 * 	  Improved README,
 * 	  Added QUIAT to the makefiles.
 * 	  Cast EOF to (char) inside mkfor.c mklink.c.
 *
 * Revision 1.3  2000/08/24 17:37:11  gaius
 * fixed the linking and install rules
 *
 * Revision 1.2  1999/12/03 17:06:24  gaius
 * interim check of the sources. The gcc backend is underway - loads
 * of bugs and only partially done. Stage1 build and will compile
 * "tiny" fragments only.
 *
 * Revision 1.1.1.1  1996/05/28 10:13:09  gaius
 * Modula-2 compiler sources imported
 *
 *
 */

#define TRUE           (1==1)
#define FALSE          (1==0)
#define MAX_FILE_NAME  4096
#define MAXSTACK       100
#define STDIN            0
#define STDOUT           1
#define ENDOFILE       ((char) -1)
#define ERROR(X)       (fprintf(stderr, "%s:%d error %s\n", __FILE__, __LINE__, X) && \
			(fflush(stderr)))
#define DEBUG(X)       ((Debug) && (fprintf(stderr, "%s\n", X) && (fflush(stderr))))


#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>


typedef struct functlist {
  char             *functname;
  struct functlist *next;
} functList;


/* Prototypes */

static void ParseFileLinkCommand (void);
static void ParseFileStartup (void);
static void ParseFile (char *Name);
static void ParseComments (void);
static void CopyUntilEof (void) ;
static void CopyUntilEol (void) ;
static int  IsSym (char *s) ;
static int  SymIs (char *s) ;
static int  FindString (char *String) ;
static void GetNL (void) ;
static char GetChar (void) ;
static void ResetBuffer (void) ;
static int  GetSingleChar (char *ch) ;
static int  InRange (int Element, unsigned int Min, unsigned int Max) ;
static char PutChar (char ch) ;
static int  IsSpace (char ch) ;
static void SkipSpaces (void) ;
static void SkipText (void) ;
static void SilentSkipSpaces (void);
static void SilentSkipText (void);
static void PushBack (char *s) ;
static int  IsDigit (char ch) ;
static void GetName (char *Name) ;
static void OpenOutputFile (void) ;
static void CloseFile (void);
static void FindSource (char *Name);
static void CopyUntilEolInto (char *Buffer);
static void FindObject (char *Name);
static int  IsExists (char *Name);

/* Global variables */ 

static char      *NameOfMain        = "main";
static int        StackPtr          = 0;
static char       Stack[MAXSTACK];
static int        CurrentFile       = STDIN;
static int        OutputFile;
static int        CopyStdout;
static int        LinkCommandLine   = FALSE;
static int        ProfilePCommand   = FALSE;
static int        ProfilePGCommand  = FALSE;
static int        ExitNeeded        = TRUE;
static char      *P2CLibrary        = "../p2c/home/libp2c.a";
static functList *head              = NULL;
static functList *tail              = NULL;


main (int argc, char *argv[])
{
    int i;

    if (argc >= 3) {
	if (strcmp(argv[1], "-l") == 0) {
	    LinkCommandLine = TRUE;
	} else if (strcmp(argv[1], "-s") == 0) {
	    LinkCommandLine = FALSE;
	} else {
	    fprintf(stderr, "Usage: mklink (-l|-s) [-pg|-p] [-p2c p2clibrary] [-main name] [-exit] <modulelistfile>\n");
	    fprintf(stderr, "       must supply -l or -s option\n");
	    exit(1);
	}
	ProfilePCommand = FALSE;
	ProfilePGCommand = FALSE;
	i = 2;
	while (i<argc-1) {
	  if (strcmp(argv[i], "-pg") == 0) {
	    ProfilePGCommand = TRUE;
	  } else if (strcmp(argv[i], "-p") == 0) {
	    ProfilePCommand = TRUE;
	  } else if (strcmp(argv[i], "-exit") == 0) {
	    ExitNeeded = FALSE;
	  } else if (strcmp(argv[i], "-p2c") == 0) {
	    i++;
	    P2CLibrary = argv[i];
	  } else if (strcmp(argv[i], "-main") == 0) {
	    i++;
	    NameOfMain = argv[i];
	  }
	  i++;
	}
	ParseFile(argv[i]);
    } else {
        fprintf(stderr, "Usage: mklink (-l|-s) [-pg|-p] [-main name] [-exit] <modulelistfile>\n");
	exit(1);
    }
    exit(0);
}


/*
   ParseFile - parses the input file and generates the output file.
*/

static void ParseFile (char *Name)
{
    FindSource(Name);
    OpenOutputFile();
    if (LinkCommandLine) {
	ParseFileLinkCommand();
    } else {
	ParseFileStartup();
    }
    CloseFile();
}


/*
   ParseFileLinkCommand - generates the link command.
*/

static void ParseFileLinkCommand (void)
{
  char name[MAX_FILE_NAME];
  char *s;

  s = getenv("CC");
  if (s == NULL)
    printf("gcc -g");
  else
    printf("%s -g", s);

  if (ProfilePGCommand)
    printf(" -pg");
  else if (ProfilePCommand)
    printf(" -p");

  while (PutChar(GetChar()) != (char)EOF) {
    CopyUntilEolInto(name);
#if defined(XENIX)
    name[10] = (char)0;  /* truncate object file name */
#endif
    if ((strlen(name) > 0) && (name[0] != '#'))
      FindObject(name);
  }
  printf(" %s\n", P2CLibrary);
}


/*
   FindObject - searches the M2PATH variable to find the object file.
                If it finds the object file it prints it to stdout
		otherwise it writes an error on stderr.
*/

static void FindObject (char *Name)
{
    char m2search[4096];
    char m2path  [4096];
    char name    [4096];
    char exist   [4096];
    int  s, p;

    if (getenv("M2PATH") == NULL) {
	strcpy(m2path, ".");
    } else {
	strcpy(m2path, getenv("M2PATH"));
    }
    sprintf(name, "%s.o", Name);
    p = 0;
    while (m2path[p] != (char)0) {
	s = 0;
	while ((m2path[p] != (char)0) && (m2path[p] != ' ')) {
	    m2search[s] = m2path[p];
	    s++;
	    p++;
	}
	if (m2path[p] == ' ') {
	    p++;
	}
	m2search[s] = (char)0;
	sprintf(exist, "%s/%s", m2search, name);
	if (IsExists(exist)) {
	    printf(" %s", exist);
	    return;
	}
    }
    fprintf(stderr, "cannot find %s\n", name);
}


/*
   IsExists - returns true if a file, Name, exists. It returns
              false otherwise.
*/

static int IsExists (char *Name)
{
    struct stat buf;

    return( stat(Name, &buf) == 0 );
}


/*
   add_function - adds a name to the list of functions, in order.
 */

void add_function (char *name)
{
  functList *p = (functList *)malloc(sizeof(functList));
  p->functname = (char *)malloc(strlen(name)+1);
  strcpy(p->functname, name);

  if (head == NULL) {
    head = p;
    tail = p;
    p->next = NULL;
  } else {
    tail->next = p;
    tail       = p;
    tail->next = NULL;
  }    
}


/*
   ParseFileStartup - generates the startup code.
*/

static void ParseFileStartup (void)
{
    char name[MAX_FILE_NAME];
    functList *p;

    while (PutChar(GetChar()) != (char)EOF) {
	CopyUntilEolInto(name);
	if ((strlen(name) > 0) && (strcmp(name, "mod_init") != 0) &&
            (name[0] != '#')) {
            add_function(name);
	}
    }
    p = head;

    while (p != NULL) {
      printf("extern void _M2_%s_init(int argc, char *argv[]);\n", p->functname);
      p = p->next;
    }
    printf("extern void exit(int);\n");

    p = head;
    printf("\n\nint %s(int argc, char *argv[])\n", NameOfMain);
    printf("{\n");
    while (p != NULL) {
      printf("   _M2_%s_init(argc, argv);\n", p->functname);
      p = p->next;
    }
    if (ExitNeeded) {
      printf("   exit(0);\n");
    }
    printf("   return(0);\n");
    printf("\n}\n");
}


/*
   OpenOutputFile - shut down stdout and open the new mod_init.c
*/

static void OpenOutputFile (void)
{
    char FileName[MAX_FILE_NAME];

    CopyStdout = dup(STDOUT);
    if (close(STDOUT) != 0) {
	ERROR("Unable to close stdout"); exit(1);
    }
    if (LinkCommandLine) {
	sprintf(FileName, "linkcommand");
	system("/bin/rm -f linkcommand");
    } else {
	sprintf(FileName, "mod_init.c");
	system("/bin/rm -f mod_init.c");
    }
    OutputFile = open(FileName, O_CREAT | O_RDWR, 0666);
    if (OutputFile != STDOUT) {
	ERROR("Expected that the file descriptor should be 1");
    }
}


/*
   CloseFile - flush and close the temporary par file.
*/

static void CloseFile (void)
{
    fflush(stdout);
    if (close(STDOUT) != 0) {
	ERROR("Unable to close our output file"); exit(1);
    }
    if (dup(CopyStdout) != STDOUT) {
	ERROR("Expected that dup should use Stdout");
    }
    if (close(CopyStdout) != 0) {
	ERROR("Unable to close CopyStdout"); exit(1);
    }
}


/*
   CopyUntilEof - copies from the current input marker
                  until ENDOFILE is reached.
*/

static void CopyUntilEof (void)
{
    char ch;

    while ((ch=GetChar()) != ENDOFILE) {
	putchar(ch);
    }
}


/*
   CopyUntilEol - copies from the current input marker
                  until '\n' is reached.
*/

static void CopyUntilEol (void)
{
    char ch;

    while (((ch=GetChar()) != '\n') && (ch != (char)EOF)) {
	putchar(ch);
    }
    if (ch == '\n') {
	putchar(ch);
    }
}


/*
   CopyUntilEolInto - copies from the current input marker
                      until '\n' is reached into a Buffer.
*/

static void CopyUntilEolInto (char *Buffer)
{
    char ch;
    int  i=0;

    while (((ch=GetChar()) != '\n') && (ch != (char)EOF)) {
	Buffer[i] = ch;
	i++;
    }
    if ((ch == '\n') || (ch == (char)EOF)) {
	Buffer[i] = (char)0;
    }
}


/*
   IsSym - returns true if string, s, was found in the input stream.
           The input stream is uneffected.
*/

static int IsSym (char *s)
{
    int i=0;

    while ((s[i] != (char)0) && (s[i] == PutChar(GetChar()))) {
	GetChar();
	i++;
    }
    if (s[i]==(char)0) {
	PushBack(s);
	/* found s in input string */
	return( TRUE );
    } else {
	/* push back the characters we have scanned */
	if (i>0) {
	    do {
		i--;
		PutChar(s[i]);
	    } while (i>0);
	}
	return( FALSE );
    }
}


/*
   SymIs - returns true if string, s, was found in the input stream.
           The token s is consumed from the input stream.
*/

static int SymIs (char *s)
{
    int i=0;

    while ((s[i] != (char)0) && (s[i] == PutChar(GetChar()))) {
	GetChar();
	i++;
    }
    if (s[i]==(char)0) {
	/* found s in input string */
	return( TRUE );
    } else {
	/* push back the characters we have scanned */
	if (i>0) {
	    do {
		i--;
		PutChar(s[i]);
	    } while (i>0);
	}
	return( FALSE );
    }
}


/*
   FindString - keeps on reading input until a string,
                String, is matched.
		If end of file is reached then FALSE is returned,
                otherwise TRUE is returned.
*/

static int FindString (char *String)
{
    int StringIndex=0;
    int Found      =FALSE;
    int eof        =FALSE;
    char ch;

    while ((! Found) && (!eof)) {
	if (String[StringIndex] == (char)0) {
	    /* must have found string */
	    Found = TRUE;
	} else {
	    ch = GetChar();
	    eof = (ch == ENDOFILE);
	    if (ch == String[StringIndex]) {
		StringIndex++;
	    } else {
		StringIndex = 0;
	    }
	}
    }
    return( Found );
}


/*
   GetNL - keeps on reading input from until
           a new line is found.
*/

static void GetNL (void)
{
    char ch;

    while ((ch=GetChar()) != '\n') {
	putchar(ch);
    }
    putchar('\n');
}


/*
   GetChar - returns the current character in input.
*/

static char GetChar (void)
{
    char ch;

    if (StackPtr > 0) {
	StackPtr--;
	return(Stack[StackPtr]);
    } else {
	if (GetSingleChar(&ch)) {
	    return( ch );
	} else {
	    return( ENDOFILE );
	}
    }
}


#define MAXBUF  0x1000
static int  Pointer=0;
static int  AmountRead=0;
static char Buffer[MAXBUF];


/*
   ResetBuffer - resets the buffer information to an initial state.
*/

static void ResetBuffer (void)
{
    StackPtr = 0;
    Pointer = 0;
    AmountRead = 0;
}


/*
   GetSingleChar - gets a single character from input.
                   TRUE is returned upon success.
*/

static int GetSingleChar (char *ch)
{
    if (Pointer == AmountRead) {
	AmountRead = read(CurrentFile, &Buffer, MAXBUF);
	if (AmountRead < 0) {
	    AmountRead = 0;
	}
	Pointer = 0;
    }
    if (Pointer == AmountRead) {
	*ch = ENDOFILE;
	return( FALSE );
    } else {
	*ch = Buffer[Pointer];
	Pointer++;
	return( TRUE );
    }

}

    
/*
   InRange - returns true if Element is within the range Min..Max.
*/

static int InRange (int Element, unsigned int Min, unsigned int Max)
{
    return( (Element >= Min) && (Element <= Max));
}

static int stop () {}

/*
   PutChar - pushes a character back onto input.
             This character is also returned.
*/

static char PutChar (char ch)
{
    if (StackPtr < MAXSTACK) {
	if (ch=='\n') {
	    stop();
	}
	Stack[StackPtr] = ch;
	StackPtr++;
    } else {
	ERROR("Stack overflow in PutChar");
    }
    return(ch);
}


/*
   IsSpace - returns true if character, ch, is a space.
*/

static int IsSpace (char ch)
{
    return( (ch == ' ') || (ch == '\t') );
}


/*
   SkipSpaces - eats up spaces in input.
*/

static void SkipSpaces (void)
{
    while (IsSpace(PutChar(GetChar()))) {
	putchar(GetChar());
    }
}


/*
   SilentSkipSpaces - eats up spaces in input.
*/

static void SilentSkipSpaces (void)
{
    char ch;

    while (IsSpace(PutChar(GetChar()))) {
	ch = GetChar();  /* throw away character */
    }
}


/*
   SkipText - skips ascii text, it does not skip white spaces.
*/

static void SkipText (void)
{
    while (! IsSpace(PutChar(GetChar()))) {
	putchar(GetChar());
    }
}


/*
   SilentSkipText - skips ascii text, it does not skip white spaces.
*/

static void SilentSkipText (void)
{
    char ch;

    while (! IsSpace(PutChar(GetChar()))) {
	ch = GetChar();  /* throw away character */
    }
}


/*
   PushBack - pushes a string, backwards onto the input stack.
*/

static void PushBack (char *s)
{
    int i;

    i = strlen(s);
    while (i > 0) {
	i--;
	PutChar(s[i]);
    }
}

/*
   IsDigit - returns true if a character, ch, is a decimal digit.
*/

static int IsDigit (char ch)
{
    return( ((ch >= '0') && (ch <= '9')) );
}


/*
   GetName - returns the next name found.
*/

static void GetName (char *Name)
{
    int i;
    char ch;

    SkipSpaces();
    ch = GetChar();
    i = 0;
    while (! IsSpace(ch)) {
	Name[i] = ch;
	i++;
	ch = GetChar();
    }
    Name[i]= '\0';
}


/*
   FindSource - open source file on StdIn.
*/

static void FindSource (char *Name)
{
    if (close(STDIN) != 0) {
	ERROR("close on STDIN failed");
    }
    CurrentFile = open(Name, O_RDONLY);
    if (CurrentFile < 0) {
	perror("failed to open file");
	exit(1);
    }
    if (CurrentFile != STDIN) {
	ERROR("Expecting file descriptor value of 1");
    }
}
