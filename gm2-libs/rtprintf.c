#include "stdio.h"

#define MAXBUFFER (16*1024)
#define HALT(X)  Debug_Halt(X, strlen(X), __LINE__, __FILE__, strlen(__FILE__))


#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif


/*
 *  printf and family. Implemented here is a cheap and cheerful
 *  set of printf routines.
 */


/*
 *  intToStr - converts an unsigned integer into a string, using
 *             the, base. The, base, maybe one of:
 *             'd', 'x', 'X', 'o', 'u'
 *             This function returns the length of the string created.
 *             It assumes that the number is unsigned (always positive)
 */

static int intToStr (unsigned int number, char *dest, char base)
{
  int  length=0;
  int  baseModulus;
  int  i=0;
  char buffer[50];
  int  digit;

  if ((base == 'd') || (base == 'u')) {
    baseModulus = 10;
  }
  if ((base == 'x') || (base == 'X')) {
    baseModulus = 16;
  }
  if (base == 'o') {
    baseModulus = 8;
  }
  do {
    digit   = number % baseModulus;
    number /= baseModulus;
    if (digit <= '9') {
      buffer[i] = ((char)(digit))+'0';
    } else {
      /* (digit >= 10) */
      if (base == 'x') {
	/* lower case hex */
	buffer[i] = ((char)(digit-10))+'a';
      } else {
	/* upper case hex */
	buffer[i] = ((char)(digit-10))+'A';
      }
    }
    i++;
  } while (number != 0);
  while (i>0) {
    i--;
    dest[length] = buffer[i];
    length++;
  }
  dest[length] = '\0';
  return( length );
}


/*
 *  processFormat - increments dest by the number of characters
 *                  placed into destString and increments, src,
 *                  by the number of characters processed from
 *                  the format string.
 */

static void processFormat (char *destString, const char *format,
			   va_list *argsPtr, int *dest, int *src)
{
  char  fill;                 /* fill character                      */
  int   leftJustification;    /* left justification?                 */
  int   fieldMax, fieldMin;   /* % min.max s fields                  */
  int   leading;              /* number of leading or trailing chars */
  char  sign;                 /* set to -, + or ' '                  */
  char  string[MAXBUFFER];    /* temporary number/char string        */
  int   length=0;             /* number of characters used in string */
  char *strPtr= (char *)&string;
                              /* points to string[] or va_arg(char*) */
  int          i;             /* for loop index                      */
  int          integer;       /* argument for %d %o                  */
  unsigned int unsignedInt;   /* argument for %u                     */


  (*src)++;   /* move beyond % */
  if (format[*src] == '%') {
    /* found %% */
    destString[*dest] = '%';
    (*dest)++;
    (*src)++;
    return;
  } else {
    /* check for left justified output ie %-... */
    if (format[*src] == '-') {
      leftJustification = TRUE;
      (*src)++;
    } else {
      leftJustification = FALSE;
    }
    /* allow for zero filled outputs ie %0... */
    if (format[*src] == '0') {
      fill = '0';
      (*src)++;
    } else {
      fill = ' ';
    }
    /*
     * allow for minimum field width specifier for %d, %u, %x, %c, %s
     * and also allow %* for variable width  (%0* as well)
     */
    fieldMin = 0;
    if (format[*src] == '*') {
      fieldMin = va_arg(*argsPtr, int);
      (*src)++;
    } else {
      while ((format[*src] >= '0') && (format[*src] <= '9')) {
	fieldMin = (fieldMin * 10) + ((int)(format[*src] - '0'));
	(*src)++;
      }
    }
    /* allow for maximum string width for %s */
    fieldMax = 0;
    if (format[*src] == '.') {
      (*src)++;
      if (format[*src] == '*') {
	fieldMax = va_arg(*argsPtr, int);
	(*src)++;
      } else {
	while ((format[*src] >= '0') && (format[*src] <= '9')) {
	  fieldMax = (fieldMax * 10) + ((int)(format[*src] - '0'));
	  (*src)++;
	}
      }
    }
    /* check for the 'l' option to force long numeric (we ignore for 32 bit) */
    if (format[*src] == 'l') {
      (*src)++;
    }
    if (format[*src] == '\0') {
      destString[*dest] = '%';
      (*dest)++;
      destString[*dest] = '\0';
      return;
    }
    sign = '\0';  /* sign == '-' for negative decimal */
    switch (format[*src]) {

    case 'c':  string[0] = va_arg(*argsPtr, char);
               string[1] = '\0';
	       fieldMax  = 0;
	       fill      = ' ';
	       length    = 1;
	       break;

    case 's':  strPtr = va_arg(*argsPtr, char *);
               fill   = 0;
	       for (length=0; strPtr[length] != '\0'; length++) {
	       }
               break;

    case 'd':  integer = va_arg(*argsPtr, int);
               if (integer < 0) {
		 integer = -integer;
		 sign    = '-';
	       }
	       length = intToStr((unsigned int)integer, strPtr, 'd');
	       break;

    case 'u':  unsignedInt = va_arg(*argsPtr, unsigned int);
               length = intToStr(unsignedInt, strPtr, 'u');
	       break;

    case 'o':  integer = va_arg(*argsPtr, int);
               if (integer < 0) {
		 integer = -integer;
		 sign    = '-';
	       }
	       length = intToStr((unsigned int)integer, strPtr, 'o');
	       break;

    case 'x':  integer = va_arg(*argsPtr, int);
               if (integer < 0) {
		 integer = -integer;
		 sign    = '-';
	       }
	       length = intToStr((unsigned int)integer, strPtr, 'x');
	       break;

    case 'X':  integer = va_arg(*argsPtr, int);
               if (integer < 0) {
		 integer = -integer;
		 sign    = '-';
	       }
	       length = intToStr((unsigned int)integer, strPtr, 'X');
	       break;

    default:   string[0] = format[*src];
               string[1] = '\0';
	       length = 1;
	       break;
    }
    (*src)++;
    if ((fieldMin > MAXBUFFER) || (fieldMin < 0)) {
      fieldMin = 0;
    }
    if ((fieldMax > MAXBUFFER) || (fieldMax < 0)) {
      fieldMax = 0;
    }
    leading = 0;
    if ((fieldMax != 0) || (fieldMin != 0)) {
      if (fieldMax != 0) {
	if (length > fieldMax) {
	  length = fieldMax;
	}
      }
      if (fieldMin != 0) {
	leading = fieldMin - length;
      }
      if (sign == '-') {
	leading--;
      }
    }
    if ((sign == '-') && (fill == '0')) {
      destString[*dest] = sign;
      (*dest)++;
    }
    if (leftJustification) {
      for (i=0; i<leading; i++) {
	destString[*dest] = fill;
	(*dest)++;
      }
    }
    if ((sign == '-') && (fill == ' ')) {
      destString[*dest] = sign;
      (*dest)++;
    }
    for (i=0; i<length; i++) {
      destString[*dest] = strPtr[i];
      (*dest)++;
    }
    if (! leftJustification) {
      for (i=0; i<leading; i++) {
	destString[*dest] = fill;
	(*dest)++;
      }
    }
  }
}


/*
 *  vsprintf - does the real work of all the *printf family.
 *             We implement %d, %s, %x only.
 */

int vsprintf (char *destString, const char *format, va_list argsPtr)
{
  int i=0;
  int last;
  int dest=0;

  while (TRUE) {
    last = i;
    while ((format[i] != '\0') && (format[i] != '%')) {
      i++;
    }
    if (i-last > 0) {
      memcpy(&destString[dest], &format[last], i-last);
      dest += (i-last);
    }
    if (format[i] == '\0') {
      destString[dest] = '\0';
      return( dest );
    } else {
      /* must have found % */
      processFormat(destString, format, &argsPtr, &dest, &i);
    }
  }
}


/*
 *  vfprintf - wraps up arguments and calls vsprintf to do the work
 */

int vfprintf (FILE *f, const char *format, va_list argsPtr)
{
  char    buffer[MAXBUFFER];
  int     res;

  res = vsprintf(buffer, format, argsPtr);
  if (res > 0) {
    __IO_putsn(f, buffer, res);
  }
  return( res );
}


int sprintf (char *destString, const char *format, ...)
{
  int     ret;
  va_list argsPtr;      /* points to each argument in turn */

  va_start(argsPtr, format);       /* argsPtr points to the first var arg */
  ret = vsprintf(destString, format, argsPtr);
  va_end(args);
  return ret;
}


int printf (const char *format, ...)
{
  int     res;
  va_list args;

  va_start(args, format);
  res = vfprintf(stdout, format, args);
  va_end(args);
  return( res );
}


int fprintf (FILE *f, const char *format, ...)
{
  int     res;
  va_list args;

  va_start(args, format);
  res = vfprintf(f, format, args);
  va_end(args);
  return( res );
}


/*
 *  convertInt - convert string representation of an integer and
 *               it returns TRUE if conversion took place, FALSE
 *               if we never saw a digit.
 */

int convertInt (const char *s, int *value)
{
  if (isdigit(*s)) {
    *value = 0;
    do {
      (*value) *= 10;
      (*value)  = ((int)(*s)) - ((int)'0');
      s++;
    } while (((*s) != '\0') && (isdigit(s)));
    return( TRUE );
  } else {
    return( FALSE );
  }
}


int iswhite (char ch)
{
  return( (ch == ' ') || (ch == '\t') );
}


char *skipWhite (char *s)
{
   while ((*s != '\0') && (iswhite(*s))) {
     s++;
   }
   return( s );
}


/*
 *  sscanf - only implements %d (minimal implementation for gas).
 */

int sscanf (char *s, const char *format, ...)
{
  va_list args;
  int     res;

  va_start(args, format);
  if (strcmp(format, "%d") == 0) {
    int *ival=va_arg(args, int *);
    s = skipWhite(s);
    res = convertInt(s, ival);
  } else {
    HALT("implementation restriction (sscanf only knows about %d");
  }
  va_end(args);
  return( res );
}
