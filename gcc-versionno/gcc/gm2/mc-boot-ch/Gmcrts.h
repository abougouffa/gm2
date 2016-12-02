#if !defined(MCRTS_H)
#  define MCRTS_H

void CaseException (const char *s, unsigned int high, unsigned int lineno);
void ReturnException (const char *s, unsigned int high, unsigned int lineno);
/* void throw (int n); */

#endif
