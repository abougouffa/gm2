/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/nameKey.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   if !defined (TRUE)
#      define TRUE (1==1)
#   endif

#   if !defined (FALSE)
#      define FALSE (1==0)
#   endif

#include <stddef.h>
#include <string.h>
#include <limits.h>
#   include "GStorage.h"
#define _nameKey_H
#define _nameKey_C

#   include "GSYSTEM.h"
#   include "GStorage.h"
#   include "GIndexing.h"
#   include "GStrIO.h"
#   include "GStdIO.h"
#   include "GNumberIO.h"
#   include "GStrLib.h"
#   include "Glibc.h"
#   include "GASCII.h"
#   include "GM2RTS.h"

#   define nameKey_NulName 0
typedef unsigned int nameKey_Name;

typedef struct _T1_r _T1;

typedef char *ptrToChar;

typedef _T1 *nameNode;

typedef enum {less, equal, greater} comparison;

struct _T1_r {
               ptrToChar data;
               nameKey_Name key;
               nameNode left;
               nameNode right;
             };

static nameNode binaryTree;
static Indexing_Index keyIndex;
static unsigned int lastIndice;

/*
   makeKey - returns the Key of the symbol, a. If a is not in the
             name table then it is added, otherwise the Key of a is returned
             directly. Note that the name table has no scope - it merely
             presents a more convienient way of expressing strings. By a Key.
*/

nameKey_Name nameKey_makeKey (char *a_, unsigned int _a_high);

/*
   makekey - returns the Key of the symbol, a. If a is not in the
             name table then it is added, otherwise the Key of a is returned
             directly. Note that the name table has no scope - it merely
             presents a more convienient way of expressing strings. By a Key.
             These keys last for the duration of compilation.
*/

nameKey_Name nameKey_makekey (void * a);

/*
   getKey - returns the name, a, of the key, Key.
*/

void nameKey_getKey (nameKey_Name key, char *a, unsigned int _a_high);

/*
   lengthKey - returns the StrLen of Key.
*/

unsigned int nameKey_lengthKey (nameKey_Name key);

/*
   isKey - returns TRUE if string, a, is currently a key.
           We dont use the Compare function, we inline it and avoid
           converting, a, into a String, for speed.
*/

unsigned int nameKey_isKey (char *a_, unsigned int _a_high);

/*
   keyToCharStar - returns the C char * string equivalent for, key.
*/

void nameKey_writeKey (nameKey_Name key);

/*
   isSameExcludingCase - returns TRUE if key1 and key2 are
                         the same. It is case insensitive.
                         This function deliberately inlines CAP for speed.
*/

unsigned int nameKey_isSameExcludingCase (nameKey_Name key1, nameKey_Name key2);

/*
   keyToCharStar - returns the C char * string equivalent for, key.
*/

void * nameKey_keyToCharStar (nameKey_Name key);

/*
   doMakeKey - finds the name, n, in the tree or else create a name.
               If a name is found then the string, n, is deallocated.
*/

static nameKey_Name doMakeKey (ptrToChar n, unsigned int higha);

/*
   compare - return the result of Names[i] with Names[j]
*/

static comparison compare (ptrToChar pi, nameKey_Name j);

/*
   findNodeAndParentInTree - search BinaryTree for a name.
                             If this name is found in the BinaryTree then
                             child is set to this name and father is set to the node above.
                             A comparison is returned to assist adding entries into this tree.
*/

static comparison findNodeAndParentInTree (ptrToChar n, nameNode *child, nameNode *father);


/*
   doMakeKey - finds the name, n, in the tree or else create a name.
               If a name is found then the string, n, is deallocated.
*/

static nameKey_Name doMakeKey (ptrToChar n, unsigned int higha)
{
  comparison result;
  nameNode father;
  nameNode child;
  nameKey_Name k;

  result = findNodeAndParentInTree (n, &child, &father);
  if (child == NULL)
    {
      if (result == less)
        {
          Storage_ALLOCATE ((void **) &child, sizeof (_T1));
          father->left = child;
        }
      else if (result == greater)
        {
          Storage_ALLOCATE ((void **) &child, sizeof (_T1));
          father->right = child;
        }
      child->right = NULL;
      child->left = NULL;
      lastIndice += 1;
      child->key = lastIndice;
      child->data = n;
      Indexing_PutIndice (keyIndex, (unsigned int) child->key, (void *) n);
      k = lastIndice;
    }
  else
    {
      Storage_DEALLOCATE ((void **) &n, higha+1);
      k = child->key;
    }
  return k;
}


/*
   compare - return the result of Names[i] with Names[j]
*/

static comparison compare (ptrToChar pi, nameKey_Name j)
{
  ptrToChar pj;
  char c1;
  char c2;

  pj = nameKey_keyToCharStar (j);
  c1 = (*pi);
  c2 = (*pj);
  while ((c1 != ASCII_nul) || (c2 != ASCII_nul))
    if (c1 < c2)
      return less;
    else if (c1 > c2)
      return greater;
    else
      {
        pi += 1;
        pj += 1;
        c1 = (*pi);
        c2 = (*pj);
      }
  return equal;
}


/*
   findNodeAndParentInTree - search BinaryTree for a name.
                             If this name is found in the BinaryTree then
                             child is set to this name and father is set to the node above.
                             A comparison is returned to assist adding entries into this tree.
*/

static comparison findNodeAndParentInTree (ptrToChar n, nameNode *child, nameNode *father)
{
  comparison result;

  (*father) = binaryTree;
  (*child) = binaryTree->left;
  if ((*child) == NULL)
    return less;
  else
    {
      do {
        result = compare (n, (*child)->key);
        if (result == less)
          {
            (*father) = (*child);
            (*child) = (*child)->left;
          }
        else if (result == greater)
          {
            (*father) = (*child);
            (*child) = (*child)->right;
          }
      } while (! (((*child) == NULL) || (result == equal)));
      return result;
    }
}


/*
   makeKey - returns the Key of the symbol, a. If a is not in the
             name table then it is added, otherwise the Key of a is returned
             directly. Note that the name table has no scope - it merely
             presents a more convienient way of expressing strings. By a Key.
*/

nameKey_Name nameKey_makeKey (char *a_, unsigned int _a_high)
{
  ptrToChar n;
  ptrToChar p;
  unsigned int i;
  unsigned int higha;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  higha = StrLib_StrLen ((char *) a, _a_high);
  Storage_ALLOCATE ((void **) &p, higha+1);
  if (p == NULL)
    M2RTS_HALT (0);
  else
    {
      n = p;
      i = 0;
      while (i < higha)
        {
          (*p) = a[i];
          i += 1;
          p += 1;
        }
      (*p) = ASCII_nul;
      return doMakeKey (n, higha);
    }
}


/*
   makekey - returns the Key of the symbol, a. If a is not in the
             name table then it is added, otherwise the Key of a is returned
             directly. Note that the name table has no scope - it merely
             presents a more convienient way of expressing strings. By a Key.
             These keys last for the duration of compilation.
*/

nameKey_Name nameKey_makekey (void * a)
{
  ptrToChar n;
  ptrToChar p;
  ptrToChar pa;
  unsigned int i;
  unsigned int higha;

  if (a == NULL)
    return nameKey_NulName;
  else
    {
      higha = libc_strlen (a);
      Storage_ALLOCATE ((void **) &p, higha+1);
      if (p == NULL)
        M2RTS_HALT (0);
      else
        {
          n = p;
          pa = a;
          i = 0;
          while (i < higha)
            {
              (*p) = (*pa);
              i += 1;
              p += 1;
              pa += 1;
            }
          (*p) = ASCII_nul;
          return doMakeKey (n, higha);
        }
    }
}


/*
   getKey - returns the name, a, of the key, Key.
*/

void nameKey_getKey (nameKey_Name key, char *a, unsigned int _a_high)
{
  ptrToChar p;
  unsigned int i;
  unsigned int higha;

  p = nameKey_keyToCharStar (key);
  i = 0;
  higha = _a_high;
  while (((p != NULL) && (i <= higha)) && ((*p) != ASCII_nul))
    {
      a[i] = (*p);
      p += 1;
      i += 1;
    }
  if (i <= higha)
    a[i] = ASCII_nul;
}


/*
   lengthKey - returns the StrLen of Key.
*/

unsigned int nameKey_lengthKey (nameKey_Name key)
{
  unsigned int i;
  ptrToChar p;

  p = nameKey_keyToCharStar (key);
  i = 0;
  while ((*p) != ASCII_nul)
    {
      i += 1;
      p += 1;
    }
  return i;
}


/*
   isKey - returns TRUE if string, a, is currently a key.
           We dont use the Compare function, we inline it and avoid
           converting, a, into a String, for speed.
*/

unsigned int nameKey_isKey (char *a_, unsigned int _a_high)
{
  nameNode child;
  ptrToChar p;
  unsigned int i;
  unsigned int higha;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  child = binaryTree->left;
  if (child != NULL)
    do {
      i = 0;
      higha = _a_high;
      p = nameKey_keyToCharStar (child->key);
      while ((i <= higha) && (a[i] != ASCII_nul))
        {
          if (a[i] < (*p))
            {
              child = child->left;
              i = higha;
            }
          else if (a[i] > (*p))
            {
              child = child->right;
              i = higha;
            }
          else
            {
              if ((a[i] == ASCII_nul) || (i == higha))
                {
                  /* avoid gcc warning by using compound statement even if not strictly necessary.  */
                  if ((*p) == ASCII_nul)
                    return TRUE;
                  else
                    child = child->left;
                }
              p += 1;
            }
          i += 1;
        }
    } while (! (child == NULL));
  return FALSE;
}


/*
   keyToCharStar - returns the C char * string equivalent for, key.
*/

void nameKey_writeKey (nameKey_Name key)
{
  ptrToChar s;

  s = nameKey_keyToCharStar (key);
  while ((s != NULL) && ((*s) != ASCII_nul))
    {
      StdIO_Write ((*s));
      s += 1;
    }
}


/*
   isSameExcludingCase - returns TRUE if key1 and key2 are
                         the same. It is case insensitive.
                         This function deliberately inlines CAP for speed.
*/

unsigned int nameKey_isSameExcludingCase (nameKey_Name key1, nameKey_Name key2)
{
  ptrToChar pi;
  ptrToChar pj;
  char c1;
  char c2;

  if (key1 == key2)
    return TRUE;
  else
    {
      pi = nameKey_keyToCharStar (key1);
      pj = nameKey_keyToCharStar (key2);
      c1 = (*pi);
      c2 = (*pj);
      while ((c1 != ASCII_nul) && (c2 != ASCII_nul))
        if (((c1 == c2) || (((c1 >= 'A') && (c1 <= 'Z')) && (c2 == ((char) ((((unsigned int) (c1))-((unsigned int) ('A')))+((unsigned int) ('a'))))))) || (((c2 >= 'A') && (c2 <= 'Z')) && (c1 == ((char) ((((unsigned int) (c2))-((unsigned int) ('A')))+((unsigned int) ('a')))))))
          {
            pi += 1;
            pj += 1;
            c1 = (*pi);
            c2 = (*pj);
          }
        else
          return FALSE;
      return c1 == c2;
    }
}


/*
   keyToCharStar - returns the C char * string equivalent for, key.
*/

void * nameKey_keyToCharStar (nameKey_Name key)
{
  if ((key == nameKey_NulName) || (! (Indexing_InBounds (keyIndex, (unsigned int) key))))
    return NULL;
  else
    return Indexing_GetIndice (keyIndex, (unsigned int) key);
}

void _M2_nameKey_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  lastIndice = 0;
  keyIndex = Indexing_InitIndex (1);
  Storage_ALLOCATE ((void **) &binaryTree, sizeof (_T1));
  binaryTree->left = NULL;
}

void _M2_nameKey_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}