/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/symbolKey.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   if !defined (FALSE)
#      define FALSE (1==0)
#   endif

#include <stddef.h>
#   include "GStorage.h"
#define _symbolKey_H
#define _symbolKey_C

#   include "GStorage.h"
#   include "GStrIO.h"
#   include "GNumberIO.h"
#   include "GDebug.h"
#   include "GnameKey.h"

#   define symbolKey_NulKey NULL
typedef struct symbolKey_isSymbol_p symbolKey_isSymbol;

typedef struct symbolKey_performOperation_p symbolKey_performOperation;

typedef struct _T1_r _T1;

typedef _T1 *symbolKey_symbolTree;

typedef unsigned int (*symbolKey_isSymbol_t) (void *);
struct symbolKey_isSymbol_p { symbolKey_isSymbol_t proc; };

typedef void (*symbolKey_performOperation_t) (void *);
struct symbolKey_performOperation_p { symbolKey_performOperation_t proc; };

struct _T1_r {
               nameKey_Name name;
               void *key;
               symbolKey_symbolTree left;
               symbolKey_symbolTree right;
             };

symbolKey_symbolTree symbolKey_initTree (void);
void symbolKey_killTree (symbolKey_symbolTree *t);
void * symbolKey_getSymKey (symbolKey_symbolTree t, nameKey_Name name);
void symbolKey_putSymKey (symbolKey_symbolTree t, nameKey_Name name, void * key);

/*
   delSymKey - deletes an entry in the binary tree.

               NB in order for this to work we must ensure that the InitTree sets
               both left and right to NIL.
*/

void symbolKey_delSymKey (symbolKey_symbolTree t, nameKey_Name name);

/*
   isEmptyTree - returns true if symbolTree, t, is empty.
*/

unsigned int symbolKey_isEmptyTree (symbolKey_symbolTree t);

/*
   doesTreeContainAny - returns true if symbolTree, t, contains any
                        symbols which in turn return true when procedure,
                        p, is called with a symbol as its parameter.
                        The symbolTree root is empty apart from the field,
                        left, hence we need two procedures.
*/

unsigned int symbolKey_doesTreeContainAny (symbolKey_symbolTree t, symbolKey_isSymbol p);

/*
   foreachNodeDo - for each node in symbolTree, t, a procedure, p,
                   is called with the node symbol as its parameter.
                   The tree root node only contains a legal left pointer,
                   therefore we need two procedures to examine this tree.
*/

void symbolKey_foreachNodeDo (symbolKey_symbolTree t, symbolKey_performOperation p);

/*
   findNodeAndParentInTree - find a node, child, in a binary tree, t, with name equal to n.
                             if an entry is found, father is set to the node above child.
*/

static void findNodeAndParentInTree (symbolKey_symbolTree t, nameKey_Name n, symbolKey_symbolTree *child, symbolKey_symbolTree *father);

/*
   searchForAny - performs the search required for doesTreeContainAny.
                  The root node always contains a nul data value,
                  therefore we must skip over it.
*/

static unsigned int searchForAny (symbolKey_symbolTree t, symbolKey_isSymbol p);

/*
   searchAndDo - searches all the nodes in symbolTree, t, and
                 calls procedure, p, with a node as its parameter.
                 It traverse the tree in order.
*/

static void searchAndDo (symbolKey_symbolTree t, symbolKey_performOperation p);


/*
   findNodeAndParentInTree - find a node, child, in a binary tree, t, with name equal to n.
                             if an entry is found, father is set to the node above child.
*/

static void findNodeAndParentInTree (symbolKey_symbolTree t, nameKey_Name n, symbolKey_symbolTree *child, symbolKey_symbolTree *father)
{
  (*father) = t;
  if (t == NULL)
    Debug_Halt ((char *) "parameter t should never be NIL", 31, 200, (char *) "../../gcc-5.2.0/gcc/gm2/mc/symbolKey.mod", 40);
  (*child) = t->left;
  if ((*child) != NULL)
    do {
      if (n < (*child)->name)
        {
          (*father) = (*child);
          (*child) = (*child)->left;
        }
      else if (n > (*child)->name)
        {
          (*father) = (*child);
          (*child) = (*child)->right;
        }
    } while (! (((*child) == NULL) || (n == (*child)->name)));
}


/*
   searchForAny - performs the search required for doesTreeContainAny.
                  The root node always contains a nul data value,
                  therefore we must skip over it.
*/

static unsigned int searchForAny (symbolKey_symbolTree t, symbolKey_isSymbol p)
{
  if (t == NULL)
    return FALSE;
  else
    return (((*p.proc) (t->key)) || (searchForAny (t->left, p))) || (searchForAny (t->right, p));
}


/*
   searchAndDo - searches all the nodes in symbolTree, t, and
                 calls procedure, p, with a node as its parameter.
                 It traverse the tree in order.
*/

static void searchAndDo (symbolKey_symbolTree t, symbolKey_performOperation p)
{
  if (t != NULL)
    {
      searchAndDo (t->right, p);
      (*p.proc) (t->key);
      searchAndDo (t->left, p);
    }
}

symbolKey_symbolTree symbolKey_initTree (void)
{
  symbolKey_symbolTree t;

  Storage_ALLOCATE ((void **) &t, sizeof (_T1));
  t->left = NULL;
  t->right = NULL;
  return t;
}

void symbolKey_killTree (symbolKey_symbolTree *t)
{
  if ((*t) != NULL)
    {
      symbolKey_killTree (&(*t)->left);
      symbolKey_killTree (&(*t)->right);
      Storage_DEALLOCATE ((void **) &(*t), sizeof (_T1));
      (*t) = NULL;
    }
}

void * symbolKey_getSymKey (symbolKey_symbolTree t, nameKey_Name name)
{
  symbolKey_symbolTree father;
  symbolKey_symbolTree child;

  if (t == NULL)
    return symbolKey_NulKey;
  else
    {
      findNodeAndParentInTree (t, name, &child, &father);
      if (child == NULL)
        return symbolKey_NulKey;
      else
        return child->key;
    }
}

void symbolKey_putSymKey (symbolKey_symbolTree t, nameKey_Name name, void * key)
{
  symbolKey_symbolTree father;
  symbolKey_symbolTree child;

  findNodeAndParentInTree (t, name, &child, &father);
  if (child == NULL)
    {
      if (father == t)
        {
          Storage_ALLOCATE ((void **) &child, sizeof (_T1));
          father->left = child;
        }
      else
        if (name < father->name)
          {
            Storage_ALLOCATE ((void **) &child, sizeof (_T1));
            father->left = child;
          }
        else if (name > father->name)
          {
            Storage_ALLOCATE ((void **) &child, sizeof (_T1));
            father->right = child;
          }
      child->right = NULL;
      child->left = NULL;
      child->key = key;
      child->name = name;
    }
  else
    Debug_Halt ((char *) "symbol already stored", 21, 116, (char *) "../../gcc-5.2.0/gcc/gm2/mc/symbolKey.mod", 40);
}


/*
   delSymKey - deletes an entry in the binary tree.

               NB in order for this to work we must ensure that the InitTree sets
               both left and right to NIL.
*/

void symbolKey_delSymKey (symbolKey_symbolTree t, nameKey_Name name)
{
  symbolKey_symbolTree i;
  symbolKey_symbolTree child;
  symbolKey_symbolTree father;

  findNodeAndParentInTree (t, name, &child, &father);
  if ((child != NULL) && (child->name == name))
    if (father->right == child)
      {
        if (child->left != NULL)
          {
            i = child->left;
            while (i->right != NULL)
              i = i->right;
            i->right = child->right;
            father->right = child->left;
          }
        else
          father->right = child->right;
        Storage_DEALLOCATE ((void **) &child, sizeof (_T1));
      }
    else
      {
        if (child->right != NULL)
          {
            i = child->right;
            while (i->left != NULL)
              i = i->left;
            i->left = child->left;
            father->left = child->right;
          }
        else
          father->left = child->left;
        Storage_DEALLOCATE ((void **) &child, sizeof (_T1));
      }
  else
    Debug_Halt ((char *) "trying to delete a symbol that is not in the tree - the compiler never expects this to occur", 92, 183, (char *) "../../gcc-5.2.0/gcc/gm2/mc/symbolKey.mod", 40);
}


/*
   isEmptyTree - returns true if symbolTree, t, is empty.
*/

unsigned int symbolKey_isEmptyTree (symbolKey_symbolTree t)
{
  return t->left == NULL;
}


/*
   doesTreeContainAny - returns true if symbolTree, t, contains any
                        symbols which in turn return true when procedure,
                        p, is called with a symbol as its parameter.
                        The symbolTree root is empty apart from the field,
                        left, hence we need two procedures.
*/

unsigned int symbolKey_doesTreeContainAny (symbolKey_symbolTree t, symbolKey_isSymbol p)
{
  return searchForAny (t->left, p);
}


/*
   foreachNodeDo - for each node in symbolTree, t, a procedure, p,
                   is called with the node symbol as its parameter.
                   The tree root node only contains a legal left pointer,
                   therefore we need two procedures to examine this tree.
*/

void symbolKey_foreachNodeDo (symbolKey_symbolTree t, symbolKey_performOperation p)
{
  searchAndDo (t->left, p);
}

void _M2_symbolKey_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_symbolKey_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}