#
#  mxmtree - handle all the tree creation and walking
#

from mxmutils import *


defaultLang = "c"
atom, node = range(2)


#
#  printf - keeps C programmers happy :-)
#

def printf (format, *args):
    print str(format) % args,


#
#  setDefaultLang - sets the default language to either C or Modula-2.
#                   It returns True if successful.
#

def setDefaultLang (lang):
    global defaultLang

    if (lang == "c" or lang == "C"):
        defaultLang = "c"
        return True
    if (lang == "m2" or lang == "M2"):
        defaultLang = "m2"
        return True
    return False


#
#  create a tree entity which is either a node or an atom.
#

class tree:
    global atom, node
    

    def __init__ (self, n, l, e):
        self.name = n
        self.kind = atom
        self.lang = l
        self.error = e
    def operands (self, o):
        self.kind = node
        self.operands = o
        if self.lang == "":
            self.error('must set lang by calling the tree constructor')
    def out (self):
        if self.lang == "m2":
            self.lang_m2()
        if self.lang == "c":
            self.lang_m2()
    def lang_m2 (self):
        if self.kind == atom:
            if isdigit(self.name[0]):
                printf("%s.0", self.name)
            elif self.name == '%pi':
                printf("pi")
            else:
                printf("%s", self.name)
        else:
            l = self.operands
            if len(l) == 1:
                printf("(%s", self.name)
                l[0].lang_m2()
                printf(")")
            elif len(l)>1:
                if self.name == '^':
                    if len(l) != 2:
                        self.error('expecting ^ to have two operands only')
                    self.doM2Power()
                else:
                    printf("(")
                    l[0].lang_m2()
                    for o in self.operands[1:]:
                        printf("%s", self.name)
                        o.lang_m2()
                        printf(")")
    def doM2Power (self):
        if self.isToPower('2'):
            printf(" sqr(")
            self.operands[0].lang_m2()
            printf(") ")
        elif self.isToPower('3'):
            printf(" cub(")
            self.operands[0].lang_m2()
            printf(") ")
        elif self.isToPower('4'):
            printf(" quart(")
            self.operands[0].lang_m2()
            printf(") ")
        else:
            printf(" topower(")
            self.operands[0].lang_m2()
            printf(", ")
            self.operands[1].lang_m2()
            printf(") ")
    def isToPower (self, power):
        return self.name == '^' and self.operands[1].isAtom() and self.operands[1].name == power
    
    def isAtom (self):
        return self.kind == atom
