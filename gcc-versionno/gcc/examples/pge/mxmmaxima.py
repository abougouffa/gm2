#
#  mxmmaxima - parse the output of maxima
#

from mxmtree import tree
from mxmout import *
from mxmutils import *

import mxmstack
import sys

tokens   = [ '(', ')', '-', '+', '*', '/', '^', '<eof>' ]


class parse:
    #
    #  readFile - reads the complete input file.
    #

    def __init__ (self, filename, line, lang, content):
        self.inputFile = filename
        self.contents = content
        self.powerpos = 0
        self.columnNo = 0
        self.curpos = self.skipLine(self.powerpos)
        self.lineOffset = line
        self.lineNo = 2
        self.startpos = self.curpos
        self.expressionStack = mxmstack.stack(None)  # self.internalError)
        self.tokenStack = mxmstack.stack(None)       # self.internalError)
        self.lang = lang
        self.tok = self.getNext()
        self.terms = []

    def getPolynomials (self, nTerms):
        if self.expression():
            e = self.expressionStack.pop()
            for n in reversed(range(nTerms)):
                self.terms += [e.collectPolynomial(n, 't')]
            self.terms.reverse()
            return self.terms
        return None


    #
    #  skipLine - returns the position of the next line.
    #

    def skipLine (self, pos):
        while (pos<len(self.contents)) and (self.contents[pos] != '\n'):
            pos += 1
        pos += 1
        return pos


    #
    #  isWhite - returns True if s[i] is white space.  If i exceeds
    #            the length of s, False is returned.
    #

    def isWhite (self, s, i):
        if i<len(s):
            return (s[i]==' ') or (s[i] == '\n') or (s[i] == '\t')
        else:
            return False


    #
    #  skipWhite - skips all white characters
    #

    def skipWhite (self):
        while self.isWhite(self.contents, self.curpos):
            if self.contents[self.curpos] == '\n':
                self.assertBlank()
                self.lineNo += 2
                self.columnNo = 0
                self.curpos += 1
                self.powerpos = self.curpos
                self.curpos = self.skipLine(self.curpos)
                self.startpos = self.curpos
            else:
                self.curpos += 1
                self.columnNo += 1


    #
    #  checkPower - checks to see if the token is raised to a power
    #

    def checkPower (self):
        end = self.skipLine(self.powerpos)
        if (self.powerpos+self.columnNo<end) and (not self.isWhite(self.contents, self.powerpos+self.columnNo)):
            power = ""
            i = self.powerpos+self.columnNo
            while not self.isWhite(self.contents, i):
                power += self.contents[self.powerpos+self.columnNo]
                # replace self.powerpos+self.columnNo with a " "
                self.contents = self.contents[:self.powerpos+self.columnNo] + " " + self.contents[self.powerpos+self.columnNo+1:]
                i += 1
            if (len(power)>0) and (not isdigit(power[0])):
                self.internalError('bad power')
            return power
        return None


    #
    #  assertBlank - asserts the current line self.contents is blank.
    #

    def assertBlank (self):
        i = self.powerpos
        while self.contents[i] != '\n':
            if self.isWhite(self.contents, i):
                i += 1
            else:
                self.internalError("powers should have been removed")
                sys.exit(1)


    #
    #  collectText - returns the text found.
    #

    def collectText (self):
        self.tok = self.tokenStack.pop()
        if (self.tok != None) and (self.tok != ""):
            return self.tok
        self.pos = self.curpos
        self.seentoken = False
        while (not self.seentoken) and (not self.isWhite(self.contents, self.curpos)):
            if self.curpos >= len(self.contents):
                return "<eof>"
            if self.contents[self.curpos] in tokens:
                self.seentoken = True
                if self.curpos==self.pos:
                    self.curpos += 1
                    self.columnNo += 1
            else:
                self.curpos += 1
                self.columnNo += 1
        self.tok = self.contents[self.pos:self.curpos]
        #
        #  check powers
        #
        power = self.checkPower()
        if (power != None) and (power != ""):
            self.tokenStack.push(power)
            self.tokenStack.push("^")
        return self.tok


    #
    #  getNext - returns the next token.
    #

    def getNext (self):
        self.skipWhite()
        token = self.collectText()
        # self.printToken(token)
        # printf("token is '%s'\n", token)
        return token


    #
    #  expression := unaryOrConstTerm { addOperator constTerm } =:
    #

    def expression (self):
        if self.unaryOrTerm():
            while self.addOperator():
                op = self.stack.pop()
                l = self.stack.pop()
                self.term()
                r = self.stack.pop()
                t = tree(op, self.lang, self.internalError)
                t.operands([l, r])
                t.out()
                self.expressionStack.push(t)
            return True
        return False


    #
    #  unaryOrTerm := "+" simpleExpr | "-" simpleExpr | simpleExpr =:
    #
            
    def unaryOrTerm (self):
        if self.seenToken("+"):
            self.simpleExpr()
            return True
        elif self.seenToken("-"):
            self.simpleExpr()
            l = self.expressionStack.pop()
            t = tree('-', self.lang, self.internalError)
            t.operands([l])
            self.expressionStack.push(t)
            return True
        else:
            return self.simpleExpr()


    #
    #  simpleExpr := constTerm { addOperator constTerm } =:
    #

    def simpleExpr (self):
        if self.term():
            while self.addOperator():
                if self.term():
                    r = self.expressionStack.pop()
                    operator = self.expressionStack.pop()
                    l = self.expressionStack.pop()
                    t = tree(operator, self.lang, self.internalError)
                    t.operands([l, r])
                    self.expressionStack.push(t)
            return True
        return False
                

    #
    #  addOperator := "+" | "-" :=
    #

    def addOperator (self):
        if self.seenToken("+"):
            self.expressionStack.push('+')
            return True
        if self.seenToken("-"):
            self.expressionStack.push('-')
            return True
        return False


    #
    #  term := factor { mulOperator factor } =:
    #

    def term (self):
        if self.factor():
            while True:
                operator = "*"
                if self.mulOperator():
                    operator = self.expressionStack.pop()
                if self.factor():
                    r = self.expressionStack.pop()
                    l = self.expressionStack.pop()
                    t = tree(operator, self.lang, None)
                    t.operands([l, r])
                    self.expressionStack.push(t)
                else:
                    return True
        else:
            return False


    #
    #  mulOperator := "*" | "/"  =:
    #

    def mulOperator (self):
        if self.seenToken("*"):
            self.expressionStack.push('*')
            return True
        if self.seenToken("/"):
            self.expressionStack.push('/')
            return True
        return False


    #
    #  factor := "(" expression ")" | atom [ "^" factor ] =:
    #

    def factor (self):
        if self.seenToken('('):
            self.expression()
            self.expect(')')
            return True
        elif self.litorvar():
            if self.seenToken('^'):
                l = self.expressionStack.pop()
                self.factor()
                r = self.expressionStack.pop()
                t = tree('^', self.lang, None)
                t.operands([l, r])
                self.expressionStack.push(t)
            return True
        else:
            return False


    #
    #  litorvar - consume the current token and push it as an atom to the expression stack.
    #

    def litorvar (self):
        global tokens
        if not (self.tok in tokens):
            self.expressionStack.push(tree(self.tok, self.lang, None))
            self.tok = self.getNext()
            return True
        else:
            return False


    #
    #  seenToken - returns True if token, t, has been seen.  If True the token is consumed.
    #

    def seenToken (self, t):
        if t == self.tok:
            self.tok = self.getNext()
            return True
        else:
            return False


    def mystop (self):
        pass


    #
    #  expect - expects a token, t.
    #

    def expect (self, t):
        if t == self.tok:
            self.tok = self.getNext()
        else:
            internalError('expecting token ' + t + ' but found ' + self.tok)


    #
    #  internalError - displays the internal error message and the line of input
    #                  causing the problem and exit(1).
    #

    def internalError (self, message):
        stop()
        self.syntaxError(message)


    #
    #  syntaxError - issue an error message with source file coordination
    #

    def syntaxError (self, message):
        printHeader(self.inputFile, self.lineNo-1)
        i = self.skipLine(self.powerpos)
        print self.contents[self.powerpos:i]

        printHeader(self.inputFile, self.lineNo)
        i = self.skipLine(self.startpos)
        print self.contents[self.startpos:i]

        j = self.columnNo-len(self.tok)
        s = " " * j
        s += "^" * len(self.tok)
        s += " "
        s += message
        printHeader(self.inputFile, self.lineNo)
        print s
        sys.exit(1)


    #
    #  printToken - displays the current token position.
    #

    def printToken (self, token):
        printHeader(self.inputFile, self.lineNo-1)
        i = self.skipLine(self.powerpos)
        print self.contents[self.powerpos:i]

        printHeader(self.inputFile, self.lineNo)
        i = self.skipLine(self.startpos)
        print self.contents[self.startpos:i]

        j = self.columnNo-len(token)
        s = " " * j
        s += "^" * len(token)
        printHeader(self.inputFile, self.lineNo)
        print s
