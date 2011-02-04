#!/usr/bin/python

# Copyright (C) 2011
#               Free Software Foundation, Inc.
# This file is part of GNU Modula-2.
#
# GNU Modula-2 is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
# 
# GNU Modula-2 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with GNU Modula-2; see the file COPYING.  If not, write to the
# Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA. 

import sys
import os
import string


# html tag
null_tag, title_tag, header1_tag, header2_tag, header3_tag, header4_tag, center_tag, paragraph_tag, preformatted_tag, teletype_tag, italic_tag = range(11)

# html state machine
init_state, known_state, end_state, copy_state, white_state, nf_state = range(6)

# all methods prefixed by _ are for internal use only.

class htmlDevice:
    #
    #  newFragment - saves the previous fragment and creates an empty new fragment.
    #
    def newFragment (self, dataType):
        self.fragmentList += [self.currentFragment]
        self.currentFragment = ""
        sys.stderr.write('[%d] ' % (len(self.fragmentList)))
        if (len(self.fragmentList) % 10) == 0:
            sys.stderr.write('\n')
    #
    #  puts - append, s, to the currentFragment.
    #
    def puts (self, s):
        self.currentFragment += s
    #
    #  getFragments - returns all fragments.
    #
    def getFragments (self):
        return self.fragmentList
    #
    #  emitTagName - write out tag names
    #
    def _emitTagName(self, s):
        if self.state == init_state:
            return
        self.puts(s)
        if self.tag == title_tag:
            self.puts('title>')
        elif self.tag == header1_tag:
            self.puts('h1>')
        elif self.tag == header2_tag:
            self.puts('h2>')
        elif self.tag == header3_tag:
            self.puts('h3>')
        elif self.tag == header4_tag:
            self.puts('h4>')
        elif self.tag == center_tag:
            self.puts('center>')
        elif self.tag == paragraph_tag:
            self.puts('p>')
        elif self.tag == preformatted_tag:
            self.puts('pre>')
        elif self.tag == teletype_tag:
            self.puts('tt>')
        elif self.tag == italic_tag:
            self.puts('i>')
    #
    #  emitTag - write out the start tag
    #
    def _emitTag(self):
        self._emitTagName('<')
    #
    #  emitTagEnd - write out the end tag
    #
    def _emitTagEnd(self):
        if self.state in [copy_state, white_state, nf_state]:
            self._emitTagName('</')
            if self.tag in [header1_tag, header2_tag, header3_tag, header4_tag, title_tag, center_tag, paragraph_tag, preformatted_tag]:
                self.puts('\n')
            if not (self.tag in [teletype_tag, italic_tag]):
                self.puts('\n')
    def _initState (self):
        self.state = init_state
    def __init__ (self):
        self.state = init_state
        self.tag = null_tag
        self.stack = []
        self.lastnewline = False
        self.fragmentList = []
        self.currentFragment = ""
        sys.stderr.write('[1] ')
    def _to (self, s):
        self.state = s
    def _status (self):
        return self.state
    def push (self):
        self.stack += [(self.state, self.tag)]
    def pop (self):
        self.state, self.tag = self.stack[-1]
        self.stack = self.stack[:-1]
        if self.state == white_state:
            self.state = copy_state
    def _twoNewlines (self, c):
        if (c == '\n') and (self.lastnewline) and (self.tag == paragraph_tag):
            self._end()
            self.paraBegin()
            return True
        else:
            self.lastnewline = (c == '\n')
            return False
    def _doChar (self, c):
        if self.state == known_state:
            if c in string.whitespace:
                return
            self._emitTag()
            self.puts(c)
            self.state = copy_state
        elif self.state == copy_state:
            if self._twoNewlines(c):
                return
            self.puts(c)
            if c in string.whitespace:
                self.state = white_state
        elif self.state == white_state:
            if self._twoNewlines(c):
                return
            if not (c in string.whitespace):
                self.puts(c)
                self.state = copy_state
        elif self.state == nf_state:
            self.puts(c)
    def _end (self):
        if (self.state == init_state) or (self.state == known_state):
            return
        self._emitTagEnd()
        self.lastnewline = False
        self.state = init_state
    def write (self, contents):
        if self.state == init_state:
            return
        for c in contents:
            self._doChar(c)
    def titleBegin(self):
        self._end()
        self.state = known_state
        self.tag = title_tag
    def titleEnd(self):
        self._end()
    def paraBegin(self):
        self._end()
        self.state = known_state
        self.tag = paragraph_tag
    def paraEnd(self):
        self._end()
    def centerBegin(self):
        self._end()
        self.state = known_state
        self.tag = center_tag
    def centerEnd(self):
        self._end()
    def h1Begin(self):
        self._end()
        self.state = known_state
        self.tag = header1_tag
    def h1End(self):
        self._end()
    def h2Begin(self):
        self._end()
        self.state = known_state
        self.tag = header2_tag
    def h2End(self):
        self._end()
    def h3Begin(self):
        self._end()
        self.state = known_state
        self.tag = header3_tag
    def h3End(self):
        self._end()
    def h4Begin(self):
        self._end()
        self.state = known_state
        self.tag = header4_tag
    def h4End(self):
        self._end()
    def preBegin(self):
        self._end()
        self.state = nf_state
        self.tag = preformatted_tag
        self._emitTag()
    def preEnd(self):
        self._end()
    def ttBegin(self):
        self.push()
        self.state = nf_state
        self.tag = teletype_tag
        self._emitTag()
    def ttEnd(self):
        self._end()
        self.pop()
    def iBegin(self):
        self.push()
        self.tag = italic_tag
        self._emitTag()
    def iEnd(self):
        self._end()
        self.pop()
    def centerBegin(self):
        self._end()
        self.state = known_state
        self.tag = center_tag
    def end(self):
        self._end()
        self.state = init_state
        self.tag = null_tag
    #
    #  tableBegin -
    #
    def tableBegin(self):
        self.puts("<dl>\n")
        self.firstItem = True
        self.tableRightOpen = False
    #
    #  tableEnd -
    #
    def tableEnd(self):
        self.puts("</dl>\n")
    #
    #  tableLeftBegin -
    #
    def tableLeftBegin(self):
        if self.firstItem:
            self.tableRightEnd()
        self.puts("<dt>")
    #
    #  tableLeftEnd -
    #
    def tableLeftEnd(self):
        self.firstItem = False
        self.puts("</dt>\n")
    #
    #  tableRightBegin -
    #
    def tableRightBegin(self):
        self._end()
        self.tableRightOpen = True
        self.puts("<dd>")
    #
    #  tableRightEnd -
    #
    def tableRightEnd(self):
        self._end()
        if self.tableRightOpen:
            self.puts("</dd>")
            self.tableRightOpen = False
