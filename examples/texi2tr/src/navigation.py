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


headerCount = 0
anchors = {}
nodes = {}

maxNoOfShortLabels = 10    # only allow this many different short tabs
maxLabelLength     = 90    # total character length of all menus

class nodeInfo:
    #
    #  record the texinfo nodes
    #
    def __init__ (self, me, n, p, u):
        self.name = me
        self.next = n
        self.prev = p
        self.up   = u
    def generateMenu (self, write, root, last):
        emitMenuTitle(write)
        emitNodeHeading(write)
        emitRootTab(root, write, last)
        write('</div>\n')
        write('need to create node for')
        write(self.name)
        write(self.next)
        write(self.prev)
        write(self.up)
        if (root != None) and (not root.isNode()) and (root._isShort()):
            for i in root.list:
                if i == self.name:
                    return i
        return last
    def _isShort (self):
        return False
    def _genShort (self, write, active):
        pass
    def isNode (self):
        return True
    def getName (self):
        return self.name


#
#  addNode - adds a node to the dictionary.
#

def addNode (line):
    global nodes

    w = line.split(',')
    if len(w)>4:
        w = w[:4]
    while len(w)<4:
        w += [""]
    me, n, p, u = w
    nodes[me] = nodeInfo(me, n, p, u)
    return nodes[me]

class menuInfo:
    #
    #  records the data contained in the menu texinfo sections
    #
    def __init__ (self, initial):
        self.list = []
        self.short = initial
    def isNode (self):
        return False
    #
    #  parseMenu - parse the content and populate the data structures.
    #
    def parseMenu (self, content):
        for line in content.split('\n'):
            l = line.lstrip().rstrip()
            if (len(l)>1) and (l[0] == '*'):
                l = l[1:]
                l = l.lstrip()
                i = l.find('::')
                if (i>0) and (i+2<len(l)):
                    m = l[i+2:].lstrip()
                    self.list += [[l[:i], m]]
    #
    #  debugMenu - dump the data structures
    #
    def debugMenu (self):
        for m in self.list:
            print m[0], m[1]
    #
    #  generateMenu - issues the 
    #
    def generateMenu (self, write, root, last):
        # emitMenuTitle(write)
        # emitNodeHeading(write)
        # emitRootTab(root, write, last)
        if (root != self) and (not self._isShort()):
            self._genLong(write)
        return last
    #
    #  isShort - return True if the menu can be encoded in a short form.
    #
    def _isShort (self):
        global maxNoOfShortLabels, maxLabelLength

        if (len(self.list)>maxNoOfShortLabels) or (not self.short):
            return False
        t = 0
        for m in self.list:
            t += len(m[0])
        return t<maxLabelLength
    #
    #  genShort - generate a list of tabs for the short menu
    #
    def _genShort (self, write, active):
        write('<div id="tabmenu">\n')
        write('<ul id="tab">\n')
        for m in self.list:
            if anchors.has_key(m[0]):
                active = litab(write, anchors[m[0]], m[0], active)
            else:
                if (len(m[1]) > 1) and (m[1][-1] == '.') and (anchors.has_key(m[1][:-1])):
                    active = litab(write, anchors[m[1][:-1]], m[0], active)
                elif anchors.has_key(m[1]):
                    active = litab(write, anchors[m[1]], m[0], active)
                else:
                    print "cannot find anchor for section", m[0], "or", m[1]
        write('\n</ul>\n')
        write('</div>\n')
    #
    #  genLong - generate a unordered list for the short menu
    #
    def _genLong (self, write):
        write('\n<ul>\n')
        for m in self.list:
            if anchors.has_key(m[0]):
                liurl(write, anchors[m[0]], m[1])
            else:
                if (len(m[1]) > 1) and (m[1][-1] == '.') and (anchors.has_key(m[1][:-1])):
                    liurl(write, anchors[m[1][:-1]], m[1])
                elif anchors.has_key(m[1]):
                    liurl(write, anchors[m[1]], m[1])
                else:
                    print "cannot find anchor for section", m[0], "or", m[1]
        write('</ul>\n')


#
#
#
def emitRootTab (root, write, last):
    if root == None:
        return
    if root._isShort():
        root._genShort(write, last)


#
#  anchor - adds an anchor to the output and enters it into our dictionary
#

def anchor (write, label):
    global anchors, headerCount
    headerCount += 1
    s = '<a name="SEC%d"></a>\n' % headerCount
    anchors[label] = "#SEC%d" % headerCount
    write(s)

#
#  liurl - issue a <li> url </li> as long as text is not empty.
#

def liurl (write, link, text):
    if text != "":
        write('<li>\n')
        url(write, link, text)
        write('</li>\n')


#
#  url - issue a link providing that text is non nul.
#

def url (write, link, text):
    if text != "":
        write('<a href="')
        write(link)
        write('">')
        write(text)
        write('</a>')

#
#  litab - issue a tab link providing that text is non nul.
#

def litab (write, link, text, active):
    if text != "":
        if (active == ""):
            write('<li class="selected">')
            active = text
        elif active == text:
            write('<li class="selected">')
        else:
            write('<li>')
        write('<a href="')
        write(link)
        write('"><span>')
        write(text)
        write('</span></a></li>\n')
        return active

#
#  safeOpen - 
#

def safeOpen (f, description):
    fn = os.path.join('/home/gaius/GM2/graft-4.1.2/gcc-4.1.2/gcc/gm2/examples/texi2tr/test', f)
    if os.path.exists(fn) and os.path.isfile(fn):
        try:
            return open(fn, 'r').read()
        except:
            print "cannot open", description
            return ""
    else:
        print "cannot open", description
        return ""


#
#  emitMenuTitle -
#

def emitMenuTitle (write):
    write(safeOpen('title.ht', 'title template "title.ht"'))


#
#  emitNodeHeading -
#

def emitNodeHeading (write):
    write(safeOpen('heading.ht', 'heading template "heading.ht"'))

#
#
#

def getRoot (menus):
    for m in menus:
        if (not m.isNode()) and (m._isShort()):
            return m
    return None
