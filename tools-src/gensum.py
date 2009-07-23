#!/usr/bin/python

# Copyright (C) 2007, 2008, 2009
# Free Software Foundation, Inc.
# This file is part of GNU Modula-2.
#
# GNU Modula-2 is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
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
#

import sys
import os
import glob
import string
import sys, getopt


class testcase:
    name = ""
    passes = []
    fails = []
    unresolved = []
    def __init__(self, n):
        self.name = n
        self.passes = []
        self.fails = []
        self.unresolved = []
    def addPass(self, p):
        self.passes += [p]
    def addFail(self, p):
        self.fails += [p]
    def addUnresolved(self, p, r):
        self.unresolved += [p, r]
    def getPasses(self):
        return self.passes
    def getFails(self):
        return self.fails
    def getUnresolved(self):
        return self.unresolved


#
#  Usage - displays the usage
#

def Usage ():
    global noColumns

    print "gensum [-h] [-c number] filename.sum {filename.sum}"
    print "  -c number of columns per architectural table (default", noColumns, ")"
    print "  -h help"
    sys.exit(0)


#
#  collectArgs - collects the arguments supplied and places
#                useful contents into global variables.
#

def collectArgs():
    global noColumns
    try:
        optlist, list = getopt.getopt(sys.argv[1:],':hc:')
    except getopt.GetoptError:
        Usage()
    for opt in optlist:
        if opt[0] == '-h':
            Usage()
        if opt[0] == '-c':
            noColumns = int(opt[1])
    return list


#
#  scanner - generic function to read in a file, name,
#            and call, function, on each line read.
#

def scanner(name, function):
    file = open(name, 'r')
    line = file.readline()
    while line:
        function(line)
        line = file.readline()
    file.close()

#
#  addPassResult - adds the pass information into the global dictionary.
#

def addPassResult (name, varient):
    global regressionTests, configuration

    if not regressionTests.has_key(configuration):
        regressionTests[configuration] = {}

    arch = regressionTests[configuration]
    if arch.has_key(name):
        t = arch[name]
    else:
        t = testcase(name)
    t.addPass(varient)
    arch[name] = t
    regressionTests[configuration] = arch


#
#  addFailResult - adds the fail information into the global dictionary.
#

def addFailResult (name, varient):
    global regressionTests, configuration

    if not regressionTests.has_key(configuration):
        regressionTests[configuration] = {}

    arch = regressionTests[configuration]
    if arch.has_key(name):
        t = arch[name]
    else:
        t = testcase(name)
    t.addFail(varient)
    arch[name] = t
    regressionTests[configuration] = arch


#
#  addUnresolvedResult - adds the unresolved information into the global dictionary.
#

def addUnresolvedResult (name, varient, reason):
    global regressionTests, configuration

    if not regressionTests.has_key(configuration):
        regressionTests[configuration] = {}

    arch = regressionTests[configuration]
    if arch.has_key(name):
        t = arch[name]
    else:
        t = testcase(name)
    t.addUnresolved(varient, reason)
    arch[name] = t
    regressionTests[configuration] = arch

#
#  getName - returns the GM2 CVS testcase path
#

def getName (testcase, directory):
    words = string.split(directory, '/')
    result = ""
    found = False
    for word in words:
        if word == "testsuite":
            found = True
            result = word
        elif word == "gm2.exp":
            pass
        elif found:
            result = "%s/%s" % (result, word)
    words = string.split(testcase, '/')
    name = "%s/%s" % (result, words[-1])
    if name[-1] == ',':
        name = name[:-1]
    return name

#
#  processLine - 
#

def processLine(line):
    global author, date, configuration, target, directory
    words = string.split(line)
    # Test Run By xxxx on
    if (len(words)>=4) and (words[:3] == ["Test", "Run", "By"]):
        author = words[3]
        if (len(words)>=6) and (words[4] == "on"):
            date = words[-5:]
    elif (len(words)>=4) and (words[:3] == [ "Native", "configuration", "is"]):
        configuration = words[3]
    elif (len(words)>=3) and (words[:2] == [ "Running", "target"]):
        target = words[2]
    elif (len(words)>=2) and (words[0] == "Running"):
        directory = words[1]
    elif len(words)>1:
        testcase = words[1]
        varient = []
        reason = ""
        if words[0]=="PASS:":
            if len(words)>=2:
                varient = words[2:]
            addPassResult(getName(testcase, directory), varient)
        elif words[0]=="FAIL:":
            if len(words)>=2:
                varient = words[2:]
            addFailResult(getName(testcase, directory), varient)
        elif words[0]=="UNRESOLVED:":
            if len(words)>2:
                start = -1
                if words[-1][-1]==')':
                    while (-start<len(words)) and (words[start][0] != '('):
                        start -= 1
                varient = words[2:start]
                reason = words[start:]
            addUnresolvedResult(getName(testcase, directory), varient, reason)


#
#  printRow - prints out a table data entry for architecture and option.
#

def printRow (testcase, arch, option):
    if regressionTests[arch].has_key(testcase):
        t = regressionTests[arch][testcase]
        if option in t.getPasses():
            print '<td bgcolor="green">', string.join(option, ' '), '</td>',
        elif option in t.getFails():
            print '<td bgcolor="red">', string.join(option, ' '), '</td>',
        elif option in t.getUnresolved():
            print '<td bgcolor="yellow">', string.join(option, ' '), '</td>',
        elif option == []:
            print '<td></td>',
        else:
            print '<td></td>',
    else:
        print '<td></td>',


#
#  getListOfTests - returns the list of all tests
#

def getListOfTests ():
    global regressionTests

    list = []
    for arch in regressionTests.keys():
        t = regressionTests[arch]
        for u in t.keys():
            if not (u in list):
                list += [u]
    return list


#
#  getListOfOptions - returns the (total, optlist) for testcase
#                     in the regressionTests
#

def getListOfOptions (testcase):
    global regressionTests

    optlist = []
    total = 0
    for arch in regressionTests.keys():
        t = regressionTests[arch]
        if t.has_key(testcase):
            u = t[testcase]
            for p in u.getPasses() + u.getFails() + u.getUnresolved():
                if not (p in optlist):
                    optlist += [p]
    return len(optlist), optlist

#
#  getHeading - returns a URL to the testcase.
#

def getHeading (testcase):
    noFiles = ['pimlib/ulm', 'pimlib/pass', 'ulmlib/pass', 'ulmlib/std',
               'ulmlib/sys', 'gm2/examples']
    for n in noFiles:
        if testcase.find(n) != -1:
            return testcase
    heading = '<a href="http://cvs.savannah.nongnu.org/viewvc/%s' % testcase
    heading += '?root=gm2&view=markup">'
    heading += testcase + '</a>'
    return heading

#
#  printResults - prints the resuls in a html tabular form
#

def printResults():
    global target, configuration, author, date, regressionTests, noColumns

    print "<html><head><title>"
    print "GNU Modula-2 regression tests"
    print "</title></head>"
    print ""
    print "<h1>",
    print "GNU Modula-2 regression tests",
    print "</h1>"
    print ""
    print '<p><table border="1"><tr>'
    print '<th colspan="2">Key</th>'
    print '<tr><td>Colour</td><td>Meaning</td></tr>'
    print '<tr><td bgcolor="green"></td><td>Pass</td></tr>'
    print '<tr><td bgcolor="red"></td><td>Fail</td></tr>'
    print '<tr><td bgcolor="yellow"></td><td>Unresolved due to a prior error</td></tr>'
    print '<tr><td bgcolor="blue"></td><td>Not tested</td></tr>'
    print '<tr><td></td><td>Entire testcase not tested on this platform</td></tr>'
    print '</table>'
    archList = regressionTests.keys()
    testlist = getListOfTests()
    for testcase in testlist:
        total, optlist = getListOfOptions(testcase)
        if total>0:
            print '<p><table border="1"><tr>'
            print '<th colspan="', len(archList)*noColumns, '">',
            heading = getHeading(testcase)
            print heading, '</th></tr>'
            for arch in archList:
                print '<th colspan="', noColumns, '">', arch, '</th>',

            if total % noColumns != 0:
                total = ((total / noColumns) +1) * noColumns
            for count in range(0, total, noColumns):
                print '<tr>',
                for arch in archList:
                    for c in range(count, count+noColumns):
                        if c < len(optlist):
                            printRow(testcase, arch, optlist[c])
                        else:
                            printRow(testcase, arch, [])
                print '</tr>'
            print '</table></p>'
    print '</html>'
    

target = ""
configuration = ""
author = ""
date = ""
regressionTests = {}
noColumns = 3
directory = ""


#
#  main - collects the arguments and reads in each summary file
#         in turn populating the architecture dictionary in turn.
#

def main():
    global regressionTests
    
    filenames = collectArgs()
    if filenames==[]:
        Usage()
    else:
        for file in filenames:
            scanner(file, processLine)
        printResults()

main()
