#!/usr/bin/python

# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
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
import getopt

libraryClassifications = [['gm2-libs','Base libraries',
                           'Basic M2F compatible libraries'],
                          ['gm2-libs-pim','PIM and Logitech 3.0 Compatible',
                           'PIM and Logitech 3.0 compatible libraries'],
                          ['gm2-libs-coroutines','PIM coroutine support',
                           'PIM compatible process support'],
                          ['gm2-libs-iso','M2 ISO Libraries',
                           'ISO defined libraries'],
                          ['ulm-lib-gm2/sys','ULM System Libraries',
                           'ULM System libraries'],
                          ['ulm-lib-gm2/std','ULM Standard Libraries',
                           'ULM Standard libraries']]

def initState ():
    global inVar, inType, inConst
    inVar, inType, inConst = False, False, False


#
#  displayLibraryClass - displays a node for a library directory and invokes
#                        a routine to summarize each module
#

def displayLibraryClass():
    global buildDir
    up = "Libraries"
    previous = ""

    if len(sys.argv)>1:
        up=sys.argv[1]


    next=libraryClassifications[1][1]
    i = 0
    l = libraryClassifications[i]

    while True:
        print "@node " + l[1] + ", " + next + ", " + previous + ", " + up
        print "@section " + l[1]
        print ""
        displayModules(l[1], l[0], os.path.join(buildDir, l[0]))
        print ""
        print "@c ---------------------------------------------------------------------"
        previous = l[1]
        i += 1
        if i == len(libraryClassifications):
            break
        l = libraryClassifications[i]
        if i+1 == len(libraryClassifications):
            next = ""
        else:
            next = libraryClassifications[i+1][1]

#
#  displayMenu - displays the top level menu for library documentation
#

def displayMenu():
    print "@menu"
    for l in libraryClassifications:
        print "* " + l[1] + "::" + l[2]
    print "@end menu"

    print "\n"
    print "@c ====================================================================="
    print "\n"


#
#  removeInitialComments - removes any (* *) at the top of the definition module
#

def removeInitialComments (file, line):
    while (string.find(line, "*)") == -1):
        line = file.readline()
        
#
#  removeFields - removes Author/Date/Last edit/SYSTEM/Revision fields from a comment within the start
#                 of a definition module
#

def removeFields (file, line):
    while (string.find(line, "*)") == -1):
        if (string.find(line, "Author") != -1) and (string.find(line, ":") != -1):
            line = file.readline()
        elif (string.find(line, "Last edit") != -1) and (string.find(line, ":") != -1):
            line = file.readline()
        elif (string.find(line, "LastEdit") != -1) and (string.find(line, ":") != -1):
            line = file.readline()
        elif (string.find(line, "Last update") != -1) and (string.find(line, ":") != -1):
            line = file.readline()
        elif (string.find(line, "Date") != -1) and (string.find(line, ":") != -1):
            line = file.readline()
        elif (string.find(line, "Title") != -1) and (string.find(line, ":") != -1):
            line = file.readline()
        elif (string.find(line, "Revision") != -1) and (string.find(line, ":") != -1):
            line = file.readline()
        elif (string.find(line, "System") != -1) and (string.find(line, ":") != -1) and (string.find(line, "Description:") == -1):
            line = file.readline()
        elif (string.find(line, "SYSTEM") != -1) and (string.find(line, ":") != -1) and (string.find(line, "Description:") == -1):
            line = file.readline()
        else:
	    print string.replace(string.replace(string.rstrip(line),
                                            "{", "@{"), "}", "@}")
            line = file.readline()
    print string.rstrip(line)


#
#  checkIndex
#

def checkIndex (line):
    global inVar, inType, inConst
    
    words = string.split(line)
    procedure = ""
    if (len(words)>1) and (words[0] == "PROCEDURE"):
        inConst = False
        inType = False
        inVar = False
        if (words[1] == "__BUILTIN__") and (len(words)>2):
            procedure = words[2]
        else:
            procedure = words[1]

    if (len(line)>1) and (line[0:2] == '(*'):
        inConst = False
        inType = False
        inVar = False
    elif line == "VAR":
        inConst = False
        inVar = True
        inType = False
        return
    elif line == "TYPE":
        inConst = False
        inType = True
        inVar = False
        return
    elif line == "CONST":
        inConst = True
        inType = False
        inVar = False

    if inVar:
        words = string.split(line, ',')
        for word in words:
            word = string.lstrip(word)
            if word != "":
                if string.find(word, ':') == -1:
                    print "@findex " + word + " (var)"
                elif len(word)>0:
                    var = string.split(word, ':')
                    if len(var)>0:
                        print "@findex " + var[0] + " (var)"

    if inType:
        words = string.lstrip(line)
        if string.find(words, '=') != -1:
            word = string.split(words, "=")
            if (len(word[0])>0) and (word[0][0] != '_'):
                print "@findex " + string.rstrip(word[0]) + " (type)"
        else:
            word = string.split(words)
            if (len(word)>1) and (word[1] == ';'):
                # hidden type
                if (len(word[0])>0) and (word[0][0] != '_'):
                    print "@findex " + string.rstrip(word[0]) + " (type)"

    if inConst:
        words = string.split(line, ';')
        for word in words:
            word = string.lstrip(word)
            if word != "":
                if string.find(word, '=') != -1:
                    var = string.split(word, '=')
                    if len(var)>0:
                        print "@findex " + var[0] + " (const)"

    if procedure != "":
        name = string.split(procedure, "(")
        if name[0] != "":
            proc = name[0]
            if proc[-1] == ";":
                proc = proc[:-1]
            if proc != "":
                print "@findex " + proc


#
#  parseDefinition
#

def parseDefinition (dir, build, file):
    print ""
    if os.path.exists(os.path.join(build, file)):
        f = open(os.path.join(build, file), 'r')
    else:
        f = open(os.path.join(dir, file), 'r')
    initState()
    line = f.readline()
    while (string.find(line, "(*") != -1):
        removeInitialComments(f, line)
        line = f.readline()

    while (string.find(line, "DEFINITION") == -1):
        line = f.readline()

    print "@example"
    print string.rstrip(line)
    line = f.readline()
    if len(string.rstrip(line)) == 0:
        print string.replace(string.replace(string.rstrip(line),
                                            "{", "@{"), "}", "@}")
        line = f.readline()
        if (string.find(line, "(*") != -1):
            removeFields(f, line)
        else:
            print string.rstrip(line)
    else:
        print string.rstrip(line)

    line = f.readline()
    while line:
	line = string.rstrip(line)
	checkIndex(line)
        print string.replace(string.replace(line, "{", "@{"), "}", "@}")
        line = f.readline()
    print "@end example"
    print "@page"
    f.close()

def parseModules (up, dir, build, listOfModules):
    previous = ""
    i = 0
    if len(listOfModules)>1:
        next = dir + "/" + listOfModules[1][:-4]
    else:
        next = ""

    while i<len(listOfModules):
       print "@node " + dir + "/" + listOfModules[i][:-4] + ", " + next + ", " + previous + ", " + up
       print "@subsection " + dir + "/" + listOfModules[i][:-4]
       parseDefinition(dir, build, listOfModules[i])
       print "\n"
       previous = dir + "/" + listOfModules[i][:-4]
       i = i + 1
       if i+1<len(listOfModules):
           next = dir + "/" + listOfModules[i+1][:-4]
       else:
           next = ""


#
#  doCat - displays the contents of dir/file to stdout
#

def doCat (dir, file):
    file = open(os.path.join(dir, file), 'r')
    while 1:
        line = file.readline()
        if not line: break
        print string.rstrip(line)
    file.close()


#
#  moduleMenu - generates a simple menu for all definition modules
#               in dir
#

def moduleMenu (dir):
    print "@menu"
    listOfFiles = os.listdir(dir)
    listOfFiles.sort()
    for file in listOfFiles:
        if os.path.isfile(os.path.join(dir, file)):
            if (len(file)>4) and (file[-4:] == '.def'):
                print "* " + dir + "/" + file[:-4] + "::" + file
    print "@end menu"
    print "\n"

#
#  displayModules - walks though the files in dir and parses
#                   definition modules and includes README.texi
#

def displayModules(up, dir, build):
    if os.path.isdir(dir):
        if os.path.exists(os.path.join(dir, "README.texi")):
            doCat(dir, "README.texi")

        moduleMenu(dir)
        listOfModules = []
        if not os.path.exists(build):
            build = dir
            listOfFiles = os.listdir(build)
        else:
            listOfFiles = os.listdir(dir) + os.listdir(build)
        listOfFiles.sort()
        listOfFiles = dict.fromkeys(listOfFiles).keys()
        for file in listOfFiles:
            if os.path.isfile(os.path.join(build, file)):
                if (len(file)>4) and (file[-4:] == '.def'):
                    listOfModules = listOfModules + [file]
            elif os.path.isfile(os.path.join(dir, file)):
                if (len(file)>4) and (file[-4:] == '.def'):
                    listOfModules = listOfModules + [file]
        parseModules(up, dir, build, listOfModules)
    else:
        print "directory " + dir + " not found"

def displayCopyright ():
    print "@c Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008"
    print "@c Free Software Foundation, Inc."
    print """
@c Permission is granted to copy, distribute and/or modify this document
@c under the terms of the GNU Free Documentation License, Version 1.2 or
@c any later version published by the Free Software Foundation.
"""

def Usage():
    print "def2texi.py [-h][-bbuilddir]"
    
def collectArgs():
    buildDir="."
    try:
        optlist, list = getopt.getopt(sys.argv[1:],':hb:')
    except getopt.GetoptError:
        Usage()
        os.exit(1)
    for opt in optlist:
        if opt[0] == '-h':
            Usage()
        if opt[0] == '-b':
            buildDir = opt[1]
    return buildDir


buildDir = collectArgs()
displayCopyright()
displayMenu()
displayLibraryClass()
