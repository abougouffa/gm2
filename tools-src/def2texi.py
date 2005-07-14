#!/usr/bin/python

import sys
import os
import glob
import string

libraryClassifications = [['gm2-libs','Base libraries',
                           'Basic M2F compatible libraries'],
                          ['gm2-libs-pim','PIM and Logitech 3.0 Compatible',
                           'PIM and Logitech 3.0 compatible libraries'],
                          ['gm2-libs-coroutines','PIM coroutine support',
                           'PIM compatible process support'],
                          ['gm2-iso','M2 ISO Libraries',
                           'ISO defined libraries'],
                          ['ulm-lib-gm2/sys','ULM System Libraries',
                           'ULM System libraries'],
                          ['ulm-lib-gm2/std','ULM Standard Libraries',
                           'ULM Standard libraries']]


#
#  displayLibraryClass - displays a node for a library directory and invokes
#                        a routine to summarize each module
#

def displayLibraryClass():
    up = "Libraries"
    previous = ""

    if len(sys.argv)>1:
        up=sys.argv[1]


    next=libraryClassifications[1][1]
    i = 0
    l = libraryClassifications[i]

    while 1:
        print "@node " + l[1] + ", " + next + ", " + previous + ", " + up
        print "@section " + l[1]
        print ""
        displayModules(l[1], l[0])
        print ""
        print "@c ---------------------------------------------------------------------"
        previous = l[1]
        i = i+1
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
    words = string.split(line)
    procedure = ""
    if (len(words)>1) and (words[0] == "PROCEDURE"):
        if (words[1] == "__BUILTIN__") and (len(words)>2):
            procedure = words[2]
        else:
            procedure = words[1]

    if procedure != "":
        name = string.split(procedure, "(")                
        if name[0] != "":
            print "@findex " + name[0]


#
#  parseDefinition
#

def parseDefinition (dir, file):
    print ""
    f = open(os.path.join(dir, file), 'r')
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
    
    while 1:
        line = f.readline()
        if not line: break
	line = string.rstrip(line)
	checkIndex(line)
        print string.replace(string.replace(line, "{", "@{"), "}", "@}")
    print "@end example"
    print "@page"
    f.close()

def parseModules (up, dir, listOfModules):
    previous = ""
    i = 0
    if len(listOfModules)>1:
        next = dir + "/" + listOfModules[1][:-4]
    else:
        next = ""

    while i<len(listOfModules):
       print "@node " + dir + "/" + listOfModules[i][:-4] + ", " + next + ", " + previous + ", " + up
       print "@subsection " + dir + "/" + listOfModules[i][:-4]
       parseDefinition(dir, listOfModules[i])
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

def displayModules(up, dir):
    if os.path.isdir(dir):
        if os.path.exists(os.path.join(dir, "README.texi")):
            doCat(dir, "README.texi")

        moduleMenu(dir)
        listOfModules = []
        listOfFiles = os.listdir(dir)
        listOfFiles.sort()
        for file in listOfFiles:
            if os.path.isfile(os.path.join(dir, file)):
                if (len(file)>4) and (file[-4:] == '.def'):
                    listOfModules = listOfModules + [file]
        parseModules(up, dir, listOfModules)
    else:
        print "directory " + dir + " not found"

displayMenu()
displayLibraryClass()
