#!/usr/bin/python

import sys
import os
import glob
import string

libraryClassifications = [['gm2-libs','PIM Compatible',
                           'Basic PIM Compatible and additional libraries'],
                          ['gm2-libs-coroutines','PIM Coroutines',
                           'PIM Coroutine specific modules'],
                          ['gm2-iso','M2 ISO Libraries',
                           'ISO libraries as defined in the 4th Working Draft']]

#
#  displayLibraryClass - displays a node for a library directory and invokes
#                        a routine to summarize each module
#

def displayLibraryClass():
    up = "Top"
    previous = ""

    if len(sys.argv)>1:
        up=sys.argv[1]


    next=libraryClassifications[1][0]
    i = 0
    l = libraryClassifications[i]

    while 1:
        print "@node " + l[0] + ", " + next + ", " + previous + ", " + up
        print ""
        displayModules(l[0])
        print ""
        print "@c ---------------------------------------------------------------------"
        previous = l[0]
        i = i+1
        if i == len(libraryClassifications):
            break
        l = libraryClassifications[i]
        if i+1 == len(libraryClassifications):
            next = ""
        else:
            next = l[0]

#
#  displayMenu - displays the top level menu for library documentation
#

def displayMenu():
    print "@menu"
    for l in libraryClassifications:
        print "  * " + l[1] + "::" + l[2]
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
#  parseDefinition
#

def parseDefinition (dir, file):
    print ""
    f = open(os.path.join(dir, file), 'r')
    line = f.readline()
    while (string.find(line, "(*") != -1):
        removeInitialComments(f, line)
        line = f.readline()

    while 1:
        line = f.readline()
        if not line: break
        print string.rstrip(line)
    f.close()

def parseModules (dir, listOfModules):
    previous = ""
    up = dir
    i = 0
    if len(listOfModules)>1:
        next = listOfModules[1]
    else:
        next = ""

    while i<len(listOfModules):
       print "@node " + listOfModules[i] + ", " + next + ", " + previous + ", " + up
       parseDefinition(dir, listOfModules[i])
       print "\n"
       previous = listOfModules[i]
       i = i + 1
       if i+1<len(listOfModules):
           next = listOfModules[i]
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
    for file in os.listdir(dir):
        if os.path.isfile(os.path.join(dir, file)):
            if (len(file)>4) and (file[-4:] == '.def'):
                print "  * " + file[:-4] + "::"
    print "@end menu"
    print "\n"

#
#  displayModules - walks though the files in dir and parses
#                   definition modules and includes README.texi
#

def displayModules(dir):
    if os.path.isdir(dir):
        if os.path.exists(os.path.join(dir, "README.texi")):
            doCat(dir, "README.texi")

        moduleMenu(dir)
        listOfModules = []
        for file in os.listdir(dir):
            if os.path.isfile(os.path.join(dir, file)):
                if (len(file)>4) and (file[-4:] == '.def'):
                    listOfModules = listOfModules + [file]
        parseModules(dir, listOfModules)
    else:
        print "directory " + dir + " not found"

displayMenu()
displayLibraryClass()
