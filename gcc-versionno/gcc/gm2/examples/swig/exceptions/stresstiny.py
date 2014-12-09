#
#  Python script which tests whether we can catch a GNU Modula-2 exception
#
import libtiny
import sys

for i in range (10):
    try:
        libtiny.doSomething (i)
        print "error exception should have been thrown"
    except:
        print "caught exception in Python:  i =", i

print "stresstiny passed"
