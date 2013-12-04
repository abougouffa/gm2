#
#  Python script which tests whether we can catch a GNU Modula-2 exception
#
import tiny
import sys

for i in range(10):
    try:
        tiny.tiny_doSomething(i)
        if i != 0:
            print "error exception should have been thrown"
    except:
        if i == 0:
            print "error incorrect value of i", i

print "stresstiny passed"
