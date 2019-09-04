#
#  Python script which tests whether we can catch a GNU Modula-2 exception
#
import libtiny

try:
    libtiny.doSomething(1)
    print("error  : should not arrive here")
except:
    print("success: caught exception in Python")
