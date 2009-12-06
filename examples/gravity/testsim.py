#!/usr/bin/python

from twoDsim import *


b = box(0.0, 0.0, 1.0, 1.0)
b = fix(b)
c = circle(0.7, 0.7, 0.05)
c = mass(c, 0.01)
gravity(-9.81)
fps(24.0)
print "creating frames"
try:
    simulateFor(0.5)
    print "all done"
except:
    print "exception raised"
