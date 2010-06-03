#!/usr/bin/python

from twoDsim import *


b = box(0.0, 0.0, 1.0, 1.0)
b = fix(b)
c1 = circle(0.7, 0.7, 0.05)
c1 = mass(c1, 0.01)
c2 = circle(0.7, 0.1, 0.05)
c2 = mass(c2, 0.01)
c2 = fix(c2)
gravity(-9.81)
fps(24.0*4.0)
replayRate(24.0)
print "creating frames"
try:
    simulateFor(1.0)
    print "all done"
except:
    print "exception raised"
