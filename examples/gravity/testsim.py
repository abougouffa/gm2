#!/usr/bin/python

from twoDsim import *


b = twoDsim_box(0.0, 0.0, 1.0, 1.0)
b = twoDsim_fix(b)
c = twoDsim_circle(0.4, 0.4, 0.05)
c = twoDsim_mass(c, 0.01)
twoDsim_gravity(9.6)
twoDsim_fps(24.0)
print "creating frames"
try:
    twoDsim_simulateFor(30.0)
    print "all done"
except:
    print "failed"
print b

