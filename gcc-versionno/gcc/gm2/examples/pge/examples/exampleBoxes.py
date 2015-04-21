#!/usr/bin/env python

import pge, sys
# import pgemacro

print "starting exampleBoxes"
pge.batch ()

t = pge.rgb (1.0/2.0, 2.0/3.0, 3.0/4.0)
wood_light = pge.rgb (166.0/256.0, 124.0/256.0, 54.0/256.0)
wood_dark = pge.rgb (76.0/256.0, 47.0/256.0, 0.0)
red = pge.rgb (1.0, 0.0, 0.0)
metal = pge.rgb (0.5, 0.5, 0.5)
ball_size = 0.04
boarder = 0.01
white = pge.rgb (1.0, 1.0, 1.0)

pge.finish ()
sys.exit (0)


def play_wood (o):
    pge.play ("/home/gaius/Sandpit/penguin-tower/sounds/brokenglass.wav")

def play_crack (o):
    pge.play ("/home/gaius/Sandpit/penguin-tower/sounds/brokenglass.wav")

def play_bounce (o):
    print "callback has been called"
    pge.play ("/home/gaius/Sandpit/cluedo/sounds/cardsnap.wav")

def placeBoarders (thickness, color):
    print "placeBoarders"
    e1 = pge.box (0.0, 0.0, 1.0, thickness, color).fix ()
    e2 = pge.box (0.0, 0.0, thickness, 1.0, color).fix ()
    e3 = pge.box (1.0-thickness, 0.0, thickness, 1.0, color).fix ()
    e4 = pge.box (0.0, 1.0-thickness, 1.0, thickness, color).fix ()
    for e in [e1, e2, e3, e4]:
        e.on_collision (play_wood)
    return e1, e2, e3, e4


def placeBall (x, y, r):
    return pge.circle (x, y, r, metal)


def crate (x, y, w):
    c = pge.box (x, y, w, w, wood_dark).on_collision (crate_split).set_param (6)


def crate_split (p):
    w = p.width () / 2
    wg = w - gap
    e = p.get_param ()
    if e != None:
        if e == 0:
            # at the end of 6 collisions the crates disappear
            p.delete ()
            play_crack ()
        elif e % 2 == 1:
            # subdivide into smaller crates, every odd bounce
            m = p[0].get_mass ()
            c = p[0].get_colour ()
            for v in [[0, 0], [0, w], [w, 0], [w, w]]:
                b = pge.box (v[0], v[1], wg, wg, c).mass (m).on_collision (crate_split)
                b.set_param (e-1)
            p.delete ()
            play_crack ()
        else:
            # allow collision (bounce) without splitting every even bounce
            p.set_param (e-1)
            play_bounce ()

def main ():
    c = pge.circle (0.5, 0.5, 0.3, white, -1)
    b1, b2, b3, b4 = placeBoarders (boarder, wood_dark)
    b = placeBall (0.5, 0.5, 0.02)
    b.mass (1.0).on_collision (play_bounce)
    # b = b.fix ()
    # crate (0.5, 0.5, 0.2)
    print "before run"
    pge.gravity ()
    pge.dump_world ()
    pge.run (10.0)
    pge.finish ()

print "before main()"
main ()
