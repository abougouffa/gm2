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
gap = 0.04

# pge.finish ()
# sys.exit (0)


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
    c = pge.box (x, y, w, w, wood_dark).on_collision (crate_split).set_param (6).mass (1.0)


def crate_split (p):
    global gap

    print "crate_split", p
    w = p.get_width () / 2
    wg = w - gap
    e = p.get_param ()
    if e != None:
        if e == 0:
            print "crate piece completely gone"
            # at the end of 6 collisions the crates disappear
            p.rm ()
            play_crack (p)
        elif e % 2 == 1:
            print "crate sub divides"
            # subdivide into smaller crates, every odd bounce
            m = p.get_mass ()
            print "mass of crate is", m
            c = p.get_colour ()
            print "colour of crate is", c
            x = p.get_xpos ()
            y = p.get_ypos ()
            for v in [[0, 0], [0, w], [w, 0], [w, w]]:
                print "creating sub box", v
                b = pge.box (v[0]+x, v[1]+y, wg, wg, c).mass (m).on_collision (crate_split)
                print "set_param", e-1
                b.set_param (e-1)
            print "play_crack", p
            play_crack (p)
            print "rm", p
            p.rm ()
        else:
            print "crate bounces without breaking"
            # allow collision (bounce) without splitting every even bounce
            p.set_param (e-1)
            play_bounce (p)

def main ():
    c = pge.circle (0.5, 0.5, 0.3, white, -1)
    l = pge.box (0.0, 0.25, 1.0, 0.02, wood_light, 1)
    b1, b2, b3, b4 = placeBoarders (boarder, wood_dark)
    b = placeBall (0.5, 0.5, 0.02)
    b.mass (1.0).on_collision (play_bounce).velocity (0.9, 0.0)
    # crate (0.7, 0.8, 0.1)
    print "before run"
    pge.gravity ()
    pge.dump_world ()
    pge.run (10.0)
    pge.finish ()

print "before main()"
main ()
