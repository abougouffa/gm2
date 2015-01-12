#!/usr/bin/env python

import pge
# import pgemacro

print "starting exampleBoxes"

wood_light = pge.rgb (166.0/256.0, 124/256.0, 54.0/256.0)
wood_dark = pge.rgb (76.0/256.0, 47.0/256.0, 0.0)
metal = pge.rgb (0.5, 0.5, 0.5)
ball_size = 0.04
boarder = 0.01


def play_wood ():
    pass

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
    b = pge.circle (x, y, r, metal)


def crate (x, y, w):
    c = pge.box (x, y, w, w, wood_dark, 6).on_collision (crate_split)


def crate_split0 (p):
    w = p.width () / 2
    wg = w - gap
    e = p.get_extra ()
    if e != None:
        if e % 2 == 1:
            # subdivide into smaller crates
            c = []
            m = p[0].mass ()
            for v in [[0, 0], [0, w], [w, 0], [w, w]]:
                b = pge.box (x+v[0], y+v[1], wg, wg, e-1).mass (m).on_collision (crate_split)
                c += [b]
        elif e == 0:
            p.delete ()
        else:
            # allow collision bounce
            p.set_extra (e-1)


def crate_split (p):
    if len (p)>0:
        for c in p:
            crate_split0 (c)


def main ():
    b1, b2, b3, b4 = placeBoarders (boarder, wood_dark)

print "before main()"
main ()
