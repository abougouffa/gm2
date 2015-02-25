#!/usr/bin/env python

import pgeif
import pygame

colour_t, box_t, circle_t = range (3)
id2ob = {}
ob2id = {}
groff_d, pyg_d = range (2)
device = None


#
#  printf - keeps C programmers happy :-)
#

def printf (format, *args):
    print str (format) % args,


class object:
    def __init__ (self, t, o):
        self.deleted = False
        self.type = t
        self.o = o
        self.fixed = False

    def _id (self):
        return self.o

    def velocity (self, vx, vy):
        self._check_type ([box_t, circle_t], "assign a velocity to a")
        self._check_not_fixed ("assign a velocity")
        self._check_not_deleted ("a velocity")
        self.o = pgeif.velocity (self.o, vx, vy)
        return self

    def accel (self, ax, ay):
        self._check_type ([box_t, circle_t], "assign an acceleration to a")
        self._check_not_fixed ("assign an acceleration")
        self._check_not_deleted ("an acceleration")
        self.o = pgeif.accel (self.o, vx, vy)
        return self

    def fix (self):
        self._check_type ([box_t, circle_t], "fix a")
        self._check_not_deleted (" a fixed position")
        self.fixed = True
        self.o = pgeif.fix (self.o)
        return self

    def mass (self, m):
        self._check_type ([box_t, circle_t], "assign a mass to a")
        self._check_not_fixed ("assign a mass")
        self._check_not_deleted (" a mass")
        self.o = pgeif.mass (self.o, m)
        return self

    def on_collision_with (self, another, p):
        self.collisionp = p
        self.collisionWith = another
        return self

    def on_collision (self, p):
        self.on_collision_with ([], p)
        return self

    def _check_type (self, legal, message):
        if not self.type in legal:
            printf ("you cannot %s %s object\n", message, self._type_name ())

    def _check_not_fixed (self, message):
        if self.fixed:
            printf ("object %s is fixed, you cannot %s\n", self._type_name (), message)

    def _check_colour (self):
        if self.type != colour_t:
            printf ("object is expected to be a colour")

    def _param_colour (self, message):
        if self.type != colour_t:
            printf (message)

    def delete (self):
        if not self.deleted:
            self.o = pgeif.remove (self.o)

    def _check_not_deleted (self, message):
        if self.deleted:
            printf ("object has been deleted and now it is being given " + message)

    def collision (self, between):
        for o in between:
            if self != o:
                if o in self.collisionWith:
                    self.collisionp (self)

def rgb (r, g, b):
    print "in rgb (",r, g, b, ")"
    c = pgeif.rgb (float(r), float(g), float(b))
    print "after pgeif.rgb ->", c
    o = object (colour_t, c)
    o._check_colour ()
    return object (colour_t, c)

def white ():
    return object (colour_t, pgeif.white ())

def _register (id, ob):
    global id2ob, od2id
    id2ob[id] = ob
    ob2id[ob] = id

def box (x, y, w, h, c):
    c._param_colour ("fifth parameter to box is expected to be a colour")
    id = pgeif.box (x, y, w, h, c._id())
    ob = object (box_t, id)
    _register (id, ob)
    return ob

def circle (x, y, r, c):
    c._param_colour ("fourth parameter to box is expected to be a colour")
    id = pgeif.circle (x, y, r, c._id ())
    ob = object (circle_t, id)
    _register (id, ob)
    return ob


frame_event, collision_event = range (2)

class event:
    def __init__ (self, t, d, l):
        self._type = t
        self._edata = d
        self._elength = l
        self._fdata = None
        self._flength = 0
    def _set_frame_contents (self, data, length):
        self._fdata = data
        self._flength = length
    def _process (self):
        if self.type == frame_event:
            draw_frame (self._fdata, self._flength)
        elif self.type == collision_event:
            collision (self._between ())
    def _between (self):
        self._check (collision_event)
        # returns the two object ids of the colliding objects
        ob1 = id2ob[id1]
        ob2 = id2ob[id2]
        return [ob1, ob2]

def collision (between):
    for o in between:
        o.collision (between)

def _process (pe):
    pe._process ()

def _post_event (e, t):
    if t != -1:
        pygame.event.post (pygame.event.Event (USEREVENT, pge_event=e))
        pygame.time.set_timer (USEREVENT, t)
    return e


#
# runpy - runs pge for time, t, milliseconds and also
#         process the pygame events.  Each event is
#         passed to procedure, ep.
#

def runpy (t=-1, ep=None):
    nev = _post_event (_get_next_event ())
    fin = _post_event (finish_event (t))
    while True:
        for e in pygame.event.get():
            if e.type == USEREVENT:
                if e.pge_event == fin:
                    return
                else:
                    _process (e.pge_event)
                    nev = _post_event (_get_next_event ())
            else:
                if ep != None:
                    if nev._get_time () >= cur_time ():
                        pgeif.advance_time (cur_time ())
                    ep (e)

#
#  rungroff - runs pge for time, t.  If t < 0.0 then simulate for 30.0 seconds max.
#

def rungroff (t):
    if t < 0.0:
        t = 30.0
    nev = _get_next_event ()
    while nev._get_time () < t:
        _process (nev)
        nev = _get_next_event ()


#
# run - runs pge for time, t, milliseconds and also
#       process the pygame events
#

def run (t=-1, ep=None):
    global device

    setDefaultDevice ()
    if device == pyg_d:
        runpy (t, ep)
    else:
        rungroff (t)


def setDevice (d):
    global device

    if device == None:
        device = d
        if d == pyg_d:
            pgeif.useBuffer ()
        elif d == groff_d:
            pgeif.groff ()
    else:
        printf ("cannot change device once pge has started\n")


def setDefaultDevice ():
    global device

    if device == None:
        device = pyg_d
        pgeif.useBuffer ()
    

def groff ():
    setDevice (groff_d)
