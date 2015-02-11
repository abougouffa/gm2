#!/usr/bin/env python

import pgeif
import pygame

colour_t, box_t, circle_t = range (3)
id2ob = {}
ob2id = {}


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

def rgb (r, g, b):
    print "in rgb"
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
    id = pgeif.circle (x, y, r, c._id())
    ob = object (circle_t, id)
    _register (id, ob)
    return ob

def _process (pe):
    f = pe._get_func ()
    if pe._get_type () == collision_event:
        f (pe.between ())
    elif pe._get_type () == frame_event:
        f (pe.frame_buffer (), pe.frame_length ())

def _post_event (e, t):
    if t != -1:
        pygame.event.post (pygame.event.Event (USEREVENT, pge_event=e))
        pygame.time.set_timer (USEREVENT, t)
    return e


#
# run - runs pge for time, t, milliseconds and also
#       process the pygame events
#

def run (t=-1, ep=None):
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
