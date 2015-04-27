#!/usr/bin/env python

import pgeif
import pygame
import sys
import struct
import time

colour_t, box_t, circle_t, fb_box_t, fb_circle_t = range (5)
id2ob = {}
ob2id = {}
batch_d, pyg_d = range (2)
device = None
opened = False
output = None
lastDelay = 0.0
debugging = True
foreground= []
background= []
colours = []
levels = {}


#
#  printf - keeps C programmers happy :-)
#

def printf (format, *args):
    print str (format) % args,


def debugf (format, *args):
    global debugging

    if debugging:
        print str (format) % args,


def _emit_short (s):
    global output

    output.write (struct.pack ('!H', s))

def _emit_card (c):
    global output

    output.write (struct.pack ('!I', c))

def _emit_fract (f):
    global output

    if f == 0:
        output.write (struct.pack ('B', 0))
    elif f == 1:
        output.write (struct.pack ('B', 1))
    elif f < 1.0:
        output.write (struct.pack ('B', 2))
        output.write (struct.pack ('!QQ', f*10000.0, 10000.0))
    else:
        w = int (f)
        f -= w
        output.write (struct.pack ('B', 3))
        output.write (struct.pack ('!QQQ', w, f*10000.0, 10000.0))


class object:
    def __init__ (self, t, o):
        self.deleted = False
        self.type = t
        self.o = o
        self.fixed = False
        self.param = None
        self.kg = None
        self.collisionWith = []
        self.w = 0
        self.c = None

    def _id (self):
        return self.o

    def _get_colour (self):
        self._check_colour ()
        return self.o[-1]

    def _draw (self):
        if self.type == fb_box_t:
            if device == pyg_d:
                pass
            else:
                self._emit_fill_polygon ()
        elif self.type == fb_circle_t:
            if device == pyg_d:
                pass
            else:
                self._emit_fill_circle ()
        elif self.type == colour_t:
            pass

    def _emit_fill_circle (self):
        output.write (struct.pack ("3s", "dC"))
        _emit_fract (self.o [0])  #  x pos
        _emit_fract (self.o [1])  #  y pos
        _emit_fract (self.o [2])  #  radius
        _emit_short (self.o [3])  #  colour
        print "_emit_fill_circle, colour is ", self.o [3], self.o [0], self.o [1], self.o [2]

    def _emit_fill_polygon (self):
        output.write (struct.pack ("3s", "dP"))
        n = (len (self.o)-1)/2
        _emit_short (n)
        ier = iter (self.o[:-1])
        print self.o
        for x in ier:
            _emit_fract (x)
            _emit_fract (next (ier))
        _emit_short (self.o [-1])

    def _name (self):
        if self.type == colour_t:
            return "colour"
        elif self.type == box_t:
            return "box"
        elif self.type == circle_t:
            return "circle"
        else:
            printf ("fatal error, object not recognised\n")
            sys.exit (1)

    def velocity (self, vx, vy):
        self._check_type ([box_t, circle_t], "assign a velocity to a")
        self._check_not_fixed ("assign a velocity")
        self._check_not_deleted ("a velocity")
        print "velocity for object", self.o, vx, vy
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
        self._check_no_mass ("cannot fix " + self._name () + " as it has a mass")
        self.fixed = True
        self.o = pgeif.fix (self.o)
        print "fix", self.o
        return self

    def mass (self, m):
        self._check_type ([box_t, circle_t], "assign a mass to a")
        self._check_not_fixed ("assign a mass")
        self._check_not_deleted (" a mass")
        if m is None:
            printf ("cannot give value None as the mass\n")
            sys.exit (1)
        self.kg = m
        self.o = pgeif.mass (self.o, m)
        print "mass", self.o
        return self

    def on_collision_with (self, another, p):
        print "ok registering call back", p, another
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

    def rm (self):
        if not self.deleted:
            self.o = pgeif.rm (self.o)

    def _check_not_deleted (self, message):
        if self.deleted:
            printf ("object has been deleted and now it is being given " + message)

    def _check_no_mass (self, message):
        if self.kg != None:
            printf (message + "\n")
            sys.exit (1)

    def collision (self, between):
        print "collision seen, between:", between
        if self.collisionWith == []:
            self.collisionp (self)
        else:
            for c in self.collisionWith:
                for b in between:
                    if c == b:
                        break
            else:
                return
            self.collisionp (self)

    def get_param (self):
        return self.param

    def set_param (self, value):
        self.param = value
        return self

    def _set_width (self, value):
        self.w = value

    def get_width (self):
        self._check_type ([box_t, circle_t], "get the width")
        return self.w

    def get_mass (self):
        return self.kg

    def get_colour (self):
        return self.c

    def get_xpos (self):
        self._check_type ([box_t, circle_t], "get the xpos")
        return pgeif.get_xpos (self.o)

    def get_ypos (self):
        self._check_type ([box_t, circle_t], "get the ypos")
        return pgeif.get_ypos (self.o)


def rgb (r, g, b):
    print "in rgb (", r, g, b, ")"
    c = pgeif.rgb (float(r), float(g), float(b))
    print "after pgeif.rgb ->", c
    o = object (colour_t, [float(r), float(g), float(b), c])
    o._check_colour ()
    return o

def white ():
    return object (colour_t, pgeif.white ())

def _register (id, ob):
    global id2ob, od2id

    printf ("registering %d\n", id)
    id2ob[id] = ob
    ob2id[ob] = id


def box (x, y, w, h, c, level = 0):
    c._param_colour ("fifth parameter to box is expected to be a colour")
    if level == 0:
        id = pgeif.box (x, y, w, h, c._get_colour ())
        ob = object (box_t, id)
        ob._set_width (w)
        ob.c = c
        printf ("box ")
        _register (id, ob)
    else:
        ob = object (fb_box_t, [x, y, x+w, y, x+w, y+h, x+w, y+h, x, y+h, pgeif.h2l (c._get_colour ())])
        _add (ob, level)
    return ob


def _add (ob, level):
    global foreground, background

    if level > 0:
        if not (level in foreground):
            foreground += [level]
            foreground.sort ()
    else:
        if not (level in background):
            background += [level]
            background.sort ()

    if levels.has_key (level):
        levels[level] += [ob]
    else:
        levels[level] = [ob]


def circle (x, y, r, c, level = 0):
    c._param_colour ("fourth parameter to box is expected to be a colour")
    if level == 0:
        id = pgeif.circle (x, y, r, c._get_colour ())
        print "circle id =", id
        printf ("circle ")
        ob = object (circle_t, id)
        _register (id, ob)
        ob.c = c
    else:
        print "pge: colour", c._get_colour ()
        ob = object (fb_circle_t, [x, y, r, pgeif.h2l (c._get_colour ())])
        _add (ob, level)
    return ob


#
#  unpackFract - returns three integers:  w, n, d
#                representing fraction.
#

def unpackFract (s):
    b = s[0]
    v = struct.unpack ("B", b)[0]
    
    if v == 0:
        return (0, 0, 0)
    elif v == 1:
        return (1, 0, 0)
    elif v == 2:
        b = s[1:17]
        r = struct.unpack('!QQ', b)
        return (0, r[0], r[1])
    else:
        b = s[1:33]
        return struct.unpack('!QQQ', b)


#
#  unpackReal 
#

def unpackReal (s):
    if len (s) >= 8:
        return struct.unpack ('d', s[:8])[0]
    else:
        printf ("insufficient data passed to unpackReal\n")


def unpackCard (s):
    if len (s) >= 4:
        return struct.unpack ('!I', s[:4])[0]
    else:
        printf ("insufficient data passed to unpackCard\n")


def unpackCardPair (s):
    if len (s) >= 8:
        return [struct.unpack ('!I', s[:4])[0],
                struct.unpack ('!I', s[4:8])[0]]
    else:
        printf ("insufficient data passed to unpackCardPair (%d bytes)\n", len (s))

def unpackIdPair (s):
    p = unpackCardPair (s)
    p[0] = pgeif.l2h (p[0])
    p[1] = pgeif.l2h (p[1])
    return p

def unpackPoint (s):
    if len (s) >= 16:
        return [unpackReal (s[:8]), unpackReal (s[8:])]
    else:
        printf ("insufficient data passed to unpackPoint\n")

def draw_foreground ():
    print "draw foreground", foreground
    if foreground != []:
        for l in foreground:
            print "level", l, levels[l]
            for o in levels[l]:
                o._draw ()

def draw_background ():
    print "draw background", background
    if background != []:
        for l in background:
            print "level", l, levels[l]
            for o in levels[l]:
                o._draw ()

no_event, frame_event, collision_event, final_event = range (4)

class event:
    def __init__ (self, t, d, l):
        printf ("creating event (data is %d bytes)\n", l)
        self._type = t
        self._edata = d
        self._elength = l
        self._fdata = None
        self._flength = 0
        self._cData = None
        self._clength = 0
        # the following are the event data values
        self.__point = None
        self.__between = None
        self.__etime = 0.0
        self.__etype = 0
        self.__kind = 0
        if self._edata == None:
            printf ("expecting some event data\n")
        else:
            self.__etime = unpackReal (self._edata) # 8 bytes REAL
            if t == collision_event:
                self.__etype = unpackCard (self._edata[8:12]) # 4 bytes etype
                self.__point = unpackPoint (self._edata[12:])
                self.__between = unpackIdPair (self._edata[28:])
                print "assigning between values", self.__between

                # etype == 0 is a draw frame event
                # etype == 1 two circles colliding
                if self.__etype == 2 or self.__etype == 3:
                    # circle/polygon collision or polygon/polygon collision
                    self.__kind = unpackCard (self._edata[36:])
    def _set_frame_contents (self, data, length):
        self._fData = data
        self._flength = length
    def _set_colour_contents (self, data, length):
        self._cData = data
        self._clength = length
    def _process (self):
        printf ("about to call process_event\n")
        _flush_delay ()
        pgeif.process_event ();
        printf ("find out which event\n")
        if self._type == frame_event:
            cData = pgeif.get_cbuf ()
            printf ("cData len = %d\n", len (cData))
            self._set_colour_contents (cData, len (cData))
            fData = pgeif.get_fbuf ()
            printf ("fData len = %d\n", len (fData))
            self._set_frame_contents (fData, len (fData))
            draw_frame (self._cData, self._clength,
                        self._fData, self._flength)
            pgeif.empty_fbuffer ()
            pgeif.empty_cbuffer ()
        elif self._type == collision_event:
            printf ("collision event seen!!\n")
            collision (self._between ())
    def _check (self, et):
        if self._type != et:
            printf ("fatal error, unexpected event type\n")
            sys.exit (1)

    def _between (self):
        global id2ob

        self._check (collision_event)
        # returns the two object ids of the colliding objects
        printf ("id0 = %d, id1 = %d\n", self.__between[0], self.__between[1])
        ob1 = id2ob[self.__between[0]]
        ob2 = id2ob[self.__between[1]]
        return [ob1, ob2]
    def _get_time (self):
        return self.__etime

def _get_next_event ():
    global device
    printf ("_get_next_event\n")
    setDefaultDevice ()
    if pgeif.is_collision ():
        printf ("pgeif.is_collision\n")
        printf ("pgeif.get_ebuf\n")
        eData = pgeif.get_ebuf ()
        printf ("event (...\n")
        return event (collision_event, eData, len (eData))
    elif pgeif.is_frame ():
        printf ("pgeif.is_frame\n")
        printf ("pgeif.get_ebuf\n")
        eData = pgeif.get_ebuf ()
        print "testing -> ", unpackReal (eData)
        return event (frame_event, eData, len (eData))
    else:
        printf ("fatal error: unknown event type (terminating simulation)\n")
        sys.exit (1)
        return event (no_event, None, 0)

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


def _finish_event (t):
    return event (final_event, None, 0)


def draw_frame (cdata, clength, fdata, flength):
    global opened, output, nextFrame

    if fdata is None:
        printf ("no data in the frame!\n")
        sys.exit (1)
    if not opened:
        opened = True
        output = open ("output.raw", "w")
        nextFrame = 1
    output.write (struct.pack ("3s", "fn")) # frame note
    _emit_card (nextFrame)
    if clength > 0:
        printf ("writing colour data length = %d bytes\n", clength)
        output.write (cdata)
    draw_background ()
    if flength > 0:
        printf ("writing frame data length = %d bytes\n", flength)
        output.write (fdata)
    else:
        printf ("length of zero!!\n")
        # sys.exit (2)
    draw_foreground ()
    output.write (struct.pack ("3s", "fb")) # flip buffer
    nextFrame += 1


def gravity (value=-9.81):
    pgeif.gravity (value)


#
# runpy - runs pge for time, t, milliseconds and also
#         process the pygame events.  Each event is
#         passed to procedure, ep.
#

def runpy (t=-1, ep=None):
    pgeif.use_time_delay (False)
    cData = pgeif.get_cbuf ()
    fData = pgeif.get_fbuf ()
    draw_frame (cData, len (cData), fData, len (fData))
    pgeif.empty_fbuffer ()
    pgeif.empty_cbuffer ()
    nev = _post_event (_get_next_event ())
    fin = _post_event (_finish_event (t))
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
#  runbatch - runs pge for time, t.  If t < 0.0 then simulate for 30.0 seconds max.
#

def runbatch (t):
    if t < 0.0:
        t = 30.0
    debugf ("runbatch (%f)\n", t)
    cData = pgeif.get_cbuf ()
    fData = pgeif.get_fbuf ()
    draw_frame (cData, len (cData), fData, len (fData))
    pgeif.empty_fbuffer ()
    pgeif.empty_cbuffer ()
    nev = _get_next_event ()
    acc = 0.0
    while acc+nev._get_time () < t:
        old = acc
        acc = acc + nev._get_time ()
        delay (nev._get_time ())
        if int(acc) != int(old):
            printf ("%d/%d seconds completed %d%%\n", int (acc), int (t), int (acc*100.0/t))
            # printf ("time %f out of %f seconds\n", acc, t)
        _process (nev)
        nev = _get_next_event ()


#
#  run - runs pge for time, t, milliseconds and also
#        process the pygame events
#

def run (t=-1, ep=None):
    global device

    setDefaultDevice ()
    if device == pyg_d:
        runpy (t, ep)
    else:
        runbatch (t)


def setDevice (d):
    global device

    if device == None:
        device = d
        pgeif.use_buffer ()
    else:
        printf ("cannot change device once pge has started\n")


def setDefaultDevice ():
    global device

    if device == None:
        device = pyg_d
        pgeif.use_buffer ()
    

def batch ():
    setDevice (batch_d)


def finish ():
    global output, opened
    if opened:
        output.close ()
        opened = False

def load_sound (name):
    class NoneSound:
        def play(self):
            pass
    if not pygame.mixer or not pygame.mixer.get_init():
        return NoneSound()
    try:
        sound = pygame.mixer.Sound(name)
    except pygame.error, message:
        print 'cannot load sound file:', name
        return NoneSound()
    return sound


def play (name):
    global output

    _flush_delay ()
    if device == pyg_d:
        s = load_sound (name)
        s.play ()
    else:
        output.write (struct.pack ("3s", "ps"))
        output.write (name)
        output.write ('\0')


#
#  message - write out text to the output.
#

def message (text):
    output.write (struct.pack ("3s", "ms"))
    output.write (text)


#
#  turn the drawing of collision frames on or off.
#
#        actual:   determines whether an extra frame is generated
#                  at the time of actual collision.
#        predict:  draws a frame predicting the next collision.
#                  It will show the points predicted to collide.
#

def draw_collision (actual, predict):
    pgeif.draw_collision (actual, predict)


def dump_world ():
    pgeif.dump_world ()


def _draw (l):
    if l != []:
        for i in l:
            for o in levels[i]:
                o._draw ()


#
#  _flush_delay - write out or implement the collected delay time.
#

def _flush_delay ():
    global lastDelay

    if lastDelay > 0.0:
        debugf ("delay of %f\n", lastDelay)
        if device == pyg_d:
            time.sleep (lastDelay)
        else:
            output.write (struct.pack ("3s", "sl"))
            output.write (struct.pack ("d", lastDelay))
        lastDelay = 0.0


#
#  delay - introduce a delay for, t.
#

def delay (t):
    global lastDelay
    lastDelay += t
