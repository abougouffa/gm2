#!/usr/bin/env python

import os, pygame, string, sys, getopt, math, struct, time
from pygame.locals import *


#
#  global variables
#
versionNumber     = '0.1'
#
resolution        = (1024, 1024)
#
#
Black             = (0, 0, 0)
lightGrey         = (200, 200, 200)
fullscreen        = False
debugging         =  True
programName       = "GNU PGE Playback"
fps               = 30
multiplier        = 1.0
   
call              = {}

idTOcol           = {}
maxColour         = 0
sounds            = {}
singleStep        = False
frameNo           = 0
wantedFrame       = 0


#
#  printf - keeps C programmers happy :-)
#

def printf (format, *args):
    print str(format) % args,

#
#  error - issues an error message and exits.
#

def error (format, *args):
    print str(format) % args,
    sys.exit(1)


#
#  debugf - issues prints if debugging is set
#

def debugf (format, *args):
    global debugging
    if debugging:
        print str(format) % args,


#
#  flip - returns the y value flipped against the resolution.
#

def flip (y):
    global resolution

    return resolution[1]-y


def load_sound(name):
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


def doPlay (f):
    global sounds, wantedFrame, frameno

    name = ""
    b = f.read (1)
    while int(b) != 0:
        name += b
        b = f.read (1)
    if not sounds.has_key (name):
        sounds[name] = load_sound (name)
    if frameno == wantedFrame:
        sounds[name].play ()


#
#  initScreen - initialise the screen to the desired resolution.
#

def initScreen ():
    global screen, background, fullscreen, resolution, lightGrey

    pygame.init ()
    if fullscreen:
        screen = pygame.display.set_mode (resolution, FULLSCREEN)
    else:
        screen = pygame.display.set_mode (resolution)
    pygame.display.set_caption (programName + ' ' + versionNumber)
    background = pygame.Surface (screen.get_size ())
    background = background.convert ()
    background.fill (Black)


#
#
#

def readShort (f):
    b = f.read (2)
    c = struct.unpack ('!H', b)[0]
    return f, c


def toCol (f):
    return toFloat (f)*255

#
#  registerColour - 
#

def registerColour (f):
    global idTOcol, debugging

    f, c = readShort (f)
    f, rf = readFract (f)
    f, gf = readFract (f)
    f, bf = readFract (f)
    if debugging:
        print rf, gf, bf
    r = toCol (rf)
    g = toCol (gf)
    b = toCol (bf)
    debugf("colour %d, %d, %d\n", r, g, b)
    idTOcol[c] = (r, g, b)
    debugf("colour id %d\n", c)
    return f


#
#  drawCircle - 
#

def drawCircle (f):
    global idTOcol, screen

    b = read (4)
    c, x, y, r = struct.unpack('<IIII', bytes)
    debugf("circle colour %d  x = %d  y = %d,  r = %d\n", c, x, y, r)
    pygame.draw.circle(screen, idTOcol[c], (x, flip(y)), r, 0)
    return connection


#
#  drawFillCircle - 
#

def drawFillCircle (f):
    global screen, debugging, frameno, wantedFrame

    f, xf = readFract (f)
    f, yf = readFract (f)
    f, rf = readFract (f)
    x = mults (resolution[0], xf)
    y = mults (resolution[1], yf)
    r = mults (resolution[0], rf)

    f, c = readColour (f)
    if frameno == wantedFrame:
        debugf("circle  x = %d  y = %d,  r = %d\n", x, y, r)
        if debugging:
            print "  colour =", c
        pygame.draw.circle (screen, c, (x, flip (y)), r, 0)
    return f


#
#  drawPolygon - 
#

def drawPolygon (f):
    global debugging, frameno, wantedFrame

    f, n = readShort (f)
    l = []
    if debugging:
        print "drawPolygon", n,
    for i in range (n):
        f, xf = readFract (f)
        f, yf = readFract (f)
        if debugging:
            print xf, yf,
        x = mults (resolution[0], xf)
        y = mults (resolution[1], yf)
        l += [[x, flip(y)]]

    f, t = readFract (f)
    if debugging:
        print "draw polygon", l, "thickness", t
    if frameno == wantedFrame:
        # pygame.draw.polygon(screen, c, l, 0)
        pass
    return f



#
#  readFract - returns three integers:  w, n, d
#              representing fraction.
#

def readFract (f):
    b = f.read (1)
    v = struct.unpack ("B", b)[0]
    
    if v == 0:
        return f, (0, 0, 0)
    elif v == 1:
        return f, (1, 0, 0)
    elif v == 2:
        b = f.read (8*2)
        r = struct.unpack('!QQ', b)
        return f, (0, r[0], r[1])
    else:
        b = f.read (8*3)
        return f, struct.unpack('!QQQ', b)
    

#
#
#

def readColour (f):
    f, c = readShort (f)
    col = idTOcol[c]
    return f, col


#
#  mults -
#

def mults (s, f):
    if s == 0:
        return 0
    if f[1] == 0 or f[2] == 0:
        return f[0]*s
    return f[0]+f[1]*s/f[2]


def toFloat (f):
    if f[1] == 0 or f[2] == 0:
        return float(f[0])
    return float(f[0]) + float(f[1])/float(f[2])


#
#  drawFillPolygon - 
#

def drawFillPolygon (f):
    global screen, debugging, frameno, wantedFrame

    f, n = readShort (f)
    l = []
    if debugging:
        print "drawFillPolygon", n,
    for i in range (n):
        f, xf = readFract (f)
        f, yf = readFract (f)
        if debugging:
            print xf, yf,
        x = mults (resolution[0], xf)
        y = mults (resolution[1], yf)
        l += [[x, flip(y)]]

    f, c = readColour (f)
    if frameno == wantedFrame:
        if debugging:
            print ""
            print "drawFillPolygon (colour =", c, " l =", l, ")"
        pygame.draw.polygon (screen, c, l, 0)
    return f


def readCard (f):
    b = f.read (4)
    return f, struct.unpack ("!I", b)[0]
    

#
#  flipBuffer - flips the screen buffer.
#

def flipBuffer (f):
    global background, lightGrey, screen, frameno, wantedFrame

    f, frameno = readCard (f)
    pygame.display.set_caption (programName + ' ' + versionNumber + ' (%d)' % frameno)
    wantedFrame = frameno
    if frameno == wantedFrame:
        pygame.display.flip ()
        screen.blit (background, (0, 0))
    return f


def doExit (f):
    sys.exit (0)


def skip (frames):
    if frames != 1:
        pass


def handleSingleStep ():
    global multiplier, singleStep

    while True:
        for event in pygame.event.get():
            if event.type == KEYDOWN:
                if event.key == K_SPACE:
                    singleStep = False
                    return
                elif event.key == K_ESCAPE:
                    sys.exit (0)
                elif event.key == K_RIGHT:
                    skip (1)
                    return
                elif event.key == K_LEFT:
                    skip (-1)
                    return
                elif event.key == K_UP:
                    skip (-5)
                    return
                elif event.key == K_DOWN:
                    skip (5)
                    return


def handleRT ():
    global multiplier, singleStep

    for event in pygame.event.get():
        if event.type == KEYDOWN:
            if event.key == K_SPACE:
                singleStep = True
            elif event.key == K_ESCAPE:
                sys.exit (0)
            elif event.key == K_RIGHT:
                skip (10)
            elif event.key == K_LEFT:
                skip (-10)
            elif event.key == K_UP:
                skip (-50)
            elif event.key == K_DOWN:
                skip (50)
            elif event.key == K_PLUS:
                if multiplier > 1.0:
                    multiplier -= 1.0
                elif multiplier > 0.0:
                    multiplier -= 0.1
                else:
                    multiplier = 0.1
            elif event.key == K_MINUS:
                if multiplier < 1.0:
                    multiplier += 0.1
                elif multiplier < 10.0:
                    multiplier += 1.0
                else:
                    multiplier = 10.0
            elif event.key == K_EQUALS:
                multiplier = 1.0


def handleEvents (f):
    global singleStep

    if singleStep:
        handleSingleStep ()        
    else:
        handleRT ()
    return f
              


#
#  readFile - opens up file, name, and proceeds to interpret
#             the sequence of commands.
#

def readFile (name):
    global frameno

    frameNo = 0
    callno = 0
    f = open (name)
    header = struct.unpack ("3s", f.read (3))[0]
    while header and len (header) > 0:
        f = handleEvents (f)
        header = header[:2]
        if call.has_key (header):
            debugf ("[%d] %s\n", callno, header)
            f = call[header] (f)
            header = struct.unpack ("3s", f.read (3))[0]
            callno += 1
        else:
            printf ("error unexpected call %s\n", header)
            break
    f.close ()


def framesPerSecond (f):
    global fps
    f, fps = readCard (f)
    return f


def readReal (f):
    b = f.read (8)
    return f, struct.unpack ("d", b)[0]


def doSleep (f):
    global multiplier, singleStep, wantedFrame, frameno

    f, t = readReal (f)
    if (not singleStep) and (frameno == wantedFrame):
        t *= multiplier
        t *= 10.0
        debugf ("sleeping for %f seconds\n", t)
        time.sleep (t)
    return f


def usage ():
    printf ("pgeplayback [-v][-d][-m][-f][-g]\n")


def handleOptions ():
    global debugging, fullscreen, movie
    try:
       optlist, l = getopt.getopt(sys.argv[1:], ':vdhmfg')
       for opt in optlist:
           if opt[0] == '-h':
               usage ()
           elif opt[0] == '-d':
               debugging = True
           elif opt[0] == '-f':
               fullscreen = FULLSCREEN
           elif opt[0] == '-m':
               movie = True
           elif opt[0] == '-v':
               printf ("pgeplayback version " + versionNumber + "\n")
               
    except getopt.GetoptError:
       usage()
       sys.exit(1)


#
#  main -
#

def main ():
    global call

    handleOptions ()
    call['rc'] = registerColour
    call['dp'] = drawPolygon
    call['dP'] = drawFillPolygon
    call['dc'] = drawCircle
    call['dC'] = drawFillCircle
    call['fb'] = flipBuffer
    call['fr'] = framesPerSecond
    call['ex'] = doExit
    call['sl'] = doSleep
    call['ps'] = doPlay
    initScreen ()
    readFile ("output.raw")


main ()
