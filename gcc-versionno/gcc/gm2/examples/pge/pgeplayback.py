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
debugging         = False
programName       = "GNU PGE Playback"
fps               = 30
multiplier        = 1.0
   
call              = {}

idTOcol           = {}
maxColour         = 0
sounds            = {}
singleStep        = False
frameNo           = 0
wantedFrame       = 1
movie             = False
tmpdir            = ""
seekTable         = {}
pc                = 0
colours           = []
frameTime         = 0.0
delay             = 0.0
writtenColours    = []
margin            = 1.0
header            = 1.0
groffBox          = 5.0   # number of inches width and height

class myfile:
    def __init__ (self, name):
        self.name = name
        self.contents = open (name, 'r').read ()
        self.pos = 0
        self.length = len (self.contents)
    def read (self, n):
        if self.pos + n < self.length:
            b = self.contents[self.pos:self.pos+n]
            self.pos += n
            return b
        else:
            printf ("eof in %s reached\n", self.name)
            sys.exit (1)
    def seek (self, pos):
        printf ("seeking to %d\n", pos)
        self.pos = pos
    def record_pos (self, pos, frameno):
        global seekTable

        seekTable[frameno] = pos
    def rewind_to (self, frameno):
        global seekTable

        if seekTable.has_key (frameno):
            self.pos = seekTable[frameno]
            return True
        else:
            return False
    def close (self):
        pass
        

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
    global resolution, groffBox, movie

    if movie:
        return 1.0-y
    else:
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
    global sounds, wantedFrame, frameNo

    name = ""
    b = f.read (1)
    while int(b) != 0:
        name += b
        b = f.read (1)
    if not sounds.has_key (name):
        sounds[name] = load_sound (name)
    if frameNo == wantedFrame:
        sounds[name].play ()


#
#  initScreen - initialise the screen to the desired resolution.
#

def initScreen ():
    global screen, background, fullscreen, resolution, lightGrey

    pygame.init ()
    pygame.key.set_repeat (1, 300)
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
    global screen, debugging, frameNo, wantedFrame

    f, xf = readFract (f)
    f, yf = readFract (f)
    f, rf = readFract (f)
    x = mults (resolution[0], xf)
    y = mults (resolution[1], yf)
    r = mults (resolution[0], rf)

    f, c = readColour (f)
    if frameNo == wantedFrame:
        debugf("circle  x = %d  y = %d,  r = %d\n", x, y, r)
        if debugging:
            print "  colour =", c
        pygame.draw.circle (screen, c, (x, flip (y)), r, 0)
    return f


#
#  drawPolygon - 
#

def drawPolygon (f):
    global debugging, frameNo, wantedFrame

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
    if frameNo == wantedFrame:
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


def readColourRaw (f):
    f, c = readShort (f)
    return f, c
    

#
#
#

def readColour (f):
    f, c = readColourRaw (f)
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
    global screen, debugging, frameNo, wantedFrame

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
    if frameNo == wantedFrame:
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
    global background, lightGrey, screen, wantedFrame, multiplier

    pygame.display.set_caption (programName + ' ' + versionNumber + ' (%d) [%g]' % (frameNo, multiplier))
    if frameNo == wantedFrame:
        pygame.display.flip ()
        screen.blit (background, (0, 0))
    f = handleEvents (f)
    return f


def doExit (f):
    sys.exit (0)


def doFrameNote (f):
    global frameNo, pc

    f, frameNo = readCard (f)
    f.record_pos (pc, frameNo)
    return f


def skip (f, frames):
    global frameNo, wantedFrame

    if frameNo + frames > 0:
        wantedFrame = frameNo + frames
        if f.rewind_to (wantedFrame):
            pass
    return f


def handleSingleStep (f):
    global multiplier, singleStep, frameNo, wantedFrame

    while True:
        for event in pygame.event.get():
            if event.type == KEYDOWN:
                if event.key == K_SPACE:
                    singleStep = False
                    return skip (f, 1)
                elif event.key == K_ESCAPE:
                    sys.exit (0)
                elif event.key == K_RIGHT:
                    return skip (f, 1)
                elif event.key == K_LEFT:
                    return skip (f, -1)
                elif event.key == K_UP:
                    return skip (f, -5)
                elif event.key == K_DOWN:
                    return skip (f, 5)


def handleRT (f):
    global multiplier, singleStep, wantedFrame, frameNo

    for event in pygame.event.get():
        if event.type == KEYDOWN:
            if event.key == K_SPACE:
                singleStep = True
            elif event.key == K_ESCAPE:
                sys.exit (0)
            elif event.key == K_RIGHT:
                return skip (f, 10)
            elif event.key == K_LEFT:
                return skip (f, -10)
            elif event.key == K_UP:
                return skip (f, -50)
            elif event.key == K_DOWN:
                return skip (f, 50)
            elif event.key == K_KP_PLUS:
                if multiplier > 1.0:
                    multiplier -= 1.0
                elif multiplier > 0.0:
                    multiplier -= 0.1
                else:
                    multiplier = 0.1
            elif event.key == K_MINUS or event.key == K_KP_MINUS:
                if multiplier < 1.0:
                    multiplier += 0.1
                elif multiplier < 10.0:
                    multiplier += 1.0
                else:
                    multiplier = 10.0
            elif event.key == K_EQUALS:
                multiplier = 1.0
    if (not singleStep) and (frameNo == wantedFrame):
        wantedFrame += 1
    return f


def handleEvents (f):
    global singleStep, movie

    if not movie:
        if singleStep:
            f = handleSingleStep (f)
        else:
            f = handleRT (f)
    return f
              


#
#  readFile - opens up file, name, and proceeds to interpret
#             the sequence of commands.
#

def readFile (name):
    global frameNo, wantedFrame, pc

    wantedFrame = 1
    frameNo = 1
    f = myfile (name)
    pc = f.pos
    f = handleEvents (f)
    header = struct.unpack ("3s", f.read (3))[0]
    while header and len (header) > 0:
        header = header[:2]
        print header
        if call.has_key (header):
            f = call[header] (f)
            pc = f.pos
            header = struct.unpack ("3s", f.read (3))[0]
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
    global multiplier, singleStep, wantedFrame, frameNo, frameTime

    # printf ("doSleep (frameNo = %d, wantedFrame = %d)\n", frameNo, wantedFrame)
    f, t = readReal (f)
    frameTime += t
    if (not singleStep) and (frameNo == wantedFrame):
        t *= multiplier
        t *= 10.0
        debugf ("sleeping for %f seconds\n", t)
        time.sleep (t)
    return f


def grSleep (f):
    global frameTime

    f, t = readReal (f)
    frameTime += t
    return f


def usage (code):
    printf ("pgeplayback [-v][-d][-m][-f][-t delay] filename.raw\n")
    sys.exit (code)


def handleOptions ():
    global debugging, fullscreen, movie, delay
    try:
       optlist, l = getopt.getopt(sys.argv[1:], ':a:b:dhfmv')
       for opt in optlist:
           if opt[0] == '-a':
               delay = -float (opt[1])
           elif opt[0] == '-b':
               delay = float (opt[1])
           elif opt[0] == '-d':
               debugging = True
           elif opt[0] == '-f':
               fullscreen = FULLSCREEN
           elif opt[0] == '-h':
               usage (0)
           elif opt[0] == '-m':
               movie = True
           elif opt[0] == '-v':
               printf ("pgeplayback version " + versionNumber + "\n")
               sys.exit (0)
       if l != []:
           return l[0]

    except getopt.GetoptError:
       usage (1)
    return "output.raw"


def initMovie ():
    global tmpdir

    tmpdir = "tmpdir"
    os.system ("mkdir -p " + tmpdir)


def configDevice ():
    global movie

    if movie:
        initMovie ()
    else:
        initScreen ()


def grFlipBuffer (f):
    global outf, frameNo, writtenColours

    num = "%06d" % frameNo

    outf.close ()
    writtenColours = []
    os.system ("groff f" + num + ".ms > f" + num + ".ps")
    os.system ("gs -dNOPAUSE -sDEVICE=pnmraw -sOutputFile=t" + num + ".pnm -dGraphicsAlphaBits=4 -q -dBATCH f" + num +  ".ps > /dev/null 2>&1")
    os.system ("pnmcrop -quiet < t" + num + ".pnm | pnmtopng > e" + num + ".png 2> /dev/null")
    os.system ("convert e" + num + ".png -type truecolor f" + num + ".png 2> /dev/null")
    os.system ("rm -f t" + num + ".pnm f" + num + ".ps e" + num + ".png")
    return f


def writeColour (c):
    global outf, writtenColours, idTOcol

    if not (c in writtenColours):
        writtenColours += [c]
        s = ".defcolor col%d rgb " % c
        r, g, b = idTOcol[c]
        r = r / 255.0
        g = g / 255.0
        b = b / 255.0
        s += "%gf %gf %gf\n" % (r, g, b)
        outf.write (s)


def grFrameNote (f):
    global frameNo, outf

    f, frameNo = readCard (f)
    outf = open ("f%06d.ms" % frameNo, "w")
    return f


def finishMovie ():
    global fps

    s = "mencoder \"mf://f*.png\" -mf w=600:h=600:fps=%d:type=png -ovc lavc -lavcopts vcodec=mpeg4 -oac copy -o pge.avi" % fps
    os.system (s)


def finish ():
    if movie:
        finishMovie ()
        

#
#  grDrawFillCircle - 
#

def grDrawFillCircle (f):
    global screen, debugging, frameNo, wantedFrame

    debugf ("grDrawFillCircle\n")
    f, xf = readFract (f)
    f, yf = readFract (f)
    f, rf = readFract (f)

    x = mults (1.0, xf)
    y = mults (1.0, yf)
    r = mults (1.0, rf)
    y = flip (y)

    f, c = readColourRaw (f)
    writeColour (c)
    moveTo (x-r, y)
    outf.write (".nop ")
    s = "\\M[col%d]\\D'C " % c
    s += unit (r*2.0*groffBox)
    s += "'\\M[]\n"
    outf.write (s)
    return f


def unit (v):
    if v == 0.0:
        return '(u;0i)'
    s = ""
    if v < 0.0:
        s = '-'
        v = -v
    s += '(u;'
    s += "%g" % v
    s += 'i)'
    return s


def addOffset (x, y):
    global margin, header

    x += margin
    y += header
    return x, y


def moveTo (x, y):
    x, y = scale (x, y)
    x, y = addOffset (x, y)
    outf.write (".sp |" + unit (y) + "\n")
    outf.write (".nop \\h'" + unit (x) + "'\n")


def scale (x, y):
    global groffBox

    return x*groffBox, y*groffBox
    

def grRegisterColour (f):
    global idTOcol, debugging, colours

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
    colours += [c]
    return f


def doMessage (f):
    global frameTime

    text = ""
    b = f.read (1)
    while int(b) != 0:
        text += b
        b = f.read (1)
    print "Time:", frameTime, text


def grMessage (f):
    global frameTime

    text = ""
    b = f.read (1)
    while int(b) != 0:
        text += b
        b = f.read (1)
    print "Time:", frameTime, text


def grDrawPolygon (f):
    f, n = readShort (f)
    l = []
    if debugging:
        print "grDrawPolygon", n,
    for i in range (n):
        f, xf = readFract (f)
        f, yf = readFract (f)
        if debugging:
            print xf, yf,
        x = mults (1.0, xf)
        y = mults (1.0, yf)
        l += [[x, flip(y)]]

    f, t = readFract (f)
    if debugging:
        print "draw polygon", l, "thickness", t
    return f


def grDrawFillPolygon (f):
    global groffBox

    f, n = readShort (f)
    l = []
    if debugging:
        print "grDrawFillPolygon", n,
    for i in range (n):
        f, xf = readFract (f)
        f, yf = readFract (f)
        if debugging:
            print xf, yf,
        x = mults (1.0, xf)
        y = mults (1.0, yf)
        l += [[x, flip(y)]]

    f, c = readColourRaw (f)

    writeColour (c)
    moveTo (l[0][0], l[0][1])
    outf.write (".nop ")
    s = "\\M[col%d]\\D'P " % c
    outf.write (s)
    s = ""
    x, y = l[0]
    for p in l[1:]:
        s += unit ((p[0]-x)*groffBox) + " "
        s += unit ((p[1]-y)*groffBox) + " "
        x, y = p
    outf.write (s)
    outf.write ("'\M[]\n")
    return f


def grDrawCircle (f):
    return f


def grFramesPerSecond (f):
    return f


def grExit (f):
    return f


def grPlay (f):
    return f


#
#  main -
#

def main ():
    global call, movie

    filename = handleOptions ()
    configDevice ()
    if movie:
        call['rc'] = grRegisterColour
        call['dp'] = grDrawPolygon
        call['dP'] = grDrawFillPolygon
        call['dc'] = grDrawCircle
        call['dC'] = grDrawFillCircle
        call['fb'] = grFlipBuffer
        call['fr'] = grFramesPerSecond
        call['ex'] = grExit
        call['sl'] = grSleep
        call['ps'] = grPlay
        call['fn'] = grFrameNote
        call['ms'] = grMessage
    else:
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
        call['fn'] = doFrameNote
        call['ms'] = doMessage
    readFile (filename)
    finish ()


main ()
