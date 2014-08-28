SOURCES = test.mod \
          macroObjects.mod Points.mod Fractions.mod PolyMatrix3D.mod Transform3D.mod \
          deviceGroff.mod delay.mod GC.mod Indexing.mod Storage.mod popWorld.mod twoDsim.mod \
          roots.mod coord.mod polar.mod history.mod stairs.mod rpc.mod devicePy.mod \
          snooker.mod wall.mod npn.mod wallv.mod slope.mod para.mod testpy.mod RTgen.mod \
          deviceIf.mod ramps.mod ramps2.mod boxes.mod wallh.mod bounce.mod line.mod bounce2.mod \
          bounce3.mod bounce4.mod bounce5.mod bagatelle.mod

# OBJECTS = hack $(SOURCES:%.mod=%.o)
OBJECTS = $(SOURCES:%.mod=%.o)
PROFILE = 
GM2FLAGS = $(PROFILE) -fsoft-check-all -g -fiso -fextended-opaque 

whole: force
	gm2 -O3 -fsources -fiso -fcpp -I. -fm2-whole-program -fextended-opaque -g foo.mod 

bounce: $(OBJECTS)
	gm2 $(GM2FLAGS) -fonlylink bounce.mod

bounce2: $(OBJECTS)
	gm2 $(GM2FLAGS) -fonlylink bounce2.mod

bounce3: $(OBJECTS)
	gm2 $(GM2FLAGS) -fonlylink bounce3.mod

bounce4: $(OBJECTS)
	gm2 $(GM2FLAGS) -fonlylink bounce4.mod

bounce5: $(OBJECTS)
	gm2 $(GM2FLAGS) -fonlylink bounce5.mod

bagatelle: $(OBJECTS)
	gm2 $(GM2FLAGS) -fonlylink bagatelle.mod

line: $(OBJECTS)
	gm2 $(GM2FLAGS) -fonlylink line.mod

wallh: $(OBJECTS)
	gm2 $(GM2FLAGS) -fonlylink wallh.mod

boxes: $(OBJECTS)
	gm2 $(GM2FLAGS) -fonlylink boxes.mod

ramps: $(OBJECTS)
	gm2 $(GM2FLAGS) -fonlylink ramps.mod

ramps2: $(OBJECTS)
	gm2 $(GM2FLAGS) -fonlylink ramps2.mod

slope: $(OBJECTS)
	$(RM) *.ms *.png
	gm2 $(GM2FLAGS) -fonlylink slope.mod

all: $(OBJECTS)
	$(RM) *.ms *.png
	gm2 -pg -g -fiso -fextended-opaque -fonlylink wall.mod && ./a.out
	./produce-avi && mplayer pge.avi

stairs: $(OBJECTS)
	$(RM) *.ms *.png
	gm2 $(GM2FLAGS) -fonlylink stairs.mod

py: $(OBJECTS)
	$(RM) *.ms *.png
	gm2 -pg -g -fiso -fextended-opaque -fonlylink testpy.mod

para: $(OBJECTS)
	$(RM) *.ms *.png
	gm2 $(GM2FLAGS) -fonlylink para.mod && ./a.out
	./produce-avi

wallv: $(OBJECTS)
	$(RM) *.ms *.png
	gm2 $(GM2FLAGS) -fonlylink wallv.mod
	# ./produce-avi && mplayer pge.avi

wall: $(OBJECTS)
	$(RM) *.ms *.png
	gm2 $(GM2FLAGS) -fonlylink wall.mod
	# ./produce-avi && mplayer pge.avi

snooker: $(OBJECTS)
	$(RM) *.ms *.png
	gm2 $(GM2FLAGS) -fonlylink snooker.mod

npn: $(OBJECTS)
	$(RM) *.ms *.png
	gm2 $(GM2FLAGS) -fonlylink npn.mod && ./a.out

test: $(OBJECTS)
	gm2 $(GM2FLAGS) -fonlylink test.mod && ./a.out

Storage.o: Storage.mod
	gm2 -fsoft-check-all -g -fextended-opaque -c -g $< -o $@

twoDsim.mod: circles.m

twoDsim.o: twoDsim.mod
	gm2 -fcpp $(GM2FLAGS) -c $< -o $@

%.o:  %.mod
	gm2 $(GM2FLAGS) -c $< -o $@

later: force
	gm2 -fiso -c -g deviceGroff.mod
	gm2 -fiso -c -g macroObjects.mod

transforms: rotating-circles.asc rotating-line.asc

%.asc:  %.max
	maxima -b $< > $@

%.m:  %.mxm
	./max2code -o $@ $<

clean: force
	$(RM) *.o *.m

clear: force
	$(RM) *.png *.ms *.cpp

force:
