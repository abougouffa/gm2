
all: libcwrap.o force
	gm2 -Wmakeall -I. testselect.mod -lpth

libcwrap.o: libcwrap.c
	$(CC) -g -c -o $@ $<

clean: force
	$(RM) *.o a.out

force:
