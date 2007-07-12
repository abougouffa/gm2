SHELL=/bin/bash
DISTFILES = Makefile.in configure.in configure shell-bin config-values.in \
            README version.c aclocal.m4 COPYING ChangeLog

prefix= /home/gaius/opt
srcdir= .
mandir= ${datarootdir}/man
exec_prefix=${prefix}
bindir= ${exec_prefix}/bin
datarootdir= ${prefix}/share

all: force
	mkdir -p download
	mkdir -p sources
	sh shell-bin/runtest no yes yes

install: force
	sh shell-bin/runinstall no

clean: force
	$(RM) config.log config.cache config.status

distclean: clean force
	$(RM) config.log config.cache config.status
	$(RM) -rf download sources

release: force
	echo gm2-harness-`sed -e '/version_string/!d' \
          -e 's/[^0-9.]*\([0-9.]*\).*/\1/' -e q version.c` > .fname
	-rm -rf `cat .fname`
	mkdir `cat .fname`
	dst=`cat .fname`; for f in $(DISTFILES); do \
           cp -rp $(srcdir)/$$f $$dst/$$f ; \
        done
	tar --gzip -chf `cat .fname`.tar.gz `cat .fname`
	uuencode `cat .fname`.tar.gz `cat .fname`.tar.gz > `cat .fname`.uue
	-rm -rf `cat .fname` .fname

force:
