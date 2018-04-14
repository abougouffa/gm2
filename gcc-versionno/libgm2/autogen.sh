#!/bin/sh

rm -rf autom4te.cache

# libtoolize
rm -f aclocal.m4
aclocal -I config -I ../config
automake --add-missing
# automake
autoconf
# autoreconf2.64

rm -rf autom4te.cache

exit 0
