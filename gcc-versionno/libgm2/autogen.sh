#!/bin/sh

rm -rf autom4te.cache

# libtoolize
rm -f aclocal.m4
aclocal-1.11 -I . -I config -I ../config
autoreconf2.64

rm -rf autom4te.cache

exit 0
