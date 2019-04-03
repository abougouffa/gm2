#!/bin/sh

rm -rf autom4te.cache

# libtoolize
rm -f aclocal.m4
aclocal -I . -I config -I ../config
autoreconf

rm -rf autom4te.cache

exit 0
