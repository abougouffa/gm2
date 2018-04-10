#!/bin/sh

rm -rf autom4te.cache

libtoolize
aclocal
automake --add-missing
autoconf

exit 0
