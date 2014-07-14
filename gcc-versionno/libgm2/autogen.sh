#!/bin/sh

aclocal \
&& automake --add-missing \
&& autoconf2.64

