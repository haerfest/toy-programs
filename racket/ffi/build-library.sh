#!/usr/bin/env sh

# This is for Mac OS X, use -o libmultiply.so for Unixes.
gcc -shared multiply.c -o multiply.dylib
