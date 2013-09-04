#!/usr/bin/env sh

gcc -c -Wall -Werror -fpic foo.c
gcc -shared -o libfoo.dylib foo.o

