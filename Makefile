OL?=/usr/bin/ol
CC?=gcc
OFLAGS=-O2
CFLAGS=-O2 -Wall

bin/led: led.c
	mkdir -p bin
	$(CC) $(CFLAGS) -o led led.c

led.c: led.scm
	$(OL) $(OFLAGS) -o led.c led.scm

clean:
	-rm led.c led.log
