OL?=/usr/bin/ol
CC?=gcc
OFLAGS=-O2
CFLAGS=-O2 -Wall

.parrot: led 
	cd test && ./run ../led
	touch .parrot

led: led.c
	$(CC) $(CFLAGS) -o led led.c

led.c: led.scm
	$(OL) $(OFLAGS) -o led.c led.scm

install: led
	install -m 755 led /usr/bin

test: .parrot

clean:
	-rm led.c led.log led test/*.out
