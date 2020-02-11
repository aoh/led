CC?=gcc
OFLAGS=-O1
CFLAGS=-O2 -Wall
PREFIX=/usr
OWLURL=https://haltp.org/files/ol-0.1.22.c.gz
OL=bin/ol

everything: bin/led # .parrot

.parrot: bin/led 
	cd test && sh ./run ../bin/led
	touch .parrot

# gcc takes a while on a raspberry. this is a lot faster.
fasltest: led.fasl bin/ol
	cd test && sh ./run  ../bin/ol -l ../led.fasl --

bin/led: led.c
	mkdir -p bin
	$(CC) $(CFLAGS) -o bin/led led.c

#led.c: led/*.scm $(OL)
#	$(OL) $(OFLAGS) -o led.c led/led.scm

led.c: led/*.scm $(OL)
	$(OL) $(OFLAGS) -o led.c led/led.scm

led.fasl: bin/ol led/*.scm
	make bin/ol
	$(OL) -o led.fasl led/led.scm

install: bin/led # .parrot
	mkdir -p $(PREFIX)/bin
	install -m 755 bin/led $(PREFIX)/bin/led

uninstall:
	rm -v $(PREFIX)/bin/led

bin/ol:
	mkdir -p bin tmp
	test -f tmp/ol.c || curl $(OWLURL) | gzip -d > tmp/ol.c
	cc -O2 -o bin/ol tmp/ol.c

test: .parrot

clean:
	-rm led.c led.log test/*.out bin/led bin/ol tmp/$(OWL).c
	-rmdir bin

mrproper:
	make clean
	rm -rf tmp

future:
	test -d owl || git clone https://gitlab.com/owl-lisp/owl
	cd owl && git pull
	-cd owl && make
	test -x owl/bin/ol && make OL=owl/bin/ol

.PHONY: mrproper clean test install uninstall fasltest everything
