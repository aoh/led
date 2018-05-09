CC?=gcc
OFLAGS=-O1
CFLAGS=-O2 -Wall
PREFIX=/usr
OWLVER=0.1.15
OWLURL=https://github.com/aoh/owl-lisp/releases/download/v$(OWLVER)

everything: bin/led .parrot

.parrot: bin/led 
	cd test && sh ./run ../bin/led
	touch .parrot

# gcc takes a while on a raspberry. this is a lot faster.
fasltest: led.fasl bin/ol
	cd test && sh ./run  ../bin/ol -l ../led.fasl --

bin/led: led.c
	mkdir -p bin
	$(CC) $(CFLAGS) -o bin/led led.c

led.c: led/*.scm bin/ol
	bin/ol $(OFLAGS) -o led.c led/led.scm

led.fasl: bin/ol led/*.scm
	make bin/ol
	bin/ol -o led.fasl led/led.scm

install: bin/led .parrot
	mkdir -p $(PREFIX)/bin
	install -m 755 bin/led $(PREFIX)/bin/led

uninstall:
	rm -v $(PREFIX)/bin/led

bin/ol:
	mkdir -p bin tmp
	test -f ol-$(OWLVER).c.gz || wget $(OWLURL)/ol-$(OWLVER).c.gz
	gzip -d < ol-$(OWLVER).c.gz > tmp/ol-$(OWLVER).c
	rm ol-$(OWLVER).c.gz
	cc -O2 -o bin/ol tmp/ol-$(OWLVER).c

test: .parrot

clean:
	-rm led.c led.log test/*.out bin/led bin/ol tmp/$(OWL).c
	-rmdir bin

mrproper:
	make clean
	rm -rf tmp

.PHONY: mrproper clean test install uninstall fasltest everything
