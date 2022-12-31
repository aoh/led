CC?=gcc
OFLAGS=-O1
CFLAGS=-O2 -Wall
PREFIX=/usr
OWLURL=https://haltp.org/files/ol-0.2.1.c.gz
OL=bin/ol
everything: bin/led .parrot

.parrot: test/* bin/led
	test/run bin/led
	touch .parrot

# gcc takes a while on a raspberry. this is a lot faster.
fasltest: led.fasl bin/ol
	cd test && sh ./run  ../bin/ol -l ../led.fasl --

bin/led: c/led.c
	mkdir -p bin
	$(CC) $(CFLAGS) -o bin/led c/led.c

c/led.c: led/*.scm $(OL)
	$(OL) $(OFLAGS) -o c/led.c led/led.scm

led.fasl: bin/ol led/*.scm
	make bin/ol
	$(OL) -o led.fasl led/led.scm

install: bin/led # .parrot
	mkdir -p $(PREFIX)/bin
	install -m 755 bin/led $(PREFIX)/bin/led

uninstall:
	rm -v $(PREFIX)/bin/led

c/ol-0.2.1.c.gz:
	# this is normally bundled in the repository
	mkdir -p c
	cd c && wget $(OWLURL)

c/ol.c: c/ol-0.2.1.c.gz
	cat c/ol-0.2.1.c.gz | gzip -d > c/ol.c

bin/ol: c/ol.c
	mkdir -p bin
	cc -O2 -o bin/ol c/ol.c

test: .parrot

clean:
	-rm c/*.c led.log test/*.out bin/led bin/ol
	-rmdir bin

mrproper:
	make clean
	rm -rf tmp

future:
	test -d owl || git clone https://gitlab.com/owl-lisp/owl
	cd owl && git pull
	-cd owl && make
	test -x owl/bin/ol && make OL=owl/bin/ol

.source.map: led/*.scm
	grep -n "(define " led/*.scm | sed -re 's/: *\(define \(?/:/' -e 's/ .*//' -e 's/\)//g' > .source.map

.PHONY: mrproper clean test install uninstall fasltest everything
