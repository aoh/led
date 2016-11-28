CC?=gcc
OFLAGS=-O1
CFLAGS=-O2 -Wall
OWL=ol-0.1.13
OWLSHA=4dc2fe537f9d952d02e3c67564531c0466386b3d353a3
OWLURL=https://github.com/aoh/owl-lisp/files/449350

everything: bin/led .parrot

.parrot: bin/led 
	cd test && ./run ../bin/led
	touch .parrot

# gcc takes a while on a raspberry. this is a lot faster.
fasltest: led.fasl bin/ol
	cd test && ./run  ../bin/ol -l ../led.fasl --

bin/led: led.c
	mkdir -p bin
	$(CC) $(CFLAGS) -o bin/led led.c

led.c: led/led.scm led/terminal.scm
	make bin/ol
	bin/ol $(OFLAGS) -o led.c led/led.scm

led.fasl: bin/ol led/led.scm led/terminal.scm
	make bin/ol
	bin/ol -o led.fasl led/led.scm

install: bin/led .parrot
	install -m 755 bin/led /usr/bin

bin/ol:
	mkdir -p bin tmp
	cd tmp; test -f $(OWL).c.gz || wget $(OWLURL)/$(OWL).c.gz
	sha256sum tmp/$(OWL).c.gz | grep -q $(OWLSHA)
	gzip -d < tmp/$(OWL).c.gz > tmp/$(OWL).c
	cc -O2 -o bin/ol tmp/$(OWL).c

test: .parrot

clean:
	-rm led.c led.log test/*.out bin/led bin/ol tmp/$(OWL).c
	-rmdir bin

mrproper:
	make clean
	rm -rf tmp

