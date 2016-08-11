CC?=gcc
OFLAGS=-O1
CFLAGS=-O2 -Wall
OWLVERSION=0.1.13
OL?=owl-lisp-$(OWLVERSION)/bin/vm owl-lisp-$(OWLVERSION)/fasl/init.fasl

everything: led

.parrot: led 
	cd test && ./run ../led
	touch .parrot

led: led.c
	$(CC) $(CFLAGS) -o led led.c

led.c: led.scm
	make get-owl
	$(OL) $(OFLAGS) -o led.c led.scm

install: led
	install -m 755 led /usr/bin

get-owl:
	test -d owl-lisp-$(OWLVERSION) || curl -L https://github.com/aoh/owl-lisp/archive/v$(OWLVERSION).tar.gz | tar -zxvf -
	cd owl-lisp-$(OWLVERSION) && make bin/vm

test: .parrot

clean:
	-rm led.c led.log led test/*.out
	-cd owl-lisp-$(OWLVERSION) && make clean

mrproper:
	make clean
	rm -rf owl-lisp-$(OWLVERSION)

