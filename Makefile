CC?=gcc
OFLAGS=-O1
CFLAGS=-O2 -Wall
OWL=ol-0.1.14
OWLSHA=8df96fcb16d666700984ba9db2767dbceba6f6d027623a19be72ea87ce44e15a
OWLURL=https://github.com/aoh/owl-lisp/releases/download/v0.1.14
PREFIX=/usr

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

led.c: led/*.scm tmp/owl-lisp/bin/vm
	#bin/ol $(OFLAGS) -o led.c led/led.scm
	tmp/owl-lisp/bin/vm tmp/owl-lisp/fasl/init.fasl $(OFLAGS) -o led.c led/led.scm

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
	cd tmp; test -f $(OWL).c.gz || curl -L $(OWLURL)/$(OWL).c.gz > $(OWL).c.gz
	sha256sum tmp/$(OWL).c.gz | grep -q $(OWLSHA)
	gzip -d < tmp/$(OWL).c.gz > tmp/$(OWL).c
	cc -O2 -o bin/ol tmp/$(OWL).c

tmp/owl-lisp:
	mkdir -p tmp
	cd tmp && git clone https://github.com/aoh/owl-lisp.git

tmp/owl-lisp/bin/vm: tmp/owl-lisp
	cd tmp/owl-lisp && make bin/vm

test: .parrot

clean:
	-rm led.c led.log test/*.out bin/led bin/ol tmp/$(OWL).c
	-rmdir bin

mrproper:
	make clean
	rm -rf tmp

.PHONY: mrproper clean test install uninstall fasltest everything
