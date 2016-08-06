bin/led: led.c
	mkdir -p bin
	$(CC) -O2 -o led led.c

led.c: led.scm
	ol -O2 -o led.c led.scm
