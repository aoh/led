# git clone https://github.com/aoh/blab.git && cd blab && make && sudo make install

blab -l . -e 'led.prelude ":w hello.io.out" led.enter ":q" led.enter' > hello.io

blab -l . -e 'led.prelude "i" "foo" led.esc ":w foo.io.out" led.enter ":q" led.enter' > foo.io

