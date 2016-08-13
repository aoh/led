# git clone https://github.com/aoh/blab.git && cd blab && make && sudo make install

# just make  file
blab -l . -e 'led.prelude ":w hello.io.out" led.enter ":q" led.enter' > hello.io

# write foo to a file
blab -l . -e 'led.prelude "i" "foo" led.esc ":w foo.io.out" led.enter ":q" led.enter' > foo.io

# line-based copy and paste
blab -l . -e 'led.prelude "i" "foo" led.enter "bar" led.esc ":1" led.enter "ddpp" led.esc ":w copy-1.io.out" led.enter ":q" led.enter' > copy-1.io

# marks
blab -l . -e 'led.prelude "mai" "foo" led.enter "bar" led.esc 39 "aiX" led.esc ":w mark-1.io.out" led.enter ":q" led.enter' > mark-1.io

