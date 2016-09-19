# git clone https://github.com/aoh/blab.git && cd blab && make && sudo make install

# just make  file
blab -l . -e 'led.prelude ":w hello.io.out\r:q\r"' > hello.io

# write foo to a file
blab -l . -e 'led.prelude "i" "foo" led.esc ":w foo.io.out\r:q\r"' > foo.io

# line-based copy and paste
blab -l . -e 'led.prelude "i" "foo\rbar" led.esc ":1\rddpp" led.esc ":w copy-1.io.out\r:q\r"' > copy-1.io

# marks
blab -l . -e 'led.prelude "mai" "foo\rbar" led.esc 39 "aiX" led.esc ":w mark-1.io.out\r:q\r"' > mark-1.io

# regexp search
blab -l . -e 'led.prelude "i foo foo " led.esc "ddppp:1\rdd:2\r/foo\rnnnnniX" led.esc ":w search-1.io.out\r:q\r"' > search-1.io

# ai test
blab -l . -e 'led.prelude "i(define (map f l)\r(if (null? l)\rnull\r(cons (f (car l))\r(map f (cdr l)))))\r; foo" led.esc ":w ai-1.io.out\r:q\r"' > ai-1.io

# del words single line
blab -l . -e 'led.prelude "ifoo bar baz quux\r" led.esc "/bar\r2dw$p:w del-words-sl.io.out\r:q\r"' > del-words-sl.io

# del words multi line
blab -l . -e 'led.prelude "ifoo\rbar \rbaz  \rquux   " led.esc "/bar\r2dw$p:w del-words-ml.io.out\r:q\r"' > del-words-ml.io
