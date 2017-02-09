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
blab -l . -e 'led.prelude ":set ai\ri(define (map f l)\r(if (null? l)\rnull\r(cons (f (car l))\r(map f (cdr l)))))\r; foo" led.esc ":w ai-1.io.out\r:q\r"' > ai-1.io

# del words single line
blab -l . -e 'led.prelude "ifoo bar baz\rquux" led.esc "/bar\r2dw0p:w del-words-sl.io.out\r:q\r"' > del-words-sl.io

# del words multi line
blab -l . -e 'led.prelude "ifoo\rbar \rbaz  \rquux   " led.esc "/bar\r2dwjp:w del-words-ml.io.out\r:q\r"' > del-words-ml.io

# cut mark back single line
blab -l . -e 'led.prelude "ia\rfoo bar baz\rb" led.esc "/bar\rma/baz\rd" 39 "a:w cut-back-sl.io.out\r:q\r"' > cut-back-sl.io

# cut mark forward single line
blab -l . -e 'led.prelude "ia\rfoo bar baz\rb" led.esc "/baz\rma/bar\rd" 39 "a:w cut-forward-sl.io.out\r:q\r"' > cut-forward-sl.io

# cut mark forward multi line
blab -l . -e 'led.prelude "ia\rfoo FOO\rbar BAR\rbaz BAZ\rb" led.esc "/FOO\rma/BAZ\rd" 39 "a:w cut-forward-ml.io.out\r:q\r"' > cut-forward-ml.io

# cut mark backward multi line
blab -l . -e 'led.prelude "ia\rfoo FOO\rbar BAR\rbaz BAZ\rb" led.esc "/BAZ\rma/FOO\rd" 39 "a:w cut-back-ml.io.out\r:q\r"' > cut-back-ml.io

# tabstop
blab -l . -e 'led.prelude "ia\t" led.esc ":set expandtab\r:set tabstop=3\ra\tb" led.esc ":w tabstop.io.out\r:q\r"' > tabstop.io

# buffer dirtiness
blab -l . -e 'led.prelude "ifoo" led.esc ":w dirty.io.out\rabar" led.esc ":q\rabaz" led.esc "WQ"' > dirty.io

# directory buffer
blab -l . -e 'led.prelude ":n ..\r/test\r\r/dirlist.io.ok\r\r:w dirlist.io.out\rQQQQ"' > dirlist.io
