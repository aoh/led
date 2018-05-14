# led

[![Build status:](https://travis-ci.org/aoh/led.svg?branch=master)](https://travis-ci.org/aoh/led)

Led is a fairly vi-compatible text editor. It was written mainly to get a
vi, which has good support for editing multiple files, browsing directories
and experimenting with useful extensions.

Led is written in Owl Lisp, so the editor is highly portable across various
UNIX flavors. The editor also compiles to a standalone binary which requires
no external libraries.

## Current vi/ex features

Led has been used to implement itself after roughly first week of development.

Currently working features consist mainly of:
 - most usual vi edit mode commands
 - some ex commands
   - e.g. `:ab lambda λ`, `:set showmatch`, `:set ai` and `:set sm`
 - autoindent and paren matching
 - ex command history
 - extended regular expressions (via owl)

## Currently implemented extensions

 - multiple buffers
    - `:n` opens a new buffer
    - `Ctrl-n` and `Ctrl-p` switch buffers
    - `:move <n>` moves buffer to position `n`
    - `:q` and `Q` closes the current buffer
  - directory buffers
    - `$ led .` and `:n <directory>` open a buffer with directory contents
    - `enter` on a line in directory buffer opens the file or toggles subdirectory contents
       - `set recursive-open-directory=off` toggles behavior on directories
    - otherwise it's a regular editable buffer to which you can add notes, paths, etc.
  - experimental simple search buffers
    - `:search <foo>` searches for the literal string `<foo>` from paths in the directory buffer at position 1, if it's there.
    - the resulting buffer contains matching file paths, offsets and sample content
    - pressing `enter` on the matching line jumps to the match
  - lisp extensions
 - some lisp extensions
    - `:<range>l <lisp-extension>` calls the lisp function with contents of the range
    - `:%l sort` sorts contents
    - `:l date` replaces range with current date (you may want to `:set utc-offset +2.0`)

## Building

```
$ git clone https://github.com/aoh/led.git
$ cd led
$ make
$ bin/led
```

## Common issues

Led does not automatically resize buffer after resizing terminal. Press `^L` to have it recompute the terminal size.

File content search is slow if there are lots of files. You can remove directories holding uninteresting files from buffer 1 if this is an issue for now.

The cursor does not move when you press the arrow keys. You should be using `h`, `j`, `k` and `l`. Arrow keys will be added at some point though.

Only extended regular expressions are currently supported.

There is no line wrap. This is an opinionated feature.
I'd rather see part of the real structure than a broken representation of all of it.

Terminal scrolling is fairly slow. This can be optimized, but I haven't bothered yet since `^F` and `^B` are fast enough even on a Raspberry Pi.
