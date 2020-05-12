# Led: a simple text editor

Led is a simple text editor. It was written after realizing it is in the long run much easier write a
custom text editor than it is to configure an existing one. Text editors are simple. At least the good
ones are. Not all simple ones are good, though. Led may become good, but in the meantime it is sufficient
to be good enough to handle all the needs I have at home and at work.

While led is written from scratch, the key ideas in it are definitely not new. Led is based on vi, but
the edit operations bear more resemblence to the text editor Sam. The idea of using text from buffers
also for semantic operations was borrowed from Acme. Led is also written entirely in Lisp, much like
Emacs.

## Status

The editor was recently rewritten to enable some of the less vi-ish features. The editor is usable,
but many important features have not yet been added back to this version.

## Features

 - Not line based!
 - Unicode support
 - Infinite undo/redo
 - Delta screen update
 - Asynchronous thread-based operation
 - Multiple buffers
 - Subprocesses
 - Portable
 - No external dependencies, standalone binary!
 - Purely functional, no mutations allowed!

# Building

   $ git clone https://haltp.org/git/led.git
   $ cd led
   $ make
   $ bin/led .


# Commands

## Buffer commands

These commands work everywhere regardless off buffer content and edit mode.

Ctrl-n: switch to next buffer

Ctrl-p: switch to previous buffer

Ctrl-h: go to first buffer

Ctrl-q: forcibly close current buffer


### Command mode commands

Ctrl-f: forward one page

Ctrl-b: back one page

Ctrl-l: repaint and clear status message

Ctrl-w: write current buffer

Ctrl-x: send current selection to subprocess (if any) and paste output after selection

Enter: open selected file or expand directory contents

i: delete selectin and enter insert mode

y: yank selection to copy buffer

$: move to end of current line

w: expand selection by one word

m: wait for a key and mark position

': wait for a key and jump to corresponding marked position

.: select current line, including newline if present

h: move left

j: move down

k: move up

l: move right

H: shrink selection by one letter (as in movement)

J: grow selection to next line (as in movement)

L: grow selection by one letter (as in movement)

0: go to beginnig of line

d: delete selection and place content to yank buffer

p: replace selection with content of yank buffer

u: undo command

r: redo command

>: indent selection

<: unindent selection

%: find matching paren forward

e: select parent lisp expression of current selection

W: select word to which current selection belongs

N: toggle line numbers

/: start searching, currently always from beginnig of buffer

n: find next match to last search

Q: close buffer

### Inset mode

Esc: exit instert mode

Ctrl-w: write current buffer, including changes made in insert mode so far

Arrows: move buffer viewport

backspace: delete an inserted character (but stay in selection)



#led
