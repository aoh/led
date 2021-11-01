# Led: a simple text editor

Led is a text editor for character terminals. It is a new editor written from
scratch in a lisp dialect, in which sense it is similar to emacs, but it is
actually based on its rival vi, and to some extent, Sam and ACME.

A vi user will find led fairly familiar at first glance. Moving around,
inserting text and saving it to a file works about the same. Most other
commands are quite different. The good news is that they are also simpler. All
edit operations replace the current selection with something else. For example,
rather than deleting rest of the line with d$, you select rest of the line with
$ and then delete it with d. If you don't have a good movement command in mind
for some edit operation, you can just use H, J, K and L to select the desired
content.

Led allows editing several files in buffers. A buffer is typically associated
with a file, but one can also open directories, have anonymous buffers, and
associate buffers with an external program. In any case, buffers hold text and
behave the same.


## Requirements

Led should work on most UNIXish systems. You need git, a C-compiler, and make
to get the code and compiled led. Source code checkouts also fetch a lisp
compiler, if it is needed during compilation, but it is not needed for running
led.


## Building

```sh
   $ git clone https://haltp.org/git/led.git # or https://gitlab.com/akihe/led
   $ cd led
   $ make
   $ sudo make install
   $ led .
```

## Commands

See `:help`.


## Internals

### Static Threads

These are led-specific threads that are always running.

`ui` is the main thread receiving terminal input, draw requests and controlling
which buffer thread gets to do something.

`input-terminal` reads terminal input and forwards input events to 'ui. The main
reason to have this thread is to translate input to messages, so that 'ui needs
to only deal with messages.

`clock` sends current time every minute to subscribing threads.

`log` writes messages sent to it to log file, if one is given at startup via --log.

`screen` is responsible of actually writing anything to the terminal.


### Buffer Threads

There is always at least one buffer thread running. These are opened by ui
thread, and the names are always freshly allocated and thus unique.

`(scratch)`, a buffer thread which, was at least initially not associated with a
specific path.

`<string>`, a buffer thread, which was at least initially associated with the given
path..

`(help)` is a buffer internally opened via :help.

`(<buffer-id> . 'status-thread)` is a thread responsible of rendering the bottom
line of a buffer.


## Hacking

Led is defined in led/led.scm. It can be built by fetching the compiler,
compiling the sources to a standalone C-program, and then compiling the C-code
to native code. This is done automatically when you run `make`.

You can also interpret the source code by issuing `bin/ol --run led/led.scm
[led arguments here]`.


