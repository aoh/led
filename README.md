# Led: a simple text editor

Led is a text editor. It was written after using various flavors of vi for over a decade.
Led retains the modal operation of vi, and some of its commands, but the internal data structure
and approach to edit operatoins is slightly different.


## Requirements

Led should work on most UNIXish systems. You need a C compiler, make, and Owl Lisp is
downloaded during compilation but is not needed when running led.


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

'ui is the main thread receiving terminal input, draw requests and controlling
which buffer thread gets to do something.

'input-terminal reads terminal input and forwards input events to 'ui. The main
reason to have this thread is to translate input to messages, so that 'ui needs
to only deal with messages.

'clock sends current time every minute to subscribing threads.

'log writes messages sent to it to log file, if one is given at startup via --log.

'screen is responsible of actually writing anything to the terminal.


### Buffer Threads

There is always at least one buffer thread running. These are opened by ui
thread, and the names are always freshly allocated and thus unique.

'(scratch), a buffer thread which, was at least initially not associated with a
specific path.

<string>, a buffer thread, which was at least initially associated with the given
path..

'(help) is a buffer internally opened via :help.

(<buffer-id> . 'status-thread) is a thread responsible of rendering the bottom
line of a buffer.


## Hacking

Led is defined in led/led.scm. It can be built by fetching the compiler,
compiling the sources to a standalone C-program, and then compiling the C-code
to native code. This is done automatically when you run `make`.

You can also interpret the source code by issuing `bin/ol --run led/led.scm
[led arguments here]`.

