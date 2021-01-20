# Led: a simple text editor

Led is a simple text editor. It is based on the vi-editor, which is found on 
most UNIX systems.

## Status

The editor was recently rewritten to enable some of the less vi-ish features.
The editor is usable, but several features are not yet available in the current
version.

## Features / bugs

 - Not line based!
 - Unicode support
 - Infinite undo/redo
 - Delta screen update
 - Asynchronous thread-based operation
 - Multiple buffers
 - Subprocesses
 - Portable
 - No external dependencies, standalone binary!
 - A natural source of parenthesis!
 
# Building

```sh
   $ git clone https://haltp.org/git/led.git
   $ cd led
   $ make
   $ bin/led .
```

# Commands

See `:help`.

