(define-library (led documentation)

   (import
      (owl toplevel)
      (led log)
      (led buffer))

   (export
      help-buffer)

   (begin

      (define main-help
"GLOBAL COMMANDS ---------------------------------------------------------------

  Ctrl-n - next, switch to next buffer
  Ctrl-p - previous, switch to previous buffer
  Ctrl-q - quit, lose all changes in all buffers and exit led directly


COMMAND MODE ------------------------------------------------------------------

   h - move left by one character up to newline or start of buffer
   j - move down by one line
   k - move up by one line
   l - move right by one line up to newline or and of buffer

   i - enter insert mode

   . - select current line including newline
   H - shrink the current selection by 1 character
   J - select the rest of the current line and next line up to current position
   L - grow the current selection by 1 character
   K - shrink the selection to previous line up to current position

   d - delete the current selection and copy it
   p - paste last copied content over the current selection
   y - copy current selection to a global buffer

   n - repeat last search forwards
   m<char> - save a mark named by char to current selection (offset and length)
   '<char> - go to selection named by char

   $ - select rest of line
   0 - select beginning of line
   . - select current line

   u - undo last command
   r - redo last undone command

   > - indent selection
   < - undent selection

   Q - close current buffer

   : - start typing a command
   / - start typing a search

   w - select next word and subsequent whitespace
   e - select the parent LISP expression
   % - select everything (equals 1,$)

   N - toggle line numbers

   esc    - shrink current selection to empty string
   enter  - attempt to open the selected content or current line

   Ctrl-j - justify selected content keeping paragraphs separate
   Ctrl-e - remove trailing whitespaces from entire buffer
   Ctrl-l - repaint screen
   Ctrl-w - write buffer to file, if path is known (same as :w)
   Ctrl-x - send selection to subprocess (see SUBPROCESSES)


INSERT MODE -------------------------------------------------------------------

   Esc - exit insert mode


LEX COMMANDS ------------------------------------------------------------------

 :w [path]   - write content of current buffer to path, or the last one if
               path was not given (same as Ctrl-w)
 :read path  - replace current selection with the contents of the given file,
               and select the new contents
 :d[elete]   - delete current selection
 :u[ndo]     - undo last command
 :redo       - redo last undone command
 :q[uit]     - quit if buffer has been saved
 :q[uit]!    - quit whether or not buffer is saved
 :new [path] - open a new empty buffer or the given file
 :next-match - find next search match forwards

 :subprocess [binary] [arg] ...
             - start a persistent subprocess for use with Ctrl-x

 :call <function> - replace selection with result of calling a plugin function

 :<regex>    - apply regular expression on selection
 :set <variable> <value> - set and editor setting. see VARIABLES below.


EDITOR VARIABLES --------------------------------------------------------------

 :set tabstop <number> - change tab width


PLUGIN FUNCTIONS --------------------------------------------------------------

  sort        - sort lines lexicographically
  rev         - reverse contents of lines
  date        - insert current date (UTC)
  fmt         - format paragraphs in selection (also Ctrl-j)
  clean       - remove trailing whitespaces from lines in selection (% + Ctrl-e)
  del         - delete selection
  crash       - crash the buffer thread (don't do this)

")


      (define (help-buffer subject)
         (log "HELP: opening help on " subject)
         (cond
            (else
               (string-buffer main-help))))))
