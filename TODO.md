This file may be out of date. Don’t take it seriously.

* Support optional and default arguments?

  Only if you do these yourself using `parse-command-line`.

* Allow the procedure invoked by `parse-command-line` to halt parsing,
  maybe.  This would allow library users to decide how to handle
  interleaved operands, for example.

* `process-command-line` should follow POSIX: halt when the first
  operand is reached and return the rest of the tokens as operands.

* ✓ Decide how which argument schemes to support, and figure out how
  they interact with argument names. Should we support multiple
  arguments? The getopt fans say no, except via the `--opt=a,b,c`
  syntax. If we do, should we support variadic options?

  Allow long options, but otherwise follow the POSIX guidelines.

* ✓ `make-option` and `options` argument order: The docstring should
  probably come before the conversion function, since most options
  have documentation but not all need converters.

* ✓ Eliminate failure continuations and allow conversion procedures
  to raise parser exceptions directly. (Thanks sham1).

* Accumulate arguments in option declaration order? (Thanks jcowan)

  I’m no longer (2026) sure what I meant by this.

* Support `--no-foo` when the flag `--foo` has been defined? Should
  this be automatic, or optional? (Thanks jcowan)

  I think not.  Too surprising.

* Support `--`.

  This was done, then undone.  It remains (2026-07) TODO.

* More documentation-formatting features?

* Argument splitting (with `,` or space delimiters) on demand.

  This could be supported via an option property.  If
  `argument-delimiter`, say, has a character associated with it, then
  split the argument on that character.

* Completions for bash, etc.
