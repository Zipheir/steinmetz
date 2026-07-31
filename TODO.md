This file may be out of date. Don’t take it seriously.

* ✗ Support optional and default arguments?

  Only if you do these yourself using `parse-command-line`.

* ✓ Allow the procedure invoked by `parse-command-line` to halt parsing,
  maybe.  This would allow library users to decide how to handle
  interleaved operands, for example.

* ✓ `process-command-line` should follow POSIX: halt when the first
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

  Maybe?

* ✓ Support `--`.

* ✓ Support `--` as an argument.

* More documentation-formatting features?

* Argument splitting (with `,` or space delimiters) on demand.

  This could be supported via an option property.  If
  `argument-delimiter`, say, has a character associated with it, then
  split the argument on that character.

* Completions for bash, etc.

* ✓ Property for allowed argument values, e.g. for “enum”-type options.

* Diversify parser errors (missing argument exceptions should be
  distinguishable from invalid option exceptions, for example).

* Default arguments.  Support for these is probably the biggest missing
  feature, at the moment, but they are surprisingly difficult to
  handle.

  `process-command-line` *can* be adapted to support default arguments,
  if you either (A) initialize all options to their defaults or to a
  unique “unset” object and abstract over the option’s argument slot,
  or (B) “backpatch” all omitted options with their defaults after
  parsing is complete.  (Option A requires a mutable option-argument
  slot, and a change of key type (from string to option) for the
  options alist.)

  Things are much harder with `parse-command-line`, however.  They
  can’t be added to the options at the start of parsing, because that
  would be ambiguous: how do you tell whether an option appeared with
  its default argument or was omitted?  (This is a problem when options
  appear more than once.)  They also can’t be backpatched, since the
  type of `parse-command-line`’s return values is user-defined.

  So this is unresolved.  Suggestions are welcome.

* ✓ Could `make-cli-option` and `make-cli-flag` be removed?  This would
  leave the high-level `options` form and the nuts & bolts of
  `(steinmetz options)`.

* ✗ Could we eliminate the dummy "flag" argument parser?  The parsing
  loop could simply check whether the selected option takes and argument
  and continue if it does not.

  Better: Use a parser that signals an error if the next token looks
  like an argument.

* ✓ Read dpk’s writings on CLI parsing in the `let-posix`
  [repository](https://codeberg.org/dpk/let-posix).

* Get rid of the pre-processor.  Sometimes command lines are very long,
  and there's no way to stop pre-processing when the first operand is
  encountered without writing a (second!) parser).
