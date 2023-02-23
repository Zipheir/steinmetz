# steinmetz

This is a command-line option parsing library for Scheme. It is
currently a work-in-progress, and quite rough.

Suggestions are appreciated.

## Goals

* Prefer portable (R[67]RS) Scheme.

* Support a reasonable subset of the option styles that GNU getopt
  and Haskell’s optparse-applicative can handle.

* Provide a nice high-level macro language for describing command
  line syntax.

* Keep it simple, stupid.

## Author

Wolfgang Corcoran-Mathe

Email: wcm at sigwinch dot xyzzy without the zy

## Acknowledgments

Thanks to tomhg for suggesting the name.

Thanks to Paolo Capriotti for his complicated but inspiring
post on [applicative CLI parsing][0].

## Interface

### Options

Command-line options are described by Option structures.

The public elements of an Option include:

* Names (a list of symbols)
* Arity (number of arguments; a non-negative integer)
* Help text (a string; optional)
* Argument names (names of arguments, in order; a list of symbols;
  optional)

#### Procedures

`(make-option names [arg-name [conv]])`

Constructs a new Option. *names* is a list of symbols; each element is
a short or long name (without leading dashes) for this option. *arg-name*
is a symbol giving the name of the option’s argument. It defaults to
`ARG`. If *arg-name* is `#f`, then the option takes no arguments.

*conv* is an argument conversion procedure of type `String Procedure → *`.
When the option is parsed, each argument string will be passed to *conv*
along with a *failure* continuation. If *conv* returns normally, the
result will be the value of the argument. If instead *failure* is invoked
on a string (message), then a parser exception is raised.

Example:
```
    (let ((conv (lambda (s failure)
                  (let ((res-or-false (string->number s)))
                    (or res-or-false (failure "not a number"))))))
      (make-option '(n num) 'NUM conv))
```

`(option? x)`

True if *x* is an Option and false otherwise.

`(option-names opt)`

Returns the names of *opt* as a list of symbols. Names do not include
leading dashes. Thus, `widget` is the name of the option spelled
`--widget` on the command-line.

`(option-arity opt)`

Returns the number of arguments expected by *opt*.

`(option-map proc opt)`

*proc* should be a procedure taking a list. Returns a new Option which
applies *proc* to the option’s arguments after parsing.

`(option-add-help msg opt)`

Updates *opt* with the help text *msg* (a string).

`(option-help opt)`

Returns the help text of *opt* or `#f`.

`(option-add-arg-names names opt)`

Updates *opt* with the list of metavariable names *names*. It is an
error if the length of *names* doesn’t match *opt*’s arity.

#### Syntax

`(options <clause> ...)`

The `options` form is the main interface to the library. It’s used to
declare a set of CLI options with the usual properties. Each clause
describes a single option and is of one of the following forms:

```
    (flag   <name-or-names> [<help-text>])
    (option <name-or-names> [<arg-name> [<conv> [<help-text>]]])
```

The `(flag …)` form describes a boolean flag which takes no arguments;
`(option …)` describes an option taking a single argument.
`name-or-names` is either a symbol or a list of symbols (both unquoted)
and gives the acceptable forms of an option. `arg-name` and `conv` have
the same meaning as they do in `make-option`. `help-text` is a string
describing the option.

The value of an `options` form is a list of option structures
representing the CLI options described by its clauses.

Example:
```
    (define my-opts
      (options
        (option (f file)    FILE accept      "input file")
        (option (k chunk)   NUM  number-conv "chunk to operate on")
        (flag   (v verbose)                  "verbose output")))
```

Here, `accept` and `number-conv` are argument converters wrapping
`values` and `string->number`, respectively.

[0]: https://www.paolocapriotti.com/blog/2012/04/27/applicative-option-parser/
