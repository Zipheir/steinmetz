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
* Help text (a string; optional)
* Argument name (a symbol; meta-name of the option’s argument)

#### Procedures

`(make-option names [arg-name [conv]])`

Constructs a new Option. *names* is a list of symbols; each element is
a short or long name (without leading dashes) for this option. *arg-name*
is a symbol giving the name of the option’s argument. It defaults to
`ARG`. If *arg-name* is `#f`, then the option does not take an argument.

*conv* is an argument conversion procedure of type `String → *`.
When the option is parsed, the argument string will be passed to *conv*.
The value of the argument will be whatever *conv* returns. Converters
can use `parser-exception` to signal an invalid argument.

Example:
```
    (let ((conv (lambda (s)
                  (let ((res-or-false (string->number s)))
                    (or res-or-false
                        (parser-exception "not a number" s))))))
      (make-option '(n num) 'NUM conv))
```

`(option? x)`

True if *x* is an Option and false otherwise.

`(option-names opt)`

Returns the names of *opt* as a list of symbols. Names do not include
leading dashes. Thus, `widget` is the name of the option spelled
`--widget` on the command-line.

`(option-map proc opt)`

*proc* should be a procedure taking a list. Returns a new Option which
applies *proc* to the option’s argument value after parsing.

`(option-add-help msg opt)`

Updates *opt* with the help text *msg* (a string).

`(option-help opt)`

Returns *opt*’s help text or `#f` if none exists.

`(option-add-argument-name name opt)`

Updates *opt* with the metavariable name *name*.

`(parser-exception msg irritant ...)`

Raises an exception indicating that something went wrong during
command-line parsing. *Details to be worked out soon.*

#### Syntax

`(options <clause> ...)`

The `options` form is the main interface to the library. It’s used to
declare a set of CLI options with the usual properties. Each clause
describes a single option and is of one of the following forms:

```
    (flag   <name-or-names> [<help-text>])
    (option <name-or-names> [<arg-name> [<help-text> [<conv>]]])
```

The `(flag …)` form describes a boolean flag which takes no arguments;
`(option …)` describes an option taking an argument.
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
        (option (f file)    FILE "input file")
        (option (k chunk)   NUM  "chunk to operate on" number-conv)
        (flag   (v verbose)      "verbose output")))
```

Here, `number-conv` is an argument converter wrapping `string->number`.

[0]: https://www.paolocapriotti.com/blog/2012/04/27/applicative-option-parser/
