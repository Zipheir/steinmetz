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

[0]: https://www.paolocapriotti.com/blog/2012/04/27/applicative-option-parser/
