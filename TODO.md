Figure out how the applicative libraries collect options in a record
structure regardless of their order on the line.

We should be able to collect results "idiomatically" without first
building a list and then folding it.

The fold interface is quite flexible, however. I don't yet see
a reason to change it.

It should be possible to give an option several names, but this
raises some challenges. When an option has multiple names, which
do we pass to the collector--the one parsed, or a canonical name?
It seems clear that error messages should refer to the parsed
name, but this complicates error-message generation. Now an argument
parser needs to know the last token parsed; this is a significant
complication.

A possible solution: Instead of passing the collection procedure
an option name, we pass it the option structure. This makes the
user's life more complex. We also have to pick a canonical name
for the simple 'parse-cli->alist' interface; there's no way around
it. I don't see a simple way to solve the error message issue.

dpk's suggestion: Print all of an option's names in error messages.

Done.

The monoid-like structure of option properties is not really so
simple. Most properties should only be added once, while others
(like default values and help strings) are order-dependent.
