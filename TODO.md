This file may be out of date. Don’t take it seriously.

* ✓ Figure out how the applicative libraries collect options in a
  record structure regardless of their order on the line.

  Done. Too complicated to be worth the effort.

* The fold interface is quite flexible, so let’s keep it. We should
  provide a "dumb" driver returning an alist, though, for people who
  don’t need the general form. Figure out what this should look like.

* ✓ It should be possible to give an option several names, but this
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

  Adopted dpk's suggestion: Print all of an option's names in error
  messages.