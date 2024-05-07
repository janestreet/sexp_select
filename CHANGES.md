
## Release v0.17.0

- updated [select_single_exn] to fail on multiple matches, as promised by a comment in its
  interface.

- added support for selection of keys containing spaces.

- added a -drop flag to [sexp select] and [sexp multi-select] that will output the
  original sexp with the matching fields removed, rather than printing out the matches
  themselves.

## Release v0.16.0

- Added new function `Sexp_select.select_staged`, which is a staged version of
  `Sexp_select.select` that is more efficient in scenarios when evaluate the same select
  expression across multiple inputs.
