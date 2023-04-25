## Release v0.16.0

- Added new function `Sexp_select.select_staged`, which is a staged version of
  `Sexp_select.select` that is more efficient in scenarios when evaluate the same select
  expression across multiple inputs.
