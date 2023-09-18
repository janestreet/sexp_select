open! Base

(** sexp select "programs" are themselves formed by a series of sexps.

    Individual atoms represent fields that should be matched, with
    a series of atoms indicating matching nested values.

    A list of atoms matches any of the atoms in the list. Lists cannot
    be nested.

    The special atom "*" matches any key.

    The special atom ">" indicates that the _next_ "match" (a plain
    identifier, an "*", or a set of identifiers) must be a direct child.

    Examples:

    foo             : matches "foo"
    foo bar         : matches "bar"s inside "foo"s
    foo > bar       : matches a "bar" that's an immediate child of a "foo"
    foo > (bar baz) : matches a "bar" or "baz" that's an immediate child of a "foo"
    bar > *         : matches all immediate children of a "bar"
*)

val parse : string -> Program.t
