# Tidy eval helpers

- [`sym()`](https://rlang.r-lib.org/reference/sym.html) creates a symbol
  from a string and
  [`syms()`](https://rlang.r-lib.org/reference/sym.html) creates a list
  of symbols from a character vector.

- [`enquo()`](https://rlang.r-lib.org/reference/topic-defuse.html) and
  [`enquos()`](https://rlang.r-lib.org/reference/topic-defuse.html)
  delay the execution of one or several function arguments. `enquo()`
  returns a single quoted expression, which is like a blueprint for the
  delayed computation. `enquos()` returns a list of such quoted
  expressions.

- [`expr()`](https://rlang.r-lib.org/reference/topic-defuse.html) quotes
  a new expression *locally*. It is mostly useful to build new
  expressions around arguments captured with `enquo()` or `enquos()`:
  `expr(mean(!!enquo(arg), na.rm = TRUE))`.

- [`as_name()`](https://rlang.r-lib.org/reference/as_name.html)
  transforms a quoted variable name into a string. Supplying something
  else than a quoted variable name is an error.

  That's unlike
  [`as_label()`](https://rlang.r-lib.org/reference/as_label.html) which
  also returns a single string but supports any kind of R object as
  input, including quoted function calls and vectors. Its purpose is to
  summarise that object into a single label. That label is often
  suitable as a default name.

  If you don't know what a quoted expression contains (for instance
  expressions captured with `enquo()` could be a variable name, a call
  to a function, or an unquoted constant), then use `as_label()`. If you
  know you have quoted a simple variable name, or would like to enforce
  this, use `as_name()`.

To learn more about tidy eval and how to use these tools, visit the
[Metaprogramming section](https://adv-r.hadley.nz/metaprogramming.html)
of [Advanced R](https://adv-r.hadley.nz).
