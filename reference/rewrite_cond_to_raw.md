# Rewrite a condition expression to use raw survey variables

Replaces symbols in a parsed expression with their corresponding `raw_`
variable names when those raw columns exist in the data.

## Usage

``` r
rewrite_cond_to_raw(expr, data_names)
```

## Arguments

- expr:

  A parsed R expression, typically from
  [`rlang::parse_expr()`](https://rlang.r-lib.org/reference/parse_expr.html).

- data_names:

  Character vector of column names available in the data.

## Value

A modified expression object.
