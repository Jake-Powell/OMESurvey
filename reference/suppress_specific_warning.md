# Suppress warnings matching a specific pattern

Evaluates an expression while suppressing only those warnings whose
message matches a given regular expression. All other warnings are
allowed to propagate normally.

## Usage

``` r
suppress_specific_warning(expr, pattern)
```

## Arguments

- expr:

  Expression to evaluate.

- pattern:

  Regular expression used to identify warnings that should be
  suppressed.

## Value

A list with two elements:

- `value` — the result of evaluating `expr`

- `suppressed` — a character vector of suppressed warning messages

## Examples
