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

``` r
suppress_specific_warning({
  warning("Ignore this one")
  123
}, pattern = "Ignore")
#> $value
#> [1] 123
#> 
#> $suppressed
#> [1] "Ignore this one"
#> 

# Suppress the usual "NAs introduced by coercion" warning
# and use $value to store the result and ignore the $suppressed component
numeric_version <-
  suppress_specific_warning(
    as.numeric(c("2", "3", "z")),
    pattern = "NAs introduced"
  )$value
```
