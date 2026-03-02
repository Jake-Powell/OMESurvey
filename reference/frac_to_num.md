# Convert simple text fractions to numeric values

Parses character strings of the form `"a/b"` and returns their numeric
value \\a/b\\. Intended for ordering or comparing simple fractions.

## Usage

``` r
frac_to_num(x)
```

## Arguments

- x:

  A character vector of fractions written as `"a/b"`.

## Value

A numeric vector of the same length as `x`.

## Examples

``` r
frac_to_num(c("1/4", "1/2", "3/4"))
#> [1] 0.25 0.50 0.75
```
