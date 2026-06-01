# Format a NOTE or WARNING message for report output

Creates a formatted HTML string representing a message with a severity
level (e.g. NOTE or WARNING), suitable for use in R Markdown documents
with `results = 'asis'`.

## Usage

``` r
report_notice(..., level = c("WARNING", "NOTE"))
```

## Arguments

- ...:

  Character content of the message (concatenated).

- level:

  Character string indicating message severity. One of `"WARNING"` or
  `"NOTE"` (default).

## Value

A character string containing HTML-formatted message text.
