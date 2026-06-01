# Append a message to a message list

Helper to add a structured message (with level and text) to a list of
messages, returning the updated list. Intended for internal use within
survey data preparation functions.

## Usage

``` r
add_message(messages, text, level = "NOTE", var = NULL)
```

## Arguments

- messages:

  A list of message objects.

- text:

  Character string giving the message text.

- level:

  Character string indicating severity (e.g. "NOTE", "WARNING").
  Defaults to "NOTE".

- var:

  Optional character string giving the associated variable name.

## Value

The updated list of messages.
