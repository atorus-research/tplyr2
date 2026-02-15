# Apply parenthesis hugging to a format group

Shifts leading spaces from the formatted number to after the trailing
literal, so that characters like `(` hug the number.

## Usage

``` r
hug_format_group(prefix, num_part, trailing_literal)
```

## Arguments

- prefix:

  Character vector of accumulated result so far

- num_part:

  Character vector of formatted numbers (with leading spaces)

- trailing_literal:

  Character string of the literal after this group

## Value

Character vector with hugged result
