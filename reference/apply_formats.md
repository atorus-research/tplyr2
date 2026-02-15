# Apply format strings to numeric values

Vectorized formatting function. Takes an f_str object and numeric
vectors, returns a character vector of formatted strings.

## Usage

``` r
apply_formats(fmt, ..., precision = NULL)
```

## Arguments

- fmt:

  An f_str object or character format string

- ...:

  Numeric vectors, one per variable in the f_str (positional matching)

- precision:

  Optional list of resolved precision per group (for auto-precision)

## Value

Character vector of formatted values
