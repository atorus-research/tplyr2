# Create a format string object

Create a format string object

## Usage

``` r
f_str(format_string, ..., empty = NULL)
```

## Arguments

- format_string:

  Character string defining the display template

- ...:

  Character strings naming the variables that populate the template

- empty:

  Value to display when data is NA/missing. Supplied as
  `c(.overall = "...")`, it replaces the entire cell, but only once
  *every* format group in the string is NA. Supplied unnamed (e.g.
  `empty = "NA"`), it instead fills each NA format group in place,
  right-justified to the width that group would have occupied, so a
  partially missing cell keeps its alignment –
  `f_str("xx (xxx)", "n", "pct", empty = "NA")` renders `"NA ( NA)"`.
  The default (`NULL`) leaves NA groups as blanks of the field width.

## Value

A tplyr_f_str object

## Details

Each run of `x` characters is one format group, and each group is filled
by the correspondingly-positioned variable in `...`. The count of `x`s
sets the field width, so `"xx.x"` renders two integer digits and one
decimal. Literal text between groups is preserved verbatim. `a` (and
`A`) request auto-precision, where the decimal count comes from the
data.

## See also

[`apply_formats()`](https://atorus-research.github.io/tplyr2/reference/apply_formats.md)
to render values outside a build.

## Examples

``` r
# Two format groups filled by n and pct
fmt <- f_str("xx (xx.x%)", "n", "pct")
fmt
#> tplyr format string: "xx (xx.x%)"
#>   Variables: n, pct
apply_formats(fmt, n = c(5, 12), pct = c(4.5, 33.33))
#> [1] " 5 ( 4.5%)" "12 (33.3%)"

# Width is set by the number of x's
apply_formats(f_str("xxx", "n"), n = 7)
#> [1] "  7"
apply_formats(f_str("x", "n"), n = 7)
#> [1] "7"

# `empty` fills each NA group in place, preserving alignment
apply_formats(f_str("xx (xxx)", "n", "pct", empty = "NA"),
              n = NA, pct = NA)
#> [1] "NA ( NA)"

# `.overall` replaces the whole cell, but only when every group is NA
both_na <- f_str("xx (xxx)", "n", "pct", empty = c(.overall = "Not est."))
apply_formats(both_na, n = NA, pct = NA)
#> [1] "Not est."

# Used in a layer
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("AGEGR1", settings = layer_settings(
      format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct"))))
  )
)
tplyr_build(spec, tplyr_adsl)
#>   rowlabel1       res1       res2       res3 ord_layer_1 ord_layer_index
#> 1       <65 14 (16.3%) 11 (13.1%)  8 ( 9.5%)           1               1
#> 2     65-80 42 (48.8%) 55 (65.5%) 47 (56.0%)           2               1
#> 3       >80 30 (34.9%) 18 (21.4%) 29 (34.5%)           3               1
```
