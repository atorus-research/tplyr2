# Read a tplyr_spec from JSON or YAML

Deserializes a spec from a file. Expressions are reconstructed from
their string representations.

## Usage

``` r
tplyr_read_spec(path)
```

## Arguments

- path:

  File path to a JSON or YAML spec file

## Value

A tplyr_spec object

## See also

[`tplyr_write_spec()`](https://atorus-research.github.io/tplyr2/reference/tplyr_write_spec.md)
to write one.

## Examples

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  where = SAFFL == "Y",
  layers = tplyr_layers(group_count("AGEGR1", by = "SEX"))
)

path <- file.path(tempdir(), "spec.json")
tplyr_write_spec(spec, path)

# The round trip reproduces the spec, and the same table
spec2 <- tplyr_read_spec(path)
spec2
#> tplyr2 table specification
#> Column variables: TRT01PWhere: SAFFL == "Y"Layers: 1[1] count: AGEGR1 (Layer 1)
identical(tplyr_build(spec, tplyr_adsl), tplyr_build(spec2, tplyr_adsl))
#> [1] TRUE

# tplyr_build() also accepts a spec file path directly
head(tplyr_build(path, tplyr_adsl))
#>   rowlabel1 rowlabel2       res1       res2       res3 ord_layer_1
#> 1         F       <65  9 (10.5%)  5 ( 6.0%)  5 ( 6.0%)           1
#> 2         F     65-80 22 (25.6%) 28 (33.3%) 28 (33.3%)           2
#> 3         F       >80 22 (25.6%)  7 ( 8.3%) 17 (20.2%)           3
#> 4         M       <65  5 ( 5.8%)  6 ( 7.1%)  3 ( 3.6%)           4
#> 5         M     65-80 20 (23.3%) 27 (32.1%) 19 (22.6%)           5
#> 6         M       >80  8 ( 9.3%) 11 (13.1%) 12 (14.3%)           6
#>   ord_layer_index
#> 1               1
#> 2               1
#> 3               1
#> 4               1
#> 5               1
#> 6               1

unlink(path)
```
