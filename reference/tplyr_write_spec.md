# Write a tplyr_spec to JSON or YAML

Serializes a spec object to a file. The format is determined by the file
extension: `.json` for JSON, `.yaml` or `.yml` for YAML.

## Usage

``` r
tplyr_write_spec(spec, path)
```

## Arguments

- spec:

  A tplyr_spec object

- path:

  File path. Extension determines format.

## Value

Invisible file path

## Details

Expressions (e.g., `where` clauses) are deparsed to strings and
reconstructed on read. Format string objects (`f_str`) are stored as
their component parts and regenerated on read.

## See also

[`tplyr_read_spec()`](https://atorus-research.github.io/tplyr2/reference/tplyr_read_spec.md)
to read one back.

## Examples

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  where = SAFFL == "Y",
  layers = tplyr_layers(
    group_count("AGEGR1", by = "SEX", settings = layer_settings(
      denoms_by = c("TRT01P", "SEX"),
      format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct"))))
  )
)

path <- file.path(tempdir(), "spec.json")
tplyr_write_spec(spec, path)
cat(readLines(path), sep = "\n")
#> {
#>   "cols": "TRT01P",
#>   "where": {
#>     "_expr": "SAFFL == \"Y\""
#>   },
#>   "layers": [
#>     {
#>       "target_var": "AGEGR1",
#>       "by": "SEX",
#>       "where": null,
#>       "layer_type": "count",
#>       "settings": {
#>         "format_strings": {
#>           "n_counts": {
#>             "format_string": "xx (xx.x%)",
#>             "vars": ["n", "pct"],
#>             "_class": "tplyr_f_str"
#>           }
#>         },
#>         "denoms_by": ["TRT01P", "SEX"],
#>         "shift_denom": "total",
#>         "denom_row": false,
#>         "denom_row_label": "n",
#>         "total_row": false,
#>         "total_row_label": "Total",
#>         "total_row_count_missings": true,
#>         "missing_subjects": false,
#>         "missing_subjects_label": "Missing",
#>         "stats_as_columns": false,
#>         "ci_method": "clopper_pearson",
#>         "ci_level": 0.95,
#>         "zero_count_display": "full"
#>       }
#>     }
#>   ]
#> }

# YAML is chosen by the file extension
if (requireNamespace("yaml", quietly = TRUE)) {
  ypath <- file.path(tempdir(), "spec.yaml")
  tplyr_write_spec(spec, ypath)
  cat(readLines(ypath), sep = "\n")
  unlink(ypath)
}
#> cols: TRT01P
#> where:
#>   _expr: SAFFL == "Y"
#> layers:
#> - target_var: AGEGR1
#>   by: SEX
#>   where: ~
#>   layer_type: count
#>   settings:
#>     format_strings:
#>       n_counts:
#>         format_string: xx (xx.x%)
#>         vars:
#>         - 'n'
#>         - pct
#>         _class: tplyr_f_str
#>     denoms_by:
#>     - TRT01P
#>     - SEX
#>     shift_denom: total
#>     denom_row: no
#>     denom_row_label: 'n'
#>     total_row: no
#>     total_row_label: Total
#>     total_row_count_missings: yes
#>     missing_subjects: no
#>     missing_subjects_label: Missing
#>     stats_as_columns: no
#>     ci_method: clopper_pearson
#>     ci_level: 0.95
#>     zero_count_display: full
#> 
unlink(path)
```
