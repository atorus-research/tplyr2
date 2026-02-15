# Spec Serialization

## Introduction

One of the central design principles of tplyr2 is the separation of
specification from data. A
[`tplyr_spec()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_spec.md)
object describes *what* a table should contain – the column structure,
the layers, the formatting rules – without touching any actual dataset.
Data is only supplied at build time via `tplyr_build(spec, data)`.

This separation creates a natural opportunity: if the spec is pure
configuration, it can be saved to a file and loaded back later. tplyr2
supports serializing specs to both JSON and YAML formats. This opens up
several practical workflows:

- **Version control**: Specs stored as JSON or YAML are plain text, so
  they integrate naturally with Git. You can track changes to your table
  definitions alongside your analysis code.
- **Portability**: A spec file can be shared between team members or
  across studies. The recipient loads it and builds against their own
  data.
- **Reproducibility**: Archiving a spec alongside its output creates a
  clear record of exactly what configuration produced a given table.
- **Regulatory submissions**: A machine-readable table definition can
  serve as supporting documentation in a submission package.

## Writing a Spec to JSON

The
[`tplyr_write_spec()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_write_spec.md)
function takes a spec object and a file path. The format is determined
by the file extension: use `.json` for JSON output.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  where = SAFFL == "Y",
  layers = tplyr_layers(
    group_count("SEX", by = "Sex n (%)"),
    group_desc(
      "AGE",
      by = "Age (Years)",
      settings = layer_settings(
        format_strings = list(
          "n"         = f_str("xxx", "n"),
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Median"    = f_str("xx.x", "median"),
          "Min, Max"  = f_str("xx, xx", "min", "max")
        )
      )
    )
  )
)

json_path <- tempfile(fileext = ".json")
tplyr_write_spec(spec, json_path)
```

The file is now a plain-text JSON document. Let’s verify that we can
read it back and build the same table.

## Reading a Spec Back

The
[`tplyr_read_spec()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_read_spec.md)
function reads a spec from a JSON or YAML file and reconstructs the full
`tplyr_spec` object, including all expressions, format strings, and
layer configurations.

``` r
loaded_spec <- tplyr_read_spec(json_path)
loaded_spec
#> tplyr2 table specification
#> Column variables: TRT01PWhere: SAFFL == "Y"Layers: 2[1] count: SEX (Layer 1)[2] desc: AGE (Layer 2)
```

The loaded spec is functionally identical to the original. You can build
it against any dataset that has the required columns.

``` r
result <- tplyr_build(loaded_spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1   | rowlabel2 | res1         | res2         | res3         |
|:------------|:----------|:-------------|:-------------|:-------------|
| Sex n (%)   | F         | 53 (61.6%)   | 40 (47.6%)   | 50 (59.5%)   |
| Sex n (%)   | M         | 33 (38.4%)   | 44 (52.4%)   | 34 (40.5%)   |
| Age (Years) | n         | 86           | 84           | 84           |
| Age (Years) | Mean (SD) | 75.2 ( 8.59) | 74.4 ( 7.89) | 75.7 ( 8.29) |
| Age (Years) | Median    | 76.0         | 76.0         | 77.5         |
| Age (Years) | Min, Max  | 52, 89       | 56, 88       | 51, 88       |

This is the same output you would get from building the original spec
directly. The round-trip – write to disk, read back, build – preserves
everything.

## YAML Format

If you prefer YAML over JSON, simply use a `.yaml` or `.yml` file
extension. The API is identical.

``` r
yaml_path <- tempfile(fileext = ".yaml")
tplyr_write_spec(spec, yaml_path)

yaml_spec <- tplyr_read_spec(yaml_path)
yaml_result <- tplyr_build(yaml_spec, tplyr_adsl)

# Confirm the results match
identical(
  result[, !grepl("^ord", names(result))],
  yaml_result[, !grepl("^ord", names(yaml_result))]
)
#> [1] TRUE
```

YAML tends to be more readable for human review, while JSON is more
widely supported by automated tools. The choice between them is a matter
of preference; tplyr2 handles both transparently.

## What Gets Serialized

A tplyr_spec can contain several types of R objects that do not have
direct equivalents in JSON or YAML. The serialization system handles
each one with a specific convention.

### Expressions

Filter expressions like `where = SAFFL == "Y"` are R language objects.
They are serialized by deparsing the expression to a string and wrapping
it in a marker object.

In JSON, a `where` clause looks like this:

``` json
{
  "where": {
    "_expr": "SAFFL == \"Y\""
  }
}
```

On deserialization, the string is parsed back into an R expression using
[`rlang::parse_expr()`](https://rlang.r-lib.org/reference/parse_expr.html).
This preserves the original filter logic exactly.

### Format Strings

[`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
objects are stored as their component parts: the format template, the
variable names, and the optional `empty` parameter. A marker class field
(`_class: "tplyr_f_str"`) identifies them for reconstruction.

``` json
{
  "format_string": "xx.x (xx.xx)",
  "vars": ["mean", "sd"],
  "_class": "tplyr_f_str"
}
```

When read back, the
[`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
constructor is called with these components, re-parsing the format
string and rebuilding the internal structure.

### Labels in `by`

When you use
[`label()`](https://github.com/mstackhouse/tplyr2/reference/label.md) in
a `by` parameter to create an explicit text label, the label is
serialized with a type marker so it can be distinguished from a data
variable name.

``` json
{
  "value": "Age (Years)",
  "_type": "label"
}
```

Regular data variable names (character strings) pass through as-is.

### Functions

Analyze layers created with
[`group_analyze()`](https://github.com/mstackhouse/tplyr2/reference/group_analyze.md)
include a user-defined function. These are serialized by deparsing the
function body to a string.

``` json
{
  "_fn": "function(.data, .target_var) {\n  ...\n}"
}
```

On deserialization, the string is parsed and evaluated to recreate the
function object. Note that functions which depend on objects in a
specific environment (closures that capture external variables) may not
survive the round-trip. For best results, write analyze functions that
are self-contained.

## Examining the Serialized Output

Let’s look at the actual JSON content produced by our earlier spec to
see these conventions in practice.

``` r
json_content <- readLines(json_path)
cat(json_content, sep = "\n")
#> {
#>   "cols": "TRT01P",
#>   "where": {
#>     "_expr": "SAFFL == \"Y\""
#>   },
#>   "layers": [
#>     {
#>       "target_var": "SEX",
#>       "by": "Sex n (%)",
#>       "where": null,
#>       "layer_type": "count",
#>       "settings": {
#>         "indentation": "  ",
#>         "total_row": false,
#>         "total_row_label": "Total",
#>         "total_row_count_missings": true,
#>         "missing_subjects": false,
#>         "missing_subjects_label": "Missing",
#>         "stats_as_columns": false
#>       }
#>     },
#>     {
#>       "target_var": "AGE",
#>       "by": "Age (Years)",
#>       "where": null,
#>       "layer_type": "desc",
#>       "settings": {
#>         "format_strings": {
#>           "n": {
#>             "format_string": "xxx",
#>             "vars": "n",
#>             "_class": "tplyr_f_str"
#>           },
#>           "Mean (SD)": {
#>             "format_string": "xx.x (xx.xx)",
#>             "vars": ["mean", "sd"],
#>             "_class": "tplyr_f_str"
#>           },
#>           "Median": {
#>             "format_string": "xx.x",
#>             "vars": "median",
#>             "_class": "tplyr_f_str"
#>           },
#>           "Min, Max": {
#>             "format_string": "xx, xx",
#>             "vars": ["min", "max"],
#>             "_class": "tplyr_f_str"
#>           }
#>         },
#>         "indentation": "  ",
#>         "total_row": false,
#>         "total_row_label": "Total",
#>         "total_row_count_missings": true,
#>         "missing_subjects": false,
#>         "missing_subjects_label": "Missing",
#>         "stats_as_columns": false
#>       }
#>     }
#>   ]
#> }
```

You can see the structure: the top-level `cols` and `where` fields,
followed by the `layers` array. Each layer carries its `target_var`,
`by`, `where`, `layer_type`, and `settings`. Within the settings,
`format_strings` contains the serialized `f_str` objects.

## A More Complex Example

Let’s serialize a spec that exercises more of the system: nested counts
with distinct counting, a `where` clause, and a total row.

``` r
complex_spec <- tplyr_spec(
  cols = "TRT01P",
  where = SAFFL == "Y",
  total_groups = list(total_group("TRT01P", label = "Total")),
  layers = tplyr_layers(
    group_count(
      "RACE",
      by = "Race n (%)",
      settings = layer_settings(
        total_row = TRUE,
        total_row_label = "Total"
      )
    ),
    group_desc(
      c("AGE", "WEIGHTBL"),
      by = "Baseline Measurements",
      settings = layer_settings(
        format_strings = list(
          "n"         = f_str("xxx", "n"),
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Min, Max"  = f_str("xx.x, xx.x", "min", "max")
        )
      )
    )
  )
)

complex_path <- tempfile(fileext = ".json")
tplyr_write_spec(complex_spec, complex_path)
```

Now read it back and build.

``` r
reloaded <- tplyr_read_spec(complex_path)
complex_result <- tplyr_build(reloaded, tplyr_adsl)
kable(complex_result[, !grepl("^ord", names(complex_result))])
```

| rowlabel1             | rowlabel2                        | rowlabel3 | res1         | res2         | res3         | res4         |
|:----------------------|:---------------------------------|:----------|:-------------|:-------------|:-------------|:-------------|
| Race n (%)            | AMERICAN INDIAN OR ALASKA NATIVE |           | 0 ( 0.0%)    | 1 ( 0.4%)    | 1 ( 1.2%)    | 0 ( 0.0%)    |
| Race n (%)            | BLACK OR AFRICAN AMERICAN        |           | 8 ( 9.3%)    | 23 ( 9.1%)   | 9 (10.7%)    | 6 ( 7.1%)    |
| Race n (%)            | Total                            |           | 86 (100.0%)  | 254 (100.0%) | 84 (100.0%)  | 84 (100.0%)  |
| Race n (%)            | WHITE                            |           | 78 (90.7%)   | 230 (90.6%)  | 74 (88.1%)   | 78 (92.9%)   |
| Baseline Measurements | AGE                              | n         | 86           | 254          | 84           | 84           |
| Baseline Measurements | AGE                              | Mean (SD) | 75.2 ( 8.59) | 75.1 ( 8.25) | 74.4 ( 7.89) | 75.7 ( 8.29) |
| Baseline Measurements | AGE                              | Min, Max  | 52.0, 89.0   | 51.0, 89.0   | 56.0, 88.0   | 51.0, 88.0   |
| Baseline Measurements | WEIGHTBL                         | n         | 86           | 253          | 84           | 83           |
| Baseline Measurements | WEIGHTBL                         | Mean (SD) | 62.8 (12.77) | 66.6 (14.13) | 70.0 (14.65) | 67.3 (14.12) |
| Baseline Measurements | WEIGHTBL                         | Min, Max  | 34.0, 86.2   | 34.0, 108.0  | 41.7, 108.0  | 45.4, 106.1  |

The total group, total row, multi-target descriptive layer, and all
formatting survive the round-trip.

## Use Cases

### Version-Controlled Table Definitions

Storing specs as JSON or YAML files in your project repository means
that every change to a table definition is tracked. If a reviewer asks
“what changed between the draft and final version of Table 14.1?”, you
can answer that question with a `git diff` of the spec file.

``` r
# In your analysis script
spec <- tplyr_read_spec("specs/table_14_1.json")
result <- tplyr_build(spec, adsl)
```

The spec file is a plain-text artifact that reviewers can inspect
without running R.

### Sharing Across Teams

A statistician can define the table structure and save the spec. A
programmer on a different system can load it and build against the study
data. The spec travels as a lightweight file rather than an R object
that requires a specific environment.

### Applying a Spec to Different Data

Because specs carry no data, the same spec can be applied to datasets
from different studies, time points, or populations. This is useful for
standardized tables that appear across multiple studies.

``` r
# Same spec, different data subsets
saffl_result <- tplyr_build(loaded_spec, tplyr_adsl[tplyr_adsl$SAFFL == "Y", ])
ittfl_result <- tplyr_build(loaded_spec, tplyr_adsl[tplyr_adsl$ITTFL == "Y", ])
```

### Regulatory Archival

For regulatory submissions, archiving a machine-readable table
definition alongside the analysis output provides an additional layer of
documentation. The JSON or YAML file describes exactly how the table was
configured, in a format that does not require R to interpret.
