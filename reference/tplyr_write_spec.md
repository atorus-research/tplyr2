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
