# Atomic-vector types of the plain layer settings

JSON arrays always parse back as lists (`simplifyVector = FALSE`), which
erases both the vector type and the distinction between a length-1
vector and a list. This table is the single source of truth used by
[`deserialize_settings()`](https://atorus-research.github.io/tplyr2/reference/deserialize_settings.md)
to restore each plain setting to the vector type the build code expects.
`test-serialize.R` asserts that every
[`layer_settings()`](https://atorus-research.github.io/tplyr2/reference/layer_settings.md)
formal appears here or in `serialize_special_fields`, so a newly added
setting cannot silently round-trip as a list.

## Usage

``` r
settings_field_types()
```

## Value

Named list of character vectors, keyed by storage mode
