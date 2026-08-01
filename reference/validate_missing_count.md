# Validate the keys of a missing_count configuration

`missing_count` is a free-form list, so an unrecognized key used to be
accepted and then never read — the table built without the requested
behavior and nothing pointed at the mistake.

## Usage

``` r
validate_missing_count(missing_count, index)
```

## Arguments

- missing_count:

  The layer's `missing_count` setting

- index:

  Integer layer index

## Value

Invisible TRUE
