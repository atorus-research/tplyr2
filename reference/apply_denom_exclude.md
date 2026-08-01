# Drop missing-counted rows from the denominator source

Implements `missing_count$denom_exclude`: the rows folded into the
Missing row leave the denominator, so the layer's percentages are of the
non-missing population rather than of everyone.

## Usage

``` r
apply_denom_exclude(denom_dt, tv, missing_count, layer_index = NULL)
```

## Arguments

- denom_dt:

  data.table used as the denominator source

- tv:

  Character string, target variable name

- missing_count:

  The layer's `missing_count` setting

- layer_index:

  Integer layer index, used in the warning message

## Value

`denom_dt`, filtered when `denom_exclude` is TRUE
