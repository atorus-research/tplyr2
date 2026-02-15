# Compute sort key for a variable

Returns an integer or numeric vector of sort keys for the values of a
variable. Priority: factor levels \> VARN companion column \>
alphabetical. The `method` parameter can override this auto-detection.

## Usage

``` r
compute_var_order(
  values,
  var_name = NULL,
  data_dt = NULL,
  method = NULL,
  count_values = NULL
)
```

## Arguments

- values:

  Character or factor vector of values to sort

- var_name:

  Character string naming the variable (for VARN lookup)

- data_dt:

  data.table with the raw data (for VARN lookup)

- method:

  Character: NULL (auto), "byfactor", "byvarn", "bycount",
  "alphabetical"

- count_values:

  Numeric vector of counts per row (for bycount method)

## Value

Numeric vector of sort keys (lower = earlier)
