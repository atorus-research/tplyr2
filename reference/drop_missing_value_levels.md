# Drop target-variable levels that were folded into the Missing row

Values named in `missing_count$missing_values` are counted in the
Missing row. Without removing them here they would *also* keep their own
category row, counting the same records twice and pushing the column
past 100%.

## Usage

``` r
drop_missing_value_levels(counts, tv, missing_count)
```

## Arguments

- counts:

  data.table of category counts

- tv:

  Character string, target variable name

- missing_count:

  The layer's `missing_count` setting

## Value

`counts` with the folded-in levels removed
