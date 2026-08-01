# Create a population data configuration

Configuration object specifying how population data maps to the spec.
The actual population data.frame is provided at build time via
`tplyr_build(spec, data, pop_data = ...)`.

## Usage

``` r
pop_data(cols, where = NULL)
```

## Arguments

- cols:

  Character vector of column variable names in the population data. If
  named, names are the spec column names and values are the pop_data
  column names (e.g., `c("TRTA" = "TRT01P")`). If unnamed, maps
  positionally to spec cols.

- where:

  Expression for filtering the population data (optional)

## Value

A tplyr_pop_data object

## Examples

``` r
# The AE data's TRTA maps to the subject-level TRT01P. Denominators and the
# header N then come from the population, not from the AE records.
spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01P")),
  layers = tplyr_layers(
    group_count("AEBODSYS",
                settings = layer_settings(distinct_by = "USUBJID"))
  )
)
built <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
head(built)
#>                                              rowlabel1       res1       res2
#> 1                                    CARDIAC DISORDERS  5 ( 5.8%)  6 ( 7.1%)
#> 2           CONGENITAL, FAMILIAL AND GENETIC DISORDERS  0 ( 0.0%)  1 ( 1.2%)
#> 3                           GASTROINTESTINAL DISORDERS  6 ( 7.0%)  6 ( 7.1%)
#> 4 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS 11 (12.8%) 21 (25.0%)
#> 5                              IMMUNE SYSTEM DISORDERS  0 ( 0.0%)  0 ( 0.0%)
#> 6                          INFECTIONS AND INFESTATIONS  5 ( 5.8%)  4 ( 4.8%)
#>         res3 ord_layer_1 ord_layer_index
#> 1  6 ( 7.1%)           1               1
#> 2  0 ( 0.0%)           2               1
#> 3  3 ( 3.6%)           3               1
#> 4 21 (25.0%)           4               1
#> 5  1 ( 1.2%)           5               1
#> 6  3 ( 3.6%)           6               1

# Header N reflects the 254 enrolled subjects, not the 200 AE records
tplyr_header_n(built)
#>                   TRTA .n
#> 1              Placebo 86
#> 2 Xanomeline High Dose 84
#> 3  Xanomeline Low Dose 84

# Restrict the population to the safety set
saf <- pop_data(cols = c("TRTA" = "TRT01P"), where = SAFFL == "Y")
saf
#> tplyr2 population data config
#> Columns: TRT01PWhere: SAFFL == "Y"
```
