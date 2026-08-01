# Extract variable labels from a data.frame

Returns a named character vector of variable labels. Labels are
extracted from the `"label"` attribute of each column (standard for
Haven-imported CDISC data).

## Usage

``` r
get_data_labels(data)
```

## Arguments

- data:

  A data.frame

## Value

Named character vector where names are column names and values are
labels. Columns without labels return `NA_character_`.

## Examples

``` r
# CDISC data imported via haven carries a "label" attribute per column
labs <- get_data_labels(tplyr_adsl)
head(labs)
#>                            STUDYID                            USUBJID 
#>                 "Study Identifier"        "Unique Subject Identifier" 
#>                             SUBJID                             SITEID 
#> "Subject Identifier for the Study"            "Study Site Identifier" 
#>                            SITEGR1                                ARM 
#>              "Pooled Site Group 1"       "Description of Planned Arm" 

# Columns with no label attribute come back NA
get_data_labels(data.frame(a = 1, b = 2))
#>  a  b 
#> NA NA 
```
