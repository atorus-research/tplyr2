# Replace leading whitespace with a specified string

Useful for HTML rendering where leading spaces are collapsed.

## Usage

``` r
replace_leading_whitespace(x, replace_with = " ")
```

## Arguments

- x:

  Character vector

- replace_with:

  Replacement string for each leading space

## Value

Character vector with leading spaces replaced

## Examples

``` r
# Indentation survives an HTML renderer that would collapse plain spaces
terms <- c("CARDIAC DISORDERS", "   ATRIAL FIBRILLATION")
out <- replace_leading_whitespace(terms)
out
#> [1] "CARDIAC DISORDERS"      "   ATRIAL FIBRILLATION"

# One replacement per leading space; interior spacing is untouched
replace_leading_whitespace("  A B", replace_with = "-")
#> [1] "--A B"
```
