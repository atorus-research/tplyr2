# Wrap strings to a specific width with hyphenation while preserving indentation

Leverages
[`stringr::str_wrap()`](https://stringr.tidyverse.org/reference/str_wrap.html)
under the hood, but takes extra steps to preserve any indentation that
has been applied to a character element, and use hyphenated wrapping of
single words that run longer than the allotted wrapping width.

## Usage

``` r
str_indent_wrap(x, width = 10, tab_width = 5)
```

## Arguments

- x:

  An input character vector

- width:

  The desired width of elements within the output character vector

- tab_width:

  The number of spaces to which tabs should be converted

## Value

A character vector with string wrapping applied

## Details

[`stringr::str_wrap()`](https://stringr.tidyverse.org/reference/str_wrap.html)
is highly efficient, but in the context of table creation there are two
features missing — hyphenation for long running strings that overflow
width, and respect for pre-indentation of a character element. For
example, in an adverse event table, you may have body system rows as an
un-indented column, and preferred terms as indented columns. These
strings may run long and require wrapping to not surpass the column
width. Furthermore, for crowded tables a single word may be longer than
the column width itself.

This function resolves these two issues, while minimizing additional
overhead required to apply the wrapping of strings.

Note: This function automatically converts tabs to spaces. Tab width
varies depending on font, so width cannot automatically be determined
within a data frame. Users can specify the width via `tab_width`.

## Examples

``` r
ex_text1 <- c("RENAL AND URINARY DISORDERS", "   NEPHROLITHIASIS")
ex_text2 <- c("RENAL AND URINARY DISORDERS", "\tNEPHROLITHIASIS")

cat(paste(str_indent_wrap(ex_text1, width = 8), collapse = "\n\n"), "\n")
#> RENAL
#> AND
#> URINARY
#> DISORDE-
#> RS
#> 
#>    NEPHROL-
#>    ITHIASI-
#>    S 
cat(paste(str_indent_wrap(ex_text2, tab_width = 4), collapse = "\n\n"), "\n")
#> RENAL AND
#> URINARY
#> DISORDERS
#> 
#>     NEPHROLIT-
#>     HIASIS 
```
