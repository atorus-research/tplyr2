# Collapse row labels into a single column

This is a generalized post processing function that allows you to take
groups of by variables and collapse them into a single column. Repeating
values are split into separate rows, and for each level of nesting, a
specified indentation level can be applied.

## Usage

``` r
collapse_row_labels(
  x,
  ...,
  indent = "  ",
  target_col = "row_label",
  nest = FALSE
)
```

## Arguments

- x:

  Input data frame

- ...:

  Column names (as character strings) to be collapsed, must be 2 or more

- indent:

  Indentation string to be used, which is multiplied at each indentation
  level

- target_col:

  Character string naming the output column containing collapsed row
  labels

- nest:

  Logical. If TRUE, collapse row labels in-place without inserting stub
  rows for repeating values. Allows a single column to be passed.
  Default is FALSE.

## Value

data.frame with row labels collapsed into a single column

## Examples

``` r
x <- data.frame(
  row_label1 = c("A", "A", "A", "B", "B"),
  row_label2 = c("C", "C", "D", "E", "F"),
  var1 = 1:5,
  stringsAsFactors = FALSE
)

collapse_row_labels(x, "row_label1", "row_label2")
#>   row_label var1
#> 1         A   NA
#> 2         C    1
#> 3         C    2
#> 4         D    3
#> 5         B   NA
#> 6         E    4
#> 7         F    5

collapse_row_labels(x, "row_label1", "row_label2", indent = "    ",
                    target_col = "rl")
#>      rl var1
#> 1     A   NA
#> 2     C    1
#> 3     C    2
#> 4     D    3
#> 5     B   NA
#> 6     E    4
#> 7     F    5
```
