# Sort column names by their numeric suffix

Lexicographic sorting places "res10" before "res2"; ordering by the
numeric suffix keeps columns in build order once a family has more than
9 members.

## Usage

``` r
sort_by_numeric_suffix(x)
```
