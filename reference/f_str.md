# Create a format string object

Create a format string object

## Usage

``` r
f_str(format_string, ..., empty = NULL)
```

## Arguments

- format_string:

  Character string defining the display template

- ...:

  Character strings naming the variables that populate the template

- empty:

  Value to display when data is NA/missing. Supplied as
  `c(.overall = "...")`, it replaces the entire cell, but only once
  *every* format group in the string is NA. Supplied unnamed (e.g.
  `empty = "NA"`), it instead fills each NA format group in place,
  right-justified to the width that group would have occupied, so a
  partially missing cell keeps its alignment –
  `f_str("xx (xxx)", "n", "pct", empty = "NA")` renders `"NA ( NA)"`.
  The default (`NULL`) leaves NA groups as blanks of the field width.

## Value

A tplyr_f_str object
