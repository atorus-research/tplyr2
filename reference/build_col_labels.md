# Build column labels with header N suffix

Takes raw dcast column names and a col_n data.table, returns labels with
"(N=)" suffix. For shift layers where the label includes both spec-level
cols and the shift column variable, only the spec-level portion is used
for the N lookup.

## Usage

``` r
build_col_labels(raw_labels, col_n)
```

## Arguments

- raw_labels:

  Character vector of raw column labels from dcast

- col_n:

  data.table with spec-level column variables and .n column, or NULL (in
  which case labels are returned unchanged)

## Value

Character vector of labels with N suffix
