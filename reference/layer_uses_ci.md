# Does a count layer's formats reference any CI keyword?

Scans the layer's `format_strings` and `stat_columns` f_str `$vars` for
one of the four confidence-interval keywords, so the (comparatively
expensive) CI computation can be skipped entirely for layers that don't
display one.

## Usage

``` r
layer_uses_ci(settings)
```

## Arguments

- settings:

  A tplyr_layer_settings object

## Value

Logical scalar
