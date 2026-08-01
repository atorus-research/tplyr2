# Serialize an expression (or NULL)

[`rlang::expr_deparse()`](https://rlang.r-lib.org/reference/expr_print.html)
wraps at 60 characters by default, which any realistic multi-condition
filter exceeds. `parse_expr()` on read needs a single string, so the
deparsed pieces are always collapsed to one.

## Usage

``` r
serialize_expr(expr)
```
