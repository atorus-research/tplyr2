# Confidence interval for a single binomial proportion

Vectorized computation of a two-sided confidence interval for a single
proportion `x / n`, using one of five standard methods. All methods are
computed in closed form or via
[`qbeta`](https://rdrr.io/r/stats/Beta.html) (no per-row `binom.test`
loop), so the function scales to a full count table's worth of cells at
once.

## Usage

``` r
proportion_ci(
  x,
  n,
  method = c("clopper_pearson", "wilson", "wald", "agresti_coull", "jeffreys"),
  level = 0.95
)
```

## Arguments

- x:

  Numeric vector of event counts (numerators).

- n:

  Numeric vector of trial counts (denominators). Recycled against `x`
  following the usual R rules.

- method:

  One of `"clopper_pearson"` (default), `"wilson"`, `"wald"`,
  `"agresti_coull"`, or `"jeffreys"`.

- level:

  Numeric coverage probability (default `0.95`).

## Value

A `data.table` with two columns, `lower` and `upper`, giving the
interval bounds as proportions in `[0, 1]` (multiply by 100 for the
percentage scale).

## Details

The methods and their references:

- `clopper_pearson`:

  Exact interval based on the beta distribution. Matches
  [`stats::binom.test()`](https://rdrr.io/r/stats/binom.test.html) and
  SAS `PROC FREQ ... EXACT` (the clinical convention). This is the
  default.

- `wilson`:

  Wilson score interval without continuity correction. Matches
  `stats::prop.test(correct = FALSE)`.

- `wald`:

  Normal-approximation ("simple asymptotic") interval, clamped to
  `[0, 1]`.

- `agresti_coull`:

  Adds `z^2/2` pseudo-successes and pseudo-failures, then applies the
  Wald interval to the adjusted counts.

- `jeffreys`:

  Bayesian interval using the Jeffreys `Beta(0.5, 0.5)` prior, with the
  standard boundary adjustments at `x == 0` and `x == n`.

Edge cases: when `n == 0` (or either input is `NA`) both bounds are
`NA`; when `x == 0` the lower bound is exactly `0`; when `x == n` the
upper bound is exactly `1`.

## Examples

``` r
proportion_ci(c(0, 12, 40), c(40, 40, 40), method = "clopper_pearson")
#>        lower     upper
#>        <num>     <num>
#> 1: 0.0000000 0.0880973
#> 2: 0.1656272 0.4653163
#> 3: 0.9119027 1.0000000
```
