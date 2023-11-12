library(data.table)
library(rlang)

## NSE-ish like evaluation ----
load(testthat::test_path("adsl.Rdata"))
adsl_dt <- as.data.table(adsl)
x <- expr(list(USUBJID, "test"))
adsl_dt[, eval(x)]

## Basic concept of counts with data.table ----
load(testthat::test_path("adsl.Rdata"))

# Create data.table object and apply a filter
x <- as.data.table(adsl)[SAFFL == "Y"]

# Get header N's
header_ns <- x[,
               .N,
               by = .(TRT01P)
]

# Calculate little Ns
c1 <- x[,
       .(n=.N),
       by = .(TRT01P, SEX, RACE)
]

# Calculate pct
c2 <- merge(y, header_ns, by = "TRT01P")[
  ,
  `:=`(
    pct = n/N * 100
  )
]

# String format
c3 <- z[,
        n_pct := Tplyr::apply_formats("xx (XX.x)", n, pct)
][,.(TRT01P, SEX, RACE, n_pct)]

c3

# Transpose with RACE as idvar and TRT01P and SEX as column variables on n_pct
dcast(c3, RACE ~ TRT01P + SEX, fill = " 0       ", value.var = "n_pct")

## Descriptive statistics with data.table ----

d1 <- x[,
  .(
    n       = .N,
    mean    = mean(AGE, na.rm=TRUE),
    sd      = sd(AGE, na.rm=TRUE),
    median  = median(AGE, na.rm=TRUE),
    var     = var(AGE, na.rm=TRUE),
    min     = min(AGE, na.rm=TRUE),
    max     = max(AGE, na.rm=TRUE),
    iqr     = IQR(AGE, na.rm=TRUE, type=7),
    q1      = quantile(AGE, na.rm=TRUE, type=7)[[2]],
    q3      = quantile(AGE, na.rm=TRUE, type=7)[[4]],
    missing = sum(is.na(AGE))
  ),
  by=.(TRT01P, SEX, RACE)
  ]

# Character format in place
d1[,
  `:=`(
    `Mean (SD)` = Tplyr::apply_formats("xx.x (xx.xx)", mean, sd),
    `Median` = Tplyr::apply_formats("xx.x", median),
    `Q1, Q3` = Tplyr::apply_formats("xx.x - xx.x", q1, q3)
  )
]

# Transpose results down
d2 <- melt(
  d1, 
  id.vars = c("TRT01P", "RACE", "SEX"), 
  measure.vars = c("Mean (SD)", "Median", "Q1, Q3")
)

# Transpose out by columns
dcast(d2, RACE + variable ~ TRT01P + SEX, value.var = "value")
