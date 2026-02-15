pop_data <- function(cols, where) {
  assert_arg(cols, "cols", "character")
  assert_arg(cols, "where", "expr", atomic = TRUE)

  structure(
    list(cols = cols, where = where),
    class = c("tplyr_popdata", "list")
  )
}

set_pop_data <- function(x, cols, where) {
  if (!is_tplyr_spec(x)) {
    stop("`x` must be a `tplyr_spec` object")
  }

  pd <- pop_data(cols, where)
  x['pop_data'] <- pd
}
