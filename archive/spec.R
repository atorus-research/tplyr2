#' Create a new tplyr_spec object
#'
#' This function creates a new tplyr_spec S3 object.
#'
#' @param cols Grouping variable names to use as columns
#' @param where An expression used to filter the incoming data
#' @param pop_data Settings to use on the population data
#' @param layers Specifications of layers to use in the table
#' @param settings Table level configuration settings
#'
#' @return A tplyr_spec object
#' @export
#'
tplyr_spec <- function(
  cols,
  where,
  pop_data = NULL,
  layers = tplyr_layers(),
  settings = NULL
) {
  validate_tplyr_spec(cols, where, pop_data, layers, settings)
  new_tplyr_spec(cols, where, pop_data, layers, settings)
}

#' Create a new tplyr_spec object
#'
#' This function creates a new tplyr_spec
#'
#' @param cols Grouping variable names to use as columns
#' @param where An expression used to filter the incoming data
#' @param pop_data Settings to use on the population data
#' @param layers Specifications of layers to use in the table
#' @param settings Table level configuration settings
#'
#' @return A tplyr_spec object
#' @noRd
new_tplyr_spec <- function(cols, where, pop_data, layers, settings) {
  structure(
    list(
      cols = cols,
      where = where,
      pop_data = pop_data,
      settings = settings
    ),
    class = "tplyr_spec"
  )
}

validate_tplyr_spec <- function(cols, where, pop_data, layers, settings) {
  assert_arg(cols, "cols", "character")
  assert_arg(where, "where", "expr", atomic = TRUE)
}

#' Print method for tplyr_spec objects
#'
#' @param x A tplyr_spec object
#' @param ... Additional arguments passed to print
#'
#' @export
# print.tplyr_spec <- function(x, ...) {

# }

#' Check if an object is a tplyr_spec
#'
#' @param x An object to check
#'
#' @return Logical indicating whether x is a tplyr_spec object
#' @export
is_tplyr_spec <- function(x) {
  inherits(x, "tplyr_spec")
}
