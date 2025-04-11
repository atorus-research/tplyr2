arg <- function(...) {
  x <- list(...)
  if (is.null(names(x))) {
    stop("Arguments must be named")
  }

  if (length(x) > 1) {
    stop("Arguments can only be a single named element")
  }

  structure(
    x,
    class = c("tplyr_arg", "list"),
    arg_class = class(x[[1]]),
    is_expr = rlang::is_expression(x)
  )
}

args <- function(...) {
  args <- unlist(list(...), recursive = FALSE)

  structure(
    args,
    class = c("tplyr_args", "list")
  )
}
