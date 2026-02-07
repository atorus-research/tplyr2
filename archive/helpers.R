assert_arg <- function(x, arg_name, typ, atomic = FALSE) {
  if (inherits(x, 'tplyr_arg')) {
    arg_class <- attr(x, 'arg_class')
    is_expr <- attr(x, 'is_expr')
  } else {
    arg_class <- class(x)
    is_expr <- rlang::is_expression(x)
  }

  msg <- sprintf(
    "The %s argument must be provided as a %s %s or matching `tplyr_arg`",
    arg_name,
    ifelse(atomic, "single element", ""),
    typ
  )

  if (arg_class != typ && (typ == "expr" && !is_expr)) {
    stop(msg)
  }
  if (atomic && !is_expr && length(x) > 1) {
    stop(msg)
  }
}
