tplyr_layers <- function(...) {
  ls <- list(...)
  if (!all(vapply(ls, is_tplyr_layer, TRUE))) {
    stop("Objects provided to `tplyr_layers()` must be `tplyr_layer` objects")
  }
  ls
}

add_tpylr_layers <- function(x, ...) {
  ls <- tplyr_layers(...)
  x['layers'] <- append(x['layers'], ls)
}

new_tplyr_layer <- function(targets, by, where, type, settings = NULL) {
  assert_arg(targets, "targets", "character")
  assert_arg(by, "by", "character")
  assert_arg(where, "where", "expr", atomic = TRUE)
  if (!is.null(settings)) {
    assert_arg(settings, "settings", "tplyr_layer_settings")
  }

  structure(
    list(
      targets = targets,
      by = by,
      where = where,
      settings = settings
    ),
    class = c(type, "tplyr_layer", "list")
  )
}

group_count <- function(targets, by, where, settings = NULL) {
  new_tplyr_layer(targets, by, where, "count_layer", settings)
}

group_desc <- function(targets, by, where, settings = NULL) {
  new_tplyr_layer(targets, by, where, "desc_layer", settings)
}

group_shift <- function(targets, by, where, settings = NULL) {
  new_tplyr_layer(targets, by, where, "desc_layer", settings)
}

is_tplyr_layer <- function(x) {
  inherits(x, "tplyr_layer")
}
