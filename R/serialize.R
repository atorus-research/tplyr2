#' Write a tplyr_spec to JSON or YAML
#'
#' Serializes a spec object to a file. The format is determined by the file
#' extension: \code{.json} for JSON, \code{.yaml} or \code{.yml} for YAML.
#'
#' Expressions (e.g., \code{where} clauses) are deparsed to strings and
#' reconstructed on read. Format string objects (\code{f_str}) are stored as
#' their component parts and regenerated on read.
#'
#' @param spec A tplyr_spec object
#' @param path File path. Extension determines format.
#'
#' @return Invisible file path
#' @export
tplyr_write_spec <- function(spec, path) {
  if (!inherits(spec, "tplyr_spec")) {
    stop("'spec' must be a tplyr_spec object", call. = FALSE)
  }

  ext <- str_to_lower(tools::file_ext(path))
  serializable <- spec_to_serializable(spec)

  if (ext == "json") {
    json_str <- jsonlite::toJSON(serializable, auto_unbox = TRUE,
                                  pretty = TRUE, null = "null")
    writeLines(json_str, path)
  } else if (ext %in% c("yaml", "yml")) {
    if (!requireNamespace("yaml", quietly = TRUE)) {
      stop("The 'yaml' package is required for YAML serialization. ",
           "Install with: install.packages('yaml')", call. = FALSE)
    }
    yaml_str <- yaml::as.yaml(serializable)
    writeLines(yaml_str, path)
  } else {
    stop("Unsupported file extension: '.", ext,
         "'. Use .json, .yaml, or .yml", call. = FALSE)
  }

  invisible(path)
}

#' Read a tplyr_spec from JSON or YAML
#'
#' Deserializes a spec from a file. Expressions are reconstructed from their
#' string representations.
#'
#' @param path File path to a JSON or YAML spec file
#'
#' @return A tplyr_spec object
#' @export
tplyr_read_spec <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }

  ext <- str_to_lower(tools::file_ext(path))

  if (ext == "json") {
    raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  } else if (ext %in% c("yaml", "yml")) {
    if (!requireNamespace("yaml", quietly = TRUE)) {
      stop("The 'yaml' package is required for YAML deserialization. ",
           "Install with: install.packages('yaml')", call. = FALSE)
    }
    raw <- yaml::read_yaml(path)
  } else {
    stop("Unsupported file extension: '.", ext, "'", call. = FALSE)
  }

  serializable_to_spec(raw)
}

# ---- Serialization helpers (spec -> plain list) ----

#' Convert spec to a plain list suitable for JSON/YAML
#' @keywords internal
spec_to_serializable <- function(spec) {
  result <- list()

  result$cols <- spec$cols
  result$where <- serialize_expr(spec$where)

  if (!is.null(spec$pop_data)) {
    result$pop_data <- list(
      cols = spec$pop_data$cols,
      where = serialize_expr(spec$pop_data$where)
    )
  }

  if (!is.null(spec$total_groups)) {
    result$total_groups <- map(spec$total_groups, function(tg) {
      list(col_var = tg$col_var, label = tg$label)
    })
  }

  if (!is.null(spec$custom_groups)) {
    result$custom_groups <- map(spec$custom_groups, function(cg) {
      list(col_var = cg$col_var, groups = cg$groups)
    })
  }

  result$layers <- map(spec$layers, serialize_layer)
  result$settings <- spec$settings

  result
}

#' Serialize a single layer
#' @keywords internal
serialize_layer <- function(layer) {
  out <- list(
    target_var = layer$target_var,
    by = serialize_by(layer$by),
    where = serialize_expr(layer$where),
    layer_type = layer$layer_type,
    settings = serialize_settings(layer$settings)
  )

  # For analyze layers, serialize the function
  if (!is.null(layer$analyze_fn)) {
    out$analyze_fn <- serialize_function(layer$analyze_fn)
  }

  out
}

#' Serialize layer settings
#' @keywords internal
serialize_settings <- function(settings) {
  if (is.null(settings)) return(NULL)

  out <- list()

  # format_strings: list of f_str -> serialize each
  if (!is.null(settings$format_strings)) {
    out$format_strings <- map(settings$format_strings, serialize_f_str)
  }

  # Simple pass-through fields
  simple_fields <- c("denoms_by", "denom_ignore", "distinct_by",
                      "indentation", "total_row", "total_row_label",
                      "total_row_count_missings", "missing_subjects",
                      "missing_subjects_label", "keep_levels",
                      "limit_data_by", "stats_as_columns",
                      "precision_by", "precision_on", "precision_cap",
                      "order_count_method", "ordering_cols",
                      "result_order_var", "outer_sort_position", "name")
  for (f in simple_fields) {
    if (!is.null(settings[[f]])) {
      out[[f]] <- settings[[f]]
    }
  }

  # Expression fields
  if (!is.null(settings$denom_where)) {
    out$denom_where <- serialize_expr(settings$denom_where)
  }

  # custom_summaries: named list of expressions
  if (!is.null(settings$custom_summaries)) {
    out$custom_summaries <- map(settings$custom_summaries, function(expr) {
      serialize_expr(expr)
    })
  }

  # risk_diff: list config with optional f_str format
  if (!is.null(settings$risk_diff)) {
    rd <- settings$risk_diff
    out$risk_diff <- list(
      comparisons = rd$comparisons,
      ci = rd$ci
    )
    if (!is.null(rd$format)) {
      out$risk_diff$format <- serialize_f_str(rd$format)
    }
  }

  # missing_count: list with optional f_str format
  if (!is.null(settings$missing_count)) {
    mc <- settings$missing_count
    out$missing_count <- mc
    if (!is.null(mc$format) && inherits(mc$format, "tplyr_f_str")) {
      out$missing_count$format <- serialize_f_str(mc$format)
    }
  }

  # precision_data: data.frame — store as list of lists
  if (!is.null(settings$precision_data)) {
    out$precision_data <- as.list(settings$precision_data)
  }

  out
}

#' Serialize an f_str object
#' @keywords internal
serialize_f_str <- function(fmt) {
  if (is.null(fmt)) return(NULL)
  result <- list(
    format_string = fmt$format_string,
    vars = fmt$vars,
    `_class` = "tplyr_f_str"
  )
  if (!is.null(fmt$empty)) {
    result$empty <- as.list(fmt$empty)
  }
  result
}

#' Serialize an expression (or NULL)
#' @keywords internal
serialize_expr <- function(expr) {
  if (is.null(expr)) return(NULL)
  if (identical(expr, TRUE)) return(TRUE)
  if (identical(expr, FALSE)) return(FALSE)
  list(`_expr` = rlang::expr_deparse(expr))
}

#' Serialize the by parameter
#' @keywords internal
serialize_by <- function(by) {
  if (is.null(by)) return(NULL)

  # Single label object
 if (is_label(by)) {
    return(list(values = as.character(by), `_type` = "label"))
  }

  # List form with potential label elements
  if (is.list(by)) {
    return(map(by, function(b) {
      if (is_label(b)) {
        list(value = as.character(b), `_type` = "label")
      } else {
        b
      }
    }))
  }

  # Simple character vector
  by
}

#' Serialize a function
#' @keywords internal
serialize_function <- function(fn) {
  if (is.null(fn)) return(NULL)
  list(`_fn` = str_c(deparse(fn), collapse = "\n"))
}

# ---- Deserialization helpers (plain list -> spec) ----

#' Convert raw parsed JSON/YAML back to tplyr_spec
#' @keywords internal
serializable_to_spec <- function(raw) {
  where_expr <- deserialize_expr(raw$where)

  # Reconstruct pop_data
  pop <- NULL
  if (!is.null(raw$pop_data)) {
    pop_where <- deserialize_expr(raw$pop_data$where)
    pop <- structure(
      list(cols = unlist(raw$pop_data$cols), where = pop_where),
      class = "tplyr_pop_data"
    )
  }

  # Reconstruct total_groups
  total_groups <- NULL
  if (!is.null(raw$total_groups)) {
    total_groups <- map(raw$total_groups, function(tg) {
      structure(
        list(col_var = tg$col_var, label = tg$label),
        class = "tplyr_total_group"
      )
    })
  }

  # Reconstruct custom_groups
  custom_groups <- NULL
  if (!is.null(raw$custom_groups)) {
    custom_groups <- map(raw$custom_groups, function(cg) {
      # Group values come back as lists from JSON — convert to char vectors
      grps <- map(cg$groups, function(g) unlist(g))
      structure(
        list(col_var = cg$col_var, groups = grps),
        class = "tplyr_custom_group"
      )
    })
  }

  # Reconstruct layers
  layers <- map(raw$layers, deserialize_layer)

  structure(
    list(
      cols = unlist(raw$cols),
      where = where_expr,
      pop_data = pop,
      total_groups = total_groups,
      custom_groups = custom_groups,
      layers = layers,
      settings = raw$settings
    ),
    class = "tplyr_spec"
  )
}

#' Deserialize a layer from raw list
#' @keywords internal
deserialize_layer <- function(raw_layer) {
  target_var <- unlist(raw_layer$target_var)
  by <- deserialize_by(raw_layer$by)
  where_expr <- deserialize_expr(raw_layer$where)
  settings <- deserialize_settings(raw_layer$settings)
  layer_type <- raw_layer$layer_type

  classes <- switch(layer_type,
    "count" = c("tplyr_count_layer", "tplyr_layer"),
    "desc" = c("tplyr_desc_layer", "tplyr_layer"),
    "shift" = c("tplyr_shift_layer", "tplyr_layer"),
    "analyze" = c("tplyr_analyze_layer", "tplyr_layer"),
    c("tplyr_layer")
  )

  result <- list(
    target_var = target_var,
    by = by,
    where = where_expr,
    settings = settings,
    layer_type = layer_type
  )

  # For shift layers, restore names on target_var
  if (layer_type == "shift" && length(target_var) == 2 &&
      is.null(names(target_var))) {
    # target_var names might have been lost in JSON — restore from convention
    names(result$target_var) <- c("row", "column")
  }

  # For analyze layers, restore the function
  if (!is.null(raw_layer$analyze_fn)) {
    result$analyze_fn <- deserialize_function(raw_layer$analyze_fn)
  }

  structure(result, class = classes)
}

#' Deserialize f_str from raw list
#' @keywords internal
deserialize_f_str <- function(raw) {
  if (is.null(raw)) return(NULL)

  # Reconstruct empty parameter
  empty <- NULL
  if (!is.null(raw$empty)) {
    if (is.list(raw$empty)) {
      empty <- unlist(raw$empty)
    } else {
      empty <- raw$empty
    }
  }

  # Reconstruct via f_str() which re-parses the format string
  vars <- unlist(raw$vars)
  do.call(f_str, c(list(raw$format_string), as.list(vars), list(empty = empty)))
}

#' Deserialize expression
#' @keywords internal
deserialize_expr <- function(raw) {
  if (is.null(raw)) return(NULL)
  if (identical(raw, TRUE)) return(TRUE)
  if (identical(raw, FALSE)) return(FALSE)
  if (is.list(raw) && !is.null(raw[["_expr"]])) {
    return(rlang::parse_expr(raw[["_expr"]]))
  }
  raw
}

#' Deserialize by parameter
#' @keywords internal
deserialize_by <- function(raw) {
  if (is.null(raw)) return(NULL)

  # Single label
  if (is.list(raw) && !is.null(raw[["_type"]]) && raw[["_type"]] == "label") {
    return(label(unlist(raw$values)))
  }

  # List form (could contain label elements)
  if (is.list(raw)) {
    # Check if it's a list of items (not a single object)
    has_type <- !is.null(raw[["_type"]])
    if (!has_type && length(raw) > 0) {
      result <- map(raw, function(b) {
        if (is.list(b) && !is.null(b[["_type"]]) && b[["_type"]] == "label") {
          label(b$value)
        } else if (is.list(b)) {
          unlist(b)
        } else {
          b
        }
      })
      # If all elements are simple strings, collapse to character vector
      if (all(map_lgl(result, function(x) is.character(x) && !is_label(x) && length(x) == 1))) {
        return(unlist(result))
      }
      return(result)
    }
  }

  unlist(raw)
}

#' Deserialize layer settings
#' @keywords internal
deserialize_settings <- function(raw) {
  if (is.null(raw)) return(layer_settings())

  # Reconstruct format_strings
  if (!is.null(raw$format_strings)) {
    raw$format_strings <- map(raw$format_strings, deserialize_f_str)
  }

  # Reconstruct denom_where
  if (!is.null(raw$denom_where)) {
    raw$denom_where <- deserialize_expr(raw$denom_where)
  }

  # Reconstruct custom_summaries
  if (!is.null(raw$custom_summaries)) {
    raw$custom_summaries <- map(raw$custom_summaries, deserialize_expr)
  }

  # Reconstruct risk_diff format
  if (!is.null(raw$risk_diff) && !is.null(raw$risk_diff$format)) {
    raw$risk_diff$format <- deserialize_f_str(raw$risk_diff$format)
  }

  # Reconstruct missing_count format
  if (!is.null(raw$missing_count) && !is.null(raw$missing_count$format)) {
    if (is.list(raw$missing_count$format) &&
        !is.null(raw$missing_count$format$format_string)) {
      raw$missing_count$format <- deserialize_f_str(raw$missing_count$format)
    }
  }

  # Reconstruct precision_data
  if (!is.null(raw$precision_data)) {
    raw$precision_data <- as.data.frame(raw$precision_data, stringsAsFactors = FALSE)
  }

  # Remove internal markers
  raw[["_class"]] <- NULL

  # Build via layer_settings() to get proper S3 class
  valid_params <- names(formals(layer_settings))
  args <- raw[intersect(names(raw), valid_params)]
  do.call(layer_settings, args)
}

#' Deserialize a function
#' @keywords internal
deserialize_function <- function(raw) {
  if (is.null(raw)) return(NULL)
  if (is.list(raw) && !is.null(raw[["_fn"]])) {
    fn_expr <- parse(text = raw[["_fn"]])
    return(eval(fn_expr))
  }
  raw
}
