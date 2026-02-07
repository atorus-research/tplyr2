# data.table uses non-standard evaluation with special symbols.
# Declare them here to avoid R CMD check NOTEs and make them
# available within package functions.
# nocov start
.datatable.aware <- TRUE
# nocov end

# Import data.table operators for use within the package
#' @import data.table
#' @importFrom rlang `%||%`
NULL
