# data.table uses non-standard evaluation with special symbols.
# Declare them here to avoid R CMD check NOTEs and make them
# available within package functions.
# nocov start
.datatable.aware <- TRUE
# nocov end

# Import data.table operators for use within the package
#' @import data.table
#' @importFrom rlang `%||%`
#' @importFrom purrr map map_chr map_dbl map_lgl map_int walk walk2 imap iwalk map2 map2_chr keep discard
#' @importFrom stringr str_detect str_subset str_which str_replace str_replace_all str_sub `str_sub<-` str_extract str_extract_all str_match str_match_all str_locate str_locate_all str_glue str_c str_length str_pad str_trim str_split str_to_upper str_to_lower str_wrap fixed
NULL
