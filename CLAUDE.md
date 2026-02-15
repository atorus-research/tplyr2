# Claude Code Instructions

## Temporary Files

All temporary files created for planning, testing, scratch work, or any other transient purpose must be placed in the `scratch/` directory at the project root.

## Coding Conventions

### Iteration: Use purrr, not base R apply/loops
- Use `map()`, `map_chr()`, `map_dbl()`, `map_lgl()`, `map_int()` instead of `lapply()`, `vapply()`, `sapply()`
- Use `walk()`, `iwalk()`, `imap()` for side-effect iteration instead of `for` loops
- **Exception**: `for` loops iterating over columns (e.g., setting data.table columns by reference) are acceptable
- Prefer the most specific typed variant when the return type is known (`map_chr` over `map` for character results, etc.)

### Strings: Use stringr, not base R string functions
- `str_detect()` instead of `grepl()`
- `str_subset()` instead of `grep(..., value = TRUE)`
- `str_replace()` / `str_replace_all()` instead of `sub()` / `gsub()`
- `str_sub()` instead of `substr()` / `substring()`
- `str_extract()` / `str_extract_all()` instead of `regmatches()` + `regexpr()` / `gregexpr()`
- `str_locate()` / `str_locate_all()` instead of `regexpr()` / `gregexpr()` for positions
- `str_c()` instead of `paste0()` / `paste()`
- `str_glue()` instead of `sprintf()` for string interpolation
- `str_length()` instead of `nchar()`
- `str_trim()` instead of `trimws()`
- `str_split()` instead of `strsplit()` (use `fixed()` wrapper for fixed patterns)
- `str_to_lower()` / `str_to_upper()` instead of `tolower()` / `toupper()`
- **Exception**: `formatC()` and `format()` for numeric formatting remain as-is (no stringr equivalent)