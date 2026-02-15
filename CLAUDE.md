# Claude Code Instructions

## Temporary Files

All temporary files created for planning, testing, scratch work, or any
other transient purpose must be placed in the `scratch/` directory at
the project root.

## Coding Conventions

### Iteration: Use purrr, not base R apply/loops

- Use `map()`, `map_chr()`, `map_dbl()`, `map_lgl()`, `map_int()`
  instead of [`lapply()`](https://rdrr.io/r/base/lapply.html),
  [`vapply()`](https://rdrr.io/r/base/lapply.html),
  [`sapply()`](https://rdrr.io/r/base/lapply.html)
- Use `walk()`, `iwalk()`, `imap()` for side-effect iteration instead of
  `for` loops
- **Exception**: `for` loops iterating over columns (e.g., setting
  data.table columns by reference) are acceptable
- Prefer the most specific typed variant when the return type is known
  (`map_chr` over `map` for character results, etc.)

### Strings: Use stringr, not base R string functions

- `str_detect()` instead of
  [`grepl()`](https://rdrr.io/r/base/grep.html)
- `str_subset()` instead of `grep(..., value = TRUE)`
- `str_replace()` / `str_replace_all()` instead of
  [`sub()`](https://rdrr.io/r/base/grep.html) /
  [`gsub()`](https://rdrr.io/r/base/grep.html)
- `str_sub()` instead of
  [`substr()`](https://rdrr.io/r/base/substr.html) /
  [`substring()`](https://rdrr.io/r/base/substr.html)
- `str_extract()` / `str_extract_all()` instead of
  [`regmatches()`](https://rdrr.io/r/base/regmatches.html) +
  [`regexpr()`](https://rdrr.io/r/base/grep.html) /
  [`gregexpr()`](https://rdrr.io/r/base/grep.html)
- `str_locate()` / `str_locate_all()` instead of
  [`regexpr()`](https://rdrr.io/r/base/grep.html) /
  [`gregexpr()`](https://rdrr.io/r/base/grep.html) for positions
- `str_c()` instead of [`paste0()`](https://rdrr.io/r/base/paste.html) /
  [`paste()`](https://rdrr.io/r/base/paste.html)
- `str_glue()` instead of
  [`sprintf()`](https://rdrr.io/r/base/sprintf.html) for string
  interpolation
- `str_length()` instead of
  [`nchar()`](https://rdrr.io/r/base/nchar.html)
- `str_trim()` instead of
  [`trimws()`](https://rdrr.io/r/base/trimws.html)
- `str_split()` instead of
  [`strsplit()`](https://rdrr.io/r/base/strsplit.html) (use `fixed()`
  wrapper for fixed patterns)
- `str_to_lower()` / `str_to_upper()` instead of
  [`tolower()`](https://rdrr.io/r/base/chartr.html) /
  [`toupper()`](https://rdrr.io/r/base/chartr.html)
- **Exception**: [`formatC()`](https://rdrr.io/r/base/formatc.html) and
  [`format()`](https://rdrr.io/r/base/format.html) for numeric
  formatting remain as-is (no stringr equivalent)
