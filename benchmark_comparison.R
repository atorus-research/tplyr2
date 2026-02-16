# Comprehensive Tplyr vs tplyr2 Performance Benchmarks
# Tests across different layer types, data sizes, and build options
#
# Usage:
#   source("benchmark_comparison.R")
#   # Results saved to benchmark_results.rds
#
# This script benchmarks:
#   1. Nested count layers (AEBODSYS/AEDECOD)
#   2. Non-nested count layers
#   3. Desc layers (descriptive statistics)
#   4. Shift layers
#   5. Multiple layer tables
#   6. build(metadata=TRUE) scenarios
#
# Both Tplyr (original) and tplyr2 are benchmarked side-by-side.

library(dplyr)

# ============================================================================
# Configuration - adjust paths to your local setup
# ============================================================================
TPLYR_PATH  <- normalizePath("~/Documents/repos/Tplyr")
TPLYR2_PATH <- normalizePath("~/Documents/repos/tplyr2")

# Load tplyr_adas from Tplyr (tplyr2 doesn't ship this dataset)
load(file.path(TPLYR_PATH, "data", "tplyr_adas.rda"))

# Data size multipliers to test
sizes <- c(1, 10, 50, 100, 250, 500)

# Number of iterations per benchmark for more stable timing
n_iterations <- 3

# Helper function to scale up datasets
scale_dataset <- function(data, size, key_vars) {
  if (size == 1) return(data)

  do.call(rbind, lapply(1:size, function(i) {
    temp <- data
    for (var in key_vars) {
      if (var %in% names(temp)) {
        temp[[var]] <- paste0(temp[[var]], "_", i)
      }
    }
    temp
  }))
}


# ############################################################################
#
#  TPLYR (ORIGINAL) BENCHMARKS
#
# ############################################################################

# ============================================================================
# Tplyr Benchmark 1: Nested Count Layer (AEBODSYS/AEDECOD)
# ============================================================================
run_nested_count_benchmark_tplyr <- function(size, metadata = FALSE) {
  devtools::load_all(TPLYR_PATH, quiet = TRUE)

  adae <- scale_dataset(tplyr_adae, size, c("AEBODSYS", "AEDECOD"))

  x <- Sys.time()

  tplyr_table(adae, TRTA) |>
    set_pop_data(tplyr_adsl) |>
    set_pop_treat_var(TRT01A) |>
    add_layer(
      group_count(vars(AEBODSYS, AEDECOD)) |>
        set_nest_count(TRUE) |>
        set_distinct_by(USUBJID) |>
        set_order_count_method("bycount", break_ties = "desc")
    ) |>
    build(metadata = metadata)

  as.numeric(Sys.time() - x, units = "secs")
}

# ============================================================================
# Tplyr Benchmark 2: Non-nested Count Layer
# ============================================================================
run_count_benchmark_tplyr <- function(size, metadata = FALSE) {
  devtools::load_all(TPLYR_PATH, quiet = TRUE)

  adae <- scale_dataset(tplyr_adae, size, c("AEDECOD"))

  x <- Sys.time()

  tplyr_table(adae, TRTA) |>
    set_pop_data(tplyr_adsl) |>
    set_pop_treat_var(TRT01A) |>
    add_layer(
      group_count(AEDECOD) |>
        set_distinct_by(USUBJID)
    ) |>
    build(metadata = metadata)

  as.numeric(Sys.time() - x, units = "secs")
}

# ============================================================================
# Tplyr Benchmark 3: Desc Layer (Descriptive Statistics)
# ============================================================================
run_desc_benchmark_tplyr <- function(size, metadata = FALSE) {
  devtools::load_all(TPLYR_PATH, quiet = TRUE)

  adas <- scale_dataset(tplyr_adas, size, c("PARAMCD"))

  x <- Sys.time()

  tplyr_table(adas, TRTP, where = EFFFL == "Y" & ITTFL == "Y" & ANL01FL == "Y") |>
    add_layer(
      group_desc(CHG, by = vars(PARAMCD, AVISIT)) |>
        set_format_strings(
          "n"        = f_str("xxx", n),
          "Mean (SD)" = f_str("xxx.x (xxx.xx)", mean, sd),
          "Median"   = f_str("xxx.x", median),
          "Q1, Q3"   = f_str("xxx.x, xxx.x", q1, q3),
          "Min, Max" = f_str("xxx.x, xxx.x", min, max)
        )
    ) |>
    build(metadata = metadata)

  as.numeric(Sys.time() - x, units = "secs")
}

# ============================================================================
# Tplyr Benchmark 4: Shift Layer
# ============================================================================
run_shift_benchmark_tplyr <- function(size, metadata = FALSE) {
  devtools::load_all(TPLYR_PATH, quiet = TRUE)

  adlb <- scale_dataset(tplyr_adlb, size, c("PARAMCD"))

  x <- Sys.time()

  tplyr_table(adlb, TRTA, where = ABLFL != "Y") |>
    add_layer(
      group_shift(vars(row = BNRIND, column = ANRIND), by = PARAMCD) |>
        set_format_strings(f_str("xxx (xx.x%)", n, pct))
    ) |>
    build(metadata = metadata)

  as.numeric(Sys.time() - x, units = "secs")
}

# ============================================================================
# Tplyr Benchmark 5: Multiple Layers (Combined Table)
# ============================================================================
run_multi_layer_benchmark_tplyr <- function(size, metadata = FALSE) {
  devtools::load_all(TPLYR_PATH, quiet = TRUE)

  adae <- scale_dataset(tplyr_adae, size, c("AEBODSYS", "AEDECOD"))

  x <- Sys.time()

  tplyr_table(adae, TRTA) |>
    set_pop_data(tplyr_adsl) |>
    set_pop_treat_var(TRT01A) |>
    # Layer 1: Nested count
    add_layer(
      group_count(vars(AEBODSYS, AEDECOD)) |>
        set_nest_count(TRUE) |>
        set_distinct_by(USUBJID)
    ) |>
    # Layer 2: Simple count
    add_layer(
      group_count(AESEV, by = "Severity") |>
        set_distinct_by(USUBJID)
    ) |>
    # Layer 3: Another count
    add_layer(
      group_count(AESER, by = "Serious") |>
        set_distinct_by(USUBJID)
    ) |>
    build(metadata = metadata)

  as.numeric(Sys.time() - x, units = "secs")
}


# ############################################################################
#
#  TPLYR2 BENCHMARKS
#
# ############################################################################

# ============================================================================
# tplyr2 Benchmark 1: Nested Count Layer (AEBODSYS/AEDECOD)
# ============================================================================
run_nested_count_benchmark_tplyr2 <- function(size, metadata = FALSE) {
  devtools::load_all(TPLYR2_PATH, quiet = TRUE)

  adae <- scale_dataset(tplyr_adae, size, c("AEBODSYS", "AEDECOD"))

  x <- Sys.time()

  spec <- tplyr_spec(
    cols = "TRTA",
    pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
    layers = tplyr_layers(
      group_count(c("AEBODSYS", "AEDECOD"),
        settings = layer_settings(
          distinct_by = "USUBJID",
          order_count_method = "bycount"
        )
      )
    )
  )

  tplyr_build(spec, adae, pop_data = tplyr_adsl, metadata = metadata)

  as.numeric(Sys.time() - x, units = "secs")
}

# ============================================================================
# tplyr2 Benchmark 2: Non-nested Count Layer
# ============================================================================
run_count_benchmark_tplyr2 <- function(size, metadata = FALSE) {
  devtools::load_all(TPLYR2_PATH, quiet = TRUE)

  adae <- scale_dataset(tplyr_adae, size, c("AEDECOD"))

  x <- Sys.time()

  spec <- tplyr_spec(
    cols = "TRTA",
    pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
    layers = tplyr_layers(
      group_count("AEDECOD",
        settings = layer_settings(
          distinct_by = "USUBJID"
        )
      )
    )
  )

  tplyr_build(spec, adae, pop_data = tplyr_adsl, metadata = metadata)

  as.numeric(Sys.time() - x, units = "secs")
}

# ============================================================================
# tplyr2 Benchmark 3: Desc Layer (Descriptive Statistics)
# ============================================================================
run_desc_benchmark_tplyr2 <- function(size, metadata = FALSE) {
  devtools::load_all(TPLYR2_PATH, quiet = TRUE)

  # Use ADAS data (loaded from Tplyr at top of script since tplyr2 doesn't ship it)
  adas <- scale_dataset(tplyr_adas, size, c("PARAMCD"))

  x <- Sys.time()

  spec <- tplyr_spec(
    cols = "TRTP",
    where = EFFFL == "Y" & ITTFL == "Y" & ANL01FL == "Y",
    layers = tplyr_layers(
      group_desc("CHG",
        by = c("PARAMCD", "AVISIT"),
        settings = layer_settings(
          format_strings = list(
            "n"         = f_str("xxx", "n"),
            "Mean (SD)" = f_str("xxx.x (xxx.xx)", "mean", "sd"),
            "Median"    = f_str("xxx.x", "median"),
            "Q1, Q3"    = f_str("xxx.x, xxx.x", "q1", "q3"),
            "Min, Max"  = f_str("xxx.x, xxx.x", "min", "max")
          )
        )
      )
    )
  )

  tplyr_build(spec, adas, metadata = metadata)

  as.numeric(Sys.time() - x, units = "secs")
}

# ============================================================================
# tplyr2 Benchmark 4: Shift Layer
# ============================================================================
run_shift_benchmark_tplyr2 <- function(size, metadata = FALSE) {
  devtools::load_all(TPLYR2_PATH, quiet = TRUE)

  adlb <- scale_dataset(tplyr_adlb, size, c("PARAMCD"))

  x <- Sys.time()

  spec <- tplyr_spec(
    cols = "TRTA",
    where = ABLFL != "Y",
    layers = tplyr_layers(
      group_shift(c(row = "BNRIND", column = "ANRIND"),
        by = "PARAMCD",
        settings = layer_settings(
          format_strings = list(
            shift_fmt = f_str("xxx (xx.x%)", "n", "pct")
          )
        )
      )
    )
  )

  tplyr_build(spec, adlb, metadata = metadata)

  as.numeric(Sys.time() - x, units = "secs")
}

# ============================================================================
# tplyr2 Benchmark 5: Multiple Layers (Combined Table)
# ============================================================================
run_multi_layer_benchmark_tplyr2 <- function(size, metadata = FALSE) {
  devtools::load_all(TPLYR2_PATH, quiet = TRUE)

  adae <- scale_dataset(tplyr_adae, size, c("AEBODSYS", "AEDECOD"))

  x <- Sys.time()

  spec <- tplyr_spec(
    cols = "TRTA",
    pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
    layers = tplyr_layers(
      # Layer 1: Nested count
      group_count(c("AEBODSYS", "AEDECOD"),
        settings = layer_settings(
          distinct_by = "USUBJID"
        )
      ),
      # Layer 2: Simple count
      group_count("AESEV",
        by = "Severity",
        settings = layer_settings(
          distinct_by = "USUBJID"
        )
      ),
      # Layer 3: Another count
      group_count("AESER",
        by = "Serious",
        settings = layer_settings(
          distinct_by = "USUBJID"
        )
      )
    )
  )

  tplyr_build(spec, adae, pop_data = tplyr_adsl, metadata = metadata)

  as.numeric(Sys.time() - x, units = "secs")
}


# ############################################################################
#
#  BENCHMARK RUNNER
#
# ############################################################################

# ============================================================================
# Run All Benchmarks (both packages)
# ============================================================================
run_all_benchmarks <- function() {

  cat("=" |> rep(70) |> paste(collapse = ""), "\n")
  cat("Tplyr vs tplyr2 Comprehensive Benchmark Suite\n")
  cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

  benchmark_configs <- list(
    list(name = "nested_count", fn_tplyr = run_nested_count_benchmark_tplyr,
         fn_tplyr2 = run_nested_count_benchmark_tplyr2),
    list(name = "count",        fn_tplyr = run_count_benchmark_tplyr,
         fn_tplyr2 = run_count_benchmark_tplyr2),
    list(name = "desc",         fn_tplyr = run_desc_benchmark_tplyr,
         fn_tplyr2 = run_desc_benchmark_tplyr2),
    list(name = "shift",        fn_tplyr = run_shift_benchmark_tplyr,
         fn_tplyr2 = run_shift_benchmark_tplyr2),
    list(name = "multi_layer",  fn_tplyr = run_multi_layer_benchmark_tplyr,
         fn_tplyr2 = run_multi_layer_benchmark_tplyr2)
  )

  results <- data.frame(
    package = character(),
    size = integer(),
    layer_type = character(),
    metadata = logical(),
    iteration = integer(),
    time_seconds = numeric(),
    stringsAsFactors = FALSE
  )

  for (config in benchmark_configs) {
    cat("\n", "-" |> rep(50) |> paste(collapse = ""), "\n")
    cat("Benchmark:", config$name, "\n")
    cat("-" |> rep(50) |> paste(collapse = ""), "\n")

    for (size in sizes) {
      for (meta in c(FALSE, TRUE)) {
        meta_label <- if (meta) "with metadata" else "without metadata"

        # --- Run Tplyr ---
        cat(sprintf("  [Tplyr]  Size: %3dx, %s... ", size, meta_label))
        times_tplyr <- numeric(n_iterations)
        for (iter in 1:n_iterations) {
          tryCatch({
            times_tplyr[iter] <- config$fn_tplyr(size, metadata = meta)
          }, error = function(e) {
            cat("ERROR: ", e$message, "\n")
            times_tplyr[iter] <<- NA
          })
        }
        avg_tplyr <- mean(times_tplyr, na.rm = TRUE)
        cat(sprintf("%.2fs (avg of %d runs)\n", avg_tplyr, n_iterations))

        # --- Run tplyr2 ---
        cat(sprintf("  [tplyr2] Size: %3dx, %s... ", size, meta_label))
        times_tplyr2 <- numeric(n_iterations)
        for (iter in 1:n_iterations) {
          tryCatch({
            times_tplyr2[iter] <- config$fn_tplyr2(size, metadata = meta)
          }, error = function(e) {
            cat("ERROR: ", e$message, "\n")
            times_tplyr2[iter] <<- NA
          })
        }
        avg_tplyr2 <- mean(times_tplyr2, na.rm = TRUE)
        speedup <- avg_tplyr / avg_tplyr2
        cat(sprintf("%.2fs (avg of %d runs) | speedup: %.2fx\n",
                    avg_tplyr2, n_iterations, speedup))

        # Store results
        for (iter in 1:n_iterations) {
          results <- rbind(results,
            data.frame(
              package = "Tplyr", size = size, layer_type = config$name,
              metadata = meta, iteration = iter,
              time_seconds = times_tplyr[iter], stringsAsFactors = FALSE
            ),
            data.frame(
              package = "tplyr2", size = size, layer_type = config$name,
              metadata = meta, iteration = iter,
              time_seconds = times_tplyr2[iter], stringsAsFactors = FALSE
            )
          )
        }
      }
    }
  }

  results
}


# ############################################################################
#
#  SUMMARY & COMPARISON
#
# ############################################################################

# ============================================================================
# Summary Functions
# ============================================================================
summarize_results <- function(results) {
  results |>
    group_by(package, layer_type, size, metadata) |>
    summarize(
      mean_time = mean(time_seconds, na.rm = TRUE),
      sd_time = sd(time_seconds, na.rm = TRUE),
      min_time = min(time_seconds, na.rm = TRUE),
      max_time = max(time_seconds, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(layer_type, metadata, size, package)
}

print_summary <- function(results) {
  summary_df <- summarize_results(results)

  cat("\n")
  cat("=" |> rep(70) |> paste(collapse = ""), "\n")
  cat("BENCHMARK SUMMARY\n")
  cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

  for (lt in unique(summary_df$layer_type)) {
    cat(sprintf("\n--- %s ---\n", toupper(lt)))
    layer_data <- summary_df |> filter(layer_type == lt)
    print(as.data.frame(layer_data), row.names = FALSE)
  }
}

print_comparison <- function(results) {
  summary_df <- summarize_results(results)

  # Pivot to wide format for side-by-side comparison
  tplyr_times <- summary_df |>
    filter(package == "Tplyr") |>
    select(layer_type, size, metadata, tplyr_mean = mean_time)

  tplyr2_times <- summary_df |>
    filter(package == "tplyr2") |>
    select(layer_type, size, metadata, tplyr2_mean = mean_time)

  comparison <- tplyr_times |>
    inner_join(tplyr2_times, by = c("layer_type", "size", "metadata")) |>
    mutate(
      speedup = tplyr_mean / tplyr2_mean,
      pct_faster = (1 - tplyr2_mean / tplyr_mean) * 100
    )

  cat("\n")
  cat("=" |> rep(78) |> paste(collapse = ""), "\n")
  cat("TPLYR vs TPLYR2 COMPARISON\n")
  cat("=" |> rep(78) |> paste(collapse = ""), "\n")

  for (lt in unique(comparison$layer_type)) {
    cat(sprintf("\n--- %s ---\n", toupper(lt)))
    layer_comp <- comparison |>
      filter(layer_type == lt) |>
      select(-layer_type) |>
      mutate(
        tplyr_mean  = sprintf("%.3f", tplyr_mean),
        tplyr2_mean = sprintf("%.3f", tplyr2_mean),
        speedup     = sprintf("%.2fx", speedup),
        pct_faster  = sprintf("%.1f%%", pct_faster)
      )
    print(as.data.frame(layer_comp), row.names = FALSE)
  }

  # Overall summary
  overall <- comparison |>
    group_by(layer_type) |>
    summarize(
      avg_speedup = mean(speedup, na.rm = TRUE),
      median_speedup = median(speedup, na.rm = TRUE),
      min_speedup = min(speedup, na.rm = TRUE),
      max_speedup = max(speedup, na.rm = TRUE),
      .groups = "drop"
    )

  cat("\n")
  cat("=" |> rep(78) |> paste(collapse = ""), "\n")
  cat("OVERALL SPEEDUP BY LAYER TYPE\n")
  cat("=" |> rep(78) |> paste(collapse = ""), "\n\n")

  overall_fmt <- overall |>
    mutate(
      avg_speedup    = sprintf("%.2fx", avg_speedup),
      median_speedup = sprintf("%.2fx", median_speedup),
      min_speedup    = sprintf("%.2fx", min_speedup),
      max_speedup    = sprintf("%.2fx", max_speedup)
    )
  print(as.data.frame(overall_fmt), row.names = FALSE)

  grand_avg <- mean(comparison$speedup, na.rm = TRUE)
  grand_median <- median(comparison$speedup, na.rm = TRUE)
  cat(sprintf("\nGrand average speedup:  %.2fx\n", grand_avg))
  cat(sprintf("Grand median speedup:   %.2fx\n", grand_median))
  cat(sprintf("(>1.0 = tplyr2 is faster, <1.0 = Tplyr is faster)\n"))

  invisible(comparison)
}


# ============================================================================
# Main Execution
# ============================================================================
if (interactive()) {
  cat("Run benchmarks with: results <- run_all_benchmarks()\n")
  cat("View summary with:   print_summary(results)\n")
  cat("View comparison with: print_comparison(results)\n")
  cat("Save results with:   saveRDS(results, 'benchmark_results.rds')\n")
} else {
  # When sourced non-interactively, run benchmarks
  results <- run_all_benchmarks()
  print_summary(results)
  print_comparison(results)

  # Save results with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0("benchmark_tplyr_vs_tplyr2_", timestamp, ".rds")
  saveRDS(results, filename)
  cat(sprintf("\nResults saved to: %s\n", filename))
}
