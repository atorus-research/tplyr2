# Package index

## Build

High-level functions to create and build a table

- [`tplyr_spec()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_spec.md)
  : Create a tplyr2 table specification
- [`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)
  : Build a tplyr2 table from a spec and data
- [`is_tplyr_spec()`](https://github.com/mstackhouse/tplyr2/reference/is_tplyr_spec.md)
  : Check if an object is a tplyr_spec

## Layers

Creating layers and adding them to a spec

- [`tplyr_layers()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_layers.md)
  : Create a list of layers
- [`is_tplyr_layer()`](https://github.com/mstackhouse/tplyr2/reference/is_tplyr_layer.md)
  : Check if an object is a tplyr_layer
- [`group_count()`](https://github.com/mstackhouse/tplyr2/reference/group_count.md)
  : Create a count layer
- [`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md)
  : Create a descriptive statistics layer
- [`group_shift()`](https://github.com/mstackhouse/tplyr2/reference/group_shift.md)
  : Create a shift layer
- [`group_analyze()`](https://github.com/mstackhouse/tplyr2/reference/group_analyze.md)
  : Create a custom analysis layer
- [`label()`](https://github.com/mstackhouse/tplyr2/reference/label.md)
  : Create a text label for use in by parameters
- [`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)
  : Create layer settings

## Formatting

Controlling the display of numeric results

- [`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
  : Create a format string object
- [`apply_formats()`](https://github.com/mstackhouse/tplyr2/reference/apply_formats.md)
  : Apply format strings to numeric values

## Population Data and Groups

Population data, total groups, and custom groups

- [`pop_data()`](https://github.com/mstackhouse/tplyr2/reference/pop_data.md)
  : Create a population data configuration
- [`is_pop_data()`](https://github.com/mstackhouse/tplyr2/reference/is_pop_data.md)
  : Check if an object is a tplyr_pop_data
- [`total_group()`](https://github.com/mstackhouse/tplyr2/reference/total_group.md)
  : Create a total group configuration
- [`custom_group()`](https://github.com/mstackhouse/tplyr2/reference/custom_group.md)
  : Create a custom column group configuration
- [`tplyr_header_n()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_header_n.md)
  : Extract header N from a tplyr2 build result

## Metadata

Cell-level traceability and source data extraction

- [`tplyr_meta()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_meta.md)
  : Metadata object for a tplyr output cell
- [`tplyr_meta_result()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_meta_result.md)
  : Get metadata for a specific output cell
- [`tplyr_meta_subset()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_meta_subset.md)
  : Get source data rows for a specific output cell
- [`generate_row_ids()`](https://github.com/mstackhouse/tplyr2/reference/generate_row_ids.md)
  : Generate unique row IDs for output rows

## Numeric Data

Accessing raw unformatted results

- [`tplyr_numeric_data()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_numeric_data.md)
  : Retrieve raw numeric data from a tplyr_build result
- [`tplyr_stats_data()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_stats_data.md)
  : Retrieve raw statistic values from a tplyr_build result

## Post-Processing

Post-processing functions for display formatting

- [`apply_conditional_format()`](https://github.com/mstackhouse/tplyr2/reference/apply_conditional_format.md)
  : Conditional reformatting of a pre-populated string of numbers
- [`apply_row_masks()`](https://github.com/mstackhouse/tplyr2/reference/apply_row_masks.md)
  : Apply row masks to blank repeated row labels
- [`collapse_row_labels()`](https://github.com/mstackhouse/tplyr2/reference/collapse_row_labels.md)
  : Collapse row labels into a single column
- [`replace_leading_whitespace()`](https://github.com/mstackhouse/tplyr2/reference/replace_leading_whitespace.md)
  : Replace leading whitespace with a specified string
- [`str_extract_num()`](https://github.com/mstackhouse/tplyr2/reference/str_extract_num.md)
  : Extract numeric values from formatted strings
- [`str_indent_wrap()`](https://github.com/mstackhouse/tplyr2/reference/str_indent_wrap.md)
  : Wrap strings to a specific width with hyphenation while preserving
  indentation

## Serialization

Saving and loading spec files

- [`tplyr_write_spec()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_write_spec.md)
  : Write a tplyr_spec to JSON or YAML
- [`tplyr_read_spec()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_read_spec.md)
  : Read a tplyr_spec from JSON or YAML

## Analysis Results Data

ARD format conversion

- [`tplyr_to_ard()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_to_ard.md)
  : Convert tplyr_build output to Analysis Results Data (ARD) format
- [`tplyr_from_ard()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_from_ard.md)
  : Reconstruct a formatted table from ARD and a spec

## Datasets

Example clinical trial datasets

- [`tplyr_adae`](https://github.com/mstackhouse/tplyr2/reference/tplyr_adae.md)
  : Adverse events analysis dataset
- [`tplyr_adlb`](https://github.com/mstackhouse/tplyr2/reference/tplyr_adlb.md)
  : Laboratory data analysis dataset
- [`tplyr_adsl`](https://github.com/mstackhouse/tplyr2/reference/tplyr_adsl.md)
  : Subject-level analysis dataset

## Options and Helpers

Package options and helper functions

- [`tplyr2_options()`](https://github.com/mstackhouse/tplyr2/reference/tplyr2_options.md)
  : Get or set tplyr2 package options
- [`get_data_labels()`](https://github.com/mstackhouse/tplyr2/reference/get_data_labels.md)
  : Extract variable labels from a data.frame
