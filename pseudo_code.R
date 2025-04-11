t <- tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_total_group() %>% 
  add_layer(name = 'Sex', 
    group_count(SEX, by = "Sex n (%)") %>% 
      set_missing_count(f_str('xx', n), Missing=NA, denom_ignore=TRUE)
  ) %>% 
  add_layer(name = 'Age',
    group_desc(AGE, by = "Age (Years)")
  ) %>% 
  add_layer(name = 'Age group', 
    group_count(AGEGR1, by = "Age Categories n (%)") %>% 
      set_missing_count(f_str('xx', n), Missing=NA, denom_ignore=TRUE)
  ) %>% 
  add_layer(name = 'Race', 
    group_count(RACE, by = "Race n (%)") %>% 
      set_missing_count(f_str('xx', n), Missing=NA, denom_ignore=TRUE) %>% 
      set_order_count_method("byfactor")
  ) %>% 
  add_layer(name = 'Ethnic', 
    group_count(ETHNIC, by = "Ethnicity n (%)") %>% 
      set_missing_count(f_str('xx', n), Missing=NA, denom_ignore=TRUE)
  )

ts <- tplyr_spec(
  cols = c("TRT01P", "SEX"),
  layers = tplyr_layers(
    # layer settings could work as named arguments that are passed as functions of the same names
    # this would allow arguments to either be dropped in settings, or piping could be used. 
    # It also allows documentation to be written for the layer settings themselves
    group_count("SEX", by = "Sex n (%)", settings = layer_settings(
        format_strings = f_str('xx', n),
        missing_counts = list(
          f_str('xx', n),
          Missing=NA, 
          denom_ignore=TRUE
        )
      ),
    group_desc("AGE", by = "Age (Years)")
  )
)



tplyr_build(ts, data = adsl, cols=("TRT01A", "SEX"))

tplyr_build(ts, data=adae, pop_data = adae)

# For the build specification, use the `arg()` function to allow users to override arguments in the 
# build function as named arguments. It could work as a named argument that's the "argument" name, and 
# the value you assign to that name is the default.
tplyr_spec(
  cols = arg(cols = c("TRTA", "SEX")),
  where = expr(ANL01FL == "Y"),
  pop_data = pop_data(
    cols = arg(col_map = c("TRTA" = "TRT01P")),
    where = expr(TRUE)
  ),
  layers = tplyr_layers(
    group_count(arg(vars = c("AEBODSYS", "AEDECOD")), by = "Sex n (%)", settings = layer_settings(
        format_strings = f_str('xx', n)
        )
      )
  )
)

