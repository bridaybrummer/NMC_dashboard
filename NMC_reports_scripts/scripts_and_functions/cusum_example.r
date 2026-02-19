
# example of running cusum window
new_master <- arrow::read_feather("~/Desktop/SAFETP/CLA/NMC_database/master/new_master.feather")
library(NMCleaner)


new_master %>%setDT() 

new_master %>%
  group_by( 
    condition 
  )%>%
  reframe( 
    n = n() 
  )%>%
  arrange(-n) %>%
  filter( n >100)%>%
  pull( condition )-> 
  condition_atleast_100


condition_atleast_100%>%
map( 
  ~ 
    new_master%>%
      filter( condition == .x)%>%
      epicurve_df(
        date_index = "notification_date", 
        grouping_vars = c("prov_") 
      )%>%
      mutate( condition  = .x)
) %>%
bind_rows()-> 
aggregation_condition_prov


aggregation_condition_prov

#source( "scripts_and_functions/time_series_functions.R")
# Nested map: iterate over provinces, then conditions within each province
aggregation_condition_prov$prov_ %>% unique() %>%
  map(
    function(prov_val) {
      aggregation_condition_prov_filtered <- 
        aggregation_condition_prov %>% filter(prov_ == prov_val)
      condition_atleast_100 %>%
        map(
          function(cond_val) {
            aggregation_condition_prov_filtered %>%
              filter(condition == cond_val) %>%
              arrange(year, month, epiweek, date, day) %>%
              group_by(year, month, epiweek, date, day, prov_, condition) %>%
              summarise(n = sum(n), .groups = "drop") %>%
              cusum_window_start(
                window = 180,
                date_index = "date",
                start_date = as.Date("2018-01-01")
              ) %>%
              mutate(
                prov_ = prov_val,
                condition = cond_val
              )
          }
        ) %>%
        bind_rows()
    }
  ) %>%
  bind_rows() -> aggregation_condition_prov_with_ears

# you now neeed to plot each condition by province or vice versa. 

poison_age_group$agecategory_unit %>%
  unique() %>%
  map(
    ~ {
      poison_age_group %>%
        filter(agecategory_unit == .x) %>%
        mutate(
          year = factor(year, levels = 2018:2024)
        ) %>%
        arrange(year, month, epiweek, date, day) %>%
        group_by(
          year, month, epiweek, date, day, agecategory_unit
        ) %>%
        summarise(
          n = sum(n)
        ) %>%
        cusum_window_start(
          window = 180,
          date_index = "date",
          start_date = as.Date("2018-01-01")
        ) %>%
        mutate(
          agecategory_unit = .x,
        )
    }
  ) %>%
  bind_rows() -> poison_agegroup_data_with_ears


  poison_agegroup_data_with_ears
