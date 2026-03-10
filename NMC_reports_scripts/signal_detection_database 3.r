
# load NMCleaner package 
library(NMCleaner)
conflicted::conflicts_prefer( dplyr::filter)
conflicted::conflicts_prefer( dplyr::lag)
conflicted::conflicts_prefer( tidyr::extract)


load("~/Desktop/SAFETP/CLA/NMC_database/master/new_master.rda")

# Implementing time series parts
source( "scripts_and_functions/time_series_functions.r")
source( "scripts_and_functions/make_fever_rash.r")


new_master %>%
    #filter( nmccategories ==1)%>%
    #filter( condition %in% 
    #c(
     #"Cholera",
    #"Measles"
    #),
    #Year_notification == 2024
    #)%>%
    mutate_dates( date_index = "notification_date") ->
df
df


# convert it to a to_plot format 
NMCleaner::epicurve_df(
    df, 
    date_index = "notification_date", 
    grouping_vars = "condition",
    lag_size = 7 ,
    add_rolling_avg = c(TRUE, 14)
    )->
df_to_plot

# Need to make a df_to_plot for each province 



load(file = "dashboard/aggregated_data/df_with_ears.rda")

df_with_ears %>% 
    group_by( condition) %>%
    summarise( 
        latest_date = max( as_date(date) )
    )
# looks like the last update was from 9 November. # perhaps consider making a dahsboard df_with_ears

df_with_ears %>%glimpse()
# remember the structure is for every condition. 

# Check for the dates that exist in new_master that havent been acoutned for in df_with_ears. and complete the function for those missing dates and then combine them for an updated df_with_ears
df_with_ears$date %>%as_date() %>%min()

df_with_ears%>%
    #filter( condition %in% "Rubella")%>%
    select( date, n )%>%
    mutate( date = as_date(date))%>%
    filter( !n == 0) %>%
    filter(date == max(date))%>%pull(date) %>%unique->  max_df_with_ears_date
max_df_with_ears_date
new_master$date %>%as_date() %>% max(na.rm = TRUE) -> max_new_master_date
max_new_master_date
dates_with_no_ears <- seq( max_df_with_ears_date, max_new_master_date, by = "day")

dates_with_no_ears

dates_with_no_ears[1] -days(90)-> dates_with_no_ears
dates_with_no_ears

        
####################################

# To update the df_with_ears with the new data. 

####################################

new_master$condition[grepl( "Agricultural", new_master$condition, ignore.case = TRUE)]%>%unique()
df_to_plot$condition[grepl( "Agricultural", df_to_plot$condition, ignore.case = TRUE)]%>%unique()




if (FALSE){

c( NMCleaner::condition_df$condition[1:27], "Fever-Rash", 
"Agricultural or stock remedy poisoning")%>%
    map( ~
        {
        df_to_plot %>%
            arrange(condition, date) %>%
            {if (.x == "Fever-Rash") make_fever_rash(.)%>%
                group_by( year, 
                month, epiweek, date, day, condition )%>%
                summarise( 
                across( n:roll_avg, sum)
        )
            else .} %>%
            filter(condition == .x) %>%
            ears() %>%
            cusum_window_start(window = 180, date_index = "date", start_date = as.Date(min(dates_with_no_ears))) %>%
            ungroup()
        }
    )%>% 
    bind_rows() ->
df_with_ears_update

    save( df_with_ears_update, file = "dashboard/aggregated_data/df_with_ears_update.rda")

dates_with_no_ears[1]

df_with_ears %>% filter( as_date(as.character(date) )< dates_with_no_ears[1])-> df_with_ears_old

df_with_ears_update %>%filter( as_date(as.character(date) ) > dates_with_no_ears[1]) -> df_with_ears_new
df_with_ears_new%>%glimpse()

bind_rows( df_with_ears_old, df_with_ears_new) -> df_with_ears_updated

df_with_ears_big <- df_with_ears_updated
df_with_ears_big$date %>%as.character%>%as_date() %>%max()

save( df_with_ears_big, file = "dashboard/aggregated_data/df_with_ears_big.rda")

df_with_ears <- df_with_ears_big
save( df_with_ears, file = "dashboard/aggregated_data/df_with_ears.rda")

}else {
    #load("~/Desktop/SAFETP/CLA/NMC_database/master/new_master.rda")
    load(file = "dashboard/aggregated_data/df_with_ears.rda")
}



#################################################################

# If you wish to completely redo the df_with ears NB NB  approx 15 mins per condition!

#################################################################

condition_df$condition[which(grepl("Agri", condition_df$condition, ignore.case = TRUE))]

if (FALSE){

c( NMCleaner::condition_df$condition[1:27], "Fever-Rash", "Agricultural or stock remedy poisoning")%>%
    map( ~
        {
        df_to_plot %>%
            arrange(condition, date) %>%
            {if (.x == "Fever-Rash") make_fever_rash(.)%>%
                group_by( year, 
                month, epiweek, date, day, condition )%>%
                summarise( 
                across( n:roll_avg, sum)
        )
            else .} %>%
            filter(condition == .x) %>%
            ears() %>%
            cusum_window(window = 180, date_index = "date") %>%
            ungroup()
        }
    )%>% 
    bind_rows() ->
    df_with_ears

    save( df_with_ears, file = "dashboard/aggregated_data/df_with_ears.rda")

} else  {

    load(file = "dashboard/aggregated_data/df_with_ears.rda")

}


# Create signal detection for conditions wihtin a province


df$prov_%>%unique-> provinces

map(provinces, ~ {
  cnt <- df %>% filter(prov_ %in% .x) %>% nrow()
  cat("Province:", .x, "has", cnt, "rows.\n")
  cnt  # return the count (for further inspection if needed)
})

provinces %>%
  map(~ {
    # .x represents the current province in the iteration.
    # Filter df for the current province using dplyr.
    # If df is already a data.table, you could also use:
    #    df[prov_ %in% .x]
    filtered_df <- df %>% 
      filter(prov_ %in% .x)
    
    # Apply the epicurve_df function on the filtered data.
    # This function is assumed to perform various transformations like
    # generating the epicurve and calculating lags/rolling averages.
    epicurve_result <- filtered_df %>% 
      epicurve_df(
        date_index      = "notification_date", 
        grouping_vars   = "condition",
        lag_size        = 7,
        add_rolling_avg = c(TRUE, 14)
      )%>%mutate( prov_ = .x)
    
    # Convert the result to a data.table to ensure consistency for later steps.
    as.data.table(epicurve_result)
  }) %>%
  # Combine all the data.tables from the map() iteration into one data.table.
  rbindlist()-> df_to_plot_provinces

  df_to_plot_provinces

  save( df_to_plot_provinces, file = "dashboard/aggregated_data/df_to_plot_provinces.rda")



  results_dt <- cusum_window(df_to_plot_provinces[prov_ %in% "WC"], window = 180, date_index = "date")
