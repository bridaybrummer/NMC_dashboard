
context_table<- function(data1,  end_date = NULL , conditions_list = NULL, weeks_window = NULL){
  library(NMCleaner)
  # get average notifications per disease for last few years 
  #data<- df

  if(is.null(end_date)){
    end_date <- as_date(Sys.Date())
  }

  if(is.null(weeks_window)){
    weeks_window <- 20
  }
  
  new_master<- data1 #get data into funciton 
  
  conditions <- new_master$condition %>%unique()%>%sort
  
  df_epicurve_all <- 
    new_master %>%
    #mutate_dates(., date_index = "notification_date")%>%
    #mutate( epiweek = as.numeric(as.character(epiweek)))%>%
    #filter( year %in% 2024 &
    #as.numeric(epiweek) > as.numeric(epiweek) - 20 )%>%
    ungroup() %>%
    mutate(condition = factor(condition, levels = sort((conditions))))-> df_epicurve_all


  if (!is.null(conditions_list)) {
    df_epicurve_all <- df_epicurve_all %>% dplyr::filter(condition %in% conditions_list)
  } else {
    df_epicurve_all <- df_epicurve_all %>% dplyr::filter(nmccategories == 1)
  }
    #dplyr::filter( 
    #  nmccategories ==1
    #  ) %>%
    #dplyr::filter( condition =="COVID-19") %>%
    
    #dplyr::filter( notification_date < as_date("2024-01-01") )%>%
    df_epicurve_all%>%
    NMCleaner::epicurve_df(., 
                date_index = "notification_date", 
                grouping_vars = "condition")-> 
                df_epicurve_all
  #pivot)wider so that values in condition names become columns 
  
  weekly_average_notifications<- df_epicurve_all%>%
    select( condition, year, month, epiweek, date, n, day)%>%
    group_by(epiweek, year, condition) %>%
    summarise( n = sum(n))%>%
    ungroup()%>%
    pivot_wider(names_from = condition, values_from = n)%>%
    select(any_of(conditions)) %>%
    ungroup()%>%
    tbl_summary(., 
                type = everything() ~ "continuous",
                statistic = all_continuous() ~ "{mean}"
    )%>%
    modify_header(all_stat_cols() ~ "") %>%
    add_ci(method = everything() ~ "wilcox.test")%>%
    modify_footnote(update = everything() ~ NA) 
  
  weekly_average_notifications
  
  #end_date
  #find wich number the 
  
  #end_date_month_number<- as_date(end_date)%>%format(., "%m")%>% as.integer() 
  
  df_2024<- df_epicurve_all %>% 
  dplyr::filter(
    #year %in% 2024 
    )

  dates_in_last_x_weeks <- seq( as.Date(end_date) - (weeks_window*7), as.Date(end_date), by = "1 day")

 df_2024%>%
 filter(
    date %in% dates_in_last_x_weeks)%>%
    ungroup() %>%
    #dplyr::filter( nmccategories ==1) %>%
    #dplyr::filter( condition =="COVID-19") %>%
    #dplyr::filter( notification_date < as_date("2024-01-01") )%>%
    group_by(epiweek, condition, year) %>%
    summarise( n = sum(n, na.rm  =TRUE))%>%
    ungroup()%>%
    pivot_wider(names_from = condition, values_from = n)%>%
    #mutate( epiweek  = factor(epiweek, levels = unique(df_2024$epiweek)))%>%
    select(any_of(c(conditions, "epiweek", "year")) )%>%
    mutate(across( c(epiweek, year), ~  as.numeric(as.character(.))))%>%
    arrange( year, epiweek)%>%
    # i want the last 20 weeks in 2024 and the earliest week in 2025 
    #filter( epiweek > max(epiweek) -20 )%>%
    ungroup()-> 
  for_tbl_summary_odered_weeks

#    for_tbl_summary_odered_weeks%>%glimpse()

    #arrange( epiweek) %>%
    for_tbl_summary_odered_weeks%>%
    #select(-year)%>%
    mutate( epiweek = factor( epiweek, 
    levels =     for_tbl_summary_odered_weeks$epiweek))%>%
    #mutate( epiweek = factor( epiweek, levels = seq(min(epiweek), max(epiweek), 1))) %>%
    ungroup() ->  for_tbl_summary


for_tbl_summary%>%
  select( - year)%>%
    #mutate( epiweek = as.numeric(as.character(epiweek)))%>%
    tbl_summary(., 
                by = epiweek,
                type = everything() ~ "continuous",
                statistic = all_continuous() ~ "{mean}",
                digits = list(everything() ~ c(0, 0))) %>%#%>%add_ci(method = everything() ~ "wilcox.test")
    modify_header(all_stat_cols() ~ "**{level}**") %>%
    modify_footnote(update = everything() ~ NA) ->epiweeks2024
  
  epiweeks2024[["table_body"]]<-   epiweeks2024[["table_body"]]%>%select( variable, var_type, row_type, var_label, label, stat_1 , everything())


  
  epiweeks2024[["table_body"]] <- epiweeks2024[["table_body"]] %>%
    mutate(across(everything(), ~ ifelse(. == "NA", "", .)))
  
  
  
  context_table<- tbl_merge( list(weekly_average_notifications,epiweeks2024 ), 
                             tab_spanner = c("Average\nNotifications", "Epiweeks"))%>%
    as_flex_table()
  
for_tbl_summary%>%
  mutate( 
    across( c(epiweek,year),  ~ as.character(.)),
  )%>%
  filter( 
    epiweek == max(epiweek) & year == max(year)
  )%>%select(epiweek)%>%
  pull() %>%as.numeric()-> max_epiweek
  
  #max_epiweek<- for_tbl_summary$epiweek%>%as.character() %>%as.numeric() %>%max

  return(list(context_table, max_epiweek))
}


