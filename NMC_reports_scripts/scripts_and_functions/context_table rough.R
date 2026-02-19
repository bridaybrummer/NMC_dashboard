# Report context script 


# make simukation data with rnorm and plot 
# make a good epitable with number of notifications form previous year etc. 

#if( !require("devtools")) install.packages("devtools")

#devtools::install_github("bridaybrummer/NMCLeaner", force = TRUE)

library(NMCleaner)
load("~/Desktop/SAFETP/CLA/NMC_database/master/new_master.rda")

# see if gtsummary can work for giving the mean number of notifications per year

#new_master %>%mutate_dates(., date_index = "notification_date")


new_master$condition %>%tabyl()

?epicurve_df

new_master$notification_date%>%as_date

epicurve_df(new_master, "notification_date", grouping_var = "condition")


theme_gtsummary_compact()

context_table<- function(data, start_date){
  #library(NMCleaner)
# get average notifications per disease for last few years 
  
  new_master<- data
  conditions <- new_master$condition %>%unique()
  
df_epicurve_all<- 
  new_master%>%
    ungroup() %>%
    dplyr::filter( nmccategories ==1) %>%
    #dplyr::filter( condition =="COVID-19") %>%
    
  #dplyr::filter( notification_date < as_date("2024-01-01") )%>%
  epicurve_df(., 
              date_index = "notification_date", 
              grouping_vars = "condition")
  #pivot)wider so that values in condition names become columns 

weekly_average_notifications<- 
  df_epicurve_all%>%
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
  modify_header(all_stat_cols() ~ "Average weekly notifications") %>%
  add_ci(method = everything() ~ "wilcox.test")%>%
  modify_footnote(update = everything() ~ NA) 

weekly_average_notifications


#find wich number the 

start_date_month_number<- as_date(start_date)%>%format(., "%m")%>% as.integer() 

df_2024<- df_epicurve_all %>% dplyr::filter(year == 2024 & 
                                              month %in% month.abb[1:start_date_month_number])



epiweeks2024<- df_2024%>%
  ungroup() %>%
  #dplyr::filter( nmccategories ==1) %>%
  #dplyr::filter( condition =="COVID-19") %>%
  
  #dplyr::filter( notification_date < as_date("2024-01-01") )%>%
  #group_by(epiweek, condition) %>%
  #summarise( n = sum(n))%>%
  #ungroup()%>%
  pivot_wider(names_from = condition, values_from = n)%>%
  select(any_of(c(conditions, "epiweek")) )%>%
  ungroup()%>%
  tbl_summary(., 
              by = epiweek, 
              type = everything() ~ "continuous",
              statistic = all_continuous() ~ "{mean}",
              digits = list(everything() ~ c(0, 0))) %>%#%>%add_ci(method = everything() ~ "wilcox.test")
modify_header(all_stat_cols() ~ "**{level}**") %>%
  modify_footnote(update = everything() ~ NA)


epiweeks2024[["table_body"]] <- epiweeks2024[["table_body"]] %>%
  mutate(across(everything(), ~ ifelse(. == "NA", "", .)))

epiweeks2024

context_table<- tbl_merge( list(weekly_average_notifications,epiweeks2024 ), 
           tab_spanner = c("**Average Notifications**", "**Number of notifications in 2024**"))%>%
  as_flex_table()

context_table

return(context_table)
}

context_table(new_master, "2024-04-01")

save(context_table, file = "outputs/context_table.rda")

# any footnotes to add?
## somethign about the average notificaitons. 


## Category notifications 

df_notifications<- new_master%>%
  dplyr::filter(!condition %in% "Covid-19")%>%
  epicurve_df(., 
              date_index = "notification_date", 
              grouping_vars = "nmccategories")

plot_categories<- df_notifications%>%plot_epicurve_df(., 
                                   x_axis_option = "epiweek", 
                                   grouping_vars = "nmccategories", 
                                   color_select = c("green", "blue", "red")
)

plot_categories$data



df_epicurve_all<- 
  new_master%>%
  ungroup() %>%
  dplyr::filter( nmccategories ==1) %>%
  #dplyr::filter( condition =="COVID-19") %>%
  
  #dplyr::filter( notification_date < as_date("2024-01-01") )%>%
  epicurve_df(., 
              date_index = "notification_date", 
              grouping_vars = "condition")

plot<- df_epicurve_all%>%
  dplyr::filter( condition %in% c("Cholera", 
                           "Covid-19", 
                           "Diphtheria", 
                           "Malaria", 
                           "Measles", 
                           "Mpox", 
                           "Pertussis", 
                           "Rubella"))%>%
  plot_epicurve_df(., 
                                   x_axis_option = "epiweek", 
                                   grouping_vars = "condition",
                                    color_select = "olivedrab"
                   )

plot$plot



####


#df<- epicurve_df(conditions_df,
#                 date_index = "notification_date", grouping_vars = "condition")


df_epicurve_important<- 
  df_epicurve_all%>%
  dplyr::filter( condition %in% c("Cholera", 
                                  "Covid-19", 
                                  "Diphtheria", 
                                  "Malaria", 
                                  "Measles", 
                                  "Mpox", 
                                  "Pertussis", 
                                  "Rubella"))
  
df_2024<- df_epicurve_important %>%dplyr::filter( year %in% 2024)
df_previous<- df_epicurve_important %>%dplyr::filter( !year %in% 2024)
df_previous

#source("create_date_template.R")

# put this in a COVID/NMC related project and maybe make into a function. 
standard_group_vars<- c("year", "month", "epiweek", "date")


df_2024_template<- create_date_template("2024-01-01", "2024-12-31",
                                        reps = length(df_2024[["condition"]]%>%unique),
                                        rep_on_var = (df_2024[["condition"]]%>%unique),
                                        rep_var_name = "condition"
)


full_2024<- left_join(df_2024_template, df_2024, by = c("condition", "day", standard_group_vars) )

full_2024

df_2023<- df_epicurve_important%>%
  dplyr::filter( year == 2023)#%>%
#select( - year, -date, -epiweek )


# for df_2024, keep the standard grouping vars and n,

replacement_vars<- c("roll_avg", "ci_upper", "ci_lower")
matching_vars<- c("month", "day", "condition" )

full_2024 <- full_2024%>%select(-all_of(replacement_vars), all_of(matching_vars))
full_2024
full_2024%>%
  group_by(condition, month) %>%
  summarise( n = n())

df_previous <- df_previous%>%select(all_of(replacement_vars), all_of(matching_vars))
df_previous
df_previous<- df_previous%>%
  group_by(condition, month, day) %>%
  summarise( across( all_of(replacement_vars), mean))
df_previous
full_2024

pre_rolling<- left_join(full_2024, df_previous, by = c("month", "day", "condition"),  )
pre_rolling
pre_rolling%>%
  group_by(condition, month) %>%
  summarise( n = n())



outputss<- plot_epicurve_df(pre_rolling%>%dplyr::filter( year %in% 2024), x_axis_option = "epiweek",
                            grouping_vars = "condition",
                            add_bar = TRUE,
)

outputss$plot
