# Tables and graphs for NMC dashboard rough

pacman::p_load(ggplot2, plotly, gtsummary, tidyverse, magrittr)

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



df_with_ears%>%filter( 
    #condition %in% "Agricultural or stock remedy poisoning", 
    date > dates_with_no_ears[1]
)%>%view()

df_with_ears%>%filter( 
    condition %in% "Agricultural or stock remedy poisoning", 
    as_date(as.character(date ))> dates_with_no_ears[1]
)%>%view()

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


df_with_ears%>%glimpse()

# Check i this works with conditions with very low notifications 
NMCleaner::condition_df$condition[20]
    
# may need to replace NA values with 0 

# To save on computing time, we want to just run the cusum on threshdl values that are missing. 

# So, we would need to add in the most recent df_to_plot (from the NCMleaner::epicurve_df() ) to the df_with_ears, 
    # and then run the cusum on the missing values.

# So, update the df_with_ears with the most recent df_to_plot


condition_select<- "Measles"
df_with_ears
df_with_ears %>%
    #filter(year %in% 2023:2024) %>%
    ungroup() %>%
    ggplot(., aes(x = !!sym("date"))) +
    # greyed barsof notificatiosn per day
    geom_bar(aes(y = n), stat = "identity", alpha = 0.5) +
    theme_classic()->
simple_epicurve
    
simple_epicurve
simple_epicurve%>%plotly::ggplotly()

# Epicurve for CUSUM from RI datasicen project 

df_with_ears%>%
  filter( condition %in%condition_select )%>%
    ggplot(., aes(x = !!sym("date"))) +
    geom_bar(aes(y = n), stat = "identity", alpha = 0.5) +
    geom_line(group = 1, aes( y = latest_cusum_threshold, color = "CUSUM (Summed variance from the previos 100 days)") )+
    geom_point(aes( y = -12.5, color = factor(latest_is_cusums_outbreak)), shape = 24, size = 2, fill = "transparent") +  # Triangle shape for alarms
  scale_color_manual(
    name = "Legend",  # Legend title
    values = c(
        "CUSUM (Summed variance from the previos 100 days)" = "steelblue", 
        "TRUE" = "red", 
        "FALSE" = "transparent", 
        "NA" = "transparent"),
    labels = c(
        "CUSUM (Summed variance from the previos 100 days)", 
        "TRUE" = "Potential Outbreak Detected", 
        "FALSE" = "",
        "NA" = ""
               )
  ) +
    labs(title = "CUSUM Signal Detection for Measles Notifications", x = "Iteration", y = "Infection Counts") +
    theme_minimal() +
    theme(legend.position = "bottom")


df_with_ears%>%glimpse()
df_with_ears$ci_lower


df_with_ears$is_cusums_outbreak%>%tabyl()

condition_select <- "Fever-Rash"

df_with_ears %>%
    filter( condition %in%condition_select )%>%
    #filter(year %in% 2023:2024) %>%
    ungroup() %>%
    ggplot(., aes(x = !!sym("date"))) +
    # greyed barsof notificatiosn per day
    geom_bar(aes(y = n), stat = "identity", alpha = 0.5) +
#    geom_line(aes(y = ci_lower, color = "NMCleaner 7 day rolling average"), show.legend = TRUE) 
# show the breach of Ears Threshold...see if you can reduce signal detection in low transmission time? 
    
    # EARS signal detection 
    geom_point( 
        data = df_with_ears%>%
            filter( 
                is_ears_outbreak == TRUE, 
                condition %in% condition_select
            ) ,
            aes( y = ears_threshold, x = date ,color = "EARS threshold"), 
                color = "black", show.legend = TRUE)+
        # EARS threshold lin 

# CUSUM
    geom_point(
            data =  df_with_ears%>%
                filter( 
                    is_cusums_outbreak == TRUE,
                    condition %in% condition_select
                    #as_date(date) %in% dates_seq
            ) ,
            aes( y = cusums_threshold, x = date, color = "CUSUM threshold"), show.legend = TRUE)+


    theme_classic() +
    #labs(color = "Legend") +
    #scale_color_manual(values = c("darkblue", "darkgreen", "darkred"),
    #labels = c("NMCleaner package", "EARS", "CUSUM")) +
    theme(legend.position = "top") ->
p


p

p%>%ggplotly()


###########

# Make category wise epicurves of conditions. 

###########


new_master %>% as.data.table()-> new_master_dt

categories <- c(1, 2 , 3)

for( category in categories){

plot_name <- paste0("plot_epicurve_df_cat", category)

df_to_plot <- NMCleaner::epicurve_df(
        new_master_dt[ nmccategories %in% category & year %in% c( 2023:2025), ] , 
        date_index = "notification_date", 
        grouping_vars = "condition",
        lag_size = 7 ,
        add_rolling_avg = c(TRUE, 14)
        )->
        df_to_plot 

  
df_to_plot %>%
    filter( year %in%2023:2025)%>%
    plot_epicurve_df(
        ., 
        x_axis_option = "epiweek",
        grouping_vars = "condition",
    )->
    plot
  
  # Use assign to store the plot with the dynamically generated name
  assign(plot_name, plot)
  save(list = plot_name, file = paste0("dashboard/", plot_name, ".RData"))
}



###########

# Make category wise epicurves of conditions. 

###########

new_master$case_type%>% tabyl()

new_master%>%
mutate_dates( date_index = "notification_date")%>%
        filter( year %in% 2023:2025,
        nmccategories %in% 1)%>%
    NMCleaner::epicurve_df(
        ., 
        date_index = "notification_date",
        grouping_vars = "case_type"
        )->
case_type_df

case_type_df %>%
    NMCleaner::plot_epicurve_df(
        x_axis_option = "epiweeks",
        grouping_vars = "case_type"
    )-> 
case_type_plot

case_type_plot$data$case_type %>%unique()-> case_types

case_type_plot$plot 

case_type_plot$plot +
     geom_col( group = 1, 
        aes( 
            y = n,
            fill = case_type,
            
            ),color = "transparent"
    )+
 #   scale_color_manual(
 #       values  = c("green", "black", "blue", "red" )
 #   )
    scale_fill_manual(
        values  = c(
            "transparent",
             "mediumpurple", # clinical
              "darkgreen",  # lab
              "steelblue" ) # merged
    )-> 
case_type_plot_coloured

save( case_type_plot_coloured, file = "dashboard/case_type_plot_coloured.RData")

    # I think the idea will be to have run all the case_types for conditions and have a filter to investigate. 


##########

# Case ource and sector

##########

1:2%>%
map(
    ~ new_master %>%
    filter( year %in% 2024, 
    nmccategories  %in% .x )%>%
    mutate( 
        facility_sector= if_else( 
            grepl( "private", facility_sector, ignore.case = TRUE), "Private", "Public"
        )
    )%>%
    select( case_source, facility_sector)%>%
            mutate( Total = TRUE) %>%
    tbl_summary(
        by = facility_sector
    )%>%
    modify_header(
         label = "",
    all_stat_cols() ~ "**{level}**")%>%
    bold_labels%>% 
    clean_labels() 
) %>%
tbl_stack(
    tbls = ., 
    group_header = c( paste0( "Category ", 1:2,""))
)->
case_source_by_sector_tbl 

save( case_source_by_sector_tbl, file = "dashboard/case_source_by_sector_tbl.RData")

# Not sure whether to use row or column percentages. 
# My main question is to know what the proportion of each case source is in the differnt sectors. 
# A differnt table can tell us the poprotion of categories that come from each sector 

new_master %>%
    filter( year %in% 2024, 
    #nmccategories  %in% .x 
    )%>%
    mutate( 
        facility_sector= if_else( 
            grepl( "private", facility_sector, ignore.case = TRUE), "Private", "Public"
        )
    )%>%
    select( nmccategories, facility_sector)%>%
            mutate( Total = TRUE) %>%
    tbl_summary(
        by = facility_sector,
        percent = "row",
        label = list(nmccategories ~ "NMC category")
    )%>%
    bold_labels() -> 
sector_by_nmc_category_tbl

save( sector_by_nmc_category_tbl, file = "dashboard/sector_by_nmc_category_tbl.RData")


########################################

# Graph for looking at clinical vs lab confirmed cases 

########################################

# Some points 
    # Need to only pick catgeory 1 notifiacitsn that require lab confirmation
    # Need to handle merged cases - these can potetnially be exlucded, or there to show that the facilit is managing it well. 
    # since there is no lab confimation in private yet, we can leave them out too, 
    # However, we could do something sector specific, for instance, if a public facility has rubella, the pirvate sector should also be notifying?
    # You could show the percentage of laboratory notifications out of clinical notifications, if it is >100% then you know there is a flag. 


condition_df %>%as.data.table() -> condition_dt

condition_dt[nmccategories ==1, condition][c(5, 6, 7, 8, 9, 12, 16, 17, 18, 19, 25 )]-> condition_lab_confirmed

new_master %>% 
    group_by( 
        condition, case_type
    )%>%
    summarise( n = n() )%>%
    filter( 
        case_type == "Laboratory notifications"
    )
# So we want to identify places or facilities that have lab notification but low clinical notifications. 

new_master %>% 
    filter( year %in% 2024, 
    condition  %in% condition_lab_confirmed) %>%
    group_by( 
        case_type , 
        epiweek , 
        facility
    )%>%
    summarise( n = n() )%>%
    mutate( epiweek = factor(epiweek , levels = 1:52) 
    )-> data_to_plot

    #NMCleaner::mutate_dates( 
    #    date_index = "date"
    #)%>%
data_to_plot%>%
    ggplot()  + 
    geom_col( 
        aes( 
            x = epiweek, 
            y = n,
            color = case_type, 
            fill = case_type,
            #group = case_type, 
        ),
        position = "stack"
    )-> country_view 
    country_view

save( country_view, file = "dashboard/country_view.RData")


# Do one for Diphtheria example 

new_master %>% 
    filter( year %in% 2024:2025, 
    condition  %in% "Diphtheria") %>%
    epicurve_df( 
        date_index = "notification_date",
        grouping_vars = "case_type",
    )-> data_to_plot
    #NMCleaner::mutate_dates( 
    #    date_index = "date"
    #)%>%
data_to_plot%>%
    group_by(  year, epiweek, case_type)%>%
    summarise( n = sum(n) )%>%
    ggplot()  + 
    geom_line( 
        aes( 
            x = epiweek, 
            y = n,
            color = case_type, 
            fill = case_type,
            group = case_type, 
        ),
        position = "stack"
    )+
    facet_nested(cols = vars(!!sym("year")), 
                #!!sym(x_2nd_level)), 
                #rows = vars(!!sym(grouping_vars)), 
                scales = "free_y", 
                switch = "x")+
    theme(
      panel.spacing=unit(0.17,"lines"),
      strip.background=element_rect(color=NA, fill=NA),
      #panel.border=element_rect(color="grey90"),
      #axis.ticks.x=element_blank(),
      strip.placement = "outside",
      text = element_text(family = "Century Gothic", size = 10, color = "#333333"),
      panel.background = element_rect(fill = "#F8F8F8"),
      plot.background = element_rect(fill = "#F8F8F8"),
      panel.grid = element_line(color = "#DDDDDD"),
      legend.text = element_text(color = "#333333", size = 8),
      legend.title = element_text(color = "#333333", size = 10),
      plot.title = element_text(size = 10, color = "#666666"),
      plot.subtitle = element_text(size = 8, color = "#666666"),
      plot.caption = element_text(size = 8, color = "#666666"),
      legend.position = "bottom", #"right", #"none" is a global option to remove the legend.
      legend.direction = "vertical",
      legend.key = element_blank(),
      legend.title.align = 0.5,
      panel.grid.major = element_blank(),
      axis.text.x = element_text(angle = 30, size = 6),
      axis.text.y = element_text(size = 6, color = "#666666"),
      axis.title = element_text(size = 10, color = "#666666"),
      panel.grid.minor.x =element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks = element_line(size = 0.15, color = "grey20"),
      legend.background = element_rect(fill = "#F8F8F8", size = 0.25, linetype = "solid", colour = "grey50"),
      #strip.text.x = ggplot2::element_text(size = 9, angle=90)
      strip.text.y.right = element_text(angle = 0)
    )-> diphtheria_case_type_plot

    diphtheria_case_type_plot
    save( diphtheria_case_type_plot, file = "plots/diphtheria_case_type_plot.RData")
    
    ggsave( "plots/diphtheria_case_type_plot.png", diphtheria_case_type_plot, width = 12, height = 6, dpi = 300)
    # add the epicurve theme to this. 


#save( facility_flextable_with_graphs, file = "dashboard/facility_flextable_with_graphs.RData")


return_facilities_view <- function(
        facility_selection = NULL, 
        condition_selection = NULL
        ) {
    # Default ggplot builder
    create_ggplot <- function(data, x_col, y_col, fill_col, type = "box") {
        data <- dplyr::select(data, x = {{x_col}}, y = {{y_col}}, fill = {{fill_col}})
        
        # Define geometry
        geom <- switch(
            type,
            line = geom_line(aes(x = x, y = y, color = fill, group = fill), show.legend = FALSE, size = 0.5),
            box = geom_boxplot(aes(x = x, y = y, fill = fill), size = 0.1, show.legend = FALSE),
            stop("Invalid type specified. Use 'line' or 'box'.")
        )
        
        # Return ggplot object
        ggplot(data) +
            geom +
            labs(x = "Epiweek", y = "n", fill = "Case Type") +
            theme_void()
    }
    
    # Handle default or invalid facility selection
    if (!is.character(facility_selection)) {
        message("Invalid input. Using default facilities.")
        facility_selection <- c("groote schuur", "bara", "tshwane district", 
                                "pelenomi", "tygerberg", "stanger", "rob fer")
    }
    
    # Filter facilities
    collapsed_selection <- paste0(tolower(facility_selection), collapse = "|")
    facilities_selection <- new_master$facility[
        grepl(collapsed_selection, new_master$facility, ignore.case = TRUE)
    ] %>% unique()
    
    condition_selection<- "Diphtheria"
    # Handle default or invalid condition selection 
    if( !is.character(condition_selection)) {
        message( "Invalid input. Using default conditions.")
        condition_dt <- condition_df %>% as.data.table()
        condition_lab_confirmed <- condition_dt[nmccategories == 1, condition][c(5, 6, 7, 8, 9, 12, 16, 17, 18, 19, 25)]
    
    }
    # Prepare condition data
    collapsed_condition_selection <- paste0(tolower(condition_selection), collapse = "|")
        condition_selection <- condition_df$condition[
            grepl(collapsed_condition_selection, condition_df$condition, ignore.case = TRUE)
        ] %>% unique()
    
    # Filter and summarize data for plotting
    data_to_plot <- new_master %>%
        filter(
            year == 2024,
            facility %in% facilities_selection,
            condition %in% condition_lab_confirmed
        ) %>%
        group_by(prov_, facility_type, facility_sector, facility, case_type, epiweek) %>%
        summarise(n = n(), .groups = "drop") %>%
        mutate(epiweek = factor(epiweek, levels = 1:52))
    
    # Prepare nested data for plotting by condition 
    nested_data_long <- data_to_plot %>%
        tidyr::nest(data = -facility) %>%
        mutate(
            line = purrr::map(data, ~ create_ggplot(.x, epiweek, n, case_type, "line")),
            box = purrr::map(data, ~ create_ggplot(.x, epiweek, n, case_type, "box"))
        ) %>%
        select(-data)
    
    # Summarize data for wide table
    data_summary <- data_to_plot %>%
        group_by(facility, case_type) %>%
        summarise(n = sum(n), .groups = "drop") %>%
        pivot_wider(names_from = case_type, values_from = n, values_fill = 0) %>%
        mutate(
            `More lab than clinical` = case_when(
                `Laboratory notifications` > `Clinical notifications` ~ "Yes",
                is.na(`Laboratory notifications`) ~ "No labs",
                TRUE ~ "No"
            ),
            sensitivity = scales::percent(`Merged Cases` / (`Merged Cases` + `Laboratory notifications`), accuracy = 0.1)
        )
    
    # Combine nested and summary data
    nested_data <- nested_data_long %>%
        left_join(data_summary, by = "facility") %>%
        select(
            facility,
            `Clinical notifications`,
            `Laboratory notifications`,
            `Merged Cases`,
            `More lab than clinical`,
            sensitivity,
            line
        )
    
    # Create flextable
    ft <- nested_data %>%
        flextable() %>%
        flextable::bg(j = "Clinical notifications", bg = "lightsalmon") %>%
        flextable::bg(j = "Laboratory notifications", bg = "lightblue") %>%
        flextable::bg(j = "Merged Cases", bg = "lightgreen") %>%
        flextable::border(
            i = ~ `More lab than clinical` == "Yes",
            border.top = officer::fp_border(color = "darkred"),
            border.bottom = officer::fp_border(color = "darkred")
        ) %>%
        flextable::border(
            i = ~ `More lab than clinical` == "Yes",
            j = 1,
            border.left = officer::fp_border(color = "darkred")
        ) %>%
        flextable::border(
            i = ~ `More lab than clinical` == "Yes",
            j = ncol(nested_data),
            border.right = officer::fp_border(color = "darkred")
        )
    
    # Add embedded ggplots
    ft <- flextable::compose(
        ft,
        j = "line",
        value = as_paragraph(gg_chunk(value = ., height = 0.8, width = 1.5)),
        use_dot = TRUE
    )
    
    return(ft)
}

return_facilities_view()

condition_df$condition[1:27]

return_facilities_view(facility_selection = c("bara"), 
                       condition_selection = c("Diphtheria"))


# find the facilities with the highest number of notifications 
new_master %>%
    filter( year %in% 2024, 
    nmccategories == 1)%>%
    group_by( facility)%>%
    summarise( n = n())%>%
    arrange( desc(n))%>%
    slice( 1:20)%>%
    pull( facility)%>% 
    return_facilities_view()


# Try add the contribution of each condition to the table 

facility_selection<- NULL
condition_selection <- NULL

return_facilities_view <- function(
        facility_selection = NULL, 
        condition_selection = NULL

        ) {
    # Default ggplot builder
    create_ggplot <- function(data, x_col, y_col, fill_col, type = "box") {
        data <- dplyr::select(data, x = {{x_col}}, y = {{y_col}}, fill = {{fill_col}})
        
        # Define geometry
        geom <- switch(
            type,
            line = geom_line(aes(x = x, y = y, color = fill, group = fill), show.legend = FALSE, size = 0.5),
            box = geom_boxplot(aes(x = x, y = y, fill = fill), size = 0.1, show.legend = FALSE),
            stop("Invalid type specified. Use 'line' or 'box'.")
        )
        
        # Return ggplot object
        ggplot(data) +
            geom +
            labs(x = "Epiweek", y = "n", fill = "Case Type") +
            theme_void()
    }

    # Handle default or invalid facility selection
    if (!is.character(facility_selection)) {
        message("No facility selected Using default facilities.")
        facility_selection <- c("groote schuur", "bara", "tshwane district", 
                                "pelenomi", "tygerberg", "stanger", "rob fer")
    }

    # Filter facilities
    collapsed_selection <- paste0(tolower(facility_selection), collapse = "|")
    facilities_selection <- new_master$facility[
        grepl(collapsed_selection, new_master$facility, ignore.case = TRUE)
    ] %>% unique()
    
    NMCleaner::condition_df %>% as.data.table() -> condition_dt
    # Handle default or invalid condition selection 


    if( !is.character(condition_selection)) {
        message( "No condition selected. Using default conditions.")
        condition_selection <- condition_dt[nmccategories == 1, condition][c(5, 6, 7, 8, 9, 12, 16, 17, 18, 19, 25)]
    }
    # Prepare condition data
    collapsed_condition_selection <- paste0(tolower(condition_selection), collapse = "|")
        condition_selection <- condition_dt$condition[
            grepl(collapsed_condition_selection, condition_dt$condition, ignore.case = TRUE)
        ] %>% unique()-> condition_selection

condition_selection
    # Filter and summarize data for plotting
    data_to_plot <- new_master %>%
        filter(
            year == 2024,
            facility %in% facilities_selection,
            condition %in% condition_selection
        ) %>%
        group_by(
            condition,
            prov_, 
            facility_type, 
            facility_sector, 
            facility, 
            case_type, 
            epiweek) %>%
        summarise(n = n(), .groups = "drop") %>%
        mutate(epiweek = factor(epiweek, levels = 1:52))
    
    # Prepare nested data for plotting of all the notifications per facility 

data_to_plot %>%
        tidyr::nest(data = -c(facility, condition) )%>%
        mutate(
            line = purrr::map(data, ~ create_ggplot(.x, epiweek, n, case_type, "line")),
            box = purrr::map(data, ~ create_ggplot(.x, epiweek, n, case_type, "box"))
        ) %>%
        select(-data)%>%
        arrange( facility, condition ) -> 
            nested_data_long_conidition
    
    nested_data_long_conidition

    #data_summary_condition <- 
    data_to_plot %>%
        mutate( 
            case_type = factor( 
                case_type, 
                levels = c("Clinical notifications", "Laboratory notifications", "Merged Cases")
            )
        )%>%
        ungroup() %>%
        complete( case_type, condition,prov_,facility_type, facility_sector, facility, epiweek,  fill = list(n = 0))%>%
        group_by(facility, case_type, condition ) %>%
        summarise(n = sum(n), .groups = "drop")%>%
        pivot_wider(names_from = case_type, values_from = n, values_fill = 0) %>%
        mutate(
            `More lab than clinical` = case_when(
                `Laboratory notifications` > `Clinical notifications` ~ "Yes",
                `Laboratory notifications` == 0 ~ "No labs",
                TRUE ~ "No"
            ),
            sensitivity = scales::percent(`Merged Cases` / (`Merged Cases` + `Laboratory notifications`), accuracy = 0.1)
        )
    
    left_join( 
        nested_data_long_conidition, 
        data_summary_condition , 
        by = c( "condition", "facility")
    )-> 
    condition_nested_data_condition

    data_to_plot %>%
     mutate( 
            case_type = factor( 
                case_type, 
                levels = c("Clinical notifications", "Laboratory notifications", "Merged Cases")
            )
        )%>%
        ungroup() %>%
        complete( case_type, condition,prov_,facility_type, facility_sector, facility, epiweek,  fill = list(n = 0))%>%
        group_by( prov_, 
            facility_type, 
            facility_sector, 
            facility, 
            case_type, 
            epiweek) %>%
            summarise( n = sum(n))%>%
        tidyr::nest(data = -c(facility) )%>%
        mutate( condition = "All")%>%
        mutate(
            line = purrr::map(data, ~ create_ggplot(.x, epiweek, n, case_type, "line")),
            box = purrr::map(data, ~ create_ggplot(.x, epiweek, n, case_type, "box"))
        ) %>%
        select(-data)%>%
        arrange( facility, condition ) -> 
            nested_data_long_all
    
    nested_data_long_all

    # Prepare nested data for plotting of the summary of the notifications of each condition per facility
data_to_plot
    # Summarize data for wide table
 data_to_plot %>%
          mutate( 
            case_type = factor( 
                case_type, 
                levels = c("Clinical notifications", "Laboratory notifications", "Merged Cases")
            )
        )%>%
        ungroup() %>%
        complete( case_type, condition,prov_,facility_type, facility_sector, facility, epiweek,  fill = list(n = 0))%>%
        group_by(facility, case_type) %>%
        summarise(n = sum(n), .groups = "drop") %>%
        pivot_wider(names_from = case_type, values_from = n, values_fill = 0) %>%
        mutate(
            `More lab than clinical` = case_when(
                `Laboratory notifications` > `Clinical notifications` ~ "Yes",
                `Laboratory notifications` == 0 ~ "No labs",
                TRUE ~ "No"
            )
        ) %>%
            mutate(
            sensitivity = scales::percent(`Merged Cases` / (`Merged Cases` + `Laboratory notifications`), accuracy = 0.1)
        )-> 
           data_summary_all

    left_join( 
        nested_data_long_all, 
        data_summary_all , 
        by = c( "facility")
    )-> 
    condition_nested_data_all


        bind_rows(
            condition_nested_data_all, 
            condition_nested_data_condition
        ) %>%
                select(
                    facility,
                    condition, 
                    `Clinical notifications`,
                    `Laboratory notifications`,
                    `Merged Cases`,
                    `More lab than clinical`,
                    sensitivity,
                    line
                )%>%
                arrange( facility, condition) -> 
                nested_data
            
   
    # Create flextable
    nested_data %>%flextable()%>%
        flextable::bg(j = "Clinical notifications", bg = "lightsalmon") %>%
        flextable::bg(j = "Laboratory notifications", bg = "lightblue") %>%
        flextable::bg(j = "Merged Cases", bg = "lightgreen") %>%
        flextable::border(
            i = ~ `More lab than clinical` == "Yes",
            border.top = officer::fp_border(color = "darkred"),
            border.bottom = officer::fp_border(color = "darkred")
        ) %>%
        flextable::border(
            i = ~ `More lab than clinical` == "Yes",
            j = 1,
            border.left = officer::fp_border(color = "darkred")
        ) %>%
        flextable::border(
            i = ~ `More lab than clinical` == "Yes",
            j = ncol(nested_data),
            border.right = officer::fp_border(color = "darkred")
        )-> ft
    
    # Add embedded ggplots
    ft <- flextable::compose(
        ft,
        j = "line",
        value = as_paragraph(gg_chunk(value = ., height = 0.8, width = 1.5)),
        use_dot = TRUE
    )

    # if the condtion != "All", then remove the contents in the first row 

    flextable::compose( 
        ft , 
        i = ~ condition != "All",
        j = 1,
        value = as_paragraph("")
    )%>%
    bold(  j = 1)-> ft
    
    return(ft)
}

return_facilities_view(
)

return_facilities_view(
    facility_selection = c("bara"),
    condition_selection = c("Diphtheria")
)
 # you need to change the filter on line 792 and also see somethign in the case_when function 

 # Great, this works now. We should be able to implement it in a dashboard. 