#| This will have 
#|  dot counts of category 1 
#|  dot counts of category 2 
#|  pyramid of case types by sector
#|  Trends graph of case types 

# Category 1 ----
    conflicted::conflicts_prefer(dplyr::filter)
    
    df1$condition <- factor(df1$condition, levels = rev(unique(df1$condition)))
    
    condition_levels_dot<- tabyl(dat = df1%>%filter(nmccategories == 1) , condition ) %>%arrange(n)%>%select(condition) %>%pull
    
    prov_levels_dot<- tabyl(dat = df1%>%filter(nmccategories == 1) , prov_) %>%arrange(-n)%>%select(prov_) %>%pull
    
    dot_nmc1<-df1%>%filter(nmccategories ==1, !is.na(prov_)) %>%
      mutate(prov_ = factor(prov_ , 
                           levels = prov_levels_dot[1:9]),
             condition = factor(condition , 
                                levels = condition_levels_dot))%>%
      ggplot(., ) +
      #geom_count(aes(x = prov_, y = condition, color = condition, alpha = 0.01),
      #           show.legend = TRUE, stroke = 0.01) +
       #subset the data for only the case_defintion %in% Confirmed and make those dots alpha = 1
      geom_count(data = df1%>%filter(nmccategories == 1, case_definition == "Confirmed"), 
                 aes(x = prov_, y = condition, color = condition),
                 show.legend = TRUE, stroke = 1) +
      scale_size_area(max_size = 25)+
      labs(#title = paste0("Notifications of category ", df1 %>%select(nmccategories)%>%unique() %>%pull(),   " by Province"),
           x = "Province", y = "Condition") +
      theme_minimal() +
      #  scale_color_brewer(palette = "Set1")+
      theme(text = element_text(family = "Century Gothic", size = 12, color = "#333333"),
            panel.background = element_rect(fill = "#F8F8F8"),
            plot.background = element_rect(fill = "#F8F8F8"),
            panel.grid = element_line(color = "#DDDDDD"),
            legend.background = element_rect(fill = "#F8F8F8"),
            legend.text = element_text(color = "#333333"),
            legend.title = element_text(color = "#333333", size = 10),
            plot.title = element_text(size = 16, #face = "bold",
                                      color = "#666666"),
            plot.subtitle = element_text(size = 12, color = "#666666"),
            plot.caption = element_text(size = 10, color = "#666666"),
            legend.position = "right",
            legend.direction = "vertical",
            #legend.key = 
            #legend.title = element_blank()
            ) +
      guides(color = "none",
             #fill = "none",
             alpha = "none",
             size = "legend")+
      #scale_fill_alpha()+
      #scale_color_brewer(palette = "Set2")
      scale_color_viridis_d( option = "turbo") 
    
    dot_nmc1
    
    save(dot_nmc1, file = "plots/dot_nmc1.rda" )

# Category 2 ----
        
    new_levels<- df1%>%filter(nmccategories == 2) %>%select(condition ) %>%unique%>%pull%>%as.character%>%sort(decreasing   = TRUE)
    
    condition_levels_dot<- tabyl(dat = df1%>%filter(nmccategories == 2) ,
                                 condition ) %>%arrange(n)%>%select(condition) %>%pull
    
    prov_levels_dot<- tabyl(dat = df1%>%filter(nmccategories == 2) , 
                            prov_) %>%arrange(-n)%>%select(prov_) %>%pull
    
    
    
    dot_nmc2<-df1%>%dplyr::filter(nmccategories ==2, !is.na(prov_)) %>%
      mutate( condition = factor(condition, levels = new_levels))%>%
      mutate(prov_ = factor(prov_ , 
                            levels = prov_levels_dot[1:9]),
             condition = factor(condition , 
                                levels = condition_levels_dot))%>%
      ggplot(., aes(x = prov_, y = condition, color = condition)) +
      geom_count(show.legend = TRUE) +
      scale_size_area(max_size = 15)+
      labs(#title = paste0("Notifications of category 2 within Each Province"),
           x = "Province", y = "Condition") +
      theme_minimal() +
      #  scale_color_brewer(palette = "Set1")+
      scale_color_viridis_d( option = "C") +
      
      # i want to try different colors scales and palletes
      # scale_color_viridis_c(option = "C") +
      # scale_color_viridis_c(option = "D") +
      # scale_color_viridis_c(option = "E") +
       #scale_color_viridis_c(option = "F") +
    
      theme(text = element_text(family = "Century Gothic", size = 12, color = "#333333"),
            panel.background = element_rect(fill = "#F8F8F8"),
            plot.background = element_rect(fill = "#F8F8F8"),
            panel.grid = element_line(color = "#DDDDDD"),
            legend.background = element_rect(fill = "#F8F8F8"),
            legend.text = element_text(color = "#333333"),
            legend.title = element_text(color = "#333333", size = 10),
            plot.title = element_text(size = 16, #face = "bold",
                                      color = "#666666"),
            plot.subtitle = element_text(size = 12, color = "#666666"),
            plot.caption = element_text(size = 10, color = "#666666"),
            legend.position = "right",
            legend.direction = "vertical",
            legend.key = element_blank()
            #legend.title = element_blank()
      ) +
      guides(color = "none")
    
    dot_nmc2
    save(dot_nmc2, file = "plots/dot_nmc2.rda" )
    #dot_nmc2


xtabs(~ condition , data = df1) 


## Number 1 is information for action. Are there outbreaks? What are trends over last month. 
## I actually think we need to restructure it to show how the month fits into the yearly trend. 

## This would require the entire years data and then a system whereby we add the next month/week to it. 

# Big one is differences between provinces. Show the endemic provinces. 
# Show trends of NMC diseases by line graph? 
# case source etc should all be 

# see if we can make average notifications per day of the week per month .

# Case type notificaitons on days of the week ----

    new_master$case_type%>%unique
    new_master %>%mutate_dates(.,date_index = "notification_date")%>%
      mutate( day_name = factor(day_name, levels =     format( as_date(c(4:10)), "%A" ))
              )-> master_dates
      
    master_dates%>% 
      group_by( nmccategories, case_type, day_name) %>%
      summarise (n = mean(., na.rm = TRUE) ) %>%
      ggplot(., aes(x = case_type, y = day_name, )) +
      geom_count(aes(size = n), show.legend = , max_size = 10)+
      facet_nested( ~ nmccategories) +
      coord_flip()
      

# case_type 
    # git install birdaybrummer/NMCleaner
    #remotes::install_git("https://github.com/bridaybrummer/NMCleaner.git")
    #library(NMCleaner)
    #new_master %>%mutate_dates(.,date_index = "notification_date")%>%
    #  epicurve_df( ., date_index = "date", grouping_vars = "case_type"
    #               ) -> df_for_epicurve
    #df_for_epicurve

### symptoms with count s
symptoms_split<- df1$symptoms%>%
    strsplit(., "\\|")%>%
    unlist()%>%unique()%>%na.omit()
  
  #tabyl(symptoms_split)%>%arrange(-n)%>%filter( )

df_symptoms <- df1 %>%
  separate_rows(symptoms, sep = "\\|") %>%
  filter(!is.na(symptoms))  # Remove NA values



# Create an empty list to store results
result_list <- list()

# run the following lines and skip error:



        # Iterate over each symptom
        for (symptom in unique(df_symptoms$symptoms)) {
          # Count occurrences of each symptom for each condition
          symptom_counts <- df_symptoms %>%
            filter(condition %in% category_1)%>%
            filter(symptoms == symptom) %>%
            group_by(condition) %>%
            summarise(count = n()) %>%
            mutate(symptom = symptom)
          
          # Print the result
          result_list<- bind_rows(symptom_counts, result_list)
          
          # Store the result in the list
          #result_list[[symptom]] <- symptom_counts
        }
        
        tryCatch({
          result_list %>% 
            filter(condition %in% category_1) %>%
            ggplot(aes(x = symptom, y = condition)) +
            geom_count(aes(size = count), show.legend = TRUE, max_size = 10) +
            coord_flip()

          ggplot(result_list %>% dplyr::filter(condition %in% category_1), aes(x = symptom, y = condition, fill = count)) +
            geom_tile() +
            scale_fill_gradient(low = "white", high = "blue") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

          mat <- pivot_wider(result_list, names_from = condition, values_from = count, values_fill = 0) %>%
            column_to_rownames(var = "symptom") %>%
            as.matrix()

          order <- rev(order(rowSums(mat)))
          # Relevel variables based on the order
          result_list$symptom <- factor(result_list$symptom, levels = rownames(mat)[order])

          ggplot(result_list, aes(x = symptom, y = condition, fill = count)) +
            geom_tile() +
            scale_fill_gradient(low = "white", high = "blue") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
        }, error = function(e) {
          message("An error occurred: ", e$message)
        })

# Case Type pyramid ----

case_types_desc<- tabyl(df$case_type)%>%arrange(n)%>%select( 1)%>%pull()
prov_desc<- tabyl(df$prov_)%>%arrange(n)%>%select( 1)%>%na.omit()%>%pull()

color_pallette_case_type <- c("darkkhaki", "lightsalmon", "darkcyan", "grey5", "grey65")

# case type by sector pyramid
df %>%
  dplyr::filter( nmccategories %in% 1:2  ) %>%
  group_by(
  nmccategories,
  prov_, 
  facility_sector, 
  case_type) %>%
  summarise(n = n()) %>%
  mutate( case_type = factor(case_type, levels = case_types_desc),
          prov_ = factor( prov_ , levels = prov_desc))%>%
  ungroup%>%
  #complete all categories 
  complete( prov_, facility_sector, case_type, fill = list(n = 0))%>%
  mutate( n = if_else( facility_sector %in% "Private", -n, n),
          )%>%
  na.omit()-> pyr_df 

pyr_df

max_notification_prov<- pyr_df%>%group_by(prov_)%>%summarise( n = sum(n))%>%arrange(-n)%>%select(n)%>%pull()%>%max# find nearest thousand 
# find the closest thousand to the max value of n
max_notification_prov <- ceiling(max_notification_prov/1000)*1000
max_notification_prov

rect_data <- data.frame(
  facility_sector = c("Private", "Public"),
  xmin = c(-max_notification_prov, 0),
  xmax = c(0, max_notification_prov),
  ymin = c(-Inf, -Inf),
  ymax = c(Inf, Inf)
)

pyramid_case_source<- pyr_df %>%
  ggplot() + 
    geom_rect(data = rect_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = facility_sector), alpha = 0.75) +
  geom_col(data =pyr_df , aes(x = n, y = prov_, fill = case_type)) +
 
  facet_grid(nmccategories ~ .) +
  scale_fill_manual(values = color_pallette_case_type) +
  theme(axis.text.x = element_text(margin = margin(t = 10))) +
  scale_x_continuous(
    labels = abs(pretty(-500:max_notification_prov, n = 8)), 
    breaks = pretty(-500:max_notification_prov, n = 8)
  ) +
  coord_cartesian(xlim = c(-max_notification_prov / 5, max_notification_prov)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.1) +
  geom_text(
    data = pyr_df %>% group_by(prov_, nmccategories) %>% filter(n == min(n)),
    aes(label = prov_, y = prov_, x = n, group = facility_sector), 
    hjust = 1.1, size = 3, color = "white",
    family = "Century Gothic"
  )+
  #theme_classic() +
  theme(text = element_text(family = "Century Gothic", size = 12, color = "#333333"),
        panel.background = element_rect(fill = "#F8F8F8"),
        plot.background = element_rect(fill = "#F8F8F8"),
        panel.grid = element_line(color = "#DDDDDD"),
        legend.background = element_rect(fill = "#F8F8F8"),
        legend.text = element_text(color = "#333333"),
        legend.title = element_text(color = "#333333", size = 10),
        plot.title = element_text(size = 16, #face = "bold",
                                  color = "#666666"),
        plot.subtitle = element_text(size = 12, color = "#666666"),
        plot.caption = element_text(size = 10, color = "#666666"),
        legend.position = "right",
        legend.direction = "vertical",
        legend.key = element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y = element_blank(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank(),
        strip.text.y = element_text(size = 16, color = "#666666", angle = 0 ),
        # remove minor x axis grid lines
        panel.grid.minor.x = element_blank(),
        ) +
  # make more space vertically inbetween facet girds
  theme(panel.spacing = unit(2, "lines")) +
  labs(x = "Number Notifications", # Change the x-axis label
       y = "Province",          # Change the y-axis label
       #title = "Title of the Plot",  # Add title
       #subtitle = "Subtitle of the Plot", # Add subtitle
       #caption = "Source: Data Source", 
       fill = "Notification Source/Process") # Add caption


pyramid_case_source

save(pyramid_case_source, file= "plots/pyramid_case_source.rda" )

# age gender graph 
#df$notification_date%>%as_date() %>%hist(., breaks = 100)
# look for most ocmplete age cat 
xtabs(~ is.na(df$agecategory))
xtabs(~ is.na(df$agecategory_unit))
tabyl( (df$agecategory_unit))[1]%>%pull
new_master %>% filter( condition %in% "Mpox")%>%
  mutate(agecategory_unit = factor(agecategory_unit, levels =tabyl( (df$agecategory_unit))[1]%>%pull )) -> mpox

# Case type trends ----



# Mpox age and gender pyramid ----

mpox %>%
  group_by(
    case_definition, 
    agecategory_unit,
    patient_gender) %>%
  summarise(n = n()) %>%
  ungroup%>%
  #complete all categories 
  complete( agecategory_unit, patient_gender,case_definition, fill = list(n = 0))%>%
  mutate( n = if_else( patient_gender %in% "Male", -n, n),
  )%>%
  na.omit()-> mpox_df 

mpox_df %>% filter (patient_gender %in% "Male" & 
                      agecategory_unit %in% c("35-39", "30-34", "25-29", "20-24") )%>%
  group_by(agecategory_unit)%>%
  mutate( n= if_else(case_definition %in% "Confirmed" , min(n)+2, min(n)-2))

ggplot() + 
  geom_col(data = mpox_df, aes(x = n, y = agecategory_unit, fill = patient_gender), position = "stack", alpha = 0.7) +
  geom_col(data = mpox_df %>% filter(case_definition %in% "Confirmed"), aes(x = n, y = agecategory_unit, fill = patient_gender), alpha = 1) +
  scale_fill_manual(values = color_pallette_case_type)  +
  # add in some text on the y = 5 axis for the sespected and confirmed sections 
  geom_text(
    data = mpox_df %>% filter (#case_definition %in% "Confirmed", 
                               patient_gender %in% "Male" & 
                                agecategory_unit %in% c("35-39", "30-34", "25-29") )%>%
      group_by(agecategory_unit)%>%
      mutate( n= if_else(case_definition %in% "Confirmed" , -min(abs(n)) -1.9, -min(abs(n))))%>%ungroup(),
    aes(label = case_definition, y = agecategory_unit , x = n,
    ), hjust = -0, size = 3, color = "grey5", family = "Century Gothic"
  ) +
  theme(axis.text.x = element_text(margin = margin(t = 10))) +
  scale_x_continuous(
    labels = abs(pretty(-12:12, n = 8)), 
    breaks = pretty(-12:12, n = 8)
  ) +
  coord_cartesian(xlim = c(-12, 12)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.1) +
  labs(
    x = "Number of Cases",
    y = "Age Category",
    fill = "Sex",
    title = "Mpox Cases by Age Category and Sex"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Century Gothic", size = 12, color = "#333333"),
    panel.background = element_rect(fill = "#F8F8F8"),
    plot.background = element_rect(fill = "#F8F8F8"),
    panel.grid = element_line(color = "#DDDDDD"),
    legend.background = element_rect(fill = "#F8F8F8"),
    legend.text = element_text(color = "#333333"),
    legend.title = element_text(color = "#333333", size = 10),
    plot.title = element_text(size = 16, color = "#666666"),
    plot.subtitle = element_text(size = 12, color = "#666666"),
    plot.caption = element_text(size = 10, color = "#666666"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.key = element_blank(),
    strip.background.y = element_blank(),
    strip.background.x = element_blank(),
    strip.text.y = element_text(size = 16, color = "#666666", angle = 0),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(2, "lines")
  )

ggplot() + 
  geom_col(data = mpox_df, aes(x = n, y = agecategory_unit, fill = patient_gender), position = "stack") +
  geom_col(data = mpox_df %>% filter(case_definition %in% "Confirmed"), aes(x = n, y = agecategory_unit, fill = "Confirmed"), alpha = 0.6) +
  scale_fill_manual(
    values = c("Male" = "darkkhaki", "Female" = "lightsalmon", "Confirmed" = "yellow"),
    labels = c("Male", "Female", "Confirmed Case")
  ) +
  theme(axis.text.x = element_text(margin = margin(t = 10))) +
  scale_x_continuous(
    labels = abs(pretty(-12:12, n = 8)), 
    breaks = pretty(-12:12, n = 8)
  ) +
  coord_cartesian(xlim = c(-12, 12)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.1) +
  labs(
    x = "Number of Cases",
    y = "Age Category",
    fill = "Legend",
    title = "Monkeypox Cases by Age Category and Gender"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Century Gothic", size = 12, color = "#333333"),
    panel.background = element_rect(fill = "#F8F8F8"),
    plot.background = element_rect(fill = "#F8F8F8"),
    panel.grid = element_line(color = "#DDDDDD"),
    legend.background = element_rect(fill = "#F8F8F8"),
    legend.text = element_text(color = "#333333"),
    legend.title = element_text(color = "#333333", size = 10),
    plot.title = element_text(size = 16, color = "#666666"),
    plot.subtitle = element_text(size = 12, color = "#666666"),
    plot.caption = element_text(size = 10, color = "#666666"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.key = element_blank(),
    strip.background.y = element_blank(),
    strip.background.x = element_blank(),
    strip.text.y = element_text(size = 16, color = "#666666", angle = 0),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(2, "lines")
  )
  


  ggplot() + 
  geom_col(data =mpox_df , aes(x = n, y = agecategory_unit, fill = patient_gender)) +
  geom_col(data =mpox_df%>%filter( case_definition %in% "Confirmed") , aes(x = n, y = agecategory_unit, fill = c("yellow", "firebrick"))) +
  #scale_fill_manual(values = c("darkkhaki", "lightsalmon")) +
  theme(axis.text.x = element_text(margin = margin(t = 10))) +
    scale_x_continuous(
      labels = abs(pretty(-12:12, n = 8)), 
      breaks = pretty(-12:12, n = 8)
    ) +
    coord_cartesian(xlim = c(-12 , 12/5)) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.1) +
    theme_minimal()

