#incidence_function 


#We want to select what it groups by in the settings and that should then run through. 
#rm(list= ls() )
#source("") # have the two "full" datasets. 


incidence_calc<- function(  df_cases , by = var , nmc_cat = NA, condition_filter = NA ){
  library(colorspace)
  library(RColorBrewer)
  library(flextable)
  conflicted::conflicts_prefer( dplyr::filter)
  by <- "Age"
  vars_pop <- c("Date",  as.character(by))
  
  vars_cases <- c("Date", "condition", "nmccategories", as.character(by)) 

  age_cat <-  c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65+", "Unknown"
    )
  
### This will create 
district_pop_model <- NMCleaner::pop%>%
  select(-Year, )%>%
  mutate(prov = if_else(prov == "LIM", "LP", prov),
         prov = if_else(prov == "GT", "GP", prov),
        Age = ifelse(Age %in% c("65-69", "70-74", "75-79", "80+"), "65+", Age) # we could do the inverse of this with the Age_years function in the script  but for now it is okay. 
         )%>%
  group_by(across(all_of(vars_pop)))%>%
  summarise(pop= as.integer(sum(Population, na.rm = TRUE)))


df_agg<- df_cases%>% 
  dplyr::filter(nmccategories %in% as.numeric(nmc_cat),
         if( !is.na(condition_filter)) {
           condition %in% condition_filter
         }else{
           condition == condition
           }
  )%>%
  mutate(prov = str_remove(district, " .*"),
         Date = as_date(as_yearmonth(notification_date)),
         Age = agecategory,
         Sex = gender)%>%
  group_by(across(all_of(vars_cases))
  )%>%
  summarise(cases = n())

         
  merging<- merge(district_pop_model , df_agg , by = vars_pop)%>%
  mutate(inci = cases/pop *100000)
          #%>%mutate(Age = factor(Age, levels = age_cat))
    
  if(by == "prov"){ #
  condition_order <- merging %>%
    group_by(condition) %>% 
    summarise(inci= sum(inci))%>%
    arrange(inci  )%>%pull(condition)
  
  df_order_cases <- merging %>%
    group_by(prov) %>%
    summarise(total_inci = sum( cases)) %>%
    arrange(-total_inci)%>%
    pull(prov) 
  
  df_order_inci <- merging %>%
    group_by(prov) %>%
    summarise(total_inci = sum( inci))%>%
    arrange(-total_inci)%>%
    pull(prov) 
  
  merging1 <- merging%>% 
    mutate(
    prov = factor(prov, levels =df_order_cases ),
    condition = factor( condition , levels = condition_order)
  )
} else
{
  condition_order <- merging %>%
    group_by(condition) %>% 
    summarise(inci= sum(cases))%>%
    arrange(inci  )%>%pull(condition)
  
  merging1<- merging%>% 
    mutate(
      condition = factor( condition , levels = condition_order),
      Age = factor(Age, levels = age_cat )
    )
  
  }
  
###---- create a custome colour scale with >12 ----
airqual_colors <- brewer.pal(12, "Paired")

# Define additional custom colors
additional_colors <- c("#1f78b4", "darkolivegreen", "darkred", "orange", "#6a3d9a", "#b15928")

# Combine all colors into the custom color palette
custom_colors <- c(airqual_colors, additional_colors)

###---- plot 

#incidenceOrCases <- c("cases", "inci")

plot_case<- ggplot(merging1,
              aes(x = eval(parse(text = by)), 
                  y = cases , 
                  fill = condition
              )
) +
  geom_bar(stat = "identity",
           color = "black") +
  #scale_fill_viridis_d(option = "C", begin = 0.1, end = 0.9)+
  #scale_fill_viridis(discrete = T)+
  #scale_fill_viridis()
  #scale_fill_brewer(palette = "Paired") +
  scale_fill_manual(values = custom_colors,
                    name = paste0("Category ",nmc_cat," Conditions"))+
  labs(x = 
         if(by =="prov"){
           paste0("Province")
         }
       else{
         paste0("Age Category")
       }, 
       y = "Notifications"
       ) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

plot_incidence<- ggplot(if(by == "prov"){
  merging1%>%
                          mutate(prov = factor(prov, levels = df_order_inci))
  }else{merging1},
                                    aes(x = eval(parse(text = by)), 
                                        y = inci , 
                                        fill = condition
                                    )) +
  geom_bar(stat = "identity",
           color = "black") +
  #scale_fill_viridis_d(option = "C", begin = 0.1, end = 0.9)+
  #scale_fill_viridis(discrete = T)+
  #scale_fill_viridis()
  #scale_fill_brewer(palette = "Paired") +
  scale_fill_manual(values = custom_colors,
                    name = paste0("Category ",nmc_cat," Conditions"))+
  labs(x = 
         if(by =="prov"){
           paste0("Province")
         }
       else{
         paste0("Age Category")
       }, 
       y = "Notifications per 100 000 population"
  ) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


## flextable ----
if(by == "prov"){
  
  df_flex_order <- merging1 %>% 
    mutate( prov_pop = paste0(prov, " pop =               ", format(pop, big.mark = " "))) %>%
    group_by(prov_pop) %>%
    summarise(total_inci = sum(cases))%>%
    arrange(-total_inci)%>%
    pull(prov_pop)
  
merging2 <- merging1%>%
  mutate( prov_pop = paste0(prov, " pop =               ", format(pop, big.mark = " ")))%>%
  mutate( prov_pop = factor(prov_pop, levels = df_flex_order ))%>%
  arrange(prov_pop)

  long <- merging2 %>% pivot_longer(
    cols = cases:inci, 
    values_to = "number"
  )%>%
  select(prov, prov_pop, condition, name, number
  )%>%
    mutate(name = case_when( name == "cases" ~ "N", 
                             name == "inci" ~ "NI")
    )


my_stats <- function(data, ...) {
  mean_number <- mean(data$number, na.rm = TRUE)
  #mean_pop <- mean(data$pop, na.rm = TRUE)
  dplyr::tibble(
    mean_number = mean_number,
    #mean_pop = mean_pop
  )
}

inci_table<- long %>% mutate( number = ifelse(is.na(number), " ", number),
                              ) %>%
  tbl_strata(., 
             strata = prov_pop, 
             .tbl_fun = 
               ~ .x %>%
               tbl_custom_summary(
                 missing = "no",
                 by = name, 
                 stat_fns = everything() ~ my_stats,
                 statistic = list(everything() ~ "{mean_number}"),
                 label = list(condition ~ "")
                 #digits = list(c("") ~ c(3, 0),
                 #all_categorical() ~ "{n}"
               ),
  )  %>%
  modify_header(all_stat_cols() ~ "**{level}**",
                label = "**Condition**")%>%
  modify_footnote(update = everything() ~ "N = Notifications, NI = Notification Incidence")



inci_table[["table_body"]]<- inci_table[["table_body"]]%>%mutate(across(everything(),  ~ case_when(is.na(.) | . =="NA" ~ "-", 
                                                                                                   .default = .)
)
)%>%
  mutate(across(starts_with("stat_1"), ~ round(as.numeric(.), 0)))%>%
  filter(variable == "condition")



inci_table

inci_table[["table_body"]]

#flextable::set_flextable_defaults(
#  font.family = "Century Gothic", 
#  font.size = 9, 
#  header.font.size = 9,
#  border.color = "black")
length<- inci_table[["table_body"]]%>%nrow()
inci_table[["table_body"]]<- inci_table[["table_body"]][2:length,]

inci_table[["table_body"]]

inci_flex<- inci_table%>% as_flex_table() 
#flextable::set_caption("Age distribution by gender, admission status, and patient outcome") %>%
inci_flex

pops<-  inci_flex[["header"]][["dataset"]][1,]%>%
  str_replace(., ".*pop", "Population")%>% 
  str_replace(., "\\*+$", "")
pops

inci_flex[["header"]][["dataset"]][1,]%>%str_replace(., ".*pop", "pop")

provs <- inci_flex[["header"]][["dataset"]][1,]%>%
  str_replace(., "pop.*", "")%>%
  str_replace(., "^\\*+", "")


header_length <- inci_flex[["body"]][["colwidths"]]%>%length()

c(1, seq(2, header_length, 2) )


#inci_flex1<- inci_flex %>% bold(, j = c(1,2, 4, 6, 8, 10, 12, 14, 16, 18))%>%
#  add_header_row(, values = pops, top = T)%>%
#  merge_at(., i = 1, j = 2:3, part = "header")%>%
#  merge_at(., i = 1, j = 4:5, part = "header")%>%
#  merge_at(., i = 1, j = 6:7, part = "header")%>%
#  merge_at(., i = 1, j = 8:9, part = "header")%>%
#  merge_at(., i = 1, j = 10:11, part = "header")%>%
#  merge_at(., i = 1, j = 12:13, part = "header")%>%
#  merge_at(., i = 1, j = 14:15, part = "header")%>%
#  merge_at(., i = 1, j = 16:17, part = "header")%>%
#  merge_at(., i = 1, j = 18:19, part = "header")%>%
#  add_header_row(, values = provs, top = T)%>%
#  merge_at(., i = 1, j = 2:3, part = "header")%>%
#  merge_at(., i = 1, j = 4:5, part = "header")%>%
#  merge_at(., i = 1, j = 6:7, part = "header")%>%
#  merge_at(., i = 1, j = 8:9, part = "header")%>%
#  merge_at(., i = 1, j = 10:11, part = "header")%>%
#  merge_at(., i = 1, j = 12:13, part = "header")%>%
#  merge_at(., i = 1, j = 14:15, part = "header")%>%
#  merge_at(., i = 1, j = 16:17, part = "header")%>%
#  merge_at(., i = 1, j = 18:19, part = "header")%>%
#  bold(., part = "header")%>%
#  align(, align = "center", part = "all")




flextable<- inci_flex%>%
  bold(., j = c(1, seq(2, header_length, 2) ))%>% 
  set_table_properties(layout = "autofit", width = 0.99)%>%fontsize(., size = 6, part = "header")

}else{
  
  #flextable <- flextable
  #flextable <- "You still need to create the Age version of this thing bruh"
  
  ### AGE 

  long_age <- merging1 %>% pivot_longer(
    cols = cases:inci, 
    values_to = "number"
  )%>%
    select(Age, condition, name, number
    )%>%
    mutate(name = case_when( name == "cases" ~ "N", 
                             name == "inci" ~ "NI")
    )
  
  
  my_stats <- function(data, ...) {
    mean_number <- mean(data$number, na.rm = TRUE)
    #mean_pop <- mean(data$pop, na.rm = TRUE)
    dplyr::tibble(
      mean_number = mean_number,
      #mean_pop = mean_pop
    )
  }
  
  inci_table_age<- long_age %>% mutate( number = ifelse(is.na(number), " ", number),
  ) %>%
    tbl_strata(., 
               strata = Age, 
               .tbl_fun = 
                 ~ .x %>%
                 tbl_custom_summary(
                   missing = "no",
                   by = name, 
                   stat_fns = everything() ~ my_stats,
                   statistic = list(everything() ~ "{mean_number}"),
                   label = list(condition ~ "")
                   #digits = list(c("") ~ c(3, 0),
                   #all_categorical() ~ "{n}"
                 ),
    )  %>%
    modify_header(all_stat_cols() ~ "**{level}**",
                  label = "**Condition**")%>%
    modify_footnote(update = everything() ~ "N = Notifications, NI = Notification Incidence")
  
  
  
  inci_table_age[["table_body"]]<- inci_table_age[["table_body"]]%>%mutate(across(everything(),  ~ case_when(is.na(.) | . =="NA" ~ "-", 
                                                                                                     .default = .)
  )
  )%>%
    mutate(across(starts_with("stat_1"), ~ round(as.numeric(.), 0)))%>%
    filter(variable == "condition")
  
  
  
  inci_table_age
  
  length<- inci_table_age[["table_body"]]%>%nrow()
  inci_table_age[["table_body"]]<- inci_table_age[["table_body"]][2:length,]
  
  inci_table_age[["table_body"]]
  
  #flextable::set_flextable_defaults(
  #  font.family = "Century Gothic", 
  #  font.size = 9, 
  #  header.font.size = 9,
  #  border.color = "black")
  
  inci_flex_age<- inci_table_age%>% as_flex_table() 
  #flextable::set_caption("Age distribution by gender, admission status, and patient outcome") %>%
  inci_flex_age
 
  
  header_length <- inci_flex_age[["body"]][["colwidths"]]%>%length()
  
  c(1, seq(2, header_length, 2) )
  
  
  #inci_flex1<- inci_flex %>% bold(, j = c(1,2, 4, 6, 8, 10, 12, 14, 16, 18))%>%
  #  add_header_row(, values = pops, top = T)%>%
  #  merge_at(., i = 1, j = 2:3, part = "header")%>%
  #  merge_at(., i = 1, j = 4:5, part = "header")%>%
  #  merge_at(., i = 1, j = 6:7, part = "header")%>%
  #  merge_at(., i = 1, j = 8:9, part = "header")%>%
  #  merge_at(., i = 1, j = 10:11, part = "header")%>%
  #  merge_at(., i = 1, j = 12:13, part = "header")%>%
  #  merge_at(., i = 1, j = 14:15, part = "header")%>%
  #  merge_at(., i = 1, j = 16:17, part = "header")%>%
  #  merge_at(., i = 1, j = 18:19, part = "header")%>%
  #  add_header_row(, values = provs, top = T)%>%
  #  merge_at(., i = 1, j = 2:3, part = "header")%>%
  #  merge_at(., i = 1, j = 4:5, part = "header")%>%
  #  merge_at(., i = 1, j = 6:7, part = "header")%>%
  #  merge_at(., i = 1, j = 8:9, part = "header")%>%
  #  merge_at(., i = 1, j = 10:11, part = "header")%>%
  #  merge_at(., i = 1, j = 12:13, part = "header")%>%
  #  merge_at(., i = 1, j = 14:15, part = "header")%>%
  #  merge_at(., i = 1, j = 16:17, part = "header")%>%
  #  merge_at(., i = 1, j = 18:19, part = "header")%>%
  #  bold(., part = "header")%>%
  #  align(, align = "center", part = "all")
  
  flextable<- inci_flex_age%>%
    bold(., j = c(1, seq(2, header_length, 2) ))%>% 
    set_table_properties(layout = "autofit", width = 0.99)%>%
    fontsize(., size = 6, part = "header")

  }

return(list(plot_incidence = plot_incidence, plot_case = plot_case, merging= merging, merging1 = merging1, flextable = flextable ))

  }

#load("~/Desktop/SAFETP/CLA/NMC_database/master/new_master.rda")
prov<- incidence_calc( new_master%>%filter(grepl("tuberc", ignore.case = T, condition)) , by = "Age", nmc_cat = 2)

prov$flextable

flextable_draft <- prov$flextable

prov$flextable%>% bold(, j = c(1,2, 4, 6, 8, 10, 12, 14, 16, 18))


header_length <- flextable_draft[["body"]][["colwidths"]]%>%length()

c(1, seq(2, header_length, 2) )
