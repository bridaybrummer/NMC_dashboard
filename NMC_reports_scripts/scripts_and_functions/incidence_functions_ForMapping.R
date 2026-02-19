
map_incidence  <- function(data, condition_select) {
#condition_select<- "Cholera"
data  %>% 
  {if (condition_select == "Fever-Rash") make_fever_rash(.) else .} %>%
   dplyr::filter(! district %in% c("Unknown", "Isazi Import", "Not Applicable"))%>%
   dplyr:: filter( #!condition %in% "Covid-19", 
    #nmccategories ==1 , 
    grepl( condition_select,  condition, ignore.case = TRUE),  
    #year %in% 2024
    )%>%
    group_by( district) %>%
    summarise( n = n())%>%
    mutate_district_name( ., "district")->
    notifications_df



#standardise pop data 
#NMCleaner::pop$Name
NMCleaner::pop%>%
    mutate( district = gsub( ".*- ", "", Name ))%>%
    mutate_district_name(., "district")-> 
    pop

pop$district_standard%>%unique()

pop %>% 
    mutate( district = district_standard)->
    pop



NMCleaner::pop%>%
    dplyr::filter( 
      Year %in% 2024
      ) %>%
    mutate( district = gsub( ".*- ", "", Name ))%>%
    select( - Name)%>%
    group_by( district)%>%
    summarise( pop = sum( Population))%>%
    mutate_district_name(., "district")-> 
    pop_df 


shape_files[["districts"]]$district%>%unique
shape_files[["sub_districts"]]$district%>%unique

# standardsie the district names in these two datasets. 
shape_files[c("districts", "sub_districts")] <- 
  map(c("districts", "sub_districts"), ~
    shape_files[[.x]] %>%
      as_tibble() %>%
      mutate_district_name("district") %>%
      st_as_sf()%>%
      ms_simplify(, keep  = 0.01)
      
  )


shape_files$districts%>%
    as_tibble() %>%
    mutate_district_name(., "district")%>%
    st_as_sf()->
    shape_df

shape_df%>%
    left_join( pop_df, by = "district_standard")%>%
    left_join( notifications_df, by = "district_standard")%>%
    mutate( incidence = n/pop*100000)->
    final_df


###########

# Incidence  counts 

###########

final_df%>% 
# maybe in the last 5 weeks? 
    arrange( -incidence) %>%
    slice(1:10 )%>%
    group_by( district )%>%
    filter( incidence ==max(incidence))-> 
peaks_df
    

final_df %>% 
    ggplot()+
    geom_sf(aes(fill = incidence, group = district))+
        scale_fill_viridis_c()+
    geom_sf_text(data = peaks_df, 
      aes(label = district_standard), size = 2)+
    theme_classic()+
    labs( 
        x = "", 
        y = ""
    )+
    theme(
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
    )->
    notifications_map_incidence 


plotly::ggplotly(notifications_map_incidence)

}




#################################

# Create the same but for case coutns only, 
# At a later stage make it into aone function with an options for better efficiency

#################################

map_n  <- function(data, condition_select) {

data  %>% 
  # if condition_select == "Fever-Rash" then apply fucntion make_fever_rash
    {if (condition_select== "Fever-Rash") make_fever_rash(.) else .} %>%
   dplyr::filter(! district %in% c("Unknown", "Isazi Import", "Not Applicable"))%>%
   dplyr:: filter( #!condition %in% "Covid-19", 
    #nmccategories ==1 , 
    grepl( condition_select,  condition, ignore.case = TRUE),  
    year %in% 2024:2025
    )%>%
    group_by( district) %>%
    summarise( n = n())%>%
    mutate_district_name( ., "district")->
    notifications_df

#standardise pop data 
#NMCleaner::pop$Name
NMCleaner::pop%>%
    mutate( district = gsub( ".*- ", "", Name ))%>%
    mutate_district_name(., "district")-> 
    pop

pop$district_standard%>%unique()

pop %>% 
    mutate( district = district_standard)->
    pop



NMCleaner::pop%>%
    dplyr::filter( Year %in% 2024) %>%
    mutate( district = gsub( ".*- ", "", Name ))%>%
    select( - Name)%>%
    group_by( district)%>%
    summarise( pop = sum( Population))%>%
    mutate_district_name(., "district")-> 
    pop_df 


shape_files[["districts"]]$district%>%unique
shape_files[["sub_districts"]]$district%>%unique

# standardsie the district names in these two datasets. 
shape_files[c("districts", "sub_districts")] <- 
  map(c("districts", "sub_districts"), ~
    shape_files[[.x]] %>%
      as_tibble() %>%
      mutate_district_name("district") %>%
      st_as_sf()%>%
      ms_simplify(, keep  = 0.01)
      
  )


shape_files$districts%>%
    as_tibble() %>%
    mutate_district_name(., "district")%>%
    st_as_sf()->
    shape_df

shape_df%>%
    left_join( pop_df, by = "district_standard")%>%
    left_join( notifications_df, by = "district_standard")%>%
    mutate( 
    #  incidence = n/pop*100000
      )->
    final_df

###########

# Case counts 

###########

final_df%>% 
# maybe in the last 5 weeks? 
    arrange( -n) %>%
    slice(1:10 )%>%
    group_by( district )%>%
    filter( n ==max(n))-> 
peaks_df
    

final_df %>% 
    ggplot()+
    geom_sf(aes(fill = n, group = district))+
    scale_fill_viridis_c()+
    geom_sf_text(data = peaks_df, 
      aes(label = district_standard), size = 2)+
    theme_classic()+
    labs( 
        x = "", 
        y = ""
    )+
    theme(
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
    )->
    notifications_map_n


plotly::ggplotly(notifications_map_n)

}



#map_incidence(data = new_master, condition_select = "Rubella")

  #knit_print( decompose_condition(data = df_with_ears , condition_select =  condition))
  #map_n(data = new_master, condition_select = "Fever-Rash")
  #map_incidence(data = new_master, condition_select = "Cholera")

  #cat(knit_print( map_n(data = new_master, condition_select = "Fever-Rash")))

  #cat(knit_print( map_incidence(data = new_master, condition_select = "Rubella")))
# Incidence Table 

#map_n  <- function(data, condition_select) {


# make a table of the top 10 districts with the highest incidence and case counts and tabulate them with th poplation size 
new_master$condition[grepl( "agri", new_master$condition, ignore.case = TRUE)]%>%unique()


tabulate_case_incidence  <- function(data, condition_select) {
#condition_select<-"Cholera"

data  %>% 
  # if condition_select == "Fever-Rash" then apply fucntion make_fever_rash
    {if (condition_select== "Fever-Rash") make_fever_rash(.) else .} %>%
   dplyr::filter(! district %in% c("Unknown", "Isazi Import", "Not Applicable"))%>%
   dplyr:: filter( !condition %in% "Covid-19", 
    #nmccategories ==1 , 
    grepl( condition_select,  condition, ignore.case = TRUE),  
    year %in% 2024:2025)%>%
    group_by( district) %>%
    summarise( n = n())%>%
    mutate_district_name( ., "district")->
    notifications_df

#standardise pop data 
#NMCleaner::pop$Name
NMCleaner::pop%>%
    mutate( district = gsub( ".*- ", "", Name ))%>%
    mutate_district_name(., "district")-> 
    pop

pop$district_standard%>%unique()

pop %>% 
    mutate( district = district_standard)->
    pop



NMCleaner::pop%>%
    dplyr::filter( Year %in% 2024) %>%
    mutate( district = gsub( ".*- ", "", Name ))%>%
    select( - Name)%>%
    group_by( district)%>%
    summarise( pop = sum( Population))%>%
    mutate_district_name(., "district")-> 
    pop_df 


shape_files[["districts"]]$district%>%unique
shape_files[["sub_districts"]]$district%>%unique

# standardsie the district names in these two datasets. 
shape_files[c("districts", "sub_districts")] <- 
  map(c("districts", "sub_districts"), ~
    shape_files[[.x]] %>%
      as_tibble() %>%
      mutate_district_name("district") %>%
      st_as_sf()%>%
      ms_simplify(, keep  = 0.01)
      
  )


shape_files$districts%>%
    as_tibble() %>%
    mutate_district_name(., "district")%>%
    st_as_sf()->
    shape_df

shape_df%>%
    left_join( pop_df, by = "district_standard")%>%
    left_join( notifications_df, by = "district_standard")%>%
    mutate( 
      incidence = round( n/pop*100000,1) ,
      population = round( pop, 0)
      )->
    final_df

final_df

# find top 5 incidence 
final_df%>% 
    arrange( -incidence) %>%
    slice(1:5 )%>%
    select( district, incidence, n, pop)-> 
    top5_incidence

#find top 5 case counts
final_df%>% 
    arrange( -n) %>%
    slice(1:5 )%>%
    select( district, incidence, n, pop)-> 
    top5_n

bind_rows( top5_incidence, top5_n)%>%
as_tibble() %>%
    select( district, incidence, n, pop)%>%
distinct( ., .keep_all = TRUE)->
data

data

colourer_incidence <- col_numeric(
  palette = c("transparent", "red"),
  domain = c(
    if_else( is.na(min(data$incidence, na.rm = TRUE)),0, min(data$incidence, na.rm = TRUE)), 
    max(data$incidence, na.rm = TRUE)))

colourer_n <- col_numeric(
  palette = c("transparent", "red"),
  domain = c(
    if_else( is.na(min(data$n, na.rm = TRUE)),0, min(data$n, na.rm = TRUE)),
  max(data$n, na.rm = TRUE)))


data %>%
flextable()%>%
set_header_labels(
  district = "District",
  n = "Case Counts",
  pop = "Population",
  incidence = "Incidence"
)%>%
  set_header_labels(
    district = "District",
    n = "Case Counts",
    pop = "Population",
    incidence = "Incidence"
  ) %>%
  #theme_()%>%
  bg(
     bg = colourer_incidence, 
     j = "incidence", 
     part="body"
  )%>%
  bg(
     bg = colourer_n, 
     j = "n", 
     part="body"
  )%>%

    bold(
      j = "district"
    )%>%
    bold( 
      part = "header"
    )%>%
    autofit() 

}

