# gganimate with Fever Rash NMC surveillance

load("~/Desktop/SAFETP/CLA/NMC_database/master/new_master.rda")
library(NMCleaner)
conflicts_prefer(dplyr::filter)

#https://stackoverflow.com/questions/63441444/two-scale-colors-in-maps-ggplot

new_master %>%
    filter( condition %in% c("Measles", "Rubella")) %>%
    mutate_dates(date_index = "notification_date")%>%
    mutate( epiweek = floor_date(as_date(notification_date), unit = "week"))%>%
    group_by( condition, prov_, district, epiweek, year)%>%
    summarise( n = n() )-> 
df_cases

"2022-01-01"%>%as_date()%>%floor_date( ., unit ="week")

df_cases%>%
  group_by(condition, district, epiweek) %>%
  summarise( Cases = n())%>%
  ungroup() %>%
  mutate( 
    district = str_to_lower(district), 
    district = gsub( "city|metro|of|district", "", district), 
    district = trimws(district, "both")
  )%>%
  mutate( 
    district = case_when(
      grepl("durban", ignore.case = TRUE, district) ~ "ethekwini",
      grepl("durban", ignore.case = TRUE, district) ~ "ethekwini",
      grepl("enhlan", ignore.case = TRUE, district) ~ "ehlanzeni",
      grepl("mopa", ignore.case = TRUE, district) ~ "ba-phalaborwa",
      grepl("capricorn", ignore.case = TRUE, district) ~ "polokwane",
      grepl("xariep", ignore.case = TRUE, district) ~ "xhariep",
      grepl("uthungula", ignore.case = TRUE, district) ~ "king cetshwayo",
      grepl("bergville|uthukela|uthekela", ignore.case = TRUE, district) ~ "uthukela",
      .default = district 
    )
  )%>%
    group_by(condition,district, epiweek) %>%
    mutate( Cases = sum(Cases))%>%
    ungroup()%>%
    distinct( condition, district, epiweek, .keep_all  = TRUE) -> 
cases_districts

cases_districts

cases_districts%>%
  group_by(district) %>%
  summarise( n = sum(Cases) )%>%
  select(n)%>%
sum( ) -> total_cases

total_cases == nrow( df_cases)  # sanity check to ensure groupings have been correct. This shoudl sum to the total number of rows 

cases_districts$district%>%unique%>%sort()
cases_districts%>%filter( grepl("nelson", ignore.case = TRUE, district))

# Cleaning of shape file 

NMCleaner::shape_files$districts%>%
  mutate( 
    district = str_to_lower(district), 
    district = gsub( "city|metro|of|district", "", district), 
    district = trimws(district, "both")
  ) %>%
    group_by(district) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  mutate( 
    district = case_when( 
      grepl("eden", ignore.case = TRUE, district) ~ "garden route",
      grepl("cacad", ignore.case = TRUE, district) ~ "sarah baartman",
      grepl("mopani", ignore.case = TRUE, district) ~ "ba-phalaborwa",
      grepl("sisonke", ignore.case = TRUE, district) ~ "harry gwala",
      grepl("capricorn", ignore.case = TRUE, district) ~ "polokwane",
      grepl("uthung", ignore.case = TRUE, district) ~ "king cetshwayo",
            .default = district)

  )%>%
  ungroup()->

shape_districts

shape_districts%>%filter( grepl("king", ignore.case = TRUE, district ))


fuzzyjoin::stringdist_left_join(
    shape_districts, # from the shape file 
    cases_districts, #from NNMC or df_cases
    by = "district",
    method = "jw", 
    max_dist = 0.2,# use a liberal matching score to maximie matches 
    distance_col = "dist"
  )%>%
  group_by(district.x, epiweek) %>%
  mutate( Cases = sum(Cases))%>%
  filter( dist == min(dist))%>% # only take the best match 
  ungroup()%>%
  filter( 
    #year %in% 2019:2024
  )->

df_to_plot


cases_districts%>% filter( !district %in% df_to_plot$district.y) 

jarowinkler( "uthekela", "utukhela")

df_to_plot$Cases %>%sum() 
df_to_plot$Cases %>%sum() == nrow(df_cases)

# Two scales plot
df_to_plot%>%group_by(district.x)%>%
    mutate( Cases = sum(Cases))%>%
    ungroup()%>%
    distinct(district.x, .keep_all = TRUE)->
df_for_twoscale

df_for_twoscale

#install.packages("ggnewscale")
#devtools::install_github("eliocamp/ggnewscale")
df_to_plot

ggplot(df_to_plot) +
    geom_sf(aes(fill = Cases, group = condition), alpha = 0.5, data = df_to_plot%>%filter(condition == "Measles")) +
    scale_fill_continuous(
    name = "Nr. of Measles",
    high="#54278f", 
    low="#d8c6ef", 
    na.value="white",
    #alpha = 0.5
    #breaks = c(1,2),
    #limits = c(1, 3)
    )+
     ggnewscale::new_scale_fill() +
    geom_sf(aes(fill = Cases, group = condition), alpha = 0.1, data = df_to_plot%>%filter(condition == "Rubella")) +
        scale_fill_continuous(
    name = "Nr. of Rubella",
    high="darkred", 
    low="coral", 
    na.value="white",
    #alpha = 0.5
    #breaks = c(1,2),
    #limits = c(1, 3)
    )

# Try gganimate 
library(gganimate)
#df_to_plot$year<-df_to_plot$year%>%as.character() %>%as.numeric()
#df_to_plot$epiweek <- df_to_plot$epiweek%>%as.character() %>%as.numeric()

df_to_plot%>%filter( epiweek %in% seq(as_date("2024-01-01"),as_date( "2024-08-01"), by= "day"))%>%
ggplot() +
    #geom_sf( data = shape_districts, linewidth = 0.1, aes(group = epiweek) ) +
    geom_sf(aes(fill = Cases, group = epiweek), alpha = 1, data = df_to_plot%>%filter(condition == "Measles")) +
    scale_fill_continuous(
    name = "Nr. of Measles",
    high="#54278f", 
    low="#d8c6ef", 
    na.value="white",
    #alpha = 0.5
    #breaks = c(1,2),
    #limits = c(1, 3)
    )-> 
base_map

df_to_plot%>%ungroup() %>%arrange(desc(Cases))
df_to_plot$Cases %>%sum()

base_map

base_map+
facet_wrap(~epiweek)

map_with_animation<- base_map +
  transition_time(epiweek) +
  ggtitle('Year: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}')

#num_years <- max(df_to_plot$year) - min(df_to_plot$year) + 1

df_to_plot$epiweek%>%unique%>%length

animate(map_with_animation, nframes = 270)->animation

last_animation()
anim_save("measles_animation.gif", animation = last_animation(), path = "animations")


# FACET plot 
ggplot(
  data = df_to_plot) +
  geom_sf( data = shape_districts, linewidth = 0.1 ) +
  geom_sf( aes(fill = Cases), 
  na.rm = TRUE) + 
scale_fill_continuous(
  name = "Nr. of cases",
  high="#54278f", 
  low="#d8c6ef", 
  na.value="white",
  #breaks = c(1,2),
  #limits = c(1, 3)
  ) +    # change color gradient
  labs(
    title = "Number of confirmed mpox cases in South Africa, by district",
    #caption = paste0("District not available for ", cases_district_22$counts[is.na(cases_district_22$NAME_2)], " cases")
    )+
  # facet the plots by year 
  facet_wrap(~year, ncol = 1) +
  theme_minimal()+
  theme( 
    font = "Centruy Gotchic",
    )-> 

maps_plot
