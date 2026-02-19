# Map 

# MAke a MAP? ----
pacman::p_load( tidyverse, data.table, magrittr, dplyr, janitor, stringdist)
library(ggplot2)
library(sf)
library(dplyr)
conflicts_prefer(ggplot2::geom_sf_label)
#install.packages("rgdal")
#library(rgdal)


# Read the subdistrict shapefile and filter for Free State province

#subdist_shape <- st_read("~/Desktop/SAFETP/CLA/2.2 HDR (Mortality surveillance)/HDR_Covid_RiskFactors/shapes/zaf_admbnda_adm_sadb_ocha_20201109.shp")

#save(subdist_shape, file = "subdist_shape.rda")

#load("subdist_shape.rda")

NMCleaner::shape_files$districts->subdist_shape

#st_read("/Users/YourUsername/Desktop/SAFETP/CLA/2.2 HDR (Mortality surveillance)/HDR_Covid_RiskFactors/shapes/zaf_admbnda_adm2_sadb_ocha_20201109.shp")


prov_shape<- subdist_shape %>% 
  group_by(province) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()


prov_shape

#fs_shape <- subdist_shape %>% filter(ADM1_EN == "Free State")

# Extract unique district names from your data frame
provinces <- df$prov_ %>% unique()

# Prepare the data for plotting

df_shape <- df %>% 
  group_by(province = prov_) %>%
  summarise(Cases = n()) %>% 
  mutate(province = factor(province, levels = provinces))%>%
  filter(!is.na(province))

df_shape$province
prov_shape$province
# Merge shape data with your summary data
shape_data <-fuzzyjoin::stringdist_inner_join(prov_shape, df_shape, by = "province", method = "jw", distance_col = "string_dist")%>%
  group_by(province.x) %>%filter( string_dist == min(string_dist))%>%
  mutate(text = paste0( province.y, "\nn=", ifelse(is.na(Cases), 0, Cases)))%>%
  rename(province = province.y)

shape_data$geometry
# Plot the map using ggplot2
map<- ggplot(data = shape_data) +
  geom_sf( aes(fill = Cases)) +
  geom_sf_label(aes(label = text), size = 3) + 
  scale_fill_gradient(low = "lightyellow", high = "darkolivegreen") +  # Customize fill color gradient
  labs(#title = #"Covid Cases in Free State Districts",       # Add a title
    fill = "Number of Cases",                           # Legend title for fill color
    #caption = "Source: Your Data Source"
  ) +             # Caption/source information
  theme(text = element_text(family = "Century Gothic"))+
  theme_classic()                       # Use a minimal theme for the plot


map

# can we make a districts map 

# map the districts 
 new_master%>%filter( condition %in% "Mpox") %>%
  group_by(district, year) %>%
  summarise( Cases = n())%>%
  mutate( 
    district = str_to_lower(district), 
    district = gsub( "city|metro|of", "", district), 
    district = trimws(district)
  ) -> 
  nmc_districts

nmc_districts$year

NMCleaner::shape_files$districts%>%
  mutate( 
    district = str_to_lower(district), 
    district = gsub( "city|metro|of", "", district), 
    district = trimws(district)
  ) %>%
    group_by(district) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()->
  
  shape_districts

shape_districts


fuzzyjoin::stringdist_left_join(shape_districts, nmc_districts, by = "district",
method = "jw", max_dist = 0.5 ,distance_col = "dist")%>%
  group_by(district.x, year) %>%
  filter( dist == min(dist))%>%
  ungroup()->
  df_to_plot


ggplot(data = df_to_plot) +
  geom_sf( aes(fill = Cases))

