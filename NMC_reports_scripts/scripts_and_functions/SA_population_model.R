#---- July report, exploratory analysis framework ----
# Combine all the library names into a vector
#rm(list = ls())
#gc()

libraries_to_load <- c(
  "jsonlite", "janitor", "magrittr", "geojsonR", "geojsonio", "leaflet",
  "rgdal", "rjson", "sf", "RColorBrewer", "tidyverse", "dplyr", "fuzzyjoin",
  "grates", "data.table", "lubridate", "gtsummary", "shiny", "splines", "officer", "flextable", "readxl"
)

# Load all the libraries in one function call
lapply(libraries_to_load, library, character.only = TRUE)



##----Population estimates----#


# Replace 'path/to/new_directory' with the desired path for your new directory
pop_directory <- "population_estimates"

#source("pop_estimates_statsSA.R")

#pop <- read_excel(paste0(pop_directory,"/pop.xlsx"), skip= 4) %>%
#  filter(!is.na(Name)) #this it like the toals column in excel

pop <- read_excel("population_estimates/District_projections_by_sex_and_age_2023_2027.xlsx", skip= 3) %>%
    filter(!is.na(Name)) #this it like the toals column in excel

#pop_projection <- read_excel(paste0(pop_directory,"/projection.xlsx"), skip= 3)%>%
#  filter(!is.na(Name)) #this it like the toals column in excel

pop_projection <- read_excel("population_estimates/District_Council_projection_by_sex_and_age_2002_2022.xlsx", skip= 3)%>%
  filter(!is.na(Name))

#Merge them
pop_merged <- merge(pop, pop_projection, by = c("Name", "Sex", "Age"))

#get it in a long format
pop_long <- pop_merged %>%
  pivot_longer(cols = -c(Name, Sex, Age),
               names_to = "Year",
               values_to = "Population")%>%
  mutate( Date = ymd(paste0( Year, "-07-01"))) #these are mid-year popestimtates so this is probs the best data to actually represent that.

pop_long

# id# Create a vector to store AIC values
aic_values <- numeric(0)

# Loop through different numbers of splines
#for (i in 1:20) {
#  formula <- reformulate(
#    response = "Population",
#    termlabels = c("Name", "Sex", "Age", "Name:Sex", "Name:Age", 
#                   "Name:Date", "Sex:Age", "Sex:Date", "Age:Date",
#                   paste0("ns(Date,", i, ")"))
#  )
#  
#  model <- lm(formula, data = pop_long %>%
#                filter(as_year(Date) %in%  2002:2027))
#  
#  aic_values[i] <- AIC(model)
#}

# Create a data frame for plotting
#aic_df <- data.frame(Number_of_Splines = 1:20, AIC = aic_values)

#save(aic_df, file = "population_estimates/aic_df.rda")
load("population_estimates/aic_df.rda")
# Plot AIC values
ggplot(aic_df, aes(x = Number_of_Splines, y = AIC)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Splines", y = "AIC") +
  ggtitle("AIC vs. Number of Splines")

# this shows an optimal number fo splines as 3. 

pop_model <- lm(Population ~ Name + Sex + ns(Date,3) + Age+ Name:Sex + Name:Age + Name:Date + Sex:Age + Sex:Date + Age:Date, 
                data = pop_long %>%
                  filter(as_year(Date) %in%  2002:2027))

summary(pop_model)

#get "in_between dates"


pop_expanded <- pop_long%>% 
  complete(Name, Sex, Age, Date = seq(min(Date), max(Date), by =  "month") )


#pop_expanded$population_model <- predict(pop_model, newdata = pop_expanded,# se.fit = TRUE
#                                       )

#save(pop_expanded, file = "population_estimates/pop_expanded.rda")

load("population_estimates/pop_expanded.rda")

pop_expanded$prov <- str_remove(pop_expanded$Name, " - .*|- .*")


##########################
group_vars_prov <- c("Date", "condition", "prov")
group_vars_prov_pop <- c("Date", "prov")
group_vars_age <- c("Date", "condition", "Age", "nmccategories")
group_vars_age_pop <- c("Date", "Age")

# so for the graph of prov, you need to group by prov, condition, Date
# create aggregated data for the modelled data by Month 

district_pop_model <- pop_expanded%>%
  select(-Year, -Population)%>%
  group_by(across(all_of(group_vars_prov_pop)))%>%
  summarise(pop= as.integer(sum(population_model, na.rm = T)))
##----
###----- plot modelled data ----
district_pop_model
district_pop_model %>%
  ggplot(aes(x = Date, y = pop, color = prov)) +
  geom_line(size = 1) +          # Line plot with color grouping
  #geom_smooth(se = FALSE) +     # Smoother line without standard error ribbon
  theme_classic()               # Optional: Apply a minimal theme

###---- not the modelled data ----
district_pop_not_model<- pop_long%>%
  
  mutate(prov  = str_remove(Name, " - .*|- .*"))%>%
  
  group_by(prov, Date )%>%
  
  summarise(pop= sum(Population, na.rm = T))

district_pop_not_model
names(district_pop_not_model)

district_pop_not_model%>%
  ggplot(aes(x = Date, y = pop, color = prov)) +
  geom_line(size = 1) +          # Line plot with color grouping
  #geom_smooth(se = FALSE) +     # Smoother line without standard error ribbon
  theme_minimal()               # Optional: Apply a minimal theme
##---- check the data ----
district_pop_model <- district_pop_model %>%
  mutate(DataType = "Modelled")

district_pop_not_model <- district_pop_not_model %>%
  mutate(DataType = "Not Modelled")

# Combine the two data frames
combined_data <- bind_rows(district_pop_model, district_pop_not_model)

# Plot the combined data with different line styles based on the "DataType" column
ggplot(combined_data, aes(x = Date, y = pop, color = prov, linetype = DataType)) +
  geom_line(size = 1) +
  theme_minimal()

##---- Aggregate NMC data ----

#names(SA_shape_data)
#ggplot() +
#        geom_sf(data = SA_shape_data, aes(fill = Cases)) +
#    geom_text_repel(data = SA_shape_data, aes(label = ADM1_EN), 
#        fontface = "bold", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
#            -0.25, 0.5, 0.5, -0.5)) #%>%

#   geom_sf_label(data = SA_shape_data%>%filter(!is.na(Cases)), aes(label = ADM1_EN))+
#  scale_fill_distiller(palette = "YlOrRd")
#scale_fill_viridis_c() 

