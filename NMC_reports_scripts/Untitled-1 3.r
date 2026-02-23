

poison %>%
    mutate(year = as.numeric(as.character(year))) %>%
    filter(year %in% 2020:2024) %>%
    transmute(
        Age = agecategory_unit,
        Sex = patient_gender,
        Year = year
    ) %>%
    filter(Age %in% age_levels, Sex %in% c("Male", "Female")) %>%
    count(Age, Sex, Year, name = "cases") %>%
    complete(Age = age_levels, Sex = c("Male", "Female"), Year = Year, fill = list(cases = 0)) %>%
    mutate(
        Age = factor(Age, levels = age_levels),
        Sex = factor(Sex, levels = c("Male", "Female")),
        Year = as.character(Year)
    ) ->
cases_by_age_sex



rates_age_sex_by_year_with_CI_wide
rates_age_sex_with_CI_wide


system("mkdir poisoning_incidence_tables_for_MeganP")

openxlsx::write.xlsx(
    rates_age_sex_by_year_with_CI_wide, 
    file = "poisoning_incidence_tables_for_MeganP/ rates_age_sex_by_year_with_CI_wide.xlsx"    
)

openxlsx::write.xlsx(
    rates_age_sex_with_CI_wide,
    file = "poisoning_incidence_tables_for_MeganP/ rates_age_sex_with_CI_wide.xlsx"
)



load("pih_cleaned.rda")


library(dplyr)
library(tidyr)
library(janitor)
library(stringr)

# put in population esimtates to PIH data
pih_cleaned%>% glimpse()
pih_cleaned$patient_age%>%tabyl()
# may need to do  restructuring of data to account for a more general age category, 0-1, 1-4, 5-19, 20-59, 60+ ?
pih_age_categories <- c( "0-1", "1-4", "5-12", "13-19", "20-59", "60+")
pih_cleaned$sex %>%tabyl()

pih_cleaned%>%
filter( 
    year %in% 2020:2024
)%>%
    mutate( 
        age_category = 
               factor( 
        case_when( 
            patient_age == "0" | patient_age == "<1 yr" ~ "0-1",
            patient_age %in% c("1-<2 yrs", "2-<3 yrs", "3-<4 yrs") ~ "1-4",
            patient_age %in% c("5-<6 yrs", "6-<12 yrs", "15-<20 yrs") ~ "5-12",
            patient_age %in% c("13-<19 yrs") ~ "13-19",
            patient_age %in% c("Adult", "20-59 yrs") ~ "20-59",
            patient_age %in% c("≥60 yrs") ~ "60+"
        ), 
        levels = c(pih_age_categories )
       ), 
       sex = if_else( sex == "M", "Male", "Female")
    )%>%
    na.omit() %>%
    group_by( 
        age_category, 
        sex,
        year
    )%>%
    reframe( 
        n = n() 
    )-> 
    pih_cases_by_age_year

    pih_cases_by_age_year%>%names()

# need to come up with groupings for the 5-12, and 13-19 age groups. 
pih_cases_by_age_year


pih_cases_by_age_year%>%
filter( Year %in% 2020)%>%
print( n = 100)

# =====================
# Get in population data from 
# =====================

#source( Ager_standardise_Rates.r)


pop_by_age_sex_year -> data



pop_by_age_sex_year$Age_group%>%tabyl()

pop_by_age_sex_year%>%
    ungroup() %>%
    mutate(
        Age_group = case_when(
            Age %in% c("0-1") ~ "0-1",
            Age %in% c("1-4") ~ "1-4",
            Age %in% c("5-9", "10-14") ~ "5-12", # merge 5-9 & part of 10-14
            Age %in% c("15-19") ~ "13-19",
            Age %in% c(
                "20-24", "25-29", "30-34",
                "35-39", "40-44", "45-49",
                "50-54", "55-59"
            ) ~ "20-59",
            Age %in% c("60-64", "65+") ~ "60+",
            TRUE ~ NA_character_
        )
    ) %>%
    group_by(Year, Sex, Age_group) %>%
    summarise(
        pop = sum(pop, na.rm = TRUE),
        weight = sum(weight, na.rm = TRUE), # if weights are proportional by age
        weight_pct = sum(weight_pct, na.rm = TRUE), # check: recompute if not additive
        pop_standard = sum(pop_standard, na.rm = TRUE),
        .groups = "drop"
    )-> pop_by_age_sex_year

df_collapsed

data
# proportional split of 10-14 between 5-12 (3 years) and 13-19 (2 years)
df_split <- data %>%
    mutate(
        Age_group = case_when(
            Age %in% c("0-1") ~ "0-1",
            Age %in% c("1-4") ~ "1-4",
            Age %in% c("5-9") ~ "5-12",
            Age %in% c("15-19") ~ "13-19",
            Age %in% c(
                "20-24", "25-29", "30-34",
                "35-39", "40-44", "45-49",
                "50-54", "55-59"
            ) ~ "20-59",
            Age %in% c("60-64", "65+") ~ "60+",
            TRUE ~ NA_character_
        ),
        split_factor = case_when(
            Age == "10-14" & Age_group == "5-12" ~ 3 / 5,
            Age == "10-14" & Age_group == "13-19" ~ 2 / 5,
            TRUE ~ 1
        )
    ) %>%
    # create two rows out of each 10-14 row
    tidyr::uncount(2, .id = "split_id") %>%
    mutate(
        Age_group = if_else(split_id == 1 & Age == "10-14", "5-12",
            if_else(split_id == 2 & Age == "10-14", "13-19", Age_group)
        ),
        pop = pop * split_factor,
        weight = weight * split_factor,
        weight_pct = weight_pct * split_factor,
        pop_standard = pop_standard * split_factor
    ) %>%
    group_by(Year, Sex, Age_group) %>%
    summarise(across(c(pop, weight, weight_pct, pop_standard), sum, na.rm = TRUE),
        .groups = "drop"
    )


df_collapsed <- df_collapsed %>%
    group_by(Year, Sex) %>%
    mutate(
        weight_pct = pop / sum(pop) * 100,
        weight = pop / sum(pop)
    ) %>%
    ungroup()

df_collapsed
    