# Age stanrdisation of poisoning cases from NMC 
library(NMCleaner)
load("poison.rda")
load("pih_cleaned.rda")


library(NMCleaner)
conflicted::conflicts_prefer(dplyr::filter)

# set printers
gtsummary::theme_gtsummary_compact()

theme_gtsummary_journal(
    journal = c("jama"),
    set_theme = TRUE
)

theme_gtsummary_printer(
    print_engine = c("flextable"),
    set_theme = TRUE
)


library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
pih_cleaned%>% glimpse()
pih_cleaned$patient_age%>%tabyl()
# may need to do  restructuring of data to account for a more general age category, 0-1, 1-4, 5-19, 20-59, 60+ ?
pih_cleaned%>%
    mutate( 
        age_category = 
        case_when( 
            patient_age == "0" | patient_age == "<1 yr" ~ "0-1",
            patient_age %in% c("1-<2 yrs", "2-<3 yrs", "3-<4 yrs") ~ "1-4",
            patient_age %in% c("5-<6 yrs", "6-<15 yrs", "15-<20 yrs") ~ "5-19",

        )
    )

# the age categories of the data make it dificult to standardise to the WHO standard population, we would need to make assumptions on 
# the shape of the 10-14 year age group in order to make a 5-9 age group and 10-14 age group

age_levels <- c(
    "0-1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+"
)

by <- "Age"
vars_pop <- c("Sex", as.character(by))

# old one, not fully implemented into the final dataset yet though 
live_births_male <- 453165
live_births_female <- 446138
live_births <- sum(live_births_male, live_births_female)

deaths_under_1_male <- 15023
deaths_under_1_female <- 13539
deaths_under_1_unspecificed <- 353

deaths_under_1 <- sum(
    deaths_under_1_female,
    deaths_under_1_male,
    deaths_under_1_unspecificed
)

df_0_1 <-
    tibble(
        Age = rep("0-1", 2),
        Sex = c("Male", "Female"),
        n = c(
            (live_births_male - deaths_under_1_male),
            (live_births_female - deaths_under_1_female)
        )
    )

df_0_1

# =============================================
# Create a proxy for 0-1 population from 2020-2024
# =============================================

library(tibble)
library(dplyr)

# --- Official totals from Stats SA MYPE 2025, Table 4 (flows are 1 Jul–30 Jun) ---
births_mype_2024 <- tribble(
    ~year, ~births_total,
    2020, 1150914,
    2021, 1144526,
    2022, 1131070,
    2023, 1119345,
    2024, 1115478
)

# --- Choose the sex ratio at birth (SRB = males per 1 female).
# From Stats SA Recorded Live Births 2021: ~102 male per 100 female => SRB ≈ 1.02
srb <- 1.02

births_mype_2024_sex <- births_mype_2024 %>%
    mutate(
        births_male_est   = round(births_total * (srb / (1 + srb))),
        births_female_est = births_total - births_male_est,
        srb_used          = srb,
        sex_split_source  = "Estimated using SRB=1.02 from Stats SA Recorded Live Births 2021"
    )
births_mype_2024_sex




births_mype_2024_sex%>%
pivot_longer(
    cols = starts_with("births_"),
    names_to = "sex",
    values_to = "births"
) %>%
    mutate(
        sex = recode(sex,
            "births_male_est"   = "Male",
            "births_female_est" = "Female"
        )
    )%>%filter( sex != "births_total")%>%
    mutate( 
        Age = "0-1",
    )%>%
    select( 
        year, Age, sex, births
    )%>%
    mutate( 
        NMR = 10, # neontala mortality rate 10 per 1000 live births based on 
        # https://fred.stlouisfed.org/series/SPDYNIMRTINZAF?utm_source=chatgpt.com 
        # http://childrencount.uct.ac.za/indicator.php?domain=5&indicator=23
        IMR =  as.numeric(case_when( 
            year == 2020 ~ "24.5", 
            year == 2021 ~ "24.5", 
            year == 2022 ~ "24.5",
            year == 2023 ~ "24.5",
            year == 2024 ~ "23.2",
    ))
    )%>%
    
    mutate( 
        deaths = round((IMR/1000)*births), 
        # or 
        #deaths = round((NMR/1000)*births + ((IMR - NMR)/1000)*births),
        pop = births - deaths
    )%>%
    rename(
        Year = year, 
        Sex = sex, 
        n = pop
    )%>%
    mutate( Year = as.character(Year))%>%
    select( 
        Year, Age, Sex, n
    )-> df_0_1_all_years






# Build 2020 pop with 0–1 + 1–4 split

    NMCleaner::pop_old_from_pepfar %>%
    mutate(
        Age = if_else(!Age %in% c(
            "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
            "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+"
        ),
        "65+", Age
        )
    ) %>%
    filter(Year %in% 2020:2024) %>%
    group_by(Age, Sex, Year) %>%
    summarise(pop = sum(Population), .groups = "drop") %>%
    # add <1 (0–1) using proxy, and derive 1–4 = (0–4) - (0–1)
    left_join(
        df_0_1_all_years %>% rename(pop_0_1 = n),
        by =   c( vars_pop, "Year")
    ) %>%
    bind_rows(
        # append explicit 0–1 rows
        df_0_1_all_years %>% transmute(Age, Sex, Year, pop = n)
    ) %>%
    group_by(Sex) %>%
    # adjust the 0–4 band down to 1–4 by subtracting 0–1
    mutate(
        pop = case_when(
            # NOTE: The package sent to Megan Prinsloo on 03 OCtober 2025 was with the df_0_1 dataset implemented here
                                        #Age == "0-4" & Sex == "Male" ~ pop - df_0_1$n[df_0_1$Sex == "Male"],
                                        #Age == "0-4" & Sex == "Female" ~ pop - df_0_1_df_0_1all_years$n[df_0_1$Sex == "Female"],
                                        #TRUE ~ pop
            Age == "0-4" & Sex == "Male" ~ pop - df_0_1_all_years$n[df_0_1_all_years$Sex == "Male"],
            Age == "0-4" & Sex == "Female" ~ pop - df_0_1_all_years$n[df_0_1_all_years$Sex == "Female"],
            TRUE ~ pop
        ),
        Age = if_else(Age == "0-4", "1-4", Age)
    ) %>%
    ungroup() %>%
    # keep required bands and tidy
    filter(
        Age %in% c(
            "0-1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
            "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+"
        ),
        Sex %in% c("Male", "Female")
    ) %>%
    group_by(Age, Sex, Year) %>%
    summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop") %>%
    # complete and set signs for pyramid if you need it later
    complete(Age, Sex, fill = list(pop = 0)) %>%
    mutate(
        Sex  = factor(Sex, levels = c("Male", "Female")),
        Age  = factor(Age, levels = age_levels)
    )-> 
    pop_by_age_sex_year


pop_by_age_sex_year
pop_by_age_sex_year%>%filter( Age == "0-1")
pop_by_age_sex_year %>% tabyl(Age, Sex)


pop_by_age_sex_year%>%
    group_by( 
        Year, Sex
    )%>%
    reframe( 
        pop = sum(pop)
    )


#-------------------------------------------------
# 1.  Implement the WHO 2000-2025 standard weights  (from Ahmad et al. WHO 2001)
#-------------------------------------------------
who_std <- tribble(
    ~Age,     ~weight,
    "0-4",    0.0886,
    "5-9",    0.0869,
    "10-14",  0.0860,
    "15-19",  0.0847,
    "20-24",  0.0822,
    "25-29",  0.0793,
    "30-34",  0.0761,
    "35-39",  0.0715,
    "40-44",  0.0659,
    "45-49",  0.0604,
    "50-54",  0.0537,
    "55-59",  0.0455,
    "60-64",  0.0372,
    "65-69",  0.0296,
    "70-74",  0.0221,
    "75-79",  0.0152,
    "80-84",  0.0091,
    "85+",    0.0063
)


#-------------------------------------------------
# 2.  Include the study specific 0-1 age groups 
#-------------------------------------------------
split0_1 <- 1 / 5 # assume 2 years in 0-1 vs 3 years in 1-4

# compute the original 0-4 weight and aggregate 65+ weights
weight_0_4    <- who_std %>% filter(Age == "0-4") %>% pull(weight)
weight_65plus <- who_std %>% filter(Age %in% c("65-69","70-74","75-79","80-84","85+")) %>% summarise(w = sum(weight)) %>% pull(w)

who_std <- who_std %>%
    # drop the original 0-4 and the detailed 65+ bands
    filter(!Age %in% c("0-4", "65-69", "70-74", "75-79", "80-84", "85+")) %>%
    # add split 0-1 / 1-4 and aggregated 65+
    bind_rows(
        tibble(Age = "0-1", weight = weight_0_4 * split0_1),
        tibble(Age = "1-4", weight = weight_0_4 * (1 - split0_1)),
        tibble(Age = "65+",  weight = weight_65plus)
    ) %>%
    arrange(factor(Age, levels = c(
        "0-1","1-4","5-9","10-14","15-19","20-24","25-29",
        "30-34","35-39","40-44","45-49","50-54","55-59",
        "60-64","65+"
    ))) %>%
    mutate(weight_pct = round(weight * 100, 2))

who_std
pop_by_age_sex_year

# just as a sensitivity analysis, look at the weights of the actual SA pop
pop_by_age_sex_year %>%
    filter(Year == "2024") %>%
    group_by(Age) %>%
    summarise(
        pop = sum(pop)
    ) %>%
    mutate(
        weight_sa = pop / sum(pop)
    ) %>%
    left_join(
        .,
        who_std,
        by = "Age"
    )


pop_by_age_sex_year%>%
    group_by( Year) %>%
    mutate( 
        pop_total_year = sum(pop)
    )-> total_pop_by_year

    total_pop_by_year


left_join( 
    pop_by_age_sex_year, 
    who_std, 
    by = "Age"
)%>%
group_by(Year, Sex)%>%
mutate(
    pop_standard = sum(pop) * weight/2
)-> pop_by_age_sex_year_previous
pop_by_age_sex_year_previous

pop_by_age_sex_year_previous

# Make a more verbose version for Megan prinsloo

left_join(
    pop_by_age_sex_year,
    who_std,
    by = "Age"
) %>%
    group_by(Year, Sex) %>%
    mutate(
        pop = sum(pop) , 
        weight = weight, 
        #gender_weight = weight/2, 
        pop_standard = pop * weight
    ) -> pop_by_age_sex_year_for_Megan

pop_by_age_sex_year_for_Megan

pop_by_age_sex_year%>%
    dplyr::filter( Year == "2024")%>%
mutate( 
    pop = sum(pop),
)

left_join(
    pop_by_age_sex_year,
    who_std,
    by = "Age"
) %>%
    rename(
        actual_pop = pop
    )%>%
group_by(Year) %>%
mutate( actual_pop_total_year = sum(actual_pop))%>%
    group_by(Year, Sex) %>%
    mutate(
        sex_pop = sum(actual_pop),
        #weight = weight,
        # gender_weight = weight/2,
        #pop_standard = sex_pop * weight,
        pop_standard_1 = actual_pop_total_year * weight / 2 # this one is correct, it taks the total population for the year, multiplies the weight divided by two for equal sex
    ) -> pop_by_age_sex_year_for_Megan

pop_by_age_sex_year_for_Megan

openxlsx::write.xlsx(
    pop_by_age_sex_year_for_Megan,
    file = "outputs/population_by_age_sex_year_with_standard_weights.xlsx",
) 


left_join(
    pop_by_age_sex_year,
    who_std,
    by = "Age"
) %>%
    group_by(Year) %>%
    mutate(
        pop_standard = sum(pop) * weight / 2
    ) -> pop_by_age_sex_year_final
pop_by_age_sex_year_final

pop_by_age_sex_year_final -> pop_by_age_sex_year

# compare the final vs a previous on with an error:
pop_by_age_sex_year_previous

load("/Users/briday/Desktop/SAFETP/CLA/NMC_MAC_poison/outputs/che_final.rda")

xtabs( ~
age_tested_years+ 
agecategory_unit, 
data = che_final)

che_final%>%glimpse() 
che_final %>%rename( 
    patient_gender = gender
)

deparse( substitute(poison)) -> data_set_name
data_set_name
data_set_name<- "poison"
# to be used in titles of plots 
data_set_label <- ifelse( data_set_name %in% "poison", "ASRP NMC notifications", ifelse( data_set_name %in% "che_final", "BChE NHLS unique tests", data_set_name))

poison %>%names() -> data_names
data_names[which( data_names %in% c( "gender", "sex", "patient_gender"))] -> gender_vars
gender_vars[1]
poison$gender
che_final$gender
poison %>%
    {
        deparse(substitute(. ))-> dataset_name 
      if (dataset_name == "che_final") dplyr::rename(., patient_gender = gender) else .
    }%>%

mutate(year = as.numeric(as.character(year))) %>%
    filter(year %in% 2020:2024) %>%
    transmute(
        Age = agecategory_unit,
        Sex = patient_gender,
        Year = year
    )%>%
    filter(Age %in% age_levels, Sex %in% c("Male", "Female")) %>%
        count(Age, Sex, Year, name = "cases") %>%
        complete(Age = age_levels, Sex = c("Male", "Female"), Year = Year, fill = list(cases = 0)) %>%
        mutate(
            Age = factor(Age, levels = age_levels),
            Sex = factor(Sex, levels = c("Male", "Female")),
            Year = as.character(Year)
        ) ->cases_by_age_sex_dummy
cases_by_age_sex_dummy
# Create dummy ASR 

pop_by_age_sex_year %>%
    left_join(
        ., 
        cases_by_age_sex_dummy, by = c("Age", "Sex", "Year")
    ) -> cases_and_pop



cases_and_pop %>%
    group_by(Year, Sex) %>%
    mutate(
        rate_per100k  = cases / pop * 1e5, # age-specific rate
        w_norm        = weight / sum(weight, na.rm = TRUE), # weights renormalised within Year×Sex
        asr_component = w_norm * rate_per100k, # weighted contribution
        ASR_per100k   = sum(asr_component, na.rm = TRUE) # ASR for this Year×Sex
    ) %>%
    ungroup() %>%
    select(Year, Sex, Age, pop, cases, rate_per100k, w_norm, asr_component, ASR_per100k)


library(dplyr)

# 0) (Optional) Ensure weight is a proportion, not percent
# df <- df %>% mutate(weight = ifelse(weight > 1, weight/100, weight))

# 1) Age-specific rates (per 100,000)
df_rates <- cases_and_pop %>%
    mutate(
        rate        = cases / pop, # per person
        rate_per100k = rate * 1e5 # per 100,000
    )
df_rates
# 2) Re-normalise weights within each Year x Sex over the age bands present
w_sum_year_sex <- df_rates %>%
    distinct(Year, Sex, Age, weight) %>%
    group_by(Year, Sex) %>%
    summarise(w_sum = sum(weight, na.rm = TRUE), .groups = "drop")

df_rates_ys <- df_rates %>%
    left_join(w_sum_year_sex, by = c("Year", "Sex")) %>%
    mutate(w_norm_ys = weight / w_sum)

df_rates_ys

# 3) ASR per Year x Sex: sum_i (w_i * r_i) * 100,000
asr_year_sex <- df_rates_ys %>%
    group_by(Year, Sex) %>%
    summarise(
        ASR_per100k = sum(w_norm_ys * rate, na.rm = TRUE) * 1e5,
        .groups = "drop"
    )
asr_year_sex
# 4) ASR per Year (both sexes combined)
#    First pool sexes within each age via population-weighted rate,
#    then apply weights re-normalised within Year.
df_both <- cases_and_pop %>%
    group_by(Year, Age) %>%
    summarise(
        pop = sum(pop, na.rm = TRUE),
        cases = sum(cases, na.rm = TRUE),
        weight = dplyr::first(weight),
        .groups = "drop"
    ) %>%
    mutate(
        rate_per100k = (cases / pop) * 1e5,
        rate = cases / pop
    )

w_sum_year <- df_both %>%
    distinct(Year, Age, weight) %>%
    group_by(Year) %>%
    summarise(w_sum = sum(weight, na.rm = TRUE), .groups = "drop")

df_both_w <- df_both %>%
    left_join(w_sum_year, by = "Year") %>%
    mutate(w_norm_y = weight / w_sum)

asr_year_total <- df_both_w %>%
    group_by(Year) %>%
    summarise(
        ASR_per100k = sum(w_norm_y * rate, na.rm = TRUE) * 1e5,
        .groups = "drop"
    )

# --- Optional: nice exports for reporting ---
age_specific_rates <- df_rates %>%
    select(Year, Sex, Age, pop, cases, rate_per100k) %>%
    arrange(Year, Sex, Age)

asr_year_sex
asr_year_total
age_specific_rates

10 / 2825741.* 100000






che_final %>%setDT() 

che_bin <- che_final[case_bin==1]
che_bin
# ====================================================================================================================================
# ====================================================================================================================================


ASR_table_and_plot<- function(data) {
    #data<- che_bin
    # data_set_name <- deparse(substitute(che_bin))
    #poison %>% 
    #che_final %>% rename( patient_gender = gender)%>%
     data_set_name <- deparse(substitute(data))
     data_set_label <- ifelse(data_set_name %in% "poison", "ASRP NMC notifications", ifelse(grepl("che", data_set_name), "BChE NHLS unique tests", data_set_name))

    message("Processing dataset: ", data_set_name, "with label ", data_set_label)
    data %>%
        {
            if (grepl("che", data_set_name)) dplyr::rename(., patient_gender = gender) else .
        }%>%
    mutate( year = as.numeric(as.character( year)))%>%
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
    )-> 
cases_by_age_sex

cases_by_age_sex

# create total cases by sex
cases_by_age_sex%>%
    group_by(
         Year, Sex
    )%>%
    summarise( 
        cases = sum(cases) 
    ) %>%
    mutate( 
        Age = "Total"
    )-> 
total_cases

cases_by_age_sex%>%
bind_rows( ., 
    total_cases)-> cases_by_age_sex_with_total

    cases_by_age_sex_with_total%>%filter( Age == "Total")


# Since you have 5 years data you need to create the total cases for the "total" population. (summing the population )
pop_by_age_sex_year

pop_by_age_sex_year%>%
group_by(
    Year, Sex
) %>%
    summarise(
        pop = sum(pop), 
        pop_standard = sum(pop_standard)
    ) %>%
    mutate(
        Age = "Total", 
    ) %>%
    mutate( weight = 1, 
    weight_pct = 100) ->
pop_by_age_sex_year_total

pop_by_age_sex_year_total

pop_by_age_sex_year_total%>%
bind_rows(
    ., pop_by_age_sex_year
) -> pop_by_age_sex_year 

pop_by_age_sex_year%>%print(n = 100)
    
    cases_by_age_sex_with_total%>%
    #cases_by_age_sex %>%

    left_join(., pop_by_age_sex_year, by = c("Age", "Sex", "Year")) %>%
       mutate(
             rate_per_100k = if_else(pop > 0, (cases / pop) * 1e5, NA_real_),
             rate_per_100k_standard = if_else(pop > 0, (cases / pop_standard) *weight*  1e5, NA_real_)
         ) %>%
         na.omit() ->
     rates_age_sex_by_year

     rates_age_sex_by_year%>%filter( Sex == "Female", Age == "10-14")

    rates_age_sex_by_year %>% filter(Age == "Total")

    pop_by_age_sex_year%>%filter( Age == "Total")

    rates_age_sex_by_year%>%
        group_by( 
            Sex, Age, 
        )%>%
        reframe( 
            cases = sum(cases), 
            pop = sum(pop), 
            pop_standard = sum( pop_standard), 
            weight = mean(weight)
        )%>%
        mutate(
            rate_per_100k = if_else(pop > 0, (cases / pop) * 1e5, NA_real_), 
            rate_per_100k_standard = if_else(pop > 0, (cases / pop) * weight *  1e5, NA_real_)
        )%>%na.omit() ->
        rates_age_sex

        rates_age_sex%>%filter( Age == "Total")

    cases_by_age_sex %>%
        bind_rows( ., 
                    total_cases)%>%
        left_join(pop_by_age_sex_year, by = c("Age", "Sex", "Year")) %>%
        filter( Age == "Total")%>%
        mutate(
            rate_per_100k = if_else(pop > 0, (cases / pop) * 1e5, NA_real_),
            rate_per_100k_standard = if_else(pop > 0, (cases / pop) * weight * 1e5, NA_real_)
        ) ->
    rates_age_sex_total

    rates_age_sex_total

# rates age and sex total 


rates_age_sex%>%filter( Sex == "Female")
rates_age_sex%>%filter( Age == "Total")

fmt_ci <- function(lo, hi, digits = 3) {
  ifelse(is.na(lo) | is.na(hi), NA_character_,
         paste0(formatC(lo, format = "f", digits = digits),
                "-",
                formatC(hi, format = "f", digits = digits)))
}

# tabulate this with age as rows and gender as columns in a flextable 

create_and_unnest_ci <- function( data){

    data %>% 
    rowwise() %>%
        mutate(
            ci = list(epitools::pois.exact(cases, pt = pop, conf.level = 0.95)),
        ) %>%
        ungroup() %>%
        unnest_wider(ci) %>%
        rename(
            ci_lower = lower,
            ci_upper = upper,
        ) %>%
        select(
            -c(x, pt, rate, conf.level)
        ) %>%
        rowwise() %>%
        mutate(
            ci_standard = list(epitools::pois.exact(cases, pt = pop_standard, conf.level = 0.95))
        ) %>%
        unnest_wider(ci_standard) %>%
        rename(
            ci_lower_standard = lower,
            ci_upper_standard = upper,
        ) %>%
        mutate(
            across(contains("ci_"), ~ . * 1e5)
        ) %>%
        select(
            -c(x, pt, rate, conf.level)
        ) ->
    rates_age_sex_with_CI_wide

 data %>%
  rowwise() %>%
        mutate(
            # Jeffreys prior: shape = 0.5, rate = 0 (or small → we treat as rate = 0 meaning shape + exposure in denominator)
            alpha0 = 0.5,
            beta0 = 0, # or treat as rate parameter so posterior rate = beta0 + pop

            # posterior parameters
            alpha_post = alpha0 + cases,
            beta_post = beta0 + pop, # exposure/pop acts like rate parameter in Gamma posterior

            # compute credible interval for lambda_i (per person)
            lambda_lower = qgamma(0.025, shape = alpha_post, rate = beta_post),
            lambda_upper = qgamma(0.975, shape = alpha_post, rate = beta_post),
            lambda_mean = alpha_post / beta_post, # posterior mean rate per person

            # transform to per 100k
            rate_per100k_bayes = lambda_mean * 1e5,
            ci_lower = lambda_lower * 1e5,
            ci_upper = lambda_upper * 1e5
        ) %>%
        ungroup() %>%
     select(-c(alpha0:lambda_mean)) ->
  rates_age_sex_with_CI_wide


data %>%
    rowwise() %>%
    mutate(
        # Poisson 95% CI for the *rate per person-year*
        ci = list(epitools::pois.exact(cases, pt = pop, conf.level = 0.95))
    ) %>%
    ungroup() %>%
    unnest_wider(ci) %>%
    rename(rate = rate, ci_lower = lower, ci_upper = upper) %>%
    mutate(
        # Convert to per 100k
        rate_per_100k      = rate * 1e5,
        ci_lower_per_100k  = ci_lower * 1e5,
        ci_upper_per_100k  = ci_upper * 1e5,

        # Weighted contribution to ASR (per 100k)
        asr_contrib        = weight * rate_per_100k,
        ci_lower          = weight * ci_lower_per_100k,
        ci_upper          = weight * ci_upper_per_100k
    )%>%
    select(-c(x, pt, conf.level, rate, ci_lower_per_100k, ci_upper_per_100k, asr_contrib)) -> rates_age_sex_with_CI_wide

rates_age_sex_with_CI_wide

    return(rates_age_sex_with_CI_wide)

}

rates_age_sex%>%
    rename(
        asr = rate_per_100k_standard
    ) %>%
    rowwise() %>%
  mutate(
    # Poisson 95% CI for the *rate per person-year*
    ci = list(epitools::pois.exact(cases, pt = pop, conf.level = 0.95))
  ) %>%
  ungroup() %>%
  unnest_wider(ci) %>%
  rename(rate = rate, ci_lower = lower, ci_upper = upper) %>%
  mutate(
    # Convert to per 100k
    rate_per_100k      = rate * 1e5,
    ci_lower_per_100k  = ci_lower * 1e5,
    ci_upper_per_100k  = ci_upper * 1e5,

    # Weighted contribution to ASR (per 100k)
    asr_contrib        = weight * rate_per_100k,
    asr_lower          = weight * ci_lower_per_100k,
    asr_upper          = weight * ci_upper_per_100k
  )
rates_age_sex_by_year

create_and_unnest_ci(rates_age_sex_by_year) -> rates_age_sex_by_year_with_CI_wide
create_and_unnest_ci(rates_age_sex) -> rates_age_sex_with_CI_wide

rates_age_sex_by_year_with_CI_wide
rates_age_sex_with_CI_wide

rates_age_sex_by_year_with_CI_wide-> full_dataset_with_year
rates_age_sex_with_CI_wide-> full_dataset_final

rates_age_sex_with_CI_wide%>%filter( Sex == "Female", Age == "15-19")
rates_age_sex_with_CI_wide %>% filter(Sex == "Female", Age == "Total")

rates_age_sex_with_CI_wide

rates_age_sex_with_CI_wide%>%
    rowwise() %>%
    pivot_wider(
        names_from = Sex,
        values_from = c(cases, pop, rate_per_100k, rate_per_100k_standard,
                        ci_lower, ci_upper)
    ) %>%
# glimpse()
    mutate(
    ci_Female = fmt_ci(ci_lower_Female, ci_upper_Female, digits = 3),
    ci_Male   = fmt_ci(ci_lower_Male, ci_upper_Male, digits = 3),
    )%>%
  rowwise( )%>%
  mutate(
    pt = list(poisson.test(
          x = c(cases_Female, cases_Male), # counts
          T = c(pop_Female, pop_Male), # person-years
          alternative = "two.sided",
          conf.level = 0.95
      )), 
      p_value = style_pvalue( pt$p.value)
  )%>%
  mutate(
        across(
        c(pop_Female, pop_Male, , cases_Female, cases_Male ), ~ .x/5
    ), 
    across(
        contains( "rate_"),  ~ round(as.numeric(.x), 2)
    ),
    across( 
         contains( "pop"), ~ round(as.numeric(.x), 0)
    ),
    weight = round( weight*100, 2)
  ) %>%
  mutate( 
    Age = factor( Age, levels = c( c(age_levels, "Total")))
  )%>%
  arrange( Age) %>%
  select(
    Age,
    weight, 
    pop_Female, 
    #    pop_standard_Female, # only need to show one stadnard population

    cases_Female, 
    rate_per_100k_Female, 
    rate_per_100k_standard_Female,
    ci_Female, 
    pop_Male,   
    #pop_standard_Male,
    cases_Male,  
     rate_per_100k_Male,   
    rate_per_100k_standard_Male,
    ci_Male, 
    rate_per_100k_standard_Male, 
    p_value
  )-> age_incidence_per_100k

age_incidence_per_100k


  # compare the incidence rates between sexes with a poisson test and report p-value 
  age_incidence_per_100k%>%
    rowwise( )%>%
    mutate(
        across(
            contains("cases"),  ~ round(as.numeric(.x) , 0 ) 
        )
    )%>%
  mutate(
      # two-sample Poisson test of equality of incidence rates (Female vs Male)
      pt = list(poisson.test(
          x = c(cases_Female, cases_Male), # counts
          T = c(pop_Female, pop_Male), # person-years
          alternative = "two.sided",
          conf.level = 0.95
      )), # fig-cap "p-values were obtained from a two-sample Poisson exact test comparing the age-specific incidence rate in females versus males for each age band."
      irr = unname(pt$estimate), # incidence rate ratio (Female / Male)
      irr_lower = pt$conf.int[1],
      irr_upper = pt$conf.int[2],
      p_value = style_pvalue(pt$p.value)
  ) %>%
      ungroup() %>%
      select(
          -pt
      )


# 2-row hierarchical header: top = Gender, bottom = Measure
header_map <- tribble(
    ~col_keys,               ~Gender,  ~Measure,
    "",                   "Age",    "Age",
    "Weight",                   "weight", "WHO Weight", 
    "pop_Female",            "Female", "Average \nPopulation ",
    "cases_Female",          "Female", "Average \ncases",
    "rate_per_100k_Female",  "Female", "Age Specific Rate",
    "ci_Female",             "Female", "95% CI",
    "rate_per_100k_standard_Female", "Female", "Age Standardised Rate",
    "ci_Female",    "Female", "95% CI",

    #"pop_Male",              "Male",   "Population Average",
        "pop_Male",            "Male", "Average \nPopulation ",
    "cases_Male",            "Male",   "Average \ncases",
    "rate_per_100k_Male",    "Male",  "Age Specific Rate",
    "ci_Male",               "Male",   "95% CI",
    "rate_per_100k_standard_Male", "Male", "Age Standardised Rate",
    "ci_Male",    "Male", "95% CI", 
    "p_value", "", "p-value"
)

flextable(age_incidence_per_100k) %>%
    set_header_df(mapping = header_map, key = "col_keys") %>%
    merge_h(part = "header") %>%
    # format numbers; CI is text so leave it
    colformat_num(
        j = c(
            "pop_Female", "pop_Male",
            #"rate_per_100k_Female", "rate_per_100k_Male", 
            #"ci_Female", "ci_Male",
            "rate_per_100k_standard_Female", 
            "rate_per_100k_standard_Male",
             "ci_Female", "ci_Male"
        ),
        digits = 1
    ) %>%
    align(j = c("cases_Female", "cases_Male"), align = "right", part = "body") %>%
    theme_vanilla() %>%
    # centre certain columns 
    flextable::align(
        part = c("all") , 
        align  = "center", 
        #i = 1, 
        j = c( 
            "cases_Female", 
            "rate_per_100k_Female",
            "rate_per_100k_standard_Female",
            "cases_Male", 
            "rate_per_100k_Male",
            "rate_per_100k_standard_Male"
        )
    )%>%
    autofit() %>%
    set_caption(
        paste0( "Age standardised rates by age and sex for ", data_set_label, " between 2020 to 2024, in South Africa "),
     ) %>%
    footnote(
        # add explanation of averaging the population 
        part = "header", 
        j = c( 
            "pop_Female", "pop_Male"
        ), 
        i = 1, 
        value = as_paragraph(
            #"Population from each MYPE reported by Stats SA was used to generate the incidence for each year, the average population for the time period is reported"
            "Population estimates are taken from Statistics SA, info on the 0-1 year age group can be found in the methodology. ", 
            "Average populations and average cases from the 5 year period (2020-2024) are presented for South Africa. Age-specific Rates and Age Standardised Rates are rates per 100 000 person years."
        ), 
        ref_symbols = "*"
    )%>%
   footnote(
       .,
       part = "header",
       j = c(
           "rate_per_100k_standard_Female", "rate_per_100k_standard_Male"
           #"ci_Female", "ci_Male"
       ),
       i = 2, # second header row (where "Measure" labels live)
       value = as_paragraph(
           "Age Standardised Rates (ASR) are calculated by multiplying the weight (as a proportion) from the WHO world standard population for each age category by the age-specific rate for that age category."
          # "95% CIs via Dobson/Poisson approximation."
       ),
       ref_symbols = "†"
   )%>%
   footnote(
       j = "p_value", i = 2,
    value = as_paragraph(
      "Two-sample Poisson exact test comparing female vs male incidence within each age group (two-sided)."
    ),
    ref_symbols = "‡"
    )-> ft_incidence
  
#    save_as_docx(path = "outputs/poisoning_age_specific_incidence_rates.docx")

# footnotes: * † ‡ § ¶ || then ** †† ‡‡


# create the age pyrarmid with rates and the ribbon for CI 
rates_age_sex_with_CI_wide%>%filter( Sex == "Female")

rates_age_sex_with_CI_wide%>%
filter( Age %in% age_levels)%>%
mutate( 
    Age = factor( Age, levels = c( age_levels)
)
)%>%
    ggplot(
        aes( 
            x = Age, 
            y = rate_per_100k_standard,
            group = Sex, 
            color = Sex, 
        )
    ) + 
    geom_line()  +
    geom_ribbon( 
        aes( 
            ymin = ci_lower_standard, 
            ymax = ci_upper_standard, 
            group = Sex, 
            fill = Sex
        ), 
        alpha = 0.2
    ) + 
    theme_minimal() -> 
    plot_incidence


library(ggplot2)
library(dplyr)
library(ggsci)
library(scales)
library(ggrepel)

theme_samj <- function(base_size = 11, base_family = "Helvetica") {
    theme_minimal(base_size = base_size, base_family = base_family) +
        theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(linewidth = 0.3, colour = "grey85"),
            axis.title = element_text(face = "bold"),
            axis.text = element_text(colour = "grey20"),
            plot.title = element_text(face = "bold", hjust = 0, margin = margin(b = 4)),
            plot.subtitle = element_text(colour = "grey25", margin = margin(b = 8)),
            plot.caption = element_text(size = rel(0.9), colour = "grey30"),
            legend.position = "none" # we'll direct-label
        )
}

dat <- rates_age_sex_with_CI_wide %>%
filter( Age %in% age_levels)%>%
    mutate(Age = factor(Age, levels = age_levels)) %>%
    arrange(Age)

# compute last age group per sex for end labels

dat %>%
filter( Age %in% age_levels)%>%
    group_by(Sex) %>%
    arrange(-rate_per_100k_standard)%>%
    mutate( 
        rate_rank = row_number( )
    )%>%
    filter(rate_rank == {
        ifelse( data_set_name == "poison", 1 , 1)
    }) %>%
    # slice_tail(n = 1) %>%
    ungroup()-> ends


cdc_palette <- c(
    purple = "#500778",
    gray = "#3b3e43",
    pink = "#ef456e",
    green = "#479437",
    light_blue = "#45c1c0",
    navy = "#174883",
    yellow = "#fbaf33",
    orange = "#f05424",
    light_purple = "#89317a"
)


p_nejm <-
    ggplot(dat, aes(x = Age, y = rate_per_100k_standard, group = Sex, color = Sex)) +
    # CI ribbon underneath
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = Sex),
        alpha = 0.15, colour = NA, show.legend = FALSE
    ) +
    # line and points
    geom_line(linewidth = 1.5) +
    geom_point(size = 2.5, stroke = 0) +
    # direct end labels for clarity
    geom_text(
        data = ends,
        aes(label = Sex),
        nudge_y = 0.003, 
        hjust = -0.2, vjust = 0.5, size = 4, fontface = "bold", show.legend = FALSE
    ) +
    # neat palette & axes
    # ggsci::scale_color_nejm( ) +
    # ggsci::scale_fill_nejm() +
    scale_fill_manual(values = c("#45c1c0", "#500778")) +
    scale_color_manual(values = c("#45c1c0", "#500778")) +
    scale_y_continuous("Incidence per 100 000 person-years",
        labels = label_comma(accuracy = 0.01),
        expand = expansion(mult = c(0.02, 0.08))
    ) +
    scale_x_discrete("Age group", expand = expansion(add = 0.2)) +
    { 
      if (exists("data_set_name") && data_set_name == "poison" && requireNamespace("ggbreak", quietly = TRUE)) {
       # ggbreak::scale_y_break(c(3, 8))
      } else {
        NULL
      }
    } +
    coord_cartesian(clip = "off") +
    labs(
        title = paste( data_set_label, ""),
        #subtitle = "Age-specific incidence rates stratified by Sex with 95% confidence bands",
        #caption = "Shaded area = 95% CI. Rates per 100 000 person-years."
    ) +
    theme_samj()



    return( 
        list( 
            ft_incidence, 
            plot_incidence, 
            p_nejm, 
            full_dataset_final, 
            full_dataset_with_year
        )
    )
}

# some notes for the flextable
# Since you are showing the avergae population, also show he average cases and then the weights so that the ASR can be closeishly derived. 

graphics.off()
che_final %>% filter( case_bin ==1 )-> che_bin
che_bin %>% glimpse()
che_bin$gender %>%tabyl()

 xtabs(
     ~ age_tested_years +
         agecategory_unit,
     data = che_bin
 )

ASR_table_and_plot(che_bin) -> che_incidence
che_incidence[1]
che_incidence[3]
che_incidence[4]
che_incidence[5]
 
 poison$patient_gender%>%tabyl() 
 xtabs( 
    ~ Age_years + 
    agecategory_unit, 
    data = poison
 )

ASR_table_and_plot( poison) -> poison_incidence
poison_incidence[1]
poison_incidence[3]
poison_incidence[[3]]-> poison_plot
poison_plot

  che_incidence[[4]] %>%
    pivot_wider(
        names_from = Sex,
        values_from = c(cases, pop, pop_standard, rate_per_100k, rate_per_100k_standard,
                        ci_lower, ci_upper)
    ) %>%
      rowwise() %>%
      mutate(
          # two-sample Poisson test of equality of incidence rates (Female vs Male)
          pt = list(poisson.test(
              x = c(cases_Female, cases_Male), # counts
              T = c(pop_Female, pop_Male), # person-years
              alternative = "two.sided",
              conf.level = 0.95
          )), # fig-cap "p-values were obtained from a two-sample Poisson exact test comparing the age-specific incidence rate in females versus males for each age band."
          irr = unname(pt$estimate), # incidence rate ratio (Female / Male)
          irr_lower = pt$conf.int[1],
          irr_upper = pt$conf.int[2],
          p_value = style_pvalue(pt$p.value)
      ) %>%
      ungroup() %>%
      select(
          -pt
      )%>%
      mutate( 
        Age = factor( Age, levels = c(age_levels, "Total")
      )
      ) %>%
      #filter( Age == "15-19")%>%
      select( 
            Age, 
            rate_per_100k_standard_Male, 
            rate_per_100k_standard_Female,
            starts_with("cases"), 
            #starts_with("pop"), 
            starts_with("irr")
      )


ggsave( 
    poison_plot, 
    file = "poison_plot.png", 
    path = "figures", 
    dpi = 300, 
    width = 8, 
    height = 6
)


# put the two flextables into a single word document
 save_as_docx(
     che_incidence[[1]],
     poison_incidence[[1]],
      path = "outputs/age_standardised_incidence_rates.docx"
 )

library(ggforce)
graphics.off()
(che_incidence[[3]] + poison_incidence[[3]]) + patchwork::plot_layout(ncol = 1, guides = "collect")->combined_plot  

ggsave(
    combined_plot,
    file = "figures/combined_ASR_plot.png",
    dpi = 300,
    width = 8,
    height = 8
)


# Provinical rates 
poison%>%setDT() 

poison[, 
.(n = .N), 
by = .(year, prov_)]-> poison_province_agg

poison_province_agg%>%
    mutate( 
        across( c( prov_, year), ~ as.character(.x)
    )
    )-> poison_province_agg


NMCleaner::pop[, 
.(pop = sum(Population)), 
by = .(Year, prov)][, prov_ := prov][, year := Year]-> pop_agg

pop_agg



# Chatgpt with WHO stadardised
library(dplyr)
library(tidyr)
library(flextable)
library(tibble)

#-------------------------------------------------
# 1.  WHO 2000-2025 standard weights  (from Ahmad et al. WHO 2001)
#-------------------------------------------------
who_std <- tribble(
    ~Age,     ~weight,
    "0-4",    0.0886,
    "5-9",    0.0869,
    "10-14",  0.0860,
    "15-19",  0.0847,
    "20-24",  0.0822,
    "25-29",  0.0793,
    "30-34",  0.0761,
    "35-39",  0.0715,
    "40-44",  0.0659,
    "45-49",  0.0604,
    "50-54",  0.0537,
    "55-59",  0.0455,
    "60-64",  0.0372,
    "65-69",  0.0296,
    "70-74",  0.0221,
    "75-79",  0.0152,
    "80-84",  0.0091,
    "85+",    0.0063
)

#-------------------------------------------------
# 2.  If your table has 0-1 & 1-4 split, divide the 0-4 weight
#-------------------------------------------------
split0_1 <- 1 / 5 # assume 2 years in 0-1 vs 3 years in 1-4

# compute the original 0-4 weight and aggregate 65+ weights
weight_0_4    <- who_std %>% filter(Age == "0-4") %>% pull(weight)
weight_65plus <- who_std %>% filter(Age %in% c("65-69","70-74","75-79","80-84","85+")) %>% summarise(w = sum(weight)) %>% pull(w)

who_std <- who_std %>%
    # drop the original 0-4 and the detailed 65+ bands
    filter(!Age %in% c("0-4", "65-69", "70-74", "75-79", "80-84", "85+")) %>%
    # add split 0-1 / 1-4 and aggregated 65+
    bind_rows(
        tibble(Age = "0-1", weight = weight_0_4 * split0_1),
        tibble(Age = "1-4", weight = weight_0_4 * (1 - split0_1)),
        tibble(Age = "65+",  weight = weight_65plus)
    ) %>%
    arrange(factor(Age, levels = c(
        "0-1","1-4","5-9","10-14","15-19","20-24","25-29",
        "30-34","35-39","40-44","45-49","50-54","55-59",
        "60-64","65+"
    ))) %>%
    mutate(weight_pct = round(weight * 100, 2))

who_std

#-------------------------------------------------
# 3.  Combine WHO weights with your age-specific incidence table
#      (must already exist as age_incidence_per_100k)
#-------------------------------------------------


pop_by_age_sex_year%>%
    filter( Year == 2022)%>%
     mutate( 
        Age = factor( Age, levels = c( age_levels)
    )
    )%>%
    group_by( 
        Age
    )%>%summarise( 
        pop = sum( pop)
    )-> pop_by_age



# Look at rates over the years per province (NOT ASRs)

pih_cases <- tibble(
    age_years = c(
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
        20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
        37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53,
        54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70,
        71, 72, 73, 74, 75, 76, 77, 78, 80, 81, 82, 83, 84, 86, 87
    ),
    cases = c(
        193, 217, 147, 114, 72, 43, 27, 21, 23, 14, 19, 24, 29, 45, 57, 91,
        89, 110, 85, 72, 98, 76, 90, 82, 85, 106, 87, 88, 95, 95, 99, 72,
        107, 69, 66, 76, 54, 54, 70, 47, 73, 27, 48, 31, 37, 43, 20, 20,
        31, 24, 28, 16, 24, 12, 20, 27, 12, 12, 14, 12, 20, 11, 7, 9, 11,
        16, 10, 3, 6, 2, 4, 4, 8, 3, 7, 2, 3, 3, 3, 3, 3, 1, 2, 1, 1, 1
    )
)

age_levels

pih_cases %>%
    mutate(
        age = factor(case_when( 
            age_years <1 ~ "0-1",
            age_years %in% 1:4 ~ "1-4",
            age_years %in% 5:9 ~ "5-9",
            age_years %in% 10:14 ~ "10-14",
            age_years %in% 15:19 ~ "15-19",
            age_years %in% 20:24 ~ "20-24",
            age_years %in% 25:29 ~ "25-29",
            age_years %in% 30:34 ~ "30-34",
            age_years %in% 35:39 ~ "35-39",
            age_years %in% 40:44 ~ "40-44",
            age_years %in% 45:49 ~ "45-49",
            age_years %in% 50:54 ~ "50-54",
            age_years %in% 55:59 ~ "55-59",
            age_years %in% 60:64 ~ "60-64",
            age_years %in% 65:120 ~ "65+",
            TRUE ~ NA_character_
        ), 
        levels =  age_levels
        )
    )%>%
        group_by(age) %>%
        summarise(cases = sum(cases)) -> pih_age_grouped


left_join( 
    pih_age_grouped, 
    pop_by_age, 
    by = c( "age" = "Age")
)%>%
left_join( 
    who_std %>% select( Age, weight),
    by = c( "age" = "Age")
)%>%
    mutate( 
        #cases = cases/5, 
        rate_per_100k = ( cases / pop) * 1e5, 
        asr = rate_per_100k * weight
    )-> pih_rates

    pih_rates%>%flextable() %>%
    set_header_labels( 
        values = c( 
            age = "Age group", 
            cases = "Cases", 
            pop = "Population", 
            weight = "WHO Std Weight", 
            rate_per_100k = "Incidence per 100 000", 
            asr = "Weighted incidence"
        )
    )%>%
    bold( i = 1, j = 1:6, part = "header") %>%
    autofit()

pih_rates%>%
    ggplot(
        aes( 
            x = age, 
            y = asr, 
            group = 1, 
        )
    ) + 
    geom_line()+ 
    geom_point() + 
    scale_y_continuous( 
        labels = scales::comma, 
        limits = c(0, 1)
    ) +
    theme_minimal()
