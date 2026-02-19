#download population estimates from StatsSA

#remove.packages("NMCleaner")
#detach 
#detach("package:NMCleaner", unload=TRUE)
#remotes::install_github("bridaybrummer/NMCleaner")

library(NMCleaner)

  df <- tibble::tibble(subdistrict = c("dr pixley ka seme", "dr beyers naudé", "city of cape Town", "port saint johns"))
  res <- mutate_sub_district(df, subdistrict_variable = "subdistrict", dict = dict, use_fuzzy = FALSE, unmatched = "keep_raw")
res

# look at new_master
new_master <- arrow::read_feather("~/Desktop/SAFETP/CLA/NMC_database/master/new_master.feather")

new_master %>%
  group_by(prov_, district, sub_district)%>%
  summarise( 
    n =n() 
  )%>%
  na.omit() %>%
  ungroup() %>%
  mutate_sub_district(
    ., 
    subdistrict_variable = "sub_district",
    use_fuzzy = TRUE, 
  unmatched = c("NA"), 
  max_dist = 2,
  dict = dict
)%>%
group_by( 
  prov_, district, subdistrict_standard, 
)%>%
summarise( 
  n = sum( n)
)
# So, sort of. 


pop_directory <- "population_estimates"


df <- tibble::tibble(subdistrict = c("dr pixley ka seme", "dr beyers naudé", "city of cape Town", "port saint johns"))
res <- mutate_sub_district(df, subdistrict_variable = "subdistrict", dict = dict, use_fuzzy = FALSE, unmatched = "keep_raw")

tibble(subdist)

# Create a new directory
if (file.exists(pop_directory)) {
  # If it exists, remove it
  unlink(pop_directory, recursive = TRUE)
}

dir.create(pop_directory)

# Check if the directory was created successfully
if (file.exists(pop_directory) && file.info(pop_directory)$isdir) {
  cat("New directory created:", pop_directory, "\n")
} else {
  cat("Failed to create the directory:", pop_directory, "\n")
}

#read in the datasets but reduce their size immediately.
url <- "https://www.statssa.gov.za/publications/P0302/District_Council_projection_by_sex_and_age_2002_2022.xlsx"
url_projection <- "https://www.statssa.gov.za/publications/P0302/District_projections_by_sex_and_age_2023_2027.xlsx"



download.file(url, destfile=paste0(pop_directory,"/pop.xlsx"), mode="wb")

download.file(url_projection, destfile=paste0(pop_directory,"/projection.xlsx"), mode="wb")

# There is another population file with more granularity @ https://www.census.gov/geographies/mapping-files/time-series/demo/international-programs/subnationalpopulation.html since it was funded by PEPFAR, lol 

library(NMCleaner)

if(TRUE){
pepfar_url <- "https://www2.census.gov/programs-surveys/international-programs/tables/time-series/pepfar/south_africa.xlsx"
download.file(pepfar_url, destfile=paste0(pop_directory,"/pepfar_pop.xlsx"), mode="wb")
}else{
  print( "You are going to use a previous downloaded URL")
}

# read in that file 

pepfar_pop_metadata <- readxl::read_excel(paste0(pop_directory,"/pepfar_pop.xlsx"), sheet = 1)

pepfar_pop_data_dictionary <- readxl::read_excel(paste0(pop_directory,"/pepfar_pop.xlsx"), sheet = 2)

pepfar_pop <- readxl::read_excel(paste0(pop_directory,"/pepfar_pop.xlsx"), sheet = 3, skip = 3)


 names(pepfar_pop)

# I want a dataset that is long with vars for year, age, sex and population 
library(NMCleaner)
conflicts_prefer(dplyr::filter)
library(tm)

pepfar_pop%>%names() 

# Reshape and get demographic info for long format 
pepfar_pop%>%
    pivot_longer( 
      cols = c(BTOTL_2015:F80PL_2030),
      names_to = "var_name", 
      values_to = "pop"
    )%>%
    mutate( 
      sex = case_when( 
        grepl("M", var_name) ~ "Male", 
        grepl( "F", var_name) ~ "Female", 
        grepl( "B", var_name) ~ "Both"
      )
    )%>%
    mutate( 
      age = 
        case_when(
          grepl("0004_", var_name) ~ "0-4",
          grepl("0509_", var_name) ~ "5-9",
          grepl("1014_", var_name) ~ "10-14",
          grepl("1519_", var_name) ~ "15-19",
          grepl("2024_", var_name) ~ "20-24",
          grepl("2529_", var_name) ~ "25-29",
          grepl("3034_", var_name) ~ "30-34",
          grepl("3539_", var_name) ~ "35-39",
          grepl("4044_", var_name) ~ "40-44",
          grepl("4549_", var_name) ~ "45-49",
          grepl("5054_", var_name) ~ "50-54",
          grepl("5559_", var_name) ~ "55-59",
          grepl("6064_", var_name) ~ "60-64",
          grepl("6569_", var_name) ~ "65-69",
          grepl("7074_", var_name) ~ "70-74",
          grepl("7579_", var_name) ~ "75-79",
          grepl("80PL", var_name) ~ "80+",
          TRUE ~ "Total"
      ), 

    )%>%
    mutate( 
      year = gsub(".*_(\\d{4})$", "\\1", var_name)
    )%>%clean_names() -> 
    pop_data

# Check that all sex and age have been parsed 
pop_data$var_name %>% gsub("_2.*", "", .)%>%gsub("B|M|F", "", .) %>% unique
pop_data$age%>%unique() 

# dib-district names are stored with 3 .'s preceding. 
pop_data%>%
  filter( grepl("\\.\\.\\.", area_name))-> pop_data_filtered

# you can also just filter them as they are admin level 3 (level 4 is municipailty)

pop_data_filtered %>%
  filter(adm_level %in% 3)%>%
  mutate(
    area_name = tm::removePunctuation(area_name)
    ) -> pop_data_filtered_adm_3

pop_data_filtered_adm_3$area_name %>% unique()
pop_data_filtered_adm_3$adm_level %>% unique()
pop_data_filtered_adm_3 %>% names()
pop_data_filtered_adm_3 %>% glimpse()

# Rename key variables that can be used to join data 
pop_data_filtered_adm_3%>%rename(
  province = adm1_name,
  district = adm2_name,
  sub_district = adm3_name
)%>%
  mutate(
    across(c(sub_district, district, province), str_to_lower)
  )%>%
    filter( age != "Total", sex != "Both")  %>%
    mutate( 
      age = factor( 
        age, 
              levels = c( 
        "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+", "Total"
      )
      )
    ) -> # and filter out the totals and "both sexes" 
pop


# check that population esimtate is correcti'ish
pop %>%
  filter (year %in% 2025)%>%
  summarise( pop = sum(pop))
  

# compare to pop data in the package 
NMCleaner::pop
# some functions will just have to be reowrked so that the var nsmes are not in capitals. 



# make a population age_pyramid for the country
library(ggplot2)
pop%>%glimpse()

pop %>%
  filter(year ==2025)%>%
  group_by( 
    province, sex, age
  )%>%
  summarise( pop = sum(pop))%>%
  mutate( pop = if_else( sex %in% "Male", pop , -pop ), 
  age = as.factor( age) )%>%
  ggplot(aes(x = age, y = pop, fill = province)) +

  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(title = "Population Age Pyramid",
       x = "Age Group",
       y = "Population",
       fill = "Sex")



pop %>%
  filter(year ==2025)%>%
  group_by( 
    province, sex, age
  )%>%
  summarise( pop = sum(pop))%>%
  mutate( pop = if_else( sex %in% "Male", pop , -pop ), 
  age = as.factor( age) )%>%
ggplot(., aes(x = as.numeric(age), y = pop, color = interaction(province,sex))) +
coord_flip() +
geom_line( ) 


pop_prop <- pop %>%
  filter(year == 2025) %>%
  group_by(province, sex, age) %>%
  summarise(pop = sum(pop), .groups = "drop") %>%
  group_by(province) %>%
  mutate(
    prop = pop / sum(pop), # share of province total
    prop_signed = if_else(sex == "Male", prop, -prop),
    age = factor(age, levels = sort(unique(age))) # keep age in order
  ) %>%
  ungroup()

# 2A) Faceted age–sex pyramids (recommended)
ggplot(pop_prop, aes(x = age, y = prop_signed, color = sex)) +
  geom_line(width = 0.9) +
  coord_flip() +
  facet_wrap(~province) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Population structure by age & sex — share within province (2025)",
    x = "Age group",
    y = "Share of province population"
  ) +
  theme_minimal(base_size = 12)

  ggplot(pop_prop, aes(
    x = as.numeric(age), y = prop_signed,
    color = province, linetype = sex,
    group = interaction(province, sex)
  )) +
    geom_line(linewidth = 1) +
    coord_flip() +
    scale_x_continuous(
      breaks = seq_along(levels(pop_prop$age)),
      labels = levels(pop_prop$age)
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Age–sex structure by province (normalized within province, 2025)",
      x = "Age group",
      y = "Share of province population"
    ) +
    theme_minimal(base_size = 12)



# Shape file, it doesnt look like PEPFAR has an up to date shape file for their data 

#https://data.humdata.org/dataset/cod-ab-zaf
# now in "scripts_and_functions/shape/zad_adm_sadb_ocha_20200119_SHP"
list.dirs("scripts_and_functions")
list.files( 
  "scripts_and_functions/shape/zaf_adm_sadb_ocha_20201109_SHP", 
  pattern = ".shp"
)

read_sf("scripts_and_functions/shape/zaf_adm_sadb_ocha_20201109_SHP/zaf_admbnda_adm1_sadb_ocha_20201109.shp") # provinces
read_sf("scripts_and_functions/shape/zaf_adm_sadb_ocha_20201109_SHP/zaf_admbnda_adm2_sadb_ocha_20201109.shp") # districts
read_sf("scripts_and_functions/shape/zaf_adm_sadb_ocha_20201109_SHP/zaf_admbnda_adm3_sadb_ocha_20201109.shp" ) -> subdist_shape# sub_districts

subdist_shape%>%glimpse() 
subdist_shape$ADM1_EN%>%unique()

# rename vars
subdist_shape%>%
  rename(
    sub_district = ADM3_EN,
    district = ADM2_EN,
    province = ADM1_EN
  )%>%
  mutate(
    across(c(sub_district, district, province), str_to_lower)
  ) -> subdist_shape

# plot the sub_districts 
subdist_shape %>%
    ggplot()+ 
    geom_sf() 


subdist_shape$district %>% unique()
subdist_shape$sub_district%>%unique
pop$sub_district%>%unique


pop %>% 
  filter( 
    year %in% 2025
  )%>%
  group_by(
    district, province, sub_district
  )%>%
  reframe( 
    pop = sum( pop )
  )-> 
  pop_2025


# standardise provinces 

mutate_province_standard <- 
function( data, province_var ){

  data %>%
    mutate(
      province_standard =
        case_when(
          grepl("northern cape|$ncape|$n cape|nothern", !!sym(province_var), ignore.case = TRUE) ~ "Northern Cape",
          grepl("north west|nwest|n west", !!sym(province_var), ignore.case = TRUE) ~ "North West",
          grepl("western cape|wcape|w cape", !!sym(province_var), ignore.case = TRUE) ~ "Western Cape",
          grepl("eastern cape|$ecape|e cape", !!sym(province_var), ignore.case = TRUE) ~ "Eastern Cape",
          grepl("free state|fstate|f state", !!sym(province_var), ignore.case = TRUE) ~ "Free State",
          grepl("gauteng", !!sym(province_var), ignore.case = TRUE) ~ "Gauteng",
          grepl("kwazulu[- ]?natal|kzn", !!sym(province_var), ignore.case = TRUE) ~ "KwaZulu-Natal",
          grepl("limpopo", !!sym(province_var), ignore.case = TRUE) ~ "Limpopo",
          grepl("mpumalanga", !!sym(province_var), ignore.case = TRUE) ~ "Mpumalanga",
          TRUE ~ "unmatched"
        )
    )-> data 


  # for those that are unmatched use a fuzzy join 

    data %>%filter( province_standard == "unmatched")-> unmatched_data

    data %>% 
    select( !! province_var, province_standard )%>%
    unique()-> check 

    # if the length is > 9 then standardisation didnt go well.
  if(nrow(check) > 9){
    warning("Standardisation of provinces did not go well, consider reviewing province names")
  }

  return(data)

}



#======================
# sub-district standardisation 
#======================



pop_2025%>%
    NMCleaner::mutate_district_name( district_variable = "district" )%>%
    filter( is.na(district_standard))

pop_2025 %>%
ungroup() %>%
  mutate_province_standard( province_var = "province" )%>%
      mutate( across(c(district, sub_district), ~gsub("doctor", "dr", .)))%>% # this is a workaround for now but should be updated in the NMCleaner package
  NMCleaner::mutate_district_name( district_variable = "district" )%>%
  mutate(province = province_standard, 
              district = district_standard) -> pop_2025_standard_manual

              pop_2025_standard_manual$province %>% unique()
              pop_2025_standard_manual %>% filter(is.na(district_standard))


subdist_shape%>%as_tibble() %>%
  mutate_province_standard(province_var = "province") %>%
      mutate( across(c(district, sub_district), ~gsub("doctor", "dr", .)))%>% # this is a workaround for now but should be updated in the NMCleaner package
    NMCleaner::mutate_district_name(district_variable = "district") %>%
    mutate(
      province = province_standard,
      district = district_standard
    ) -> subdist_shape_standard_manual

    subdist_shape_standard_manual$province %>% unique()

pop_2025_standard_manual %>% select(c("sub_district", "district", "province"))
subdist_shape_standard_manual %>% select(c("sub_district", "district", "province"))


# try merging them with province and disitrct standardisation 

left_join( 
  subdist_shape_standard_manual,
  pop_2025_standard_manual,
  by = c("sub_district" ,"district" ,"province")
) %>%
st_as_sf() %>%
ggplot() + 
geom_sf(aes(
  fill = pop
))


left_join( 
  subdist_shape_standard_manual,
  pop_2025_standard_manual,
  by = c("sub_district" ,"district" ,"province")
) %>%
filter( is.na(pop))%>%
select( 
  sub_district,
  district,
  province
)-> un_matched_sub_districts

un_matched_sub_districts





# see if a sub_district function standardiser can imrpove this. 

# data-raw/build_sa_subdistrict_dictionary.R
 
build_sa_subdistrict_dictionary <- function(include_metro_regions = TRUE) {
  stopifnot(requireNamespace("rvest", quietly = TRUE))
  stopifnot(requireNamespace("dplyr", quietly = TRUE))
  stopifnot(requireNamespace("stringr", quietly = TRUE))
  stopifnot(requireNamespace("tidyr", quietly = TRUE))
  stopifnot(requireNamespace("purrr", quietly = TRUE))
  stopifnot(requireNamespace("tibble", quietly = TRUE))

  library(rvest); library(dplyr); library(stringr); library(tidyr); library(purrr); library(tibble)

  norm_txt <- function(x) {
    x %>%
      str_to_lower() %>%
      gsub("\\(.*?\\)", "", .) %>%             # drop bracketed notes
      str_replace_all("[^a-z0-9]+", "") %>%
      trimws()
  }

  # ---- 1) Pull the "Local municipalities" master table ----
  # Page has three big tables (Metros, Districts, **Local municipalities**), plus a "Former municipalities" section.
  url <- "https://en.wikipedia.org/wiki/List_of_municipalities_in_South_Africa"
  html <- read_html(url)  # main list page (last edited 15 Jul 2025)  [205 LMs remain since 2016]  :contentReference[oaicite:1]{index=1}
  tabs <- html %>% html_elements("table.wikitable") %>% html_table()

  # Keep tables that look like the "Local municipalities" table (has 'Code', 'Province', 'District', 'Seat')
  lm_raw <- tabs %>%
    keep(~ all(c("Code", "Province") %in% names(.x))) %>%
    bind_rows() %>%
    # standardize headers
    rename_with(~ str_replace_all(.x, "\\s+", "_")) %>%
    rename(
      name     = matches("^Name$"),
      code     = matches("^Code$"),
      province = matches("^Province$"),
      district = matches("^District$"),
      seat     = matches("^Seat$")
    ) %>%
    # keep rows that look like LM codes (province prefix + digits), and have a district listed
    filter(!is.na(code),
           str_detect(code, "^(EC|FS|GT|KZN|LIM|MP|NW|NC|WC)\\d+"),
           !is.na(district))

  # ---- 2) Choose a "standard" sub-district label: strip " Local Municipality" suffix ----
  lm <- lm_raw %>%
    mutate(
      standard_subdistrict = str_trim(str_remove(name, "\\s*Local Municipality$")),
      province = as.character(province),
      district = as.character(district),
      seat     = as.character(seat)
    ) %>%
    select(province, district, subdistrict_code = code, standard_subdistrict, seat)

  # ---- 3) Generate baseline variants programmatically (fast, consistent) ----
  base_variants <- lm %>%
    mutate(
      # auto variants: official with/without suffix, hyphen/space variants, code, seat-name if distinctive
      v1 = standard_subdistrict,
      v2 = paste(standard_subdistrict, "Local Municipality"),
      v3 = str_replace_all(standard_subdistrict, "[-/]", " "),
      v4 = str_replace_all(standard_subdistrict, "[-/]", ""),
      v5 = subdistrict_code,
      v6 = dplyr::if_else(!is.na(seat) & !seat %in% c("", standard_subdistrict), seat, NA_character_)
    ) %>%
    transmute(province, district, subdistrict_code, standard_subdistrict,
              variant = pmap_chr(list(v1, v2, v3, v4, v5, v6),
                                 ~ c(..1, ..2, ..3, ..4, ..5, ..6) %>%
                                   discard(is.na) %>% unique() %>% paste(collapse = "||"))) %>%
    separate_rows(variant, sep = "\\|\\|") %>%
    distinct()

  # ---- 4) Hand-curated legacy/alias variants (renames/mergers; key old town names) ----
  manual_alias_long <- tribble(
    # post-2016 mergers / popular alternates
    ~standard_subdistrict,              ~alias,
    "Big Five Hlabisa",                 "Big 5 Hlabisa",
    "Big Five Hlabisa",                 "Big Five False Bay",
    "Dr Nkosazana Dlamini-Zuma",        "Ingwe",
    "Dr Nkosazana Dlamini-Zuma",        "Kwa Sani",
    "Fetakgomo Tubatse",                "Greater Tubatse",
    "Fetakgomo Tubatse",                "Fetakgomo",
    "JB Marks",                         "Tlokwe",
    "JB Marks",                         "Ventersdorp",
    "JB Marks",                         "Potchefstroom",
    "Kagisano-Molopo",                  "Kagisano",
    "Kagisano-Molopo",                  "Molopo",
    "Modimolle–Mookgophong",            "Modimolle",
    "Modimolle–Mookgophong",            "Mookgophong",
    "Dawid Kruiper",                    "Mier",
    "Dawid Kruiper",                    "Khara Hais",
    "Dawid Kruiper",                    "//Khara Hais",
    "Inkosi Langalibalele",             "Umtshezi",
    "Inkosi Langalibalele",             "Imbabazane",
    "Musina",                           "Mutale",
    "Thulamela",                        "Mutale",
    # widely used seat/legacy names
    "Mbombela",                         "Nelspruit",
    "Emalahleni",                       "Witbank",        # (MP)
    "Polokwane",                        "Pietersburg",
    "Makhado",                          "Louis Trichardt",
    "Mogale City",                      "Krugersdorp",
    "City of Matlosana",                "Klerksdorp",
    "Merafong City",                    "Carletonville",
    "Rand West City",                   "Randfontein",
    "Rand West City",                   "Westonaria",
    "uMhlathuze",                       "Richards Bay",
    "uMhlathuze",                       "Empangeni",
    "Bitou",                            "Plettenberg Bay",
    "Mossel Bay",                       "Mosselbaai", 

    # extras added 
  "Pixley ka Seme", "dr pixley ka isaka seme", 
  "Pixley ka Seme", "Pixley ka Seme", 
  "Pixley ka Seme",   "Dr Pixley ka Seme",
  "Pixley ka Seme",   "Pixley ka Isaka Seme",
  "Pixley ka Seme",   "Dr Pixley Ka-Isaka Seme",
  "Kai !Garib",                "Kai Garib",
  "Kai !Garib",                "Kai!Garib",
  "!Kheis",                    "Kheis", 
  "Ramotshere Moiloa",         "Ramatshere", 
  "Ramotshere Moiloa", "Zeerust Municipality"
  )

  manual_alias <- manual_alias_long %>%
    inner_join(select(lm, standard_subdistrict), by = "standard_subdistrict") %>%
    transmute(standard_subdistrict, variant = alias)

  # ---- 5) Optional: Metro "regions" (Johannesburg A–G; Tshwane 1–7) ----
  # These are *not* LMs; include only if you want metro sub-districts for health-style groupings.
  metro_regions <- tibble()
  if (isTRUE(include_metro_regions)) {
    metro_regions <- tribble(
      ~province, ~district,                 ~subdistrict_code, ~standard_subdistrict,     ~seat,
      "Gauteng", "City of Johannesburg",    "JHB_A",           "Johannesburg Region A",   NA,
      "Gauteng", "City of Johannesburg",    "JHB_B",           "Johannesburg Region B",   NA,
      "Gauteng", "City of Johannesburg",    "JHB_C",           "Johannesburg Region C",   NA,
      "Gauteng", "City of Johannesburg",    "JHB_D",           "Johannesburg Region D",   NA,
      "Gauteng", "City of Johannesburg",    "JHB_E",           "Johannesburg Region E",   NA,
      "Gauteng", "City of Johannesburg",    "JHB_F",           "Johannesburg Region F",   NA,
      "Gauteng", "City of Johannesburg",    "JHB_G",           "Johannesburg Region G",   NA,
      "Gauteng", "City of Tshwane",         "TSH_1",           "Tshwane Region 1",        NA,
      "Gauteng", "City of Tshwane",         "TSH_2",           "Tshwane Region 2",        NA,
      "Gauteng", "City of Tshwane",         "TSH_3",           "Tshwane Region 3",        NA,
      "Gauteng", "City of Tshwane",         "TSH_4",           "Tshwane Region 4",        NA,
      "Gauteng", "City of Tshwane",         "TSH_5",           "Tshwane Region 5",        NA,
      "Gauteng", "City of Tshwane",         "TSH_6",           "Tshwane Region 6",        NA,
      "Gauteng", "City of Tshwane",         "TSH_7",           "Tshwane Region 7",        NA,
      "Western Cape", "City of Cape Town",  "CPT_1",           "Cape Town Region 1",      NA,
      "Western Cape", "City of Cape Town",  "CPT_2",           "Cape Town Region 2",      NA,
      "Western Cape", "City of Cape Town",  "CPT_3",           "Cape Town Region 3",      NA,
      "Western Cape", "City of Cape Town",  "CPT_4",           "Cape Town Region 4",      NA,
      "Western Cape", "City of Cape Town",  "CPT_5",           "Cape Town Region 5",      NA, 
      "Western Cape", "City of Cape Town",  "CPT",           "City of Cape Town",      NA
    )
    base_variants <- bind_rows(
      base_variants,
      metro_regions %>%
        mutate(variant = standard_subdistrict)
    )
  }

  # ---- 6) Bind base + manual variants, build list-column dictionary ----
  dict_long <- bind_rows(base_variants, manual_alias) %>%
    mutate(variant_norm = norm_txt(variant)) %>%
    distinct(province, district, subdistrict_code, standard_subdistrict, variant, variant_norm)

  # collapse to list of variants per standard (list-column)
  dict_listcol <- dict_long %>%
    group_by(province, district, subdistrict_code, standard_subdistrict) %>%
    summarise(subdistrict_variant = list(unique(variant)), .groups = "drop")

  # For matching: also provide a long (unnested) version if you want to export a CSV later
  attr(dict_listcol, "long_version") <- dict_long

  dict_listcol
}

build_sa_subdistrict_dictionary()

Make_sub_district_name <- function(data,
                                   subdistrict_variable = "subdistrict",
                                   unmatched = c("keep_raw", "NA"),
                                   use_fuzzy = requireNamespace("fuzzyjoin", quietly = TRUE),
                                   max_dist = 2,
                                   dict = NULL) {
  unmatched <- match.arg(unmatched)
  stopifnot(is.data.frame(data))
  if (!subdistrict_variable %in% names(data)) {
    stop("Column `", subdistrict_variable, "` not found in `data`.")
  }
  if (is.null(dict)) dict <- build_sa_subdistrict_dictionary(include_metro_regions = TRUE)

  # ---- helpers ----
  normalize_key <- function(x) {
    x <- stringr::str_to_lower(x)
    x <- gsub("\\(.*?\\)", "", x)
    common_words <- c(
      "municipality", "district", "city", "metro", "metropolitan",
      "region", "subdistrict", "sub-district", "local", "lm", "of", "the"
    )
    x <- stringr::str_replace_all(x, paste0("\\b(", paste(common_words, collapse = "|"), ")\\b"), " ")
    x <- stringr::str_replace_all(x, "[^a-z0-9]+", "")

    # replace "doctor" with "dr"
    x <- stringr::str_replace_all(x, "\\bdoctor\\b", "dr")
    # do the same for saint -> st 
    x <- stringr::str_replace_all(x, "saint", "st")
    # replace special characters like "é" with standard letters
    x <- stringi::stri_trans_general(x, "Latin-ASCII")

    trimws(x)
  }


  dict_long <- attr(dict, "long_version")
  if (is.null(dict_long)) {
    dict_long <- tidyr::unnest(dict, cols = "subdistrict_variant", names_repair = "minimal") |>
      dplyr::rename(variant = subdistrict_variant) |>
      dplyr::mutate(variant_norm = normalize_key(variant))
  }

  # fast exact-normalized map
  keys <- dict_long %>% dplyr::distinct(standard_subdistrict, variant_norm)
  key2std <- stats::setNames(keys$standard_subdistrict, keys$variant_norm)

  raw_vec <- as.character(data[[subdistrict_variable]])
  clean_vec <- normalize_key(raw_vec)
  std_exact <- unname(key2std[clean_vec])
  match_method <- ifelse(!is.na(std_exact) & nzchar(std_exact), "exact_norm", NA_character_)
  out_std <- std_exact

  # regex pass (first-hit)
  need_regex <- which(is.na(out_std) | !nzchar(out_std))
  if (length(need_regex) > 0) {
    regex_dict <- dict_long %>%
      dplyr::mutate(pattern = paste0(
        "\\b",
        stringr::str_replace_all(variant, "\\s+", "\\\\s*[-_]*\\\\s*"),
        "\\b"
      )) %>%
      dplyr::distinct(standard_subdistrict, pattern)

    for (i in need_regex) {
      txt <- raw_vec[i]
      hits <- regex_dict$standard_subdistrict[
        stringr::str_detect(
          stringr::str_to_lower(txt),
          stringr::regex(regex_dict$pattern, ignore_case = TRUE)
        )
      ]
      if (length(hits) >= 1) {
        out_std[i] <- hits[[1]]
        match_method[i] <- "regex"
      }
    }
  }

  # fuzzy pass (osa distance on normalized keys)
  if (isTRUE(use_fuzzy)) {
    still_need <- which(is.na(out_std) | !nzchar(out_std))
    if (length(still_need) > 0) {
      stopifnot(requireNamespace("fuzzyjoin", quietly = TRUE))
      cand <- keys %>% dplyr::rename(variant_norm = variant_norm)
      need_df <- tibble::tibble(
        idx = still_need,
        clean = clean_vec[still_need]
      )
      fz <- fuzzyjoin::stringdist_left_join(
        need_df, cand,
        by = c("clean" = "variant_norm"),
        method = "osa",
        max_dist = max_dist,
        distance_col = ".dist"
      )
      fz_best <- fz %>%
        dplyr::group_by(idx) %>%
        dplyr::slice_min(.dist, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup()

      hit_rows <- which(!is.na(fz_best$standard_subdistrict))
      if (length(hit_rows) > 0) {
        idxs <- fz_best$idx[hit_rows]
        out_std[idxs] <- fz_best$standard_subdistrict[hit_rows]
        match_method[idxs] <- "fuzzy"
      }
    }
  }

  # policy for unmatched is to keep raw
  if (identical(unmatched, "keep_raw")) {
    out_std[is.na(out_std) | !nzchar(out_std)] <- raw_vec[is.na(out_std) | !nzchar(out_std)]
  } else {
    out_std[is.na(out_std) | !nzchar(out_std)] <- NA_character_
  }

  data$subdistrict_standard <- out_std
  data$.sd_match_method <- match_method
  data
}

# Usage 

dict <- build_sa_subdistrict_dictionary(include_metro_regions = TRUE)
dict
# Long version (one row per variant)
dict_long <- attr(dict, "long_version")
# readr::write_csv(dict_long, "inst/extdata/subdistrict_dictionary.csv")
dict_long%>%view() 

# you will need to do a run of both datasets first 
pop_2025_standard_manual$sub_district

pop_2025_standard_manual %>%
  Make_sub_district_name(., "sub_district", dict = dict) -> pop_2025_standard

subdist_shape_standard_manual$sub_district

subdist_shape_standard_manual %>%
  Make_sub_district_name(., "sub_district", dict = dict) -> subdist_shape_standard

subdist_shape_standard%>%glimpse()
pop_2025_standard$sub_district %>% length() == subdist_shape_standard$sub_district %>% length()


pop_2025_standard%>%
  filter(
    grepl( "beyer", ignore.case = TRUE, sub_district)
  ) %>%
  select( 
    sub_district,subdistrict_standard,  district, province, .sd_match_method
  )

subdist_shape_standard %>%
 filter(
    grepl("beyer", ignore.case = TRUE, sub_district)
  )   %>%
  select( 
    sub_district,subdistrict_standard,  district, province, .sd_match_method
  )



left_join( 
  subdist_shape_standard,
  pop_2025_standard,
  by = c("subdistrict_standard", "district", "province")
)%>%
filter( is.na(pop))%>%
select( 
  subdistrict_standard,
  district,
  province
)%>%unique%>%print( n = 100 )


pop_2025_standard %>%arrange( -pop)

left_join(
  pop_2025_standard,
  subdist_shape_standard,
  by = c("subdistrict_standard", "district", "province")
) %>%
mutate( 
  label_true = if_else( pop > 800000, TRUE, FALSE)
)%>%
st_as_sf() %>%
ggplot() + 
geom_sf(
  aes(
  fill = pop 
  )
)+
geom_sf_label(
  data = . %>%filter( label_true), 
  aes(
    label = subdistrict_standard
  ),
)
  


# do a fuzzy join to see if the missings increase the yield 

stringdist_left_join(
  un_matched_sub_districts, 
  pop_2025, 
  by = c( "sub_district")
)%>%
select( 
  sub_district.x, sub_district.y
)


  subdist_shape$sub
  pop_2025




left_join( 
  pop,
  subdist_shape,
  by = c("sub_district" ,"district" ,"province")
)%>%
filter( !is.na(geometry))%>%
select( 
  sub_district,
  district,
  province
)%>%unique





NMCleaner::shape_files%>%names() 
NMCleaner::shape_files$sub_districts %>% names()
