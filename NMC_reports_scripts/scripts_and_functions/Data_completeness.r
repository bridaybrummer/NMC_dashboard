# data completeness for JEE

load("~/Desktop/SAFETP/CLA/NMC_database/master/new_master.rda")

library(NMCleaner)
conflicts_prefer(dplyr::filter)

new_master%>%
    mutate_dates( date_index = "notification_date")%>%
    filter( !condition %in% c("Covid-19", "MIS-C", "Malaria"))%>%
    filter( nmccategories %in% 1) -> 
df1

df1$condition%>%tabyl()

vars <- c("folder_no", "patient_name", "patient_surname", "symptom_date", "diagnosis_date", "patient_vital_status")
#vars <- setdiff(names(df1)[1:(length(df1)-2)],#vars to include 
#                c(names(df1)[16] , "", "edr_GexpertDate")#vars to exclude 
#                )

# check 

missing_df <- df1 %>%
  mutate(across(all_of(vars), .fns = ~ ifelse(is.na(.) | grepl("Unknown", ignore.case = TRUE, . ), FALSE, TRUE)
                                              #levels = c("Missing", "Present")
                                              
                )
         )%>%
    mutate(case_source1 = factor(case_when(grepl("iOS|Web|Android",df1$case_source ) ~ "App",
                                  grepl("*Paper*", df1$case_source ) ~ "Paper-based",
           grepl("micro", ignore.case = TRUE, case_source) ~ "Laboratory notification"
           ), levels = c("App", "Paper-based", "Laboratory notification"))
           )%>%
    mutate(case_source = as.factor(case_source))
  
missing_df$case_source
years<- c(2019:2024)

years  %>%
map(~
    missing_df %>%
    ungroup() %>%
    filter( as.character(year) %in% .x)%>%
    dplyr::select(
    folder_no, patient_name,patient_surname,patient_surname,symptom_date, diagnosis_date, patient_vital_status, case_source)%>%
    #mutate(across(everything(), ~ ifelse(grepl("unknown", ignore.case = TRUE), NA, 0)))%>%
    tbl_summary(
                missing = "no", 
                by = case_source,
                label = list (folder_no ~ "Folder Number", 
                                patient_name ~ "First Name", 
                                patient_surname ~ "Surname", 
                                symptom_date ~ "Symptom Onset Date", 
                                diagnosis_date ~"Date of Diagnosis",
                                patient_vital_status ~ "Vital Status"))%>%
    modify_footnote(update = everything() ~ NA)%>%
    modify_header(
        label = "", 
        all_stat_cols() ~ "{level}" )%>%
    bold_labels()%>%
    add_overall(
        last = FALSE , 
         col_label = "**Total**",
         )
    )%>%
    tbl_stack(tbls = ., 
              group_header = c("2019", "2020", "2021", "2022", "2023", "2024"))%>%
    as_flex_table%>%
        fontsize(, size = 8, part = "all")%>%
    flextable::set_table_properties(layout = "autofit", width = 0.99)%>%
    flextable::set_caption(paste0("NMC data completeness of clinical notifications on both reporting platforms, notified during "))%>%
    flextable::save_as_docx(path = "NMC_data_completeness.docx") 

?tbl_stack
#tbl_missing_df[["table_body"]][["var_label"]] <- str_to_title(str_replace_all(tbl_missing_df[["table_body"]][["var_label"]],"_", " "))