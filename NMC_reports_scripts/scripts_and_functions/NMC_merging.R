# Merging of Lab and clincal notifications. 

library(dplyr)
library(readxl)
library(tidyverse)
library(RecordLinkage)
library(janitor)
library(openai)
library(lubridate)
library(grates)
library(tidyverse)
library(DescTools)
library(RecordLinkage)
library(data.table)
library(openxlsx)
library(readxl)

# SSE (Data Linkage) Script
#Sys.setenv(OPENAI_API_KEY = "REDACTED_KEY")
#gptstudio.max_tokens = 10000


# Read in Excel file 
#NMC <- read_excel("1_30 April 20233_01052023.xlsx") %>%clean_names
#names(NMC)

# check case types
#xtabs(~ case_type, NMC)


NMC_merge_cases <- function(NMC = NMC){
  

  # Merging of Lab and clincal notifications. 
  
  library(dplyr)
  library(readxl)
  library(tidyverse)
  library(RecordLinkage)
  library(janitor)
  library(openai)
  library(lubridate)
  library(grates)
  library(tidyverse)
  library(DescTools)
  library(RecordLinkage)
  library(data.table)
  library(openxlsx)
  library(readxl)


# Create case_type as a factor so that you can order it.
  
names(NMC) <- str_to_lower(names(NMC))

exclude_vars_early<-names(NMC)[c(1:17, 22:length(NMC))] # we are picking vars to exclude so we only chnage the opposite of those for the matching. 

include_vars <- setdiff(names(NMC), exclude_vars_early) # this will make the inverse of the excluded vars

#NMC%>%mutate(across(contains("date"), as_date ))%>%
#  mutate(across(contains("date"), as.character))%>%
#  mutate(across(contains("date"), str_to_lower))%>%select(notification_date)


NMC1 <- NMC %>%
  mutate(across(contains("date"), as_date)) %>%
  mutate(across(all_of(include_vars), as.character)) %>% #string stadnardisation
  mutate(across(all_of(setdiff(include_vars, "notification_date")), str_to_lower)) %>% #string stadnardisation
  #clean_names%>%
  mutate(
    case_type = factor(case_type, levels= c("Clinical", "Lab", "Merged")) )

NMC1$notification_date

NMC1$months<- as.character(as_yearmonth(as_date(NMC1$notification_date)))

# Exact stirng matching
exact_match <- NMC1 %>%
  group_by(patient_surname, patient_name, patient_gender, district, condition) %>%
  arrange(case_type)%>%
  mutate(duplicate = if_else(n() > 1, "Duplicate", "Unique"), 
         dup_number = row_number(),
         UID = paste(patient_surname, patient_name, patient_gender, district, condition, sep = "_")) %>%
  arrange(UID, dup_number)

#Check number of duplciates identified
xtabs(~exact_match$duplicate)

#exact_match%>%filter(duplicate == "Duplicate")%>%select(UID, condition, case_type, patient_name, patient_surname, dup_number) %>%view()
#exact_match%>%filter(case_type== "Clinical|Lab")

# Find the UIDs that have both a lab and clinical reported
filtered_df <- exact_match %>%
  group_by(UID) %>%
  filter(all(c("Lab", "Clinical") %in% case_type))
filtered_df%>%view()
# ENsure there is complete data between the two of them. 
merged_df <- filtered_df %>%
  group_by(UID) %>%
  fill(everything(), .direction = "down") %>%
  fill(everything(), .direction = "up") 

#filtered_df%>%view()
merged_df%>%view()

# Now you need to take the old cases out of the original df. 
new_merged_uid <- merged_df$UID

# Now you have the final merged cases. 
final_merged <- merged_df %>%filter(dup_number == 1)%>%mutate(case_type = "Merged")


df_without_mergedUID <- exact_match %>% filter(!UID %in% new_merged_uid)

df_without_mergedUID
# then you can merge the final_merged, and the df_excluding the merged UID. 
final_full_exact <- merge(final_merged,df_without_mergedUID, keep = T, all = T ) %>% as_tibble

final_full_exact

xtabs(~case_type, final_full_exact)
######################################################################
##########
# Fuzzy matching version 
##########
######################################################################
# Now to make a fuzz matching version. 

# Create two sepearte dfs
NMC2 <- NMC1%>%ungroup()%>%
  mutate(UID = paste0(case_type, row_number()))

lab_NMC <- NMC2 %>%filter(case_type == "Lab")# %>%
#  mutate(across(everything(), as.character))


clinical_NMC <- NMC2 %>%  filter(case_type == "Clinical")# %>%
#  mutate(across(everything(), as.character))
#exclude matching vars. 
names(NMC2)

exclude_vars<-names(NMC2)[c(1:17, 22:length(NMC2))]
exclude_vars

lab_tb_pairs <- compare.linkage(clinical_NMC,lab_NMC , exclude = c(exclude_vars_early, "UID"),
                                blockfld = c("condition", "months"),
                              strcmp = TRUE, 
                              strcmpfun = jarowinkler, )

lab_tb_weights <- epiWeights(lab_tb_pairs, 0 )

summary(epiClassify(lab_tb_weights, 0.9, 0.8 ))

summary(epiClassify(lab_tb_weights, 1, 0.90 ))

EDR_extract <- setDT(getPairs(lab_tb_weights, 1, 0.9, single.rows= FALSE)) %>%as_tibble()

EDR_extract$notification_date
EDR_extract %>%view
names(EDR_extract)

lab_tb_extract_FALSE <- EDR_extract %>%
  as_tibble() %>%
  filter(id != "") %>%
  mutate(uid = rep(seq_along(seq(1, nrow(.), by = 2)), each = 2))%>%
  mutate_all(str_trim)

lab_tb_extract_FALSE <- EDR_extract %>%
  as_tibble() %>%
  filter(id != "") %>%
  mutate(uid = rep(seq_along(seq(1, nrow(.), by = 2)), each = 2))%>%
  mutate_all(str_trim)

lab_tb_extract_FALSE%>%select(id, uid, everything())
# keep the duplicate with the most earliet date, and the same will be done for EDRWeb, this is a then a "first interaction" scenario. There are also other scenrios, a best case scenario, which would be the latest test and earliest appearance on EDR_web, or a last interaction scenria. I personally think that comparing those warrant a paper all on its own. 
lab_tb_extract_FALSE <- lab_tb_extract_FALSE %>% 
  arrange(uid, notification_date) %>% 
  group_by(uid) %>% 
  mutate(dup_number = row_number())%>%ungroup()

lab_tb_extract_FALSE%>%select(Weight, uid)
names(lab_tb_extract_FALSE)


#clever_fill(lab_tb_extract_FALSE, Weight, "Weight")%>%
#  select(Weight, uid)

merged_df <- lab_tb_extract_FALSE %>%
  group_by(uid) %>%
  fill(everything(), .direction = "down") %>%
  fill(everything(), .direction = "up") 

merged_df #%>% view()
merged_df$notification_date
# Now you need to take the old cases out of the original df. 
new_merged_uid <- merged_df$UID

# need to do something to preserve confirmed cases. 

# Now you have the final merged cases. 
final_merged <- merged_df %>%filter(case_type == "Clinical")%>%mutate(case_type = "Merged")

final_merged%>%view()
final_merged$notification_date

#Remove the UID from the original dataset. 
df_without_mergedUID <- NMC2 %>% filter(!UID %in% new_merged_uid)
nrow(NMC2)
nrow(df_without_mergedUID)

final_merged<-final_merged%>% mutate(across(contains("date"), as_date))
df_without_mergedUID<-df_without_mergedUID%>% mutate(across(contains("date"), as_date))

# then you can merge the final_merged, and the df_excluding the merged UID. 
final_full_fuzzy <- merge(final_merged,df_without_mergedUID, keep = T, all = T )# %>% as_tibble

final_full_fuzzy$notification_date

xtabs(~case_type, final_full_fuzzy)

return( list(final_full_fuzzy= final_full_fuzzy, final_full_exact = final_full_exact))

}


#excel_load <- read_excel("Cases_Export_20238124_1.xlsx")
#excel_load1<- NMC_merge_cases(excel_load)











