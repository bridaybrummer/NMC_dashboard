# this is just the start of RmD docuemtns for the july report 
library(readxl)
library(officer)
library(flextable)
library(gtsummary)
library(tidyverse)
library(magrittr)
library(data.table)

as_paragraph(" 
             -Susan has mentioned that she would like us to tell more of a story with the data, 
             
             The baseline is now done but we can perform adhoc analyses. 
             
             These can include maps, graphs, tables, narratives etc. 
             
             For instance, one thing we should look at is what is the highest proportion of notified disease per age group.
             
             Other things might just be things of PH importance such as congenital syphillis. ")



#excel_load <- read_excel("NMC_June2023.xlsx")
#source("stata_2_script.R")
#load(paste0(reporting_date_month,"_NMC_cleaned.rda")) #load dup_data_32

#load("~/Desktop/SAFETP/CLA/17.NMC_reporting/NMC_reporting/data_dup32.rda")

excel_load <- read_excel("Cases_Export_20238124_1.xlsx")

source("stata_2_script.R")

data_dup32<- stata2script(excel_load)


data_dup32
data_dup32$agecategory%>%unique()
data_dup32$agecategory %>%unique()
xtabs(~ data_dup32$agecategory)
xtabs(~ data_dup32$agecategory_unit)

df<- data_dup32%>%mutate( 'reporting' = 1)%>%
  filter(as_month(data_dup32$notification_date) %in% as_month(Sys.Date() - months(1)))

df

df1<- df%>%filter(!Back_capture %in% "Back capture")

df1$agecategory%>%unique

df2<- df%>%filter( Back_capture %in% "Back capture")


#---- July report, exploratory analysis framework ----

# let us see the most common diseases per province and per age group.

# let us also do incidence with population estimates to compare between provinces more accurately. 
