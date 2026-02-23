# PURPOSE
  # Weekly report 
  # Monthly report 
  # Disease of the month 
  # Poisoning report


# re download NMCleaner 
remove.packages("NMCleaner")
# remove the paage different 

# remove cached version 
# remove all the files in the NMCleaner directory

pak::pkg_install("bridaybrummer/NMCleaner", upgrade = TRUE)
remotes::install_github("bridaybrummer/NMCleaner", force = TRUE)
if (!require("devtools")) install.packages("devtools")
options(timeout = 500) # increase download time to avoid faiure
devtools::install_github("bridaybrummer/NMCleaner", force = TRUE)

library(NMCleaner)
NMCleaner::pop -> pop_df
setDT(pop_df)
sum(pop_df[prov=="GP"&Year==2025, ]$Population)

NMCleaner::pop_old_from_pepfar


condition_df%>%dplyr::filter( nmccategories ==1 )
condition_df%>%dplyr::filter( grepl("agri", ignore.case = TRUE, condition) )
# Load libraries
  pacman::p_load( tidyverse, magrittr, data.table, janitor, pandoc)
  
  conflicted::conflicts_prefer(dplyr::filter)

  
list.files("~/Desktop/SAFETP/CLA/NMC_database", pattern = ".rda")

load("~/Desktop/SAFETP/CLA/NMC_database/database_scraper_database.rda")

database_scraper_database%>% arrange( - as_date(export_date))

#| for installing info see https://github.com/bridaybrummer/NMCleaner


# may have to run the command again and it should run wihtout errors
  library(NMCleaner)


# This is a function to run the weekly report with stadnard namign procedures
    run_weekly_reportV2<- function( ) {
      conflicted::conflicts_prefer(dplyr::filter)
    
      n_files<- length(list.files("final_reports"))
      load( "~/Desktop/SAFETP/CLA/NMC_database/database_scraper_database.rda")
      
      file_name<- database_scraper_database[1,]$file_name
      
      system(paste0("mkdir final_reports/",file_name))
     
      rmarkdown::render(input = "weekly_IDSR_V3.Rmd", output_file = paste0("weekly_IDSR_V3.docx"))
      #system(paste0("mv weekly_IDSR.docx ",file_name,".docx")) # already gets created when you send the linelist there. 
      system(paste0("mv weekly_IDSR_V3.docx  final_reports/",file_name,"/" , file_name,".docx" ))
      
      #save(weekly_linelist_exports, file = "weekly_linelist_exports.rda")
      print(paste0( "Looks like this was a success?\n
      Output will be in the directory final_reports/", file_name))
      #return(weekly_linelist_exports)
    }
###############

# Run Weekly report    

###############

  run_weekly_reportV2()
    # Add something around hosptialisations. 
    ## the proportion of notificatiosn that have hosptialisations. The proportions of conditions among those hospitalised. 

# changeing pandoc version 
     source("reset_pandoc.r")
rmarkdown::find_pandoc()
??flextable
rmarkdown::pandoc_version()
devtools::install_github("davidgohel/flextable", force = TRUE)
  # cehck version of flextable
  packageVersion("flextable")
  packageVersion("gtsummary")
  packageVersion("quarto")
  packageVersion("rmarkdown")

library(NMCleaner)

#####################

# Run monthly reports loops

#####################

    months_of_2024<- c(paste0("2024-", 1:12, "-01"))%>%as_date()
    conflicted::conflicts_prefer(dplyr::filter)
    reporting_month<- months_of_2024[2:5]
    save(reporting_month, file="reporting_month.rda" )

        for (i in 12:12) {
          reporting_month <- months_of_2024[i]
          save(reporting_month, file = "reporting_month.rda")
          # render the report
          rmarkdown::render("MONTHLY_REPORT.rmd", output_file = paste0("Monthly_reports_2024/NMCSS_report", months_of_2024[i], ".docx"))
        }
        
library(NMCleaner)
 install.packages("rmarkdown")
  install.packages("quarto")
 library(quarto)
 library(rmarkdown)
 pandoc_available()
 pandoc_version()
 Sys.setenv(RSTUDIO_PANDOC = "/opt/homebrew/bin/pandoc")


     months_of_2025 <- c(paste0("2025-", 1:12, "-01")) %>% as_date()
     conflicted::conflicts_prefer(dplyr::filter)
     reporting_month <- months_of_2025[6]
     save(reporting_month, file = "reporting_month.rda")
      
    # Run the monthly report for the ith months of the year   
    for (i in 10){
            reporting_month <- months_of_2025[i]
            save(reporting_month, file = "reporting_month.rda" )
            # render the report
            rmarkdown::render("MONTHLY_REPORT.rmd", output_file = paste0("Monthly_reports_2024/NMCSS_report", months_of_2025[i], ".docx"))
          }

for (i in seq_along(months_of_2025)) {
  reporting_month <- months_of_2025[i]
  save( reporting_month, file = "reporting_month.rda")
  output.file = paste0("Monthly_reports_2025/NMCSS_report_", reporting_month, ".html")
  # Render the Quarto report with the params$month object
  quarto::quarto_render(
    input = "monthly_report_abridged.qmd",
    #output_file = output.file,
    #execute_params = list(month = reporting_month)
  )
}

    for (i in 10) {
      reporting_month <- months_of_2025[i]
      save(reporting_month, file = "reporting_month.rda")
      output_file_name <- paste0("NMCSS_report_", months_of_2025[i], ".docx")
      # render the report using Quarto
      quarto::quarto_render("MONTHLY_REPORT.qmd",
       #output_file = output_file_name,
       #execute_params = list(month = reporting_month)
      )
      out_put_dir <- "Monthly_reports_2025"
      if(!dir.exists(out_put_dir)) {
        dir.create(out_put_dir, recursive = TRUE)
      }
    #  system(paste0("mv ", output_file_name, " ", out_put_dir))
    }

reporting_month <- "2025-06-01"
save(reporting_month, file = "reporting_month.rda")



##################

# Disease of the month 

##################
condition_df$condition[grepl("sch", condition_df$condition, ignore.case = TRUE)]
  disease_of_the_month <- "Agricultural or stock remedy poisoning"
    disease_of_the_month <- "Bilharzia (schistosomiasis)"

    save(disease_of_the_month, file = "disease_of_the_month.rda")
    rmarkdown::render("Disease_of_the_Month.Rmd", output_file = paste0("Disease_of_the_month/Disease_of_the_month_", disease_of_the_month, ".docx"))
    

      #run multiple diseases 
          condition_df$condition[grepl( "food", condition_df$condition ,ignore.case =TRUE)] 
          
          diseases <- c(
            "Agricultural or stock remedy poisoning", 
          "Food borne illness outbreak")

      Sys.Date() %>% format( "%d_%b_%Y") -> date_label 

      for (disease in diseases) {
          disease_of_the_month <- disease
          save(disease_of_the_month, file = "disease_of_the_month.rda")
        rmarkdown::render("Disease_of_the_Month.Rmd", output_file = paste0("Disease_of_the_month/Disease_of_the_month_", disease_of_the_month,"_", date_label,  ".docx"))
      }


##############

# Poisoning report 

##############
library(NMCleaner)
pacman::p_load(quarto)

directory_destination <- "poisoning_reports"
html_name_with_date <- paste0( "Poisoning_Facilities_", Sys.Date() %>% format( "%d_%b_%Y"), ".html")
quarto::quarto_render(input ="Poisoning_Facilities_V2.qmd", output_file =html_name_with_date)
#move html to destination diretory 
system( paste0("mv ", html_name_with_date, " ", directory_destination))
# Define the file to zip
file_to_zip <- html_name_with_date # Replace with your file path
filt_to_zip_with_destination <- paste0(directory_destination, "/", file_to_zip)
zip_file <- paste0(directory_destination, "/Poisoning_Facilities_", Sys.Date() %>% format( "%d_%b_%Y"), ".zip") # Replace with your zip file path
password <- "p0!son2024"  # Replace with your password

# Use system command to zip with password
system(paste0("zip -P ", password, " ", zip_file, " ", filt_to_zip_with_destination))

cat("File zipped and password-protected as", zip_file)
list.files(directory_destination)

# show mtime of files 
list.files(directory_destination, full.names = TRUE) %>% file.info() %>%arrange(  mtime)


source("auto_emailer.r")
   # to make this into a fucntion to take argumetns 
   #  to = 
   # cc = 
   # subject =
   # contents = 
   # attachment = 
   # implement for the poisoning report and provincial report 



###############

# Foodborne report 

###############



directory_destination <- "poisoning_reports"
html_name_with_date <- paste0( "Poisoning_Facilities_", Sys.Date() %>% format( "%d_%b_%Y"), ".html")
quarto::quarto_render(input ="Poisoning_Facilities.qmd", output_file =html_name_with_date)
#move html to destination diretory 
system( paste0("mv ", html_name_with_date, " ", directory_destination))
# Define the file to zip
file_to_zip <- html_name_with_date # Replace with your file path
filt_to_zip_with_destination <- paste0(directory_destination, "/", file_to_zip)
zip_file <- paste0(directory_destination, "/Poisoning_Facilities_", Sys.Date() %>% format( "%d_%b_%Y"), ".zip") # Replace with your zip file path
password <- "p0!son2024"  # Replace with your password

# Use system command to zip with password
system(paste0("zip -P ", password, " ", zip_file, " ", filt_to_zip_with_destination))

cat("File zipped and password-protected as", zip_file)
list.files(directory_destination)



#############

# Facility Report 

#############

facility_names<- new_master$facility[grepl( "waterf", new_master$facility ,ignore.case =TRUE)] %>%unique
print(facility_names)

# create single strings for all the owrds exisint in the facility names

facility_names %>% str_extract( "\\w+") %>%tabyl()%>%arrange(- n)%>%slice( 1)%>%select( 1 )%>%pull()-> common_name

facility_names-> facility_selection 

save( facility_selection, file = "facility_selection.rda")
Sys.Date() %>% format( "%d_%b_%Y") -> date_label 

rmarkdown::render("facility_overview.qmd", output_file = paste0("facility_overviews/facility_overview_", common_name,"_", date_label,  ".html"))





#############

# Rough working 

#############
new_master %>% filter( condition %in%"Covid-19")%>%
mutate_dates( date_index= "notification_date")%>%
group_by(year, month) %>%
summarise( n = n())%>%print( n =100)


# Check meningo CIF and what source they are notified on
new_master %>% filter( condition %in%"Meningococcal disease")%>%
  filter( year %in% "2024")%>%
  select( case_source, case_type)%>%
tbl_summary(by = case_type)

#Check the case_types in certain month 
paste0("2024-",  month.abb)
new_master %>%
  filter(year %in% 2024) %>%
  select( 
    Month_notification, case_type
  )%>%
  mutate(
     Month_notification = 
     factor(
      Month_notification, levels = paste0("2024-",  month.abb)
      )
      )%>%
  tbl_summary(by = case_type)%>%
  add_overall()

new_master %>%
  filter( condition %in% "Malaria")%>%
  select( case_type)%>%
  tbl_summary( )

# Check whether symptoms for mpox are reflecting 


new_master  %>%filter( condition %in% "Mpox")%>%
select( symptoms) %>% pull%>%tabyl() 


new_master %>% make_fever_rash() %>%
filter( condition %in%"Fever-Rash")%>%
#distinct( case_id, .keep_all = TRUE)%>%
group_by( notification_date, case_type)%>%
summarise(n = n() ) %>% 
ggplot( )+
geom_col(aes( x = notification_date, y = n, color = case_type))

# Check 
new_master %>%
  filter( Month_notification %in% "2024-Oct")%>%
  select( condition, case_type)%>%
  tbl_summary( 
    by = case_type
  )


# Interview questions
  new_master %>% 
    group_by( 
      prov_, Month_notification, nmccategories
    )%>%
    summarise( n_notifications  = sum( n(), na.rm = TRUE))%>%
    group_by( prov_, nmccategories) %>%
    summarise( 
        mean_notifications = mean( n_notifications)
    )%>%
    filter( prov_ %in% c("GP", "NW"))%>%
    print( n = 100)

new_master$notification_date%>%as_date() %>%summary()
new_master %>% 
filter( condition %in% "Cholera" ,grepl( "lab|merge", ignore.case = TRUE, case_type )
)%>%view()



# PROVINCIAL REPORT ------------------------------------------------------------------------------------------------------------


                  library(NMCleaner)

                  system("mkdir provincial_reports")
                  # make a good filename that can be arranged by months 
                  this_month <- Sys.Date() 
                  # this_month <- as_date("2025-01-01") # can adjust this if necessary 

                  html_name_with_date <- paste0( "Month_",this_month%>%format("%m"),"_Provincial_Report_Created_", Sys.Date() %>% format( "%d_%b_%Y") , ".html")
                  html_name_with_date
                  directory_destination <- "provincial_reports"
                  # province_selection 
                  province_selection <- c("KZN") # to implement later, perhaps in a function 

                  quarto::quarto_render(input ="Province_report_to_loop.qmd", output_file =html_name_with_date)
                  
                  #move html to destination diretory 
                  system( paste0("mv ", html_name_with_date, " ", directory_destination))
                  # Define the file to zip
                  #file_to_zip <- html_name_with_date # Replace with your file path
                  #filt_to_zip_with_destination <- paste0(directory_destination, "/", file_to_zip)
                  #zip_file <- paste0(directory_destination, "/Poisoning_Facilities_", Sys.Date() %>% format( "%d_%b_%Y"), ".zip") # Replace with your zip file path
                  #password <- "p0!son2024"  # Replace with your password

                  # Use system command to zip with password
                  #system(paste0("zip -P ", password, " ", zip_file, " ", filt_to_zip_with_destination))

                  #cat("File zipped and password-protected as", zip_file)
                  list.files(directory_destination)

                  # show mtime of files 
                  list.files(directory_destination, full.names = TRUE) %>% file.info() 



# Annual REPORT ------------------------------------------------------------------------------------------------------------
