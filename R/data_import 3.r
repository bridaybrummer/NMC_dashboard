# imports new_master from the NMC_database project 
# may need to ignore these when sednign to github 

# read in the new_master.dta 

library(arrow)

new_master <- arrow::read_feather("~/Desktop/SAFETP/CLA/NMC_database/master/new_master.feather")
