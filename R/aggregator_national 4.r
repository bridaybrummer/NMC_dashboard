# aggregated new_master for national CUSUM detection and incidence calcs 
library(NMCleaner)


#source( "R/data_import.r" )

if( file.exists( "R/data_import.r" ) ) {
  source( "R/data_import.r" )
} else if( file.exists( "../R/data_import.r" ) ) {
  source( "../R/data_import.r" )
}

library(data.table)

# ensure new_master is a data.table
if (!data.table::is.data.table(new_master)) setDT(new_master)

# aggregate counts by prov_, condition, date, year (result is keyed)
agg_dt <- new_master[, .(n = .N), keyby = .(prov_, condition, date, year)]

#new_master[, .(n = .N), by = .(prov_, condition, date, year)]-> agg_dt

