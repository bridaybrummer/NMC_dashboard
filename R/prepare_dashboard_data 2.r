
library(data.table)
library(arrow)
library(lubridate)

# Function to safely log messages
log_msg <- function(...) {
  cat(paste0(..., "\n"), file = stdout())
}

# --- 1. Load Data ---
# Use tryCatch to load data, or use sample if file missing (for reproducibility/demo)
tryCatch({
  # Adjust path as necessary based on your actual data location
  # Using absolute path based on user workspace info
  # path <- "/Users/briday/Desktop/SAFETP/CLA/NMC_database/master/new_master.feather"
  path <- file.path(Sys.getenv("HOME"), "Desktop/SAFETP/CLA/NMC_database/master/new_master.feather")
  
  if (!file.exists(path)) {
      log_msg("File DOES NOT EXIST at: ", path)
      stop("File does not exist at path: ", path)
  } else {
      log_msg("File found at: ", path)
  }
  
  dt <<- as.data.table(arrow::read_feather(path))
  log_msg("Successfully loaded data with ", nrow(dt), " rows.")
  
}, error = function(e) {
  log_msg("Error loading data: ", e$message)
  log_msg("Generating dummy data for demonstration.")
  # Dummy data generation
  dates <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by="day")
  provinces <- c("EC", "FS", "GP", "KZN", "LP", "MP", "NC", "NW", "WC")
  conditions <- c("Measles", "Cholera", "Mpox", "Rabies")
  
  dt <<- data.table( # use global assignment to ensure dt is available
    date = sample(dates, 10000, replace = TRUE),
    prov_ = sample(provinces, 10000, replace = TRUE),
    district = paste0("District_", sample(1:5, 10000, replace = TRUE)),
    condition = sample(conditions, 10000, replace = TRUE)
  )
})

# --- 2. Preprocessing ---
if(exists("dt")) {
    # Ensure dates are Date objects
    dt[, date := as.Date(date)]

    # Filter out future dates
    dt <- dt[date <= Sys.Date()]

    # Drop rows with missing province, condition, or date
    dt <- dt[!is.na(prov_) & !is.na(condition) & !is.na(date) & prov_ != ""]

    # Standardise province codes to known set
    valid_provs <- c("EC", "FS", "GP", "KZN", "LP", "MP", "NC", "NW", "WC")
    dt <- dt[prov_ %chin% valid_provs]

    log_msg("After cleaning: ", nrow(dt), " rows, ",
            uniqueN(dt$condition), " conditions, ",
            uniqueN(dt$prov_), " provinces")

    # --- 3. Aggregation ---

    # A. By Condition, Date (National)
    agg_national <- dt[, .(count = .N), keyby = .(condition, date)]
    setkey(agg_national, condition, date)
    all_dates_national <- CJ(
      condition = unique(dt$condition),
      date = seq(min(dt$date), max(dt$date), by = "day")
    )
    agg_national <- agg_national[all_dates_national, on = .(condition, date)]
    agg_national[is.na(count), count := 0L]
    agg_national[, `:=`(confirm = count, region = "National")]

    # B. By Condition, Province, Date
    agg_province <- dt[, .(count = .N), keyby = .(condition, prov_, date)]
    setkey(agg_province, condition, prov_, date)
    all_dates_prov <- CJ(
      condition = unique(dt$condition),
      prov_    = valid_provs,
      date     = seq(min(dt$date), max(dt$date), by = "day")
    )
    agg_province <- agg_province[all_dates_prov, on = .(condition, prov_, date)]
    agg_province[is.na(count), count := 0L]
    agg_province[, `:=`(confirm = count, region = prov_)]

    # C. By Condition, Province, District, Date (observed only — no CJ to keep small)
    agg_district <- dt[!is.na(district) & district != "",
                       .(count = .N), keyby = .(condition, prov_, district, date)]
    agg_district[, `:=`(confirm = count, region = district)]

    # Calculate Incidence (Placeholder Population)
    # In reality, load population data. Here strictly placeholder.
    pop_province <- data.table(
    prov_ = c("EC", "FS", "GP", "KZN", "LP", "MP", "NC", "NW", "WC"),
    pop = c(6700000, 2900000, 15500000, 11500000, 5900000, 4700000, 1300000, 4100000, 7100000)
    )

    agg_province <- merge(agg_province, pop_province, by = "prov_", all.x = TRUE)
    agg_province[, incidence_100k := (count / pop) * 100000]

    # --- 4. Save Processed Data ---
    out_dir <- "/Users/briday/Desktop/SAFETP/CLA/NMC_website/NMC_dashboard/data/processed"
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

    saveRDS(agg_national, file.path(out_dir, "agg_national.rds"))
    saveRDS(agg_province, file.path(out_dir, "agg_province.rds"))
    saveRDS(agg_district, file.path(out_dir, "agg_district.rds"))

    log_msg("Data preparation complete. Files saved to ", out_dir)
} else {
    log_msg("Failed to create 'dt' object.")
}
