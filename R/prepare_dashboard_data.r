library(data.table)
library(arrow)
library(lubridate)
library(here)

# Function to safely log messages
log_msg <- function(...) {
  cat(paste0(..., "\n"), file = stdout())
}

# --- 1. Load Data ---
# Use tryCatch to load data, or use sample if file missing (for reproducibility/demo)
dt <- tryCatch({
  # Adjust path as necessary based on your actual data location
  path <- file.path(Sys.getenv("HOME"), "Desktop/SAFETP/CLA/NMC_database/master/new_master.feather")
  
  if (!file.exists(path)) {
      log_msg("File DOES NOT EXIST at: ", path)
      stop("File does not exist at path: ", path)
  } else {
      log_msg("File found at: ", path)
  }
  
  result <- as.data.table(arrow::read_feather(path))
  log_msg("Successfully loaded data with ", nrow(result), " rows.")
  result
  
}, error = function(e) {
  log_msg("Error loading data: ", e$message)
  log_msg("Generating dummy data for demonstration.")
  # Dummy data generation
  dates <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by="day")
  provinces <- c("EC", "FS", "GP", "KZN", "LP", "MP", "NC", "NW", "WC")
  conditions <- c("Measles", "Cholera", "Mpox", "Rabies")
  
  data.table(
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

    # D. Overview Indicators (Source & Timeliness)
    if ("case_type" %in% names(dt)) {
        # 1. Cases by Source (Month + Case Type) + Condition
        # Group by month for overview, by condition for detailed views if needed
        # We aggregate to (Condition, Case Type, Month)
        agg_source <- dt[, .(count = .N), keyby = .(condition, case_type, month = tryCatch(format(date, "%Y-%m-01"), error=function(e) NA))]
        agg_source <- agg_source[!is.na(month)]
        agg_source[, month := as.Date(month)]
    } else {
        agg_source <- NULL
    }

    if ("time_to_notification" %in% names(dt)) {
        # 2. Timeliness (Median days per Condition per Month)
        # Ensure time_to_notification is numeric
        dt[, time_val := suppressWarnings(as.numeric(time_to_notification))]
        
        agg_timeliness <- dt[!is.na(time_val), .(
            median_days = median(time_val, na.rm=TRUE),
            q25 = quantile(time_val, 0.25, na.rm=TRUE),
            q75 = quantile(time_val, 0.75, na.rm=TRUE),
            n_cases = .N
        ), keyby = .(condition, month = tryCatch(format(date, "%Y-%m-01"), error=function(e) NA))]
        
        agg_timeliness <- agg_timeliness[!is.na(month)]
        agg_timeliness[, month := as.Date(month)]
    } else {
        agg_timeliness <- NULL
    }

    # --- D. Linkage Opportunity Indicators ---

    # Luhn algorithm check for SA ID numbers (13-digit South African IDs)
    luhn_valid <- function(id_str) {
        # Returns TRUE if the string passes the Luhn check
        digits <- suppressWarnings(as.integer(strsplit(id_str, "")[[1]]))
        if (anyNA(digits) || length(digits) != 13L) return(FALSE)
        # Double every second digit from the right (0-indexed: positions 1,3,5,...)
        n <- length(digits)
        for (i in seq(n - 1, 1, by = -2)) {
            digits[i] <- digits[i] * 2L
            if (digits[i] > 9L) digits[i] <- digits[i] - 9L
        }
        sum(digits) %% 10L == 0L
    }

    # Vectorised wrapper
    luhn_vec <- function(x) {
        vapply(as.character(x), function(v) {
            if (is.na(v) || nchar(v) != 13L || grepl("[^0-9]", v)) FALSE
            else luhn_valid(v)
        }, logical(1L), USE.NAMES = FALSE)
    }

    # Helper to classify a field's completeness
    field_status <- function(vec) {
        # Returns a vector: "present" / "missing"
        ifelse(!is.na(vec) & trimws(as.character(vec)) != "" &
               !grepl("^[Uu]nknown$", trimws(as.character(vec))),
               "present", "missing")
    }

    # Build a per-record linkage summary
    lnk <- copy(dt)[, .(condition, prov_, date)]

    # SA ID number
    id_col    <- if ("patient_id_no" %in% names(dt)) dt[["patient_id_no"]] else rep(NA_character_, nrow(dt))
    lnk[, id_present   := field_status(id_col)]
    lnk[, id_len_ok    := ifelse(!is.na(id_col) & nchar(trimws(as.character(id_col))) == 13L &
                                     !grepl("[^0-9]", trimws(as.character(id_col))),
                                 "correct_length", "wrong_length")]
    lnk[, id_luhn_ok   := ifelse(luhn_vec(id_col), "luhn_pass", "luhn_fail")]

    # Hospital folder number
    folder_col <- if ("folder_no" %in% names(dt)) dt[["folder_no"]] else rep(NA_character_, nrow(dt))
    lnk[, folder_present := field_status(folder_col)]

    # MRN – check several plausible column names
    mrn_candidates <- c("mrn", "medical_record_number", "patient_mrn", "MRN")
    mrn_match      <- mrn_candidates[mrn_candidates %in% names(dt)]
    mrn_col_name   <- if (length(mrn_match) > 0L) mrn_match[1] else NA_character_
    mrn_col        <- if (!is.na(mrn_col_name)) dt[[mrn_col_name]] else rep(NA_character_, nrow(dt))
    lnk[, mrn_present := field_status(mrn_col)]

    lnk[, month := as.Date(format(date, "%Y-%m-01"))]

    # --- E1. Overall completeness by province & month ---
    agg_linkage_prov <- lnk[, .(
        n_total          = .N,
        n_id_present     = sum(id_present     == "present"),
        n_folder_present = sum(folder_present == "present"),
        n_mrn_present    = sum(mrn_present    == "present"),
        # ID quality sub-metrics (among those with any ID value)
        n_id_correct_len = sum(id_present == "present" & id_len_ok  == "correct_length"),
        n_id_luhn_pass   = sum(id_present == "present" & id_luhn_ok == "luhn_pass")
    ), keyby = .(prov_, month)]

    # --- E2. Overall completeness by condition & month ---
    agg_linkage_cond <- lnk[, .(
        n_total          = .N,
        n_id_present     = sum(id_present     == "present"),
        n_folder_present = sum(folder_present == "present"),
        n_mrn_present    = sum(mrn_present    == "present"),
        n_id_correct_len = sum(id_present == "present" & id_len_ok  == "correct_length"),
        n_id_luhn_pass   = sum(id_present == "present" & id_luhn_ok == "luhn_pass")
    ), keyby = .(condition, month)]

    # --- E3. ID number length distribution (for histogram) ---
    lnk[, id_length_val := nchar(trimws(as.character(id_col)))]
    agg_id_lengths <- lnk[id_present == "present", .(n = .N), keyby = .(id_length = id_length_val)]

    # Capture whether MRN column actually exists in data
    mrn_col_found <- !is.na(mrn_col_name)

    # --- 4. Save Processed Data ---
    out_dir <- here::here("data", "processed")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

    saveRDS(agg_national, file.path(out_dir, "agg_national.rds"))
    saveRDS(agg_province, file.path(out_dir, "agg_province.rds"))
    saveRDS(agg_district, file.path(out_dir, "agg_district.rds"))

    if (!is.null(agg_source))     saveRDS(agg_source, file.path(out_dir, "agg_source.rds"))
    if (!is.null(agg_timeliness)) saveRDS(agg_timeliness, file.path(out_dir, "agg_timeliness.rds"))

    saveRDS(agg_linkage_prov, file.path(out_dir, "agg_linkage_prov.rds"))
    saveRDS(agg_linkage_cond, file.path(out_dir, "agg_linkage_cond.rds"))
    saveRDS(agg_id_lengths,   file.path(out_dir, "agg_id_lengths.rds"))
    saveRDS(list(mrn_col_found = mrn_col_found, mrn_col_name = mrn_col_name),
            file.path(out_dir, "linkage_meta.rds"))

    log_msg("Data preparation complete. Files saved to ", out_dir)
} else {
    log_msg("Failed to create 'dt' object.")
}
