# Time series signal detection methods 


# EARS 
    EARS <- function(df  ){
        svn_ma <- df$n %>% zoo::rollmean( ., k = 14, align = "centre") 
    }

create_lags_7d <- function(df) {
    for (lag_num in 1:14) {
        df <- df %>%
        mutate( 
            !!paste0("lag_", lag_num) := lag(n, lag_num)
        )
    }
    return(df)
    }

ears <- function(df) {
        return(
            df %>%
                create_lags_7d() %>%
                rowwise() %>%
                mutate(
                    std_t =  sd(c_across(lag_1:lag_14), na.rm = TRUE),
                    mean_t =  mean(c_across(lag_1:lag_14), na.rm = TRUE),
                    z_t =  (n - mean_t)/std_t,
                    is_ears_outbreak =  if_else(z_t > 3 , TRUE, FALSE),
                    ears_threshold =  mean_t + 2 * std_t
                )
        )
    }

# Cusum 
    cusum_own<- function(df) {
        y_hat <- mean(df$n, na.rm = TRUE)
        std <- sd(df$n, na.rm = TRUE)
        df$z_t <- (df$n - y_hat) / std
        
        k <- 0.5 
        cusums <- c()  # contains CUSUMs
        c_t_minus_1 <- 0 # first c_t_minus_1 must be set manually
        for (i in 1:length(df$z_t)) {
            c_t <- max(0, c_t_minus_1 + df$z_t[i] - k)
            cusums <- append(cusums, c_t)  # save result
            c_t_minus_1 <- c_t # current CUSUM is the next
        }
         
    # Control limits: Mean ± 2 standard deviations (adjust as needed)
        df$upper_limit <- y_hat + 2 * std
        df$lower_limit <- y_hat - 2 * std
    # cusum is the sum of the z_t values    
        df$cusums_threshold <- cusums

        df$is_cusums_outbreak <- if_else(cusums > 3, TRUE, FALSE)
        return(df)
    }


cusum <- function(
    df, 
    k = 0.5, 
    cusum_threshold = 10 
    ) {
    y_hat <- mean(df$n)
    s <- sd(df$n)
    df$z_t <- (df$n - y_hat) / s
    
    cusums = c()
    c_t_miuns_1 = 0 # first c_t_miuns_1 must be set manually
    
    for (i in 1:length(df$z_t)) {
        c_t <- max(0, c_t_miuns_1 + df$z_t[i] - k) # 0.5 is k 
        cusums <- append(cusums, ifelse(is.na(c_t), 0, c_t))
        c_t_miuns_1 <- c_t
    }
    df$cusums_threshold <- cusums
    df$is_cusums_outbreak <-  if_else(cusums > cusum_threshold, TRUE, FALSE)
    return(df)
}


library(progress)


cusum_window <- function(data, window = 180, date_index = "date") {
  if (nrow(data) == 0) {
        # Create a tibble (dataframe) with the desired column names and types
        empty_df <- tibble(
            year = factor(),
            month = factor(),
            epiweek = factor(),
            date = factor(),
            condition = character(),
            day = factor(),
            n = double(),
            cumulative = double(),
            roll_avg = double(),
            ci_upper = double(),
            ci_lower = double(),
            lag_1 = double(),
            lag_2 = double(),
            lag_3 = double(),
            lag_4 = double(),
            lag_5 = double(),
            lag_6 = double(),
            lag_7 = double(),
            lag_8 = double(),
            lag_9 = double(),
            lag_10 = double(),
            lag_11 = double(),
            lag_12 = double(),
            lag_13 = double(),
            lag_14 = double(),
            std_t = double(),
            mean_t = double(),
            z_t = double(),
            is_ears_outbreak = logical(),
            ears_threshold = double(),
            cusums_threshold = double(),
            is_cusums_outbreak = logical()
        )
        return(empty_df)
    }
    # Set the window size to 180.
    # This means the cusum uses the last 180 days. It avoids influence from past yearly seasons.

    total_iterations <- nrow(data) - window

    pb <- progress_bar$new(
        format = "Processing [:bar] :percent in :elapsed | ETA: :eta",
        total = total_iterations, clear = FALSE, width = 60
    )

    time_start <- Sys.time()  # Start timing

    system.time({
        df_with_ears <- data %>%
            filter(as_date(date) < as_date("2024-08-19")
            ) %>%
            #ears() %>%
            cusum(k = 0.5, cusum_threshold = 5)
    }) -> time_taken

    # Pre-allocate results list for efficiency
    results_list <- vector("list", length = total_iterations)

    for (i in 1:total_iterations) {
        # Progress bar update
        pb$tick()  # Update progress bar

        # Subset the data for the window
        subset_df_to_plot <- data[i:(i + window - 1), ]

        cusum_result <- subset_df_to_plot %>%
            ears() %>%
            cusum(k = 0.5, cusum_threshold = 5)

        latest <- tail(cusum_result, 1)

        # Store the relevant values in the results list
        results_list[[i]] <- data.frame(
            year = latest$year,
            month = latest$month,
            epiweek = latest$epiweek,
            date = latest$date,
            condition = latest$condition,
            day = latest$day,
            n = latest$n,
            iteration = i,
            latest_is_cusums_outbreak = latest$is_cusums_outbreak,
            latest_cusum_threshold = latest$cusums_threshold
        )
    }

    time_end <- Sys.time()  # End timing

    # Print total time taken
    cat("\nTotal time taken:", time_end - time_start, "seconds\n")

    # Combine the list into a data frame
    results_df <- do.call(rbind, results_list)

    return(results_df)
}

# Example Usage

#|
#|  df_to_plot %>% # from after the epicurve_df() 
#|    cusum_window(window = 180, date_index = "date") -> results_df_function

cusum_window_start <- function(data, window = 180, date_index = "date", start_date = NULL) {
    if (nrow(data) == 0) {
        # Create a tibble (dataframe) with the desired column names and types
        empty_df <- tibble(
            year = factor(),
            month = factor(),
            epiweek = factor(),
            date = factor(),
            condition = character(),
            day = factor(),
            n = double(),
            cumulative = double(),
            roll_avg = double(),
            ci_upper = double(),
            ci_lower = double(),
            lag_1 = double(),
            lag_2 = double(),
            lag_3 = double(),
            lag_4 = double(),
            lag_5 = double(),
            lag_6 = double(),
            lag_7 = double(),
            lag_8 = double(),
            lag_9 = double(),
            lag_10 = double(),
            lag_11 = double(),
            lag_12 = double(),
            lag_13 = double(),
            lag_14 = double(),
            std_t = double(),
            mean_t = double(),
            z_t = double(),
            is_ears_outbreak = logical(),
            ears_threshold = double(),
            cusums_threshold = double(),
            is_cusums_outbreak = logical()
        )
        return(empty_df)
    }

    # Convert start_date to Date type if provided
    if (!is.null(start_date)) {
        start_date <- as_date(start_date)
        data <- data %>% 
            filter(as_date(!!sym(date_index)) >= start_date-days( window))
    }

    # Validate that there is enough data for the specified window
    if (nrow(data) < window) {
        stop("Not enough data after the start_date for the specified window size.")
    }

    total_iterations <- nrow(data) - window

    pb <- progress_bar$new(
        format = "Processing [:bar] :percent in :elapsed | ETA: :eta",
        total = total_iterations, clear = FALSE, width = 60
    )

    time_start <- Sys.time()  # Start timing

    system.time({
        df_with_ears <- data %>%
            filter(as_date(!!sym(date_index)) < as_date("2024-08-19")) %>%
            cusum(k = 0.5, cusum_threshold = 5)
    }) -> time_taken

    # Pre-allocate results list for efficiency
    results_list <- vector("list", length = total_iterations)

    for (i in 1:total_iterations) {
        # Progress bar update
        pb$tick()  # Update progress bar

        # Subset the data for the window
        subset_df_to_plot <- data[i:(i + window - 1), ]

        cusum_result <- subset_df_to_plot %>%
            ears() %>%
            cusum(k = 0.5, cusum_threshold = 5)

        latest <- tail(cusum_result, 1)

        # Store the relevant values in the results list
        results_list[[i]] <- data.frame(
            year = latest$year,
            month = latest$month,
            epiweek = latest$epiweek,
            date = latest$date,
            condition = latest$condition,
            day = latest$day,
            n = latest$n,
            iteration = i,
            latest_is_cusums_outbreak = latest$is_cusums_outbreak,
            latest_cusum_threshold = latest$cusums_threshold
        )
    }

    time_end <- Sys.time()  # End timing

    # Print total time taken
    cat("\nTotal time taken:", time_end - time_start, "seconds\n")

    # Combine the list into a data frame
    results_df <- do.call(rbind, results_list)

    return(results_df)
}

# Example Usage
#df_to_plot$condition%>%unique
#df_to_plot%>%as.data.table()-> dt_to_plot

#| df_to_plot %>% # from after the epicurve_df()
#|    cusum_window_start(window = 180, date_index = "date", start_date = "2024-01-01") -> results_df_function

#dt_to_plot[condition %like% "Malaria"]%>% 

#cusum_window_start(window = 180, date_index = "date", start_date = "2024-11-01") -> results_df_function


#results_df_function

# GPT data.table version 

# Load necessary libraries
library(data.table)   # Fast data manipulation
library(lubridate)    # For date handling
library(progress)     # For a progress bar display

#########################################
## 1. EARS: Rolling Mean Calculation  ##
#########################################
# Optimized version using data.table's frollmean()
EARS <- function(dt) {
  # dt: a data.table with a numeric column 'n'
  # frollmean computes the rolling mean with window=14, centered
  dt[, svn_ma := frollmean(n, n = 14, align = "center", fill = NA)]
  return(dt)
}

##################################################
## 2. Create 7-to-14-Day Lags (Using shift)     ##
##################################################
create_lags_7d <- function(dt) {
  # dt: a data.table with a numeric column 'n'
  # The shift() function creates lagged versions of a column.
  # Here, we add 14 new columns: lag_1, lag_2, ..., lag_14.
  dt[, paste0("lag_", 1:14) := shift(n, n = 1:14, type = "lag")]
  return(dt)
}

###############################################
## 3. EARS Signal Detection (Row-wise stats) ##
###############################################
ears <- function(dt) {
  # Make sure dt is a data.table (if not, convert it)
  if (!is.data.table(dt)) setDT(dt)
  
  # Create lag columns (1 to 14 days)
  dt <- create_lags_7d(dt)
  
  # Compute the row-wise mean and standard deviation over lag columns.
  # Here we use .SD (Subset of Data) along with the pattern to select lag_* columns.
  dt[, `:=`(
    mean_t = apply(.SD, 1, mean, na.rm = TRUE),
    std_t  = apply(.SD, 1, sd, na.rm = TRUE)
  ), .SDcols = patterns("^lag_")]
  
  # Calculate the z-score for the current day's value versus the lag window
  dt[, z_t := (n - mean_t) / std_t]
  
  # Define outbreak detection and threshold criteria:
  # An outbreak is flagged if z_t exceeds 3.
  dt[, is_ears_outbreak := z_t > 3]
  
  # Calculate the EARS threshold (mean plus twice the standard deviation)
  dt[, ears_threshold := mean_t + 2 * std_t]
  
  return(dt)
}

##########################################
## 4. CUSUM Signal Detection            ##
##########################################
cusum <- function(dt, k = 0.5, cusum_threshold = 10) {
  # dt: a data.table with numeric column 'n'
  # k: slack parameter; cusum_threshold: outbreak threshold for the cumulative sum
  if (!is.data.table(dt)) setDT(dt)
  
  # Calculate the overall mean and standard deviation for 'n'
  y_hat <- dt[, mean(n, na.rm = TRUE)]
  s     <- dt[, sd(n, na.rm = TRUE)]
  
  # Compute standardized values (z-scores)
  dt[, z_t := (n - y_hat) / s]
  
  # Pre-allocate a numeric vector for CUSUM values
  n_rows <- nrow(dt)
  cusums <- numeric(n_rows)
  c_t_prev <- 0  # Initialize the previous CUSUM value
  
  # Recursive computation of the CUSUM values
  for (i in seq_len(n_rows)) {
    # Calculate the current cumulative sum: reset to 0 if negative
    c_t <- max(0, c_t_prev + dt$z_t[i] - k)
    # In case of NA, we force the value to 0
    cusums[i] <- ifelse(is.na(c_t), 0, c_t)
    c_t_prev <- c_t  # Update for the next iteration
  }
  
  # Add the computed CUSUM values to the data.table
  dt[, cusums_threshold := cusums]
  
  # Flag an outbreak if the CUSUM exceeds the specified threshold
  dt[, is_cusums_outbreak := cusums > cusum_threshold]
  
  return(dt)
}

####################################################################
## 5. Sliding Window CUSUM (for a given window over the data)     ##
####################################################################
cusum_window <- function(data, window = 180, date_index = "date") {
  # Convert input to data.table for speed
  dt <- as.data.table(data)
  
  # If the data is empty, return an empty data.table with desired columns
  if (nrow(dt) == 0) {
    empty_dt <- data.table(
      year = factor(), month = factor(), epiweek = factor(),
      date = as.Date(character()), condition = character(), day = factor(),
      n = numeric(), iteration = integer(),
      latest_is_cusums_outbreak = logical(), latest_cusum_threshold = numeric()
    )
    return(empty_dt)
  }
  
  # Number of iterations (sliding windows)
  total_iterations <- nrow(dt) - window
  
  # Create a progress bar for user feedback
  pb <- progress_bar$new(
    format = "Processing [:bar] :percent in :elapsed | ETA: :eta",
    total = total_iterations, clear = FALSE, width = 60
  )
  
  time_start <- Sys.time()  # Start timing
  
  # (Optional) Preprocess the full data if needed.
  # For example, you can filter by date:
  # dt <- dt[as.Date(get(date_index)) < as.Date("2024-08-19")]
  # And/or run an initial CUSUM calculation on the entire dataset:
  dt <- cusum(dt, k = 0.5, cusum_threshold = 5)
  
  # Pre-allocate a list to store results from each window
  results_list <- vector("list", total_iterations)
  
  # Loop over each sliding window
  for (i in 1:total_iterations) {
    pb$tick()  # Update the progress bar
    
    # Subset the data for the current window
    window_dt <- dt[i:(i + window - 1)]
    
    # Work on a copy of the subset to avoid modifying the original
    window_dt <- copy(window_dt)
    
    # Apply the EARS and CUSUM functions to the window
    window_dt <- ears(window_dt)
    window_dt <- cusum(window_dt, k = 0.5, cusum_threshold = 5)
    
    # Extract the latest (i.e. most recent) row from the window
    latest <- window_dt[.N]
    
    # Store selected values (you can add more columns as needed)
    results_list[[i]] <- data.table(
      year = latest$year,
      month = latest$month,
      epiweek = latest$epiweek,
      date = latest$date,
      condition = latest$condition,
      day = latest$day,
      n = latest$n,
      iteration = i,
      latest_is_cusums_outbreak = latest$is_cusums_outbreak,
      latest_cusum_threshold = latest$cusums_threshold
    )
  }
  
  time_end <- Sys.time()  # End timing
  cat("\nTotal time taken:", difftime(time_end, time_start, units = "secs"), "seconds\n")
  
  # Combine all window results into one data.table
  results_dt <- rbindlist(results_list)
  return(results_dt)
}

####################################################################
## 6. Sliding Window CUSUM with Start Date Filter               ##
####################################################################
cusum_window_start <- function(data, window = 180, date_index = "date", start_date = NULL) {
  # Convert input to data.table
  dt <- as.data.table(data)
  
  # Return an empty data.table if input is empty
  if (nrow(dt) == 0) {
    empty_dt <- data.table(
      year = factor(), month = factor(), epiweek = factor(),
      date = as.Date(character()), condition = character(), day = factor(),
      n = numeric(), iteration = integer(),
      latest_is_cusums_outbreak = logical(), latest_cusum_threshold = numeric()
    )
    return(empty_dt)
  }
  
  # If a start_date is provided, filter the data.
  if (!is.null(start_date)) {
    start_date <- as.Date(start_date)
    # Keep rows with date greater than or equal to (start_date - window)
    dt <- dt[as.Date(get(date_index)) >= (start_date - window)]
  }
  
  # Ensure there is enough data for at least one sliding window
  if (nrow(dt) < window) {
    stop("Not enough data after the start_date for the specified window size.")
  }
  
  total_iterations <- nrow(dt) - window
  
  pb <- progress_bar$new(
    format = "Processing [:bar] :percent in :elapsed | ETA: :eta",
    total = total_iterations, clear = FALSE, width = 60
  )
  
  time_start <- Sys.time()
  
  # Optionally, preprocess the full dataset with CUSUM before sliding window analysis
  dt <- cusum(dt, k = 0.5, cusum_threshold = 5)
  
  results_list <- vector("list", total_iterations)
  
  for (i in 1:total_iterations) {
    pb$tick()
    window_dt <- dt[i:(i + window - 1)]
    window_dt <- copy(window_dt)
    window_dt <- ears(window_dt)
    window_dt <- cusum(window_dt, k = 0.5, cusum_threshold = 5)
    
    latest <- window_dt[.N]
    
    results_list[[i]] <- data.table(
      year = latest$year,
      month = latest$month,
      epiweek = latest$epiweek,
      date = latest$date,
      condition = latest$condition,
      day = latest$day,
      n = latest$n,
      iteration = i,
      latest_is_cusums_outbreak = latest$is_cusums_outbreak,
      latest_cusum_threshold = latest$cusums_threshold
    )
  }
  
  time_end <- Sys.time()
  cat("\nTotal time taken:", difftime(time_end, time_start, units = "secs"), "seconds\n")
  
  results_dt <- rbindlist(results_list)
  return(results_dt)
}

######################################
## Example Usage of the Functions   ##
######################################
# Assuming your data.table (or data.frame) is named `df_to_plot`
# You can run the sliding window CUSUM analysis like this:
# results_dt <- cusum_window(df_to_plot, window = 180, date_index = "date")
# results_dt_start <- cusum_window_start(df_to_plot, window = 180, date_index = "date", start_date = "2024-01-01")

#source("scripts_and_functions/cusum_example.r")