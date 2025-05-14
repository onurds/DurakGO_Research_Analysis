library(tidyverse)   # For data manipulation and visualization
library(psych)       # For descriptive statistics
library(car)         # For statistical tests
library(effectsize)  # For effect sizes
library(emmeans)     # For estimated marginal means
library(broom)       # For tidying model outputs
library(kableExtra)  # For formatted tables

# Set global options
options(scipen = 999)  # Avoid scientific notation

# Set up output capture to both console and file
output_file <- "Analysis/output.txt"
# Create a connection to the output file
output_conn <- file(output_file, "w")
# Redirect output to both console and file
sink(output_conn, split = TRUE)

#------------------------------------------------------------------------------
# DATA PREPARATION
#------------------------------------------------------------------------------

# Function to read and clean the survey data
prepare_survey_data <- function(file_path) {
  # First, check if the file exists
  if (!file.exists(file_path)) {
    message("File not found: ", file_path)
    stop("File not found: ", file_path)
  }

  # Read the CSV file
  message("Reading file: ", file_path)
  survey_data <- read.csv(file_path, stringsAsFactors = FALSE,
                          fileEncoding = "UTF-8")

  message("Number of rows read: ", nrow(survey_data))

  # Function to parse multiple-choice responses - FIXED VERSION
  parse_multiple_choice <- function(x) {
    if (is.na(x) || x == "") return(NULL)

    # Extract numbers from strings like "3_1,2,3"
    # First, remove the prefix pattern (e.g., "3_")
    cleaned <- gsub("^[0-9]+_", "", x)

    # Then split by comma and convert to numeric
    if (cleaned == "") return(NULL)

    choices <- as.numeric(unlist(strsplit(cleaned, ",")))
    return(choices)
  }

  # Process multiple-choice columns (the ones with "_" pattern)
  multiple_choice_cols <- c("USE_3", "USE_4", "WAIT_5", "SAT_2",
                            "SAT_3", "SAT_4", "DEM_3", "DEM_4")

  # Loop through each multiple-choice column
  for (col in multiple_choice_cols) {
    if (col %in% names(survey_data)) {
      # Determine max number of options based on data dictionary
      max_options <- switch(
        col,
        "USE_3" = 5,   # 5 options for when they use the app
        "USE_4" = 5,   # 5 options for features used
        "WAIT_5" = 6,  # 6 options for actions during long waits
        "SAT_2" = 7,   # 7 options for aspects liked about DurakGO
        "SAT_3" = 7,   # 7 options for improvement areas
        "SAT_4" = 5,   # 5 options for features to add
        "DEM_3" = 4,   # 4 options for lines used
        "DEM_4" = 4    # 4 options for travel purposes
      )

      # Create indicator variables - FIXED APPROACH
      for (i in 1:max_options) {
        new_col <- paste0(col, "_", i)

        # Process each row individually to avoid dimension mismatch
        survey_data[[new_col]] <- logical(nrow(survey_data))  # Initialize with FALSE

        # For each row in the data
        for (row in seq_len(nrow(survey_data))) {
          choices <- parse_multiple_choice(survey_data[row, col])
          survey_data[row, new_col] <- !is.null(choices) && i %in% choices
        }
      }
    }
  }

  # Convert wait time categories to actual minutes (midpoint of range)
  wait_time_midpoints <- c(1.5, 4, 8, 13, 20)  # Midpoints of the wait time ranges

  # Safe indexing function
  get_midpoint <- function(index) {
    if (is.na(index) || index < 1 || index > length(wait_time_midpoints)) {
      return(NA)
    }
    return(wait_time_midpoints[index])
  }

  # Apply the safe function
  survey_data$WAIT_1_minutes <- sapply(survey_data$WAIT_1, get_midpoint)
  survey_data$WAIT_2_minutes <- sapply(survey_data$WAIT_2, get_midpoint)

  # Calculate wait time difference
  survey_data$wait_time_diff <- survey_data$WAIT_1_minutes - survey_data$WAIT_2_minutes

  # Print column names for verification
  message("Columns in processed data: ", paste(names(survey_data), collapse=", "))

  return(survey_data)
}

# Load the data
file_path <- "Analysis/Survey_Data/final_data.csv"
survey_data <- prepare_survey_data(file_path)

# Print dataset summary
message("Data loaded: ", nrow(survey_data), " rows and ", ncol(survey_data), " columns")
message("First few column names: ", paste(head(names(survey_data)), collapse=", "))

#------------------------------------------------------------------------------
# TABLE 1: RESPONDENT DEMOGRAPHICS AND TRAVEL HABITS
#------------------------------------------------------------------------------

generate_demographics_table <- function(data) {
  # Age distribution
  age_dist <- table(data$DEM_5)
  age_pct <- prop.table(age_dist) * 100

  age_labels <- c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64",
                  "65 and over", "Prefer not to say")

  age_table <- data.frame(
    Category = "Age Group",
    Group = age_labels[seq_along(age_dist)],
    Count = as.numeric(age_dist),
    Percentage = round(as.numeric(age_pct), 1)
  )

  # Gender distribution
  gender_dist <- table(data$DEM_6)
  gender_pct <- prop.table(gender_dist) * 100

  gender_labels <- c("Male", "Female", "Prefer not to say", "Other")

  gender_table <- data.frame(
    Category = "Gender",
    Group = gender_labels[seq_along(gender_dist)],
    Count = as.numeric(gender_dist),
    Percentage = round(as.numeric(gender_pct), 1)
  )

  # Frequency of metro usage
  metro_freq_dist <- table(data$DEM_1)
  metro_freq_pct <- prop.table(metro_freq_dist) * 100

  metro_freq_labels <- c("5+ days per week", "2-4 days per week", "Once a week",
                         "1-3 times per month", "Less than once a month")

  metro_freq_table <- data.frame(
    Category = "Metro Usage Frequency",
    Group = metro_freq_labels[seq_along(metro_freq_dist)],
    Count = as.numeric(metro_freq_dist),
    Percentage = round(as.numeric(metro_freq_pct), 1)
  )

  # Primary mode of transportation
  primary_mode_dist <- table(data$DEM_2)
  primary_mode_pct <- prop.table(primary_mode_dist) * 100

  primary_mode_labels <- c("Metro/Rail System", "Bus", "Private vehicle",
                           "Taxi", "Walking", "Bicycle")

  primary_mode_table <- data.frame(
    Category = "Primary Mode of Transport",
    Group = primary_mode_labels[seq_along(primary_mode_dist)],
    Count = as.numeric(primary_mode_dist),
    Percentage = round(as.numeric(primary_mode_pct), 1)
  )

  # Most frequently used lines
  lines_used <- c(
    sum(data$DEM_3_1, na.rm = TRUE),
    sum(data$DEM_3_2, na.rm = TRUE),
    sum(data$DEM_3_3, na.rm = TRUE),
    sum(data$DEM_3_4, na.rm = TRUE)
  )

  lines_pct <- lines_used / nrow(data) * 100

  lines_labels <- c("Ankaray (A1)", "Sincan-Çayyolu (M1)",
                    "Keçiören Metro (M4)", "Başkentray (B1)")

  lines_table <- data.frame(
    Category = "Lines Used",
    Group = lines_labels,
    Count = lines_used,
    Percentage = round(lines_pct, 1)
  )

  # Typical trip purpose
  trip_purpose <- c(
    sum(data$DEM_4_1, na.rm = TRUE),
    sum(data$DEM_4_2, na.rm = TRUE),
    sum(data$DEM_4_3, na.rm = TRUE),
    sum(data$DEM_4_4, na.rm = TRUE)
  )

  trip_purpose_pct <- trip_purpose / nrow(data) * 100

  trip_purpose_labels <- c("Work commute", "School/University commute",
                           "Shopping", "Social/Entertainment activities")

  trip_purpose_table <- data.frame(
    Category = "Trip Purpose",
    Group = trip_purpose_labels,
    Count = trip_purpose,
    Percentage = round(trip_purpose_pct, 1)
  )

  # Create three subtables as per guidelines
  # Table 1.A: Demographics (Age and Gender)
  table1a <- rbind(
    age_table,
    gender_table
  )

  # Table 1.B: Transportation Patterns (usage frequency and primary mode)
  table1b <- rbind(
    metro_freq_table,
    primary_mode_table
  )

  # Table 1.C: Transit Lines and Trip Purposes
  table1c <- rbind(
    lines_table,
    trip_purpose_table
  )

  # Also create the combined table for backward compatibility
  demographics_table <- rbind(
    age_table,
    gender_table,
    metro_freq_table,
    primary_mode_table,
    lines_table,
    trip_purpose_table
  )

  # Return a list containing all tables
  return(list(
    combined = demographics_table,
    table1a = table1a,
    table1b = table1b,
    table1c = table1c
  ))
}

# Generate Table 1 (now split into subtables)
table1_results <- generate_demographics_table(survey_data)
table1 <- table1_results$combined
table1a <- table1_results$table1a
table1b <- table1_results$table1b
table1c <- table1_results$table1c

message("Table 1 generated with ", nrow(table1), " rows")
message("Table 1.A (Demographics) generated with ", nrow(table1a), " rows")
message("Table 1.B (Transportation Patterns) generated with ", nrow(table1b), " rows")
message("Table 1.C (Transit Lines and Trip Purposes) generated with ", nrow(table1c), " rows")

message("\nTable 1.A: Demographics (Age and Gender)")
print(table1a)
message("\nTable 1.B: Transportation Patterns (usage frequency and primary mode)")
print(table1b)
message("\nTable 1.C: Transit Lines and Trip Purposes")
print(table1c)

#------------------------------------------------------------------------------
# TABLE 2: DURAKGO USAGE PATTERNS
#------------------------------------------------------------------------------

generate_usage_patterns_table <- function(data) {
  # Length of time using DurakGO
  usage_time_dist <- table(data$USE_1)
  usage_time_pct <- prop.table(usage_time_dist) * 100

  usage_time_labels <- c("Less than 1 week", "1-4 weeks", "1-3 months",
                         "More than 3 months")

  usage_time_table <- data.frame(
    Category = "Length of Time Using DurakGO",
    Group = usage_time_labels[seq_along(usage_time_dist)],
    Count = as.numeric(usage_time_dist),
    Percentage = round(as.numeric(usage_time_pct), 1)
  )

  # Frequency of use
  use_freq_dist <- table(data$USE_2)
  use_freq_pct <- prop.table(use_freq_dist) * 100

  use_freq_labels <- c("Multiple times per day", "Once a day",
                       "Several times per week", "Once a week or less")

  use_freq_table <- data.frame(
    Category = "Frequency of Use",
    Group = use_freq_labels[seq_along(use_freq_dist)],
    Count = as.numeric(use_freq_dist),
    Percentage = round(as.numeric(use_freq_pct), 1)
  )

  # Usage contexts
  usage_contexts <- c(
    sum(data$USE_3_1, na.rm = TRUE),
    sum(data$USE_3_2, na.rm = TRUE),
    sum(data$USE_3_3, na.rm = TRUE),
    sum(data$USE_3_4, na.rm = TRUE),
    sum(data$USE_3_5, na.rm = TRUE)
  )

  usage_contexts_pct <- usage_contexts / nrow(data) * 100

  usage_contexts_labels <- c("Before leaving", "While traveling to station",
                             "At the station", "During service disruptions",
                             "To plan trips in advance")

  usage_contexts_table <- data.frame(
    Category = "Usage Contexts",
    Group = usage_contexts_labels,
    Count = usage_contexts,
    Percentage = round(usage_contexts_pct, 1)
  )

  # Most used features
  features_used <- c(
    sum(data$USE_4_1, na.rm = TRUE),
    sum(data$USE_4_2, na.rm = TRUE),
    sum(data$USE_4_3, na.rm = TRUE),
    sum(data$USE_4_4, na.rm = TRUE),
    sum(data$USE_4_5, na.rm = TRUE)
  )

  features_used_pct <- features_used / nrow(data) * 100

  features_used_labels <- c("Checking next arrival times",
                            "Finding nearest stations",
                            "Viewing station map",
                            "Searching for specific stations",
                            "Viewing both directions at stations")

  features_used_table <- data.frame(
    Category = "Features Used",
    Group = features_used_labels,
    Count = features_used,
    Percentage = round(features_used_pct, 1)
  )

  # Offline usage frequency
  offline_usage_dist <- table(data$USE_5)
  offline_usage_pct <- prop.table(offline_usage_dist) * 100

  offline_usage_labels <- c("Yes, frequently", "Yes, occasionally",
                            "No, always have internet",
                            "No, didn't know it worked offline")

  offline_usage_table <- data.frame(
    Category = "Offline Usage",
    Group = offline_usage_labels[seq_along(offline_usage_dist)],
    Count = as.numeric(offline_usage_dist),
    Percentage = round(as.numeric(offline_usage_pct), 1)
  )

  # Combine all tables
  usage_patterns_table <- rbind(
    usage_time_table,
    use_freq_table,
    usage_contexts_table,
    features_used_table,
    offline_usage_table
  )

  return(usage_patterns_table)
}

# Generate Table 2
table2 <- generate_usage_patterns_table(survey_data)
message("Table 2 generated with ", nrow(table2), " rows")
print(table2)

#------------------------------------------------------------------------------
# TABLE 3: COMPARISON OF WAIT TIME PERCEPTION
#------------------------------------------------------------------------------

generate_wait_time_comparison_table <- function(data) {
  # Filter out NA values
  wait_time_data <- data %>%
    filter(!is.na(WAIT_1_minutes) & !is.na(WAIT_2_minutes))

  # Overall comparison
  overall_before <- mean(wait_time_data$WAIT_1_minutes, na.rm = TRUE)
  overall_after <- mean(wait_time_data$WAIT_2_minutes, na.rm = TRUE)
  overall_diff <- overall_before - overall_after

  # Paired t-test for overall comparison
  overall_test <- t.test(wait_time_data$WAIT_1_minutes,
                         wait_time_data$WAIT_2_minutes,
                         paired = TRUE)

  overall_tstat <- overall_test$statistic
  overall_pvalue <- overall_test$p.value

  # Function to determine significance stars
  get_significance <- function(p_value) {
    if (is.na(p_value)) return("")
    if (p_value < 0.001) return("***")
    if (p_value < 0.01) return("**")
    if (p_value < 0.05) return("*")
    return("")
  }

  # Line-specific comparisons
  line_comparison <- function(line_index, line_name) {
    line_col <- paste0("DEM_3_", line_index)

    # Filter respondents who use this line
    line_users <- wait_time_data %>%
      filter(get(line_col) == TRUE)

    if (nrow(line_users) == 0) {
      return(data.frame(
        Line = line_name,
        Before = NA,
        After = NA,
        Difference = NA,
        t_Statistic = NA,
        p_value = NA,
        Significance = ""
      ))
    }

    # Calculate means
    before <- mean(line_users$WAIT_1_minutes, na.rm = TRUE)
    after <- mean(line_users$WAIT_2_minutes, na.rm = TRUE)
    diff <- before - after

    # Paired t-test
    if (nrow(line_users) >= 2) {  # Need at least 2 observations for paired t-test
      test <- t.test(line_users$WAIT_1_minutes,
                     line_users$WAIT_2_minutes,
                     paired = TRUE)
      tstat <- test$statistic
      pvalue <- test$p.value
    } else {
      tstat <- NA
      pvalue <- NA
    }

    return(data.frame(
      Line = line_name,
      Before = round(before, 2),
      After = round(after, 2),
      Difference = round(diff, 2),
      t_Statistic = round(as.numeric(tstat), 2),
      p_value = pvalue,
      Significance = get_significance(pvalue)
    ))
  }

  # Process each line
  a1_results <- line_comparison(1, "Ankaray (A1)")
  m1_results <- line_comparison(2, "Sincan-Çayyolu (M1)")
  m4_results <- line_comparison(3, "Keçiören Metro (M4)")
  b1_results <- line_comparison(4, "Başkentray (B1)")

  # Combine results
  wait_time_comparison <- data.frame(
    Line = "Overall",
    Before = round(overall_before, 2),
    After = round(overall_after, 2),
    Difference = round(overall_diff, 2),
    t_Statistic = round(as.numeric(overall_tstat), 2),
    p_value = overall_pvalue,
    Significance = get_significance(overall_pvalue)
  )

  wait_time_comparison <- rbind(
    wait_time_comparison,
    a1_results,
    m1_results,
    m4_results,
    b1_results
  )

  return(wait_time_comparison)
}

# Generate Table 3
table3 <- generate_wait_time_comparison_table(survey_data)
message("Table 3 generated with ", nrow(table3), " rows")
print(table3)

#------------------------------------------------------------------------------
# TABLE 4: WAIT EXPERIENCE BY WAIT TIME CATEGORY
#------------------------------------------------------------------------------

generate_wait_experience_table <- function(data) {
  # Create wait time categories based on WAIT_2 (current expected wait time)
  data$wait_category <- factor(data$WAIT_2,
                               levels = 1:5,
                               labels = c("< 3 minutes",
                                          "3-5 minutes",
                                          "6-10 minutes",
                                          "11-15 minutes",
                                          "> 15 minutes"))

  # Calculate mean relaxation rating (WAIT_4) by wait time category
  relaxation_by_category <- data %>%
    group_by(wait_category) %>%
    summarize(
      Relaxation_Rating = mean(WAIT_4, na.rm = TRUE),
      Relaxation_SD = sd(WAIT_4, na.rm = TRUE),
      n = n()
    )

  # Calculate mean system satisfaction (IMP_3) by wait time category
  # Convert IMP_3 to numeric if it's a string
  data$system_satisfaction <- as.numeric(as.character(data$IMP_3))

  satisfaction_by_category <- data %>%
    group_by(wait_category) %>%
    summarize(
      System_Satisfaction = mean(system_satisfaction, na.rm = TRUE),
      Satisfaction_SD = sd(system_satisfaction, na.rm = TRUE),
      n = n()
    )

  # Combine the results
  wait_experience_table <- relaxation_by_category %>%
    left_join(satisfaction_by_category, by = c("wait_category", "n"))

  # Rename columns for the final table
  names(wait_experience_table) <- c("Wait Time Category",
                                   "Relaxation Rating",
                                   "Relaxation SD",
                                   "Sample Size",
                                   "System Satisfaction",
                                   "Satisfaction SD")

  # Run ANOVA for statistical significance testing
  # Relaxation by wait category
  if (length(unique(data$wait_category)) > 1) {
    relaxation_anova <- tryCatch({
      aov(WAIT_4 ~ wait_category, data = data)
    }, error = function(e) {
      message("Error in relaxation ANOVA: ", e$message)
      return(NULL)
    })

    if (!is.null(relaxation_anova)) {
      relaxation_p <- summary(relaxation_anova)[[1]][["Pr(>F)"]][1]
    } else {
      relaxation_p <- NA
    }

    # System satisfaction by wait category
    satisfaction_anova <- tryCatch({
      aov(system_satisfaction ~ wait_category, data = data)
    }, error = function(e) {
      message("Error in satisfaction ANOVA: ", e$message)
      return(NULL)
    })

    if (!is.null(satisfaction_anova)) {
      satisfaction_p <- summary(satisfaction_anova)[[1]][["Pr(>F)"]][1]
    } else {
      satisfaction_p <- NA
    }
  } else {
    relaxation_p <- NA
    satisfaction_p <- NA
    message("Not enough wait categories for ANOVA")
  }

  # Add p-values to the table as an attribute
  attr(wait_experience_table, "relaxation_p") <- relaxation_p
  attr(wait_experience_table, "satisfaction_p") <- satisfaction_p

  return(wait_experience_table)
}

# Generate Table 4
table4 <- generate_wait_experience_table(survey_data)
message("Table 4 generated with ", nrow(table4), " rows")
print(table4)

#------------------------------------------------------------------------------
# TABLE 5: USER SATISFACTION METRICS
#------------------------------------------------------------------------------

generate_satisfaction_metrics_table <- function(data) {
  # Calculate user satisfaction metrics

  # Overall DurakGO app rating (SAT_1)
  app_rating <- mean(data$SAT_1, na.rm = TRUE)
  app_rating_sd <- sd(data$SAT_1, na.rm = TRUE)

  # Ankara metro/rail system rating (IMP_3)
  # Note: IMP_3 may be a string, convert to numeric
  data$system_rating <- as.numeric(as.character(data$IMP_3))
  system_rating <- mean(data$system_rating, na.rm = TRUE)
  system_rating_sd <- sd(data$system_rating, na.rm = TRUE)

  # Satisfaction with offline functionality (OFF_1)
  offline_satisfaction <- mean(data$OFF_1, na.rm = TRUE)
  offline_satisfaction_sd <- sd(data$OFF_1, na.rm = TRUE)

  # Comparison to EGO Cep'te (IMP_2)
  ego_comparison <- mean(data$IMP_2, na.rm = TRUE)
  ego_comparison_sd <- sd(data$IMP_2, na.rm = TRUE)

  # Likelihood to recommend (SAT_5)
  recommend_likelihood <- mean(data$SAT_5, na.rm = TRUE)
  recommend_likelihood_sd <- sd(data$SAT_5, na.rm = TRUE)

  # Create the table
  satisfaction_metrics <- data.frame(
    Metric = c("Overall DurakGO app rating",
               "Ankara metro/rail system rating",
               "Satisfaction with offline functionality",
               "Comparison to EGO Cep'te",
               "Likelihood to recommend"),
    Rating = c(app_rating, system_rating, offline_satisfaction,
               ego_comparison, recommend_likelihood),
    SD = c(app_rating_sd, system_rating_sd, offline_satisfaction_sd,
           ego_comparison_sd, recommend_likelihood_sd)
  )

  # One-sample t-tests to check if ratings are different from midpoint (3)
  # Create a function to perform t-test
  midpoint_ttest <- function(values, midpoint = 3) {
    if (sum(!is.na(values)) < 2) return(list(t_statistic = NA, p_value = NA))

    test <- tryCatch({
      t.test(values, mu = midpoint)
    }, error = function(e) {
      message("Error in t-test: ", e$message)
      return(list(statistic = NA, p.value = NA))
    })

    return(list(t_statistic = test$statistic, p_value = test$p.value))
  }

  # Perform t-tests
  app_ttest <- midpoint_ttest(data$SAT_1)
  system_ttest <- midpoint_ttest(data$system_rating)
  offline_ttest <- midpoint_ttest(data$OFF_1)
  ego_ttest <- midpoint_ttest(data$IMP_2)
  recommend_ttest <- midpoint_ttest(data$SAT_5)

  # Add t-statistics and p-values to the table
  satisfaction_metrics$t_Statistic <- c(
    app_ttest$t_statistic,
    system_ttest$t_statistic,
    offline_ttest$t_statistic,
    ego_ttest$t_statistic,
    recommend_ttest$t_statistic
  )

  satisfaction_metrics$p_value <- c(
    app_ttest$p_value,
    system_ttest$p_value,
    offline_ttest$p_value,
    ego_ttest$p_value,
    recommend_ttest$p_value
  )

  # Add significance markers
  satisfaction_metrics$Significance <- sapply(
    satisfaction_metrics$p_value,
    function(p) {
      if (is.na(p)) return("")
      if (p < 0.001) return("***")
      if (p < 0.01) return("**")
      if (p < 0.05) return("*")
      return("")
    }
  )

  # Round numeric columns
  satisfaction_metrics$Rating <- round(satisfaction_metrics$Rating, 2)
  satisfaction_metrics$SD <- round(satisfaction_metrics$SD, 2)
  satisfaction_metrics$t_Statistic <- round(satisfaction_metrics$t_Statistic, 2)

  return(satisfaction_metrics)
}

# Generate Table 5
table5 <- generate_satisfaction_metrics_table(survey_data)
message("Table 5 generated with ", nrow(table5), " rows")
print(table5)

#------------------------------------------------------------------------------
# TABLE 6: REPORTED BEHAVIORAL CHANGES
#------------------------------------------------------------------------------

generate_behavioral_changes_table <- function(data) {
  # Extract behavioral change information

  # Using metro more frequently (IMP_1)
  # Create a binary variable for increased usage
  data$increased_usage <- ifelse(data$IMP_1 %in% c(4, 5), TRUE, FALSE)
  increased_usage_pct <- sum(data$increased_usage, na.rm = TRUE) /
    sum(!is.na(data$increased_usage)) * 100

  # Changed departure time based on app information (WAIT_5_1)
  departure_change_pct <- sum(data$WAIT_5_1, na.rm = TRUE) /
    nrow(data) * 100

  # Used alternative transportation during long waits (WAIT_5_3)
  alt_transport_pct <- sum(data$WAIT_5_3, na.rm = TRUE) /
    nrow(data) * 100

  # Changed stations based on app information (WAIT_5_2)
  station_change_pct <- sum(data$WAIT_5_2, na.rm = TRUE) /
    nrow(data) * 100

  # No change in behavior (WAIT_5_6)
  no_change_pct <- sum(data$WAIT_5_6, na.rm = TRUE) /
    nrow(data) * 100

  # Calculate 95% confidence intervals
  # Function to calculate CI for proportion
  ci_proportion <- function(proportion, n, conf_level = 0.95) {
    if (n == 0) return(c(NA, NA))

    # Use Wilson method for small samples
    if (n < 30) {
      # Calculate Wilson interval
      z <- qnorm(1 - (1 - conf_level) / 2)
      p_hat <- proportion / 100

      # Wilson score interval
      center <- (p_hat + z^2/(2*n)) / (1 + z^2/n)
      halfwidth <- z * sqrt(p_hat * (1 - p_hat) / n + z^2/(4*n^2)) / (1 + z^2/n)

      lower <- max(0, center - halfwidth) * 100
      upper <- min(1, center + halfwidth) * 100
    } else {
      # Standard normal approximation for larger samples
      z <- qnorm(1 - (1 - conf_level) / 2)
      p_hat <- proportion / 100
      se <- sqrt(p_hat * (1 - p_hat) / n)

      lower <- max(0, p_hat - z * se) * 100
      upper <- min(1, p_hat + z * se) * 100
    }

    return(c(lower, upper))
  }

  n_valid <- sum(!is.na(data$increased_usage))

  increased_usage_ci <- ci_proportion(increased_usage_pct, n_valid)
  departure_change_ci <- ci_proportion(departure_change_pct, nrow(data))
  alt_transport_ci <- ci_proportion(alt_transport_pct, nrow(data))
  station_change_ci <- ci_proportion(station_change_pct, nrow(data))
  no_change_ci <- ci_proportion(no_change_pct, nrow(data))

  # Create the table
  behavioral_changes <- data.frame(
    Behavior = c("Using metro more frequently",
                 "Changed departure time based on app information",
                 "Used alternative transportation during long waits",
                 "Changed stations based on app information",
                 "No change in behavior"),
    Percentage = c(increased_usage_pct,
                   departure_change_pct,
                   alt_transport_pct,
                   station_change_pct,
                   no_change_pct),
    CI_Lower = c(increased_usage_ci[1],
                 departure_change_ci[1],
                 alt_transport_ci[1],
                 station_change_ci[1],
                 no_change_ci[1]),
    CI_Upper = c(increased_usage_ci[2],
                 departure_change_ci[2],
                 alt_transport_ci[2],
                 station_change_ci[2],
                 no_change_ci[2])
  )

  # Round numeric columns
  behavioral_changes$Percentage <- round(behavioral_changes$Percentage, 1)
  behavioral_changes$CI_Lower <- round(behavioral_changes$CI_Lower, 1)
  behavioral_changes$CI_Upper <- round(behavioral_changes$CI_Upper, 1)

  # Add a column with formatted confidence intervals
  behavioral_changes$CI_95 <- paste0("(",
                                    behavioral_changes$CI_Lower,
                                    " - ",
                                    behavioral_changes$CI_Upper,
                                    ")")

  return(behavioral_changes)
}

# Generate Table 6
table6 <- generate_behavioral_changes_table(survey_data)
message("Table 6 generated with ", nrow(table6), " rows")
print(table6)

#------------------------------------------------------------------------------
# TABLE 7: MULTINOMIAL LOGIT MODEL FOR WAIT TIME BEHAVIOR
#------------------------------------------------------------------------------

generate_wait_behavior_model_table <- function(data) {
  # Load required packages for multinomial logit models
  if (!require("nnet")) {
    message("Installing 'nnet' package for multinomial logit model")
    install.packages("nnet", repos = "https://cloud.r-project.org")
    library(nnet)
  }
  
  # Create the dependent variable - actions during long wait times
  # Create a factor variable with the following levels:
  # 1. Do something while waiting (WAIT_5_4)
  # 2. Change to alternative transportation (WAIT_5_3)
  # 3. Change station (WAIT_5_2)
  # 4. No change (WAIT_5_6) - reference category
  
  # First check if necessary columns are available
  required_cols <- c("WAIT_5_2", "WAIT_5_3", "WAIT_5_4", "WAIT_5_6")
  missing_cols <- required_cols[!required_cols %in% names(data)]
  
  if (length(missing_cols) > 0) {
    message("Warning: Missing required columns for wait time behavior model: ", 
            paste(missing_cols, collapse = ", "))
    message("Creating placeholder model table.")
    
    # Create placeholder table
    model_table <- data.frame(
      Variable = c("Intercept", 
                   "Wait time shown (minutes)", 
                   "App usage frequency",
                   "Metro usage frequency",
                   "Age",
                   "Gender (Female vs Male)"),
      Action_DoSomething_Coef = rep(NA, 6),
      Action_DoSomething_SE = rep(NA, 6),
      Action_DoSomething_P = rep(NA, 6),
      Action_DoSomething_Sig = rep("", 6),
      Action_ChangeTransport_Coef = rep(NA, 6),
      Action_ChangeTransport_SE = rep(NA, 6),
      Action_ChangeTransport_P = rep(NA, 6),
      Action_ChangeTransport_Sig = rep("", 6),
      Action_ChangeStation_Coef = rep(NA, 6),
      Action_ChangeStation_SE = rep(NA, 6),
      Action_ChangeStation_P = rep(NA, 6),
      Action_ChangeStation_Sig = rep("", 6)
    )
    
    # Add model fit statistics as attributes
    attr(model_table, "n_obs") <- 0
    attr(model_table, "log_likelihood") <- NA
    attr(model_table, "aic") <- NA
    attr(model_table, "pseudo_r2") <- NA
    
    return(model_table)
  }
  
  # Determine primary action for each respondent based on WAIT_5 columns
  # In case of multiple actions, prioritize in this order:
  # 1. Change station 2. Change transport 3. Do something 4. No change
  
  data$wait_action <- factor(NA, levels = c("No change", "Do something", 
                                            "Change transport", "Change station"))
  
  for (i in seq_len(nrow(data))) {
    if (!is.na(data$WAIT_5_2[i]) && data$WAIT_5_2[i] == TRUE) {
      data$wait_action[i] <- "Change station"
    } else if (!is.na(data$WAIT_5_3[i]) && data$WAIT_5_3[i] == TRUE) {
      data$wait_action[i] <- "Change transport"
    } else if (!is.na(data$WAIT_5_4[i]) && data$WAIT_5_4[i] == TRUE) {
      data$wait_action[i] <- "Do something"
    } else if (!is.na(data$WAIT_5_6[i]) && data$WAIT_5_6[i] == TRUE) {
      data$wait_action[i] <- "No change"
    }
  }
  
  # Check if there is enough data for the model
  if (sum(!is.na(data$wait_action)) < 10) {
    message("Warning: Not enough data for multinomial logit model (", 
            sum(!is.na(data$wait_action)), " valid cases). Creating placeholder table.")
    
    # Create placeholder table (same as above)
    model_table <- data.frame(
      Variable = c("Intercept", 
                   "Wait time shown (minutes)", 
                   "App usage frequency",
                   "Metro usage frequency",
                   "Age",
                   "Gender (Female vs Male)"),
      Action_DoSomething_Coef = rep(NA, 6),
      Action_DoSomething_SE = rep(NA, 6),
      Action_DoSomething_P = rep(NA, 6),
      Action_DoSomething_Sig = rep("", 6),
      Action_ChangeTransport_Coef = rep(NA, 6),
      Action_ChangeTransport_SE = rep(NA, 6),
      Action_ChangeTransport_P = rep(NA, 6),
      Action_ChangeTransport_Sig = rep("", 6),
      Action_ChangeStation_Coef = rep(NA, 6),
      Action_ChangeStation_SE = rep(NA, 6),
      Action_ChangeStation_P = rep(NA, 6),
      Action_ChangeStation_Sig = rep("", 6)
    )
    
    # Add model fit statistics as attributes
    attr(model_table, "n_obs") <- sum(!is.na(data$wait_action))
    attr(model_table, "log_likelihood") <- NA
    attr(model_table, "aic") <- NA
    attr(model_table, "pseudo_r2") <- NA
    
    return(model_table)
  }
  
  # Prepare explanatory variables
  # 1. Wait time shown (use WAIT_2_minutes as proxy)
  data$wait_time_shown <- data$WAIT_2_minutes
  
  # 2. App usage frequency (USE_2, reverse coded)
  data$app_usage_freq <- 5 - data$USE_2
  
  # 3. Metro usage frequency (DEM_1, reverse coded)
  data$metro_usage_freq <- 6 - data$DEM_1
  
  # 4. Age group (DEM_5)
  data$age_group <- data$DEM_5
  
  # 5. Gender (DEM_6, binary: 1=Male, 2=Female, encode as 0/1)
  data$gender_female <- ifelse(data$DEM_6 == 2, 1, 0)
  
  # Handle missing values
  model_data <- data %>% 
    dplyr::filter(!is.na(wait_action)) %>%
    dplyr::select(wait_action, wait_time_shown, app_usage_freq, 
                  metro_usage_freq, age_group, gender_female)
  
  # Impute missing values with medians
  for (col in names(model_data)[-1]) {  # Skip the dependent variable
    if (any(is.na(model_data[[col]]))) {
      median_val <- median(model_data[[col]], na.rm = TRUE)
      model_data[[col]][is.na(model_data[[col]])] <- median_val
    }
  }
  
  # Fit the multinomial logit model
  # Using nnet::multinom with "No change" as reference category
  tryCatch({
    mlogit_model <- nnet::multinom(
      wait_action ~ wait_time_shown + app_usage_freq + metro_usage_freq + 
        age_group + gender_female,
      data = model_data,
      maxit = 1000,
      trace = FALSE
    )
    
    # Extract coefficients and standard errors
    coefs <- summary(mlogit_model)$coefficients
    ses <- summary(mlogit_model)$standard.errors
    
    # Calculate z-values and p-values
    zvalues <- coefs / ses
    pvalues <- 2 * (1 - pnorm(abs(zvalues)))
    
    # Create model table
    predictor_names <- c("Intercept", 
                        "Wait time shown (minutes)", 
                        "App usage frequency",
                        "Metro usage frequency",
                        "Age",
                        "Gender (Female vs Male)")
    
    model_table <- data.frame(
      Variable = predictor_names
    )
    
    # Function to add significance markers
    get_significance <- function(p_value) {
      if (is.na(p_value)) return("")
      if (p_value < 0.001) return("***")
      if (p_value < 0.01) return("**")
      if (p_value < 0.05) return("*")
      return("")
    }
    
    # Add results for each action (relative to "No change")
    action_levels <- levels(model_data$wait_action)
    action_levels <- action_levels[action_levels != "No change"]
    
    for (i in seq_along(action_levels)) {
      action <- action_levels[i]
      action_clean <- gsub(" ", "", action)
      
      # Add coefficients
      model_table[[paste0("Action_", action_clean, "_Coef")]] <- 
        round(coefs[i, ], 2)
      
      # Add standard errors
      model_table[[paste0("Action_", action_clean, "_SE")]] <- 
        round(ses[i, ], 2)
      
      # Add p-values
      model_table[[paste0("Action_", action_clean, "_P")]] <- 
        pvalues[i, ]
      
      # Add significance markers
      model_table[[paste0("Action_", action_clean, "_Sig")]] <- 
        sapply(pvalues[i, ], get_significance)
    }
    
    # Calculate model fit statistics
    null_model <- nnet::multinom(
      wait_action ~ 1,
      data = model_data,
      maxit = 1000,
      trace = FALSE
    )
    
    # Log-likelihood
    log_likelihood <- logLik(mlogit_model)[1]
    
    # AIC
    aic <- AIC(mlogit_model)
    
    # McFadden's Pseudo R-squared
    ll_full <- logLik(mlogit_model)[1]
    ll_null <- logLik(null_model)[1]
    pseudo_r2 <- 1 - (ll_full / ll_null)
    
    # Add model fit statistics as attributes
    attr(model_table, "n_obs") <- nrow(model_data)
    attr(model_table, "log_likelihood") <- log_likelihood
    attr(model_table, "aic") <- aic
    attr(model_table, "pseudo_r2") <- pseudo_r2
    
    return(model_table)
    
  }, error = function(e) {
    message("Error in multinomial logit model: ", e$message)
    
    # Create placeholder table on error
    model_table <- data.frame(
      Variable = c("Intercept", 
                   "Wait time shown (minutes)", 
                   "App usage frequency",
                   "Metro usage frequency",
                   "Age",
                   "Gender (Female vs Male)"),
      Action_DoSomething_Coef = rep(NA, 6),
      Action_DoSomething_SE = rep(NA, 6),
      Action_DoSomething_P = rep(NA, 6),
      Action_DoSomething_Sig = rep("", 6),
      Action_ChangeTransport_Coef = rep(NA, 6),
      Action_ChangeTransport_SE = rep(NA, 6),
      Action_ChangeTransport_P = rep(NA, 6),
      Action_ChangeTransport_Sig = rep("", 6),
      Action_ChangeStation_Coef = rep(NA, 6),
      Action_ChangeStation_SE = rep(NA, 6),
      Action_ChangeStation_P = rep(NA, 6),
      Action_ChangeStation_Sig = rep("", 6)
    )
    
    # Add model fit statistics as attributes
    attr(model_table, "n_obs") <- sum(!is.na(data$wait_action))
    attr(model_table, "log_likelihood") <- NA
    attr(model_table, "aic") <- NA
    attr(model_table, "pseudo_r2") <- NA
    
    return(model_table)
  })
}

# Generate Table 7
table7 <- generate_wait_behavior_model_table(survey_data)

# Print the table with additional information
message("Table 7 generated with ", nrow(table7), " rows")
print(table7)

# Print model fit statistics
message("Model fit statistics:")
message("Number of observations: ", attr(table7, "n_obs"))
message("Log-likelihood: ", round(attr(table7, "log_likelihood"), 2))
message("AIC: ", round(attr(table7, "aic"), 2))
message("McFadden's Pseudo R-squared: ", round(attr(table7, "pseudo_r2"), 4))

#------------------------------------------------------------------------------
# TABLE 8: FACTORS AFFECTING PERCEIVED WAIT TIME REDUCTION
#------------------------------------------------------------------------------

generate_wait_time_regression_table <- function(data) {
  # Prepare data for regression

  # Calculate wait time difference
  # Higher values indicate greater reduction in perceived wait time
  data$wait_time_reduction <- data$WAIT_1_minutes - data$WAIT_2_minutes

  # Create binary variable for offline functionality usage
  data$offline_usage <- ifelse(data$USE_5 %in% c(1, 2), 1, 0)

  # Create frequency of app use variable (reverse coded so higher = more frequent)
  data$app_use_frequency <- 5 - data$USE_2

  # Perceived accuracy of predictions (OFF_2)
  data$perceived_accuracy <- data$OFF_2

  # Primary mode (1 if metro/rail is primary, 0 otherwise)
  data$primary_mode_transit <- ifelse(data$DEM_2 == 1, 1, 0)

  # Frequency of metro use (reverse coded so higher = more frequent)
  data$metro_use_frequency <- 6 - data$DEM_1

  # Check if there is enough data for regression
  if (nrow(data) < 7) { # Need more observations than predictors
    message("Warning: Not enough observations for reliable regression. Creating placeholder table.")

    # Create a placeholder table
    regression_table <- data.frame(
      Variable = c("Intercept",
                   "Offline functionality usage (Y/N)",
                   "Frequency of app use",
                   "Perceived accuracy of predictions",
                   "Primary mode (transit vs. other)",
                   "Frequency of metro use"),
      Coefficient = c(NA, NA, NA, NA, NA, NA),
      SE = c(NA, NA, NA, NA, NA, NA),
      t_Statistic = c(NA, NA, NA, NA, NA, NA),
      p_value = c(NA, NA, NA, NA, NA, NA),
      Significance = c("", "", "", "", "", "")
    )

    # Add model fit attributes
    attr(regression_table, "r.squared") <- NA
    attr(regression_table, "adj.r.squared") <- NA
    attr(regression_table, "f.statistic") <- NA
    attr(regression_table, "f.p.value") <- NA

    return(regression_table)
  }

  # Build the regression model
  wait_time_model <- tryCatch({
    lm(wait_time_reduction ~
         offline_usage +
         app_use_frequency +
         perceived_accuracy +
         primary_mode_transit +
         metro_use_frequency,
       data = data)
  }, error = function(e) {
    message("Error in regression model: ", e$message)
    return(NULL)
  })

  if (is.null(wait_time_model)) {
    message("Regression model failed. Creating placeholder table.")

    # Create a placeholder table
    regression_table <- data.frame(
      Variable = c("Intercept",
                   "Offline functionality usage (Y/N)",
                   "Frequency of app use",
                   "Perceived accuracy of predictions",
                   "Primary mode (transit vs. other)",
                   "Frequency of metro use"),
      Coefficient = c(NA, NA, NA, NA, NA, NA),
      SE = c(NA, NA, NA, NA, NA, NA),
      t_Statistic = c(NA, NA, NA, NA, NA, NA),
      p_value = c(NA, NA, NA, NA, NA, NA),
      Significance = c("", "", "", "", "", "")
    )

    # Add model fit attributes
    attr(regression_table, "r.squared") <- NA
    attr(regression_table, "adj.r.squared") <- NA
    attr(regression_table, "f.statistic") <- NA
    attr(regression_table, "f.p.value") <- NA

    return(regression_table)
  }

  # Get model summary
  model_summary <- summary(wait_time_model)

  # Extract coefficients
  coef_data <- coef(model_summary)

  # Create the table
  regression_table <- data.frame(
    Variable = c("Intercept",
                 "Offline functionality usage (Y/N)",
                 "Frequency of app use",
                 "Perceived accuracy of predictions",
                 "Primary mode (transit vs. other)",
                 "Frequency of metro use"),
    Coefficient = coef_data[, "Estimate"],
    SE = coef_data[, "Std. Error"],
    t_Statistic = coef_data[, "t value"],
    p_value = coef_data[, "Pr(>|t|)"]
  )

  # Add significance markers
  regression_table$Significance <- sapply(
    regression_table$p_value,
    function(p) {
      if (is.na(p)) return("")
      if (p < 0.001) return("***")
      if (p < 0.01) return("**")
      if (p < 0.05) return("*")
      return("")
    }
  )

  # Round numeric columns
  regression_table$Coefficient <- round(regression_table$Coefficient, 2)
  regression_table$SE <- round(regression_table$SE, 2)
  regression_table$t_Statistic <- round(regression_table$t_Statistic, 2)

  # Add model fit statistics
  attr(regression_table, "r.squared") <- model_summary$r.squared
  attr(regression_table, "adj.r.squared") <- model_summary$adj.r.squared
  attr(regression_table, "f.statistic") <- model_summary$fstatistic[1]
  attr(regression_table, "f.p.value") <- pf(
    model_summary$fstatistic[1],
    model_summary$fstatistic[2],
    model_summary$fstatistic[3],
    lower.tail = FALSE
  )

  return(regression_table)
}

# Generate Table 8
table8 <- generate_wait_time_regression_table(survey_data)
message("Table 8 generated with ", nrow(table8), " rows")
print(table8)

#------------------------------------------------------------------------------
# TABLE 9: WAIT EXPERIENCE BY USER DEMOGRAPHIC CATEGORIES
#------------------------------------------------------------------------------

generate_wait_exp_by_demographics_table <- function(data) {
  # Create necessary derived variables

  # Calculate wait time difference
  data$wait_diff <- data$WAIT_1_minutes - data$WAIT_2_minutes

  # Create relaxation effect (WAIT_4)
  data$relaxation_effect <- data$WAIT_4

  # Create transit usage change (IMP_1)
  # Higher values indicate increased usage
  data$transit_usage_change <- data$IMP_1 - 3

  # Define age groups
  data$age_group <- factor(
    case_when(
      data$DEM_5 == 1 ~ "Under 25",
      data$DEM_5 == 2 ~ "Under 25",
      data$DEM_5 == 3 ~ "25-34",
      data$DEM_5 == 4 ~ "35-44",
      data$DEM_5 == 5 ~ "45-54",
      data$DEM_5 >= 6 ~ "55+",
      TRUE ~ NA_character_
    ),
    levels = c("Under 25", "25-34", "35-44", "45-54", "55+")
  )

  # Define usage frequency groups
  data$usage_frequency <- factor(
    case_when(
      data$USE_2 == 1 ~ "Daily",
      data$USE_2 == 2 ~ "Daily",
      data$USE_2 == 3 ~ "Several times/week",
      data$USE_2 == 4 ~ "Weekly or less",
      TRUE ~ NA_character_
    ),
    levels = c("Daily", "Several times/week", "Weekly or less")
  )

  # Create empty tables to store results
  age_results <- data.frame(
    Category = character(),
    Group = character(),
    Wait_Perception_Change = numeric(),
    P_Value = numeric(),
    Significance = character(),
    Relaxation_Effect = numeric(),
    Transit_Usage_Change = numeric(),
    Transit_Usage_P = numeric(),
    Transit_Significance = character(),
    Sample_Size = integer()
  )

  usage_results <- data.frame(
    Category = character(),
    Group = character(),
    Wait_Perception_Change = numeric(),
    P_Value = numeric(),
    Significance = character(),
    Relaxation_Effect = numeric(),
    Transit_Usage_Change = numeric(),
    Transit_Usage_P = numeric(),
    Transit_Significance = character(),
    Sample_Size = integer()
  )

  # Function to perform t-test for wait perception change
  wait_ttest <- function(subset_data) {
    if (nrow(subset_data) < 2) return(list(p_value = NA))

    # Paired t-test for wait time
    test <- tryCatch({
      t.test(subset_data$WAIT_1_minutes,
             subset_data$WAIT_2_minutes,
             paired = TRUE)
    }, error = function(e) {
      message("Error in wait time t-test: ", e$message)
      return(list(p_value = NA))
    })

    return(list(p_value = test$p.value))
  }

  # Function to perform t-test for transit usage change
  usage_ttest <- function(subset_data) {
    if (nrow(subset_data) < 2) return(list(p_value = NA))

    # One-sample t-test for transit usage change
    test <- tryCatch({
      t.test(subset_data$transit_usage_change, mu = 0)
    }, error = function(e) {
      message("Error in usage t-test: ", e$message)
      return(list(p_value = NA))
    })

    return(list(p_value = test$p.value))
  }

  # Handle special case for single row datasets
  if (nrow(data) == 1) {
    message("Warning: Only one row of data. Creating placeholder demographic table.")

    # Create a placeholder table with the single data point
    age_group_value <- as.character(data$age_group[1])
    if (is.na(age_group_value)) age_group_value <- "Unknown"

    usage_freq_value <- as.character(data$usage_frequency[1])
    if (is.na(usage_freq_value)) usage_freq_value <- "Unknown"

    # Add age group row
    age_results <- rbind(age_results, data.frame(
      Category = "Age Groups",
      Group = age_group_value,
      Wait_Perception_Change = data$wait_diff[1],
      P_Value = NA,
      Significance = "",
      Relaxation_Effect = data$relaxation_effect[1],
      Transit_Usage_Change = paste0(ifelse(data$transit_usage_change[1] > 0, "+", ""),
                                   round(data$transit_usage_change[1] * 100, 1), "%"),
      Transit_Usage_P = NA,
      Transit_Significance = "",
      Sample_Size = 1
    ))

    # Add usage frequency row
    usage_results <- rbind(usage_results, data.frame(
      Category = "Usage Frequency",
      Group = usage_freq_value,
      Wait_Perception_Change = data$wait_diff[1],
      P_Value = NA,
      Significance = "",
      Relaxation_Effect = data$relaxation_effect[1],
      Transit_Usage_Change = paste0(ifelse(data$transit_usage_change[1] > 0, "+", ""),
                                   round(data$transit_usage_change[1] * 100, 1), "%"),
      Transit_Usage_P = NA,
      Transit_Significance = "",
      Sample_Size = 1
    ))

    # Combine results
    wait_exp_demographic <- rbind(age_results, usage_results)
    return(wait_exp_demographic)
  }

  # Calculate statistics by age group
  for (group in levels(data$age_group)) {
    subset <- data %>% filter(age_group == group)

    if (nrow(subset) > 0) {
      # Calculate means
      wait_change_mean <- mean(subset$wait_diff, na.rm = TRUE)
      relaxation_mean <- mean(subset$relaxation_effect, na.rm = TRUE)
      usage_change_mean <- mean(subset$transit_usage_change, na.rm = TRUE)

      # Statistical tests
      wait_test <- wait_ttest(subset)
      usage_test <- usage_ttest(subset)

      # Get significance markers
      wait_significance <- if (is.na(wait_test$p_value)) "" else {
        if (wait_test$p_value < 0.001) "***"
        else if (wait_test$p_value < 0.01) "**"
        else if (wait_test$p_value < 0.05) "*"
        else ""
      }

      usage_significance <- if (is.na(usage_test$p_value)) "" else {
        if (usage_test$p_value < 0.001) "***"
        else if (usage_test$p_value < 0.01) "**"
        else if (usage_test$p_value < 0.05) "*"
        else ""
      }

      # Add to results
      age_results <- rbind(age_results, data.frame(
        Category = "Age Groups",
        Group = group,
        Wait_Perception_Change = round(wait_change_mean, 2),
        P_Value = wait_test$p_value,
        Significance = wait_significance,
        Relaxation_Effect = round(relaxation_mean, 2),
        Transit_Usage_Change = paste0("+", round(usage_change_mean * 100, 1), "%"),
        Transit_Usage_P = usage_test$p_value,
        Transit_Significance = usage_significance,
        Sample_Size = nrow(subset)
      ))
    }
  }

  # Calculate statistics by usage frequency
  for (group in levels(data$usage_frequency)) {
    subset <- data %>% filter(usage_frequency == group)

    if (nrow(subset) > 0) {
      # Calculate means
      wait_change_mean <- mean(subset$wait_diff, na.rm = TRUE)
      relaxation_mean <- mean(subset$relaxation_effect, na.rm = TRUE)
      usage_change_mean <- mean(subset$transit_usage_change, na.rm = TRUE)

      # Statistical tests
      wait_test <- wait_ttest(subset)
      usage_test <- usage_ttest(subset)

      # Get significance markers
      wait_significance <- if (is.na(wait_test$p_value)) "" else {
        if (wait_test$p_value < 0.001) "***"
        else if (wait_test$p_value < 0.01) "**"
        else if (wait_test$p_value < 0.05) "*"
        else ""
      }

      usage_significance <- if (is.na(usage_test$p_value)) "" else {
        if (usage_test$p_value < 0.001) "***"
        else if (usage_test$p_value < 0.01) "**"
        else if (usage_test$p_value < 0.05) "*"
        else ""
      }

      # Add to results
      usage_results <- rbind(usage_results, data.frame(
        Category = "Usage Frequency",
        Group = group,
        Wait_Perception_Change = round(wait_change_mean, 2),
        P_Value = wait_test$p_value,
        Significance = wait_significance,
        Relaxation_Effect = round(relaxation_mean, 2),
        Transit_Usage_Change = paste0("+", round(usage_change_mean * 100, 1), "%"),
        Transit_Usage_P = usage_test$p_value,
        Transit_Significance = usage_significance,
        Sample_Size = nrow(subset)
      ))
    }
  }

  # Combine results
  wait_exp_demographic <- rbind(age_results, usage_results)

  return(wait_exp_demographic)
}

# Generate Table 9
table9 <- generate_wait_exp_by_demographics_table(survey_data)
message("Table 9 generated with ", nrow(table9), " rows")
print(table9)

#------------------------------------------------------------------------------
# TABLE 10: EFFECTIVENESS OF OFFLINE FEATURE BY LINE
#------------------------------------------------------------------------------

generate_offline_by_line_table <- function(data) {
  # Create necessary variables

  # Filter to users who reported using the app offline (USE_5 is 1 or 2)
  offline_users <- data %>%
    filter(USE_5 %in% c(1, 2))

  # Calculate wait time reduction
  offline_users$wait_reduction <- offline_users$WAIT_1_minutes - offline_users$WAIT_2_minutes

  # Calculate offline accuracy (OFF_2)
  # 5 = "Mostly accurate" down to 1 = "Mostly inaccurate"
  # Convert to percentage (5 = 100%, 1 = 0%)
  offline_users$accuracy_pct <- (offline_users$OFF_2 - 1) / 4 * 100

  # User satisfaction with app (SAT_1)
  offline_users$user_satisfaction <- offline_users$SAT_1

  # Check if there are offline users
  if (nrow(offline_users) == 0) {
    message("Warning: No users reported using the app offline. Creating placeholder table.")

    # Create a placeholder table
    offline_line_table <- data.frame(
      Line = c("Ankaray (A1)", "Sincan-Çayyolu (M1)",
               "Keçiören Metro (M4)", "Başkentray (B1)"),
      Offline_Accuracy = NA,
      Wait_Time_Reduction = NA,
      Wait_Time_P = NA,
      Wait_Time_Significance = "",
      User_Satisfaction = NA,
      Sample_Size = 0
    )

    return(offline_line_table)
  }

  # Create a table for each line
  lines <- c("Ankaray (A1)", "Sincan-Çayyolu (M1)",
             "Keçiören Metro (M4)", "Başkentray (B1)")

  line_cols <- paste0("DEM_3_", 1:4)

  offline_line_table <- data.frame(
    Line = character(),
    Offline_Accuracy = numeric(),
    Wait_Time_Reduction = numeric(),
    Wait_Time_P = numeric(),
    Wait_Time_Significance = character(),
    User_Satisfaction = numeric(),
    Sample_Size = integer()
  )

  # Calculate statistics for each line
  for (i in seq_along(lines)) {
    line_name <- lines[i]
    line_col <- line_cols[i]

    # Filter users who use this line
    line_users <- offline_users %>%
      filter(get(line_col) == TRUE)

    if (nrow(line_users) > 0) {
      # Calculate means
      accuracy_mean <- mean(line_users$accuracy_pct, na.rm = TRUE)
      wait_reduction_mean <- mean(line_users$wait_reduction, na.rm = TRUE)
      satisfaction_mean <- mean(line_users$user_satisfaction, na.rm = TRUE)

      # Paired t-test for wait time reduction
      if (nrow(line_users) >= 2) {
        wait_test <- tryCatch({
          t.test(line_users$WAIT_1_minutes,
                 line_users$WAIT_2_minutes,
                 paired = TRUE)
        }, error = function(e) {
          message("Error in line wait time t-test: ", e$message)
          return(list(p.value = NA))
        })
        wait_p <- wait_test$p.value
      } else {
        wait_p <- NA
      }

      # Get significance marker
      wait_significance <- if (is.na(wait_p)) "" else {
        if (wait_p < 0.001) "***"
        else if (wait_p < 0.01) "**"
        else if (wait_p < 0.05) "*"
        else ""
      }

      # Add to table
      offline_line_table <- rbind(offline_line_table, data.frame(
        Line = line_name,
        Offline_Accuracy = round(accuracy_mean, 1),
        Wait_Time_Reduction = round(wait_reduction_mean, 2),
        Wait_Time_P = wait_p,
        Wait_Time_Significance = wait_significance,
        User_Satisfaction = round(satisfaction_mean, 2),
        Sample_Size = nrow(line_users)
      ))
    } else {
      # Add row with NAs for this line
      offline_line_table <- rbind(offline_line_table, data.frame(
        Line = line_name,
        Offline_Accuracy = NA,
        Wait_Time_Reduction = NA,
        Wait_Time_P = NA,
        Wait_Time_Significance = "",
        User_Satisfaction = NA,
        Sample_Size = 0
      ))
    }
  }

  return(offline_line_table)
}

# Generate Table 10
table10 <- generate_offline_by_line_table(survey_data)
message("Table 10 generated with ", nrow(table10), " rows")
print(table10)

#------------------------------------------------------------------------------
# EXPORT TABLES TO CSV
#------------------------------------------------------------------------------

# Function to export a table to CSV
export_table <- function(table, filename) {
  # Create Result_csv directory if it doesn't exist
  result_dir <- "Result_csv"
  if (!dir.exists(result_dir)) {
    dir.create(result_dir)
    message("Created directory: ", result_dir)
  }

  # Construct the full path
  filepath <- file.path(result_dir, basename(filename))

  # Write the CSV file
  write.csv(table, filepath, row.names = FALSE)
  message("Exported ", filepath)
}

# Export all tables
export_table(table1, "Analysis/table1_demographics.csv")
export_table(table1a, "Analysis/table1a_demographics.csv")
export_table(table1b, "Analysis/table1b_transportation_patterns.csv")
export_table(table1c, "Analysis/table1c_transit_lines_purposes.csv")
export_table(table2, "Analysis/table2_usage_patterns.csv")
export_table(table3, "Analysis/table3_wait_time_comparison.csv")
export_table(table4, "Analysis/table4_wait_experience.csv")
export_table(table5, "Analysis/table5_satisfaction_metrics.csv")
export_table(table6, "Analysis/table6_behavioral_changes.csv")
export_table(table7, "Analysis/table7_wait_behavior_model.csv")
export_table(table8, "Analysis/table8_wait_time_regression.csv")
export_table(table9, "Analysis/table9_wait_exp_demographics.csv")
export_table(table10, "Analysis/table10_offline_by_line.csv")

# Summary of results
message("\n=== Summary of Results ===")
message("All 12 tables have been generated and exported to CSV files in the Result_csv directory.")
message("Table 1: Demographics and Travel Habits (Combined) - ", nrow(table1), " rows")
message("Table 1.A: Demographics (Age and Gender) - ", nrow(table1a), " rows")
message("Table 1.B: Transportation Patterns - ", nrow(table1b), " rows")
message("Table 1.C: Transit Lines and Trip Purposes - ", nrow(table1c), " rows")
message("Table 2: DurakGO Usage Patterns - ", nrow(table2), " rows")
message("Table 3: Wait Time Perception Comparison - ", nrow(table3), " rows")
message("Table 4: Wait Experience by Wait Time Category - ", nrow(table4), " rows")
message("Table 5: User Satisfaction Metrics - ", nrow(table5), " rows")
message("Table 6: Reported Behavioral Changes - ", nrow(table6), " rows")
message("Table 7: Multinomial Logit Model for Wait Time Behavior - ", nrow(table7), " rows")
message("Table 8: Factors Affecting Wait Time Reduction - ", nrow(table8), " rows")
message("Table 9: Wait Experience by Demographics - ", nrow(table9), " rows")
message("Table 10: Effectiveness of Offline Feature by Line - ", nrow(table10), " rows")
message("=== Analysis Complete ===")

# Close the output capture
sink()
close(output_conn)
message("Output has been saved to ", output_file)
