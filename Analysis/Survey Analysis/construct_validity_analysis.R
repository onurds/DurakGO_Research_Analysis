library(psych)      # For factor analysis and psychological scales
library(lavaan)     # For confirmatory factor analysis and SEM
library(semPlot)    # For visualizing SEM models
library(dplyr)      # For data manipulation
library(tidyr)      # For data reshaping
library(ggplot2)    # For visualizations
library(corrplot)   # For correlation visualization
library(GPArotation) # For factor rotation methods
library(knitr)      # For table formatting
library(kableExtra) # For enhanced tables


#------------------------------------------------------------------------------
# DATA LOADING AND PREPARATION
#------------------------------------------------------------------------------

# Function to read and prepare the survey data
prepare_survey_data <- function(file_path) {
  # Check if the file exists
  if (!file.exists(file_path)) {
    message("File not found: ", file_path)
    stop("File not found: ", file_path)
  }

  # Read the CSV file
  message("Reading file: ", file_path)
  survey_data <- read.csv(file_path, stringsAsFactors = FALSE,
                          fileEncoding = "UTF-8")

  message("Number of rows read: ", nrow(survey_data))

  # Function to parse multiple-choice responses
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

  # Process multiple-choice columns
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

      # Create indicator variables
      for (i in 1:max_options) {
        new_col <- paste0(col, "_", i)

        # Initialize with FALSE
        survey_data[[new_col]] <- logical(nrow(survey_data))

        # For each row in the data
        for (row in seq_len(nrow(survey_data))) {
          choices <- parse_multiple_choice(survey_data[row, col])
          survey_data[row, new_col] <- !is.null(choices) && i %in% choices
        }
      }
    }
  }

  # Convert wait time categories to actual minutes (midpoint of range)
  wait_time_midpoints <- c(1.5, 4, 8, 13, 20)  # Midpoints of wait time ranges

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

  # Create meaningful derived variables for analysis
  # Wait time perception scale
  survey_data$wait_perception <- survey_data$WAIT_3

  # Relaxation effect
  survey_data$relaxation_effect <- survey_data$WAIT_4

  # System satisfaction
  survey_data$system_satisfaction <- survey_data$IMP_3

  # App satisfaction
  survey_data$app_satisfaction <- survey_data$SAT_1

  # Recommendation likelihood
  survey_data$recommend_likelihood <- survey_data$SAT_5

  # Offline functionality satisfaction
  survey_data$offline_satisfaction <- survey_data$OFF_1

  # Offline accuracy perception
  survey_data$offline_accuracy <- survey_data$OFF_2

  # Offline importance
  survey_data$offline_importance <- survey_data$OFF_3

  # Create multi-item scales for construct validity analysis
  # These are the key constructs for analysis

  # Wait Time Experience construct: WAIT_3, WAIT_4, wait_time_diff
  survey_data$wait_exp_score <- rowMeans(
    cbind(scale(survey_data$WAIT_3),
          scale(survey_data$WAIT_4),
          scale(survey_data$wait_time_diff)),
    na.rm = TRUE)

  # App Satisfaction construct: SAT_1, SAT_5, IMP_2
  survey_data$app_sat_score <- rowMeans(
    cbind(scale(survey_data$SAT_1),
          scale(survey_data$SAT_5),
          scale(survey_data$IMP_2)),
    na.rm = TRUE)

  # System Satisfaction construct: IMP_3, IMP_1
  survey_data$sys_sat_score <- rowMeans(
    cbind(scale(survey_data$IMP_3),
          scale(survey_data$IMP_1)),
    na.rm = TRUE)

  # Offline Functionality construct: OFF_1, OFF_2, OFF_3
  survey_data$offline_score <- rowMeans(
    cbind(scale(survey_data$OFF_1),
          scale(survey_data$OFF_2),
          scale(survey_data$OFF_3)),
    na.rm = TRUE)

  # Print column names for verification
  message("Columns in processed data: ", length(names(survey_data)))

  return(survey_data)
}

# Load the data
file_path <- "Analysis/Survey_Data/final_data.csv"
survey_data <- prepare_survey_data(file_path)

# Print dataset summary
message("Data loaded: ", nrow(survey_data), " rows and ", ncol(survey_data), " columns")

#------------------------------------------------------------------------------
# CONSTRUCT VALIDITY ANALYSIS
#------------------------------------------------------------------------------

# Define key constructs for validity analysis
# Each construct consists of multiple items that should theoretically measure the same underlying concept
constructs <- list(
  wait_time_experience = c("WAIT_3", "WAIT_4", "wait_time_diff"),
  app_satisfaction = c("SAT_1", "SAT_5", "IMP_2"),
  system_satisfaction = c("IMP_3", "IMP_1"),
  offline_functionality = c("OFF_1", "OFF_2", "OFF_3")
)

#------------------------------------------------------------------------------
# FUNCTION 1: EXPLORATORY FACTOR ANALYSIS (EFA)
#------------------------------------------------------------------------------

perform_exploratory_factor_analysis <- function(data, constructs) {
  # Combine all items from all constructs
  all_items <- unlist(constructs)

  # Subset the data to include only these items
  efa_data <- data[, all_items, drop = FALSE]

  # Handle missing values - use pairwise complete observations
  cor_matrix <- cor(efa_data, use = "pairwise.complete.obs")

  # Determine the number of factors to extract
  # Using parallel analysis
  parallel_result <- fa.parallel(cor_matrix, n.obs = nrow(data), fa = "fa",
                               fm = "ml", n.iter = 100)

  n_factors <- parallel_result$nfact
  message("Parallel analysis suggests ", n_factors, " factors")

  # If parallel analysis suggests fewer factors than theoretical constructs,
  # use the number of theoretical constructs instead
  if (n_factors < length(constructs)) {
    n_factors <- length(constructs)
    message("Using theoretical number of constructs (", n_factors, ") instead")
  }

  # Perform factor analysis
  efa_result <- fa(cor_matrix, nfactors = n_factors, rotate = "oblimin",
                 fm = "ml", scores = "regression")

  # Extract factor loadings
  loadings <- data.frame(efa_result$loadings[, 1:n_factors])
  names(loadings) <- paste0("Factor", 1:n_factors)
  loadings$Item <- rownames(loadings)

  # Reorder columns to put Item first
  loadings <- loadings[, c(ncol(loadings), 1:(ncol(loadings)-1))]

  # Add theoretical construct information
  loadings$TheoreticalConstruct <- NA
  for (construct_name in names(constructs)) {
    loadings$TheoreticalConstruct[loadings$Item %in% constructs[[construct_name]]] <- construct_name
  }

  # Map each factor to the most likely theoretical construct
  # by examining which items load most strongly on each factor
  factor_mapping <- character(n_factors)
  for (i in 1:n_factors) {
    factor_col <- paste0("Factor", i)
    # For each factor, find which construct has the highest average loading
    construct_avg_loadings <- sapply(constructs, function(construct_items) {
      mean(abs(loadings[loadings$Item %in% construct_items, factor_col]), na.rm = TRUE)
    })
    factor_mapping[i] <- names(construct_avg_loadings)[which.max(construct_avg_loadings)]
  }

  # Create a nicely formatted loading matrix for reporting
  formatted_loadings <- loadings
  for (i in 1:(ncol(formatted_loadings)-2)) {
    col_idx <- i + 1  # +1 because Item is the first column
    formatted_loadings[, col_idx] <- round(formatted_loadings[, col_idx], 3)

    # Highlight loadings > 0.4 (common threshold)
    formatted_loadings[abs(formatted_loadings[, col_idx]) < 0.4, col_idx] <- NA
  }

  # Return the results including factor loadings and construct mapping
  return(list(
    raw_efa = efa_result,
    loadings = loadings,
    formatted_loadings = formatted_loadings,
    factor_mapping = factor_mapping,
    n_factors = n_factors,
    variance_explained = efa_result$Vaccounted
  ))
}

#------------------------------------------------------------------------------
# FUNCTION 2: CONFIRMATORY FACTOR ANALYSIS (CFA)
#------------------------------------------------------------------------------

perform_confirmatory_factor_analysis <- function(data, constructs) {
  # Create the model specification for lavaan
  model_spec <- ""

  # Add specifications for each construct
  for (construct_name in names(constructs)) {
    items <- constructs[[construct_name]]

    # Ensure at least 2 items per construct
    if (length(items) < 2) {
      message("Skipping ", construct_name, " - needs at least 2 items for CFA")
      next
    }

    # Add construct specification
    model_spec <- paste0(model_spec,
                        "\n# ", construct_name, " construct\n",
                        construct_name, " =~ ")

    # Add items to the construct
    for (i in seq_along(items)) {
      model_spec <- paste0(model_spec, items[i])
      if (i < length(items)) {
        model_spec <- paste0(model_spec, " + ")
      }
    }
    model_spec <- paste0(model_spec, "\n")
  }

  message("CFA Model Specification:")
  message(model_spec)

  # Prepare data for CFA
  cfa_data <- data[, unlist(constructs), drop = FALSE]

  # Handle missing data - use FIML
  cfa_result <- tryCatch({
    cfa(model_spec, data = cfa_data, missing = "fiml", std.lv = TRUE)
  }, error = function(e) {
    message("Error in CFA: ", e$message)
    # Try with a simpler specification if the first attempt fails
    try(cfa(model_spec, data = cfa_data, missing = "listwise", std.lv = TRUE))
  })

  # If CFA still fails, return NULL
  if (inherits(cfa_result, "try-error")) {
    message("CFA failed with both FIML and listwise deletion. Check the data and model.")
    return(NULL)
  }

  # Extract fit indices
  fit_indices <- fitMeasures(cfa_result)

  # Create a data frame for fit indices
  fit_indices_df <- data.frame(
    Fit_Index = names(fit_indices),
    Value = as.numeric(fit_indices),
    stringsAsFactors = FALSE
  )

  # Extract standardized loadings
  loadings <- standardizedSolution(cfa_result)
  loadings <- loadings[loadings$op == "=~", ]

  # Create a more user-friendly loadings data frame
  loadings_df <- data.frame(
    lhs = loadings$lhs,     # Construct
    rhs = loadings$rhs,     # Item
    est.std = loadings$est.std,  # Standardized loading
    pvalue = loadings$pvalue,    # p-value
    stringsAsFactors = FALSE
  )

  # Return CFA results
  return(list(
    cfa_model = cfa_result,
    fit_indices = fit_indices_df,
    loadings = loadings_df,
    summary = summary(cfa_result, fit.measures = TRUE, standardized = TRUE)
  ))
}

#------------------------------------------------------------------------------
# FUNCTION 3: CONVERGENT VALIDITY ANALYSIS
#------------------------------------------------------------------------------

analyze_convergent_validity <- function(cfa_results, constructs) {
  if (is.null(cfa_results) || !("loadings" %in% names(cfa_results))) {
    message("CFA results not available for convergent validity analysis")

    # Create placeholder results
    convergent_df <- data.frame(
      Construct = names(constructs),
      AVE = NA,
      CR = NA,
      Meets_Convergent_Validity = NA,
      stringsAsFactors = FALSE
    )

    return(convergent_df)
  }

  # Initialize results data frame
  convergent_df <- data.frame(
    Construct = character(),
    AVE = numeric(),
    CR = numeric(),
    Meets_Convergent_Validity = logical(),
    stringsAsFactors = FALSE
  )

  # Calculate Average Variance Extracted (AVE) and Composite Reliability (CR) for each construct
  loadings <- cfa_results$loadings

  for (construct_name in names(constructs)) {
    # Get standardized loadings for this construct
    construct_loadings <- loadings$est.std[loadings$lhs == construct_name]

    # Skip if no loadings found
    if (length(construct_loadings) == 0) {
      message("No loadings found for construct: ", construct_name)
      next
    }

    # Calculate AVE: average of squared standardized loadings
    ave <- mean(construct_loadings^2)

    # Calculate CR: (sum of standardized loadings)^2 / [(sum of standardized loadings)^2 + sum of measurement error]
    # Where measurement error = 1 - standardized loading^2
    sum_loadings <- sum(construct_loadings)
    sum_error <- sum(1 - construct_loadings^2)
    cr <- sum_loadings^2 / (sum_loadings^2 + sum_error)

    # Check if construct meets convergent validity criteria
    # AVE > 0.5 and CR > 0.7 are common thresholds
    meets_criteria <- ave > 0.5 & cr > 0.7

    # Add to results data frame
    convergent_df <- rbind(convergent_df, data.frame(
      Construct = construct_name,
      AVE = ave,
      CR = cr,
      Meets_Convergent_Validity = meets_criteria,
      stringsAsFactors = FALSE
    ))
  }

  return(convergent_df)
}

#------------------------------------------------------------------------------
# FUNCTION 4: DISCRIMINANT VALIDITY ANALYSIS
#------------------------------------------------------------------------------

analyze_discriminant_validity <- function(cfa_results, convergent_validity) {
  if (is.null(cfa_results) || !("cfa_model" %in% names(cfa_results))) {
    message("CFA results not available for discriminant validity analysis")

    # Create placeholder results with at least one row
    if (nrow(convergent_validity) > 1) {
      constructs <- convergent_validity$Construct
      # Create pairs of constructs
      pairs <- t(combn(constructs, 2))

      discriminant_df <- data.frame(
        Construct1 = pairs[,1],
        Construct2 = pairs[,2],
        Correlation = NA,
        Squared_Correlation = NA,
        AVE_Construct1 = NA,
        AVE_Construct2 = NA,
        Meets_Discriminant_Validity = NA,
        stringsAsFactors = FALSE
      )
    } else {
      discriminant_df <- data.frame(
        Construct1 = character(),
        Construct2 = character(),
        Correlation = numeric(),
        Squared_Correlation = numeric(),
        AVE_Construct1 = numeric(),
        AVE_Construct2 = numeric(),
        Meets_Discriminant_Validity = logical(),
        stringsAsFactors = FALSE
      )
    }

    return(discriminant_df)
  }

  # Get latent variable correlations from CFA model
  construct_cors <- lavInspect(cfa_results$cfa_model, "cor.lv")

  # Initialize results data frame
  discriminant_df <- data.frame(
    Construct1 = character(),
    Construct2 = character(),
    Correlation = numeric(),
    Squared_Correlation = numeric(),
    AVE_Construct1 = numeric(),
    AVE_Construct2 = numeric(),
    Meets_Discriminant_Validity = logical(),
    stringsAsFactors = FALSE
  )

  # Process each pair of constructs
  constructs <- rownames(construct_cors)
  if (length(constructs) > 1) {
    for (i in 1:(length(constructs)-1)) {
      for (j in (i+1):length(constructs)) {
        construct1 <- constructs[i]
        construct2 <- constructs[j]

        # Get correlation between constructs
        correlation <- construct_cors[i, j]
        squared_correlation <- correlation^2

        # Get AVE for each construct
        ave1 <- convergent_validity$AVE[convergent_validity$Construct == construct1]
        ave2 <- convergent_validity$AVE[convergent_validity$Construct == construct2]

        # If AVE not found, use NA
        if (length(ave1) == 0) ave1 <- NA
        if (length(ave2) == 0) ave2 <- NA

        # Check if meets Fornell-Larcker criterion: AVE > squared correlation
        meets_criterion <- !is.na(ave1) && !is.na(ave2) &&
                          ave1 > squared_correlation && ave2 > squared_correlation

        # Add to results data frame
        discriminant_df <- rbind(discriminant_df, data.frame(
          Construct1 = construct1,
          Construct2 = construct2,
          Correlation = correlation,
          Squared_Correlation = squared_correlation,
          AVE_Construct1 = ave1,
          AVE_Construct2 = ave2,
          Meets_Discriminant_Validity = meets_criterion,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  return(discriminant_df)
}

#------------------------------------------------------------------------------
# FUNCTION 5: CONSTRUCT CORRELATIONS
#------------------------------------------------------------------------------

analyze_construct_correlations <- function(data, constructs) {
  # Create a data frame with construct scores
  # Use the construct names from the constructs parameter
  construct_scores <- data.frame(matrix(ncol = length(names(constructs)), nrow = nrow(data)))
  colnames(construct_scores) <- names(constructs)

  # Map the constructs to their corresponding score variables in the data
  construct_to_score <- c(
    "wait_time_experience" = "wait_exp_score",
    "app_satisfaction" = "app_sat_score",
    "system_satisfaction" = "sys_sat_score",
    "offline_functionality" = "offline_score"
  )

  # Fill the construct scores data frame
  for (construct_name in names(constructs)) {
    if (construct_name %in% names(construct_to_score)) {
      score_var <- construct_to_score[construct_name]
      if (score_var %in% colnames(data)) {
        construct_scores[[construct_name]] <- data[[score_var]]
      } else {
        warning("Score variable ", score_var, " not found in data for construct ", construct_name)
        # Fill with NA if score variable not found
        construct_scores[[construct_name]] <- NA
      }
    } else {
      warning("No score mapping for construct ", construct_name)
      # Calculate score from items if mapping not found
      items <- constructs[[construct_name]]
      if (all(items %in% colnames(data))) {
        # Use rowMeans with scaling to create a score
        construct_scores[[construct_name]] <- rowMeans(
          scale(as.matrix(data[, items, drop = FALSE])),
          na.rm = TRUE
        )
      } else {
        warning("Not all items for construct ", construct_name, " found in data")
        construct_scores[[construct_name]] <- NA
      }
    }
  }

  # Calculate correlation matrix
  cor_matrix <- cor(construct_scores, use = "pairwise.complete.obs")

  # Create a long-format data frame for easier processing
  cor_df <- data.frame(
    Construct1 = character(),
    Construct2 = character(),
    Correlation = numeric(),
    stringsAsFactors = FALSE
  )

  # Convert correlation matrix to data frame
  construct_names <- colnames(cor_matrix)
  for (i in seq_along(construct_names)) {
    for (j in seq_along(construct_names)) {
      cor_df <- rbind(cor_df, data.frame(
        Construct1 = construct_names[i],
        Construct2 = construct_names[j],
        Correlation = cor_matrix[i, j],
        stringsAsFactors = FALSE
      ))
    }
  }

  # Create visualization (for illustration, not saved to file)
  corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200))

  return(cor_df)
}

#------------------------------------------------------------------------------
# FUNCTION 6: KNOWN-GROUPS VALIDITY
#------------------------------------------------------------------------------

analyze_known_groups_validity <- function(data, constructs) {
  # Define groups for known-groups validity testing
  # Test differences between:
  # 1. Frequent vs. infrequent app users (USE_2)
  # 2. Experienced vs. new users (USE_1)
  # 3. Offline vs. online users (USE_5)
  # 4. Different primary metro lines (DEM_3)

  # Initialize results data frame
  known_groups_df <- data.frame(
    Construct = character(),
    Grouping_Variable = character(),
    Groups_Compared = character(),
    Mean_Difference = numeric(),
    t_value = numeric(),
    p_value = numeric(),
    Significant = logical(),
    stringsAsFactors = FALSE
  )

  # 1. Test frequency of use (frequent vs. infrequent)
  # Frequent = daily (USE_2 <= 2), Infrequent = weekly or less (USE_2 >= 3)
  data$use_frequency_group <- ifelse(data$USE_2 <= 2, "Frequent", "Infrequent")

  # 2. Test experience (experienced vs. new)
  # Experienced = more than 1 month (USE_1 >= 3), New = less than 1 month (USE_1 <= 2)
  data$experience_group <- ifelse(data$USE_1 >= 3, "Experienced", "New")

  # 3. Test offline usage (offline vs. online)
  # Offline users = USE_5 <= 2, Online-only users = USE_5 >= 3
  data$offline_usage_group <- ifelse(data$USE_5 <= 2, "Offline", "Online-only")

  # 4. Create primary line variable based on DEM_3 columns
  # For each user, find which metro line they use most frequently
  line_cols <- c("DEM_3_1", "DEM_3_2", "DEM_3_3", "DEM_3_4")
  line_names <- c("Ankaray (A1)", "Sincan-Çayyolu (M1)", "Keçiören Metro (M4)", "Başkentray (B1)")

  # Convert logical values to numeric for calculations
  for (i in seq_along(line_cols)) {
    data[[paste0("line_", i, "_numeric")]] <- as.numeric(data[[line_cols[i]]])
  }

  # Create binary indicators for each line (for comparison testing)
  for (i in seq_along(line_cols)) {
    data[[paste0("uses_line_", i)]] <- data[[line_cols[i]]]
  }

  # Determine primary line for each user
  data$primary_line <- NA
  data$primary_line_name <- NA
  data$line_count <- rowSums(data[, paste0("line_", seq_along(line_cols), "_numeric")], na.rm = TRUE)

  # For users who use only one line, that's their primary line
  for (i in seq_len(nrow(data))) {
    if (data$line_count[i] == 1) {
      # Find which line they use
      for (j in seq_along(line_cols)) {
        if (data[[paste0("line_", j, "_numeric")]][i] == 1) {
          data$primary_line[i] <- j
          data$primary_line_name[i] <- line_names[j]
          break
        }
      }
    }
    else if (data$line_count[i] > 1) {
      # For users with multiple lines, more information is needed to determine primary
      # Using trip purpose and frequency data if available

      # Check if determination can be made from trip purpose (DEM_4)

      # For demonstration, prioritize commuting (work/school)
      if (!is.na(data$DEM_4_1[i]) && data$DEM_4_1[i]) {
        # If they commute to work, prioritize M1 (most business districts)
        for (j in c(2, 3, 4, 1)) { # Priority order: M1, M4, B1, A1
          if (data[[paste0("line_", j, "_numeric")]][i] == 1) {
            data$primary_line[i] <- j
            data$primary_line_name[i] <- line_names[j]
            break
          }
        }
      }
      else if (!is.na(data$DEM_4_2[i]) && data$DEM_4_2[i]) {
        # If they commute to school, prioritize lines near universities
        for (j in c(1, 2, 3, 4)) { # Priority order: A1, M1, M4, B1
          if (data[[paste0("line_", j, "_numeric")]][i] == 1) {
            data$primary_line[i] <- j
            data$primary_line_name[i] <- line_names[j]
            break
          }
        }
      }
      else {
        # Otherwise just take the first line they use
        for (j in seq_along(line_cols)) {
          if (data[[paste0("line_", j, "_numeric")]][i] == 1) {
            data$primary_line[i] <- j
            data$primary_line_name[i] <- line_names[j]
            break
          }
        }
      }
    }
    # If line_count is 0, primary_line remains NA
  }

  # Create a factor variable for the primary line
  data$primary_line_factor <- factor(data$primary_line_name,
                                    levels = line_names,
                                    labels = line_names)

  # Function to test group differences for a construct and grouping variable
  test_group_differences <- function(construct_name, score_var, group_var, group_var_name) {
    # Get the unique groups
    groups <- unique(data[[group_var]])
    groups <- groups[!is.na(groups)]

    # Skip if fewer than 2 groups
    if (length(groups) < 2) {
      message("Skipping ", group_var, " - fewer than 2 groups")
      return(NULL)
    }

    # For binary comparisons, use t-test
    if (length(groups) == 2) {
      # Ensure group names are sorted
      groups <- sort(groups)

      # Subset data for each group
      group1_data <- data[data[[group_var]] == groups[1], score_var]
      group2_data <- data[data[[group_var]] == groups[2], score_var]

      # Skip if either group has fewer than 3 observations
      if (sum(!is.na(group1_data)) < 3 || sum(!is.na(group2_data)) < 3) {
        message("Skipping ", group_var, " - insufficient observations in groups")
        return(NULL)
      }

      # Perform t-test
      t_result <- t.test(group1_data, group2_data)

      # Calculate mean difference
      mean_diff <- mean(group2_data, na.rm = TRUE) - mean(group1_data, na.rm = TRUE)

      # Create result row
      result <- data.frame(
        Construct = construct_name,
        Grouping_Variable = group_var_name,
        Groups_Compared = paste(groups[2], "vs", groups[1]),
        Mean_Difference = mean_diff,
        t_value = t_result$statistic,
        p_value = t_result$p.value,
        Significant = t_result$p.value < 0.05,
        stringsAsFactors = FALSE
      )

      return(result)
    }

    # For multiple groups, use ANOVA
    else {
      # Create formula for ANOVA
      formula <- as.formula(paste(score_var, "~", group_var))

      # Perform ANOVA
      anova_result <- tryCatch({
        aov(formula, data = data)
      }, error = function(e) {
        message("Error in ANOVA for ", group_var, ": ", e$message)
        return(NULL)
      })

      if (is.null(anova_result)) {
        return(NULL)
      }

      # Extract F-value and p-value
      anova_summary <- summary(anova_result)
      f_value <- anova_summary[[1]][1, "F value"]
      p_value <- anova_summary[[1]][1, "Pr(>F)"]

      # Calculate largest mean difference between any two groups
      group_means <- tapply(data[[score_var]], data[[group_var]], mean, na.rm = TRUE)
      mean_diff <- max(group_means, na.rm = TRUE) - min(group_means, na.rm = TRUE)

      # Find groups with max and min means
      max_group <- names(group_means)[which.max(group_means)]
      min_group <- names(group_means)[which.min(group_means)]

      # Create result row
      result <- data.frame(
        Construct = construct_name,
        Grouping_Variable = group_var_name,
        Groups_Compared = paste(max_group, "vs", min_group),
        Mean_Difference = mean_diff,
        t_value = f_value,  # Using F-value from ANOVA in place of t-value
        p_value = p_value,
        Significant = p_value < 0.05,
        stringsAsFactors = FALSE
      )

      return(result)
    }
  }

  # Test each construct with each grouping variable
  construct_names <- names(constructs)
  score_vars <- c("wait_exp_score", "app_sat_score", "sys_sat_score", "offline_score")

  # Map construct names to score variables
  construct_to_score <- setNames(score_vars, c("wait_time_experience", "app_satisfaction",
                                             "system_satisfaction", "offline_functionality"))

  # Define grouping variables to test
  grouping_vars <- list(
    use_frequency = list(var = "use_frequency_group", name = "Usage Frequency"),
    experience = list(var = "experience_group", name = "User Experience"),
    offline_usage = list(var = "offline_usage_group", name = "Offline Usage"),
    uses_line_1 = list(var = "uses_line_1", name = "Uses Ankaray (A1)"),
    uses_line_2 = list(var = "uses_line_2", name = "Uses Sincan-Çayyolu (M1)"),
    uses_line_3 = list(var = "uses_line_3", name = "Uses Keçiören Metro (M4)"),
    uses_line_4 = list(var = "uses_line_4", name = "Uses Başkentray (B1)")
  )

  # Run tests for each construct and grouping variable
  for (construct_name in construct_names) {
    score_var <- construct_to_score[construct_name]

    for (group_info in grouping_vars) {
      result <- test_group_differences(construct_name, score_var, group_info$var, group_info$name)

      if (!is.null(result)) {
        known_groups_df <- rbind(known_groups_df, result)
      }
    }
  }

  return(known_groups_df)
}

#------------------------------------------------------------------------------
# EXECUTE VALIDITY ANALYSES
#------------------------------------------------------------------------------

# 1. Perform exploratory factor analysis
efa_results <- perform_exploratory_factor_analysis(survey_data, constructs)
message("EFA completed with ", efa_results$n_factors, " factors extracted")

# 2. Perform confirmatory factor analysis
cfa_results <- perform_confirmatory_factor_analysis(survey_data, constructs)
if (!is.null(cfa_results)) {
  message("CFA completed successfully")
} else {
  message("CFA failed, continuing with other analyses")
}

# 3. Analyze convergent validity
convergent_validity <- analyze_convergent_validity(cfa_results, constructs)
message("Convergent validity analysis completed for ", nrow(convergent_validity), " constructs")

# 4. Analyze discriminant validity
discriminant_validity <- analyze_discriminant_validity(cfa_results, convergent_validity)
message("Discriminant validity analysis completed for ", nrow(discriminant_validity), " construct pairs")

# 5. Analyze construct correlations
construct_correlations <- analyze_construct_correlations(survey_data, constructs)
message("Construct correlations analysis completed")

# 6. Analyze known-groups validity
known_groups_validity <- analyze_known_groups_validity(survey_data, constructs)
message("Known-groups validity analysis completed with ", nrow(known_groups_validity), " tests")

#------------------------------------------------------------------------------
# CREATE RESULT DIRECTORY AND EXPORT RESULTS
#------------------------------------------------------------------------------

# Create Result_validity directory if it doesn't exist
result_dir <- "Survey_Results/Analysis Results/Result_csv"
if (!dir.exists(result_dir)) {
  dir.create(result_dir)
  message("Created directory: ", result_dir)
}

# Export convergent validity results
write.csv(convergent_validity, file.path(result_dir, "convergent_validity.csv"), row.names = FALSE)
message("Exported convergent validity results to ", file.path(result_dir, "convergent_validity.csv"))

# Export discriminant validity results
write.csv(discriminant_validity, file.path(result_dir, "discriminant_validity.csv"), row.names = FALSE)
message("Exported discriminant validity results to ", file.path(result_dir, "discriminant_validity.csv"))

# Export construct correlations
write.csv(construct_correlations, file.path(result_dir, "construct_correlations.csv"), row.names = FALSE)
message("Exported construct correlations to ", file.path(result_dir, "construct_correlations.csv"))

# Export known-groups validity results
write.csv(known_groups_validity, file.path(result_dir, "known_groups_validity.csv"), row.names = FALSE)
message("Exported known-groups validity results to ", file.path(result_dir, "known_groups_validity.csv"))

# Export EFA loadings if available
if (!is.null(efa_results) && "loadings" %in% names(efa_results)) {
  write.csv(efa_results$loadings, file.path(result_dir, "efa_loadings.csv"), row.names = FALSE)
  message("Exported EFA loadings to ", file.path(result_dir, "efa_loadings.csv"))
}

# Export CFA loadings and fit indices if available
if (!is.null(cfa_results)) {
  if ("loadings" %in% names(cfa_results)) {
    write.csv(cfa_results$loadings, file.path(result_dir, "cfa_loadings.csv"), row.names = FALSE)
    message("Exported CFA loadings to ", file.path(result_dir, "cfa_loadings.csv"))
  }

  if ("fit_indices" %in% names(cfa_results)) {
    write.csv(cfa_results$fit_indices, file.path(result_dir, "cfa_fit_indices.csv"), row.names = FALSE)
    message("Exported CFA fit indices to ", file.path(result_dir, "cfa_fit_indices.csv"))
  }
}

#------------------------------------------------------------------------------
# GENERATE SUMMARY REPORT
#------------------------------------------------------------------------------

generate_validity_summary <- function(efa_results, cfa_results, convergent_validity, discriminant_validity, known_groups_validity) {
  summary_text <- "# CONSTRUCT VALIDITY ANALYSIS SUMMARY\n\n"

  # Add EFA results section
  summary_text <- paste0(summary_text, "## Exploratory Factor Analysis\n\n")

  if (!is.null(efa_results)) {
    summary_text <- paste0(summary_text,
                         "* ", efa_results$n_factors, " factors extracted, explaining ",
                         round(sum(efa_results$variance_explained[2, 1:efa_results$n_factors]), 2),
                         "% of the total variance.\n")

    # Add factor mapping
    summary_text <- paste0(summary_text, "* Factor mapping to theoretical constructs:\n")
    for (i in seq_along(efa_results$factor_mapping)) {
      summary_text <- paste0(summary_text, "  * Factor ", i, " → ", efa_results$factor_mapping[i], "\n")
    }

    # Add notes about cross-loadings or problematic items
    problem_items <- character(0)
    for (i in seq_len(nrow(efa_results$loadings))) {
      item <- efa_results$loadings$Item[i]
      theoretical_construct <- efa_results$loadings$TheoreticalConstruct[i]

      # Count significant loadings (>0.4) on different factors
      loadings <- as.numeric(efa_results$loadings[i, 2:(efa_results$n_factors+1)])
      significant_loadings <- sum(abs(loadings) > 0.4)

      # If item loads on multiple factors, add to problem items
      if (significant_loadings > 1) {
        problem_items <- c(problem_items, item)
      }

      # If item doesn't load on its theoretical construct, add to problem items
      factor_for_theoretical_construct <- which(efa_results$factor_mapping == theoretical_construct)
      if (length(factor_for_theoretical_construct) > 0) {
        # Fix for the error: ensure vector is handled properly
        loading_on_theoretical_factor <- NA
        # Check each mapping that matches the theoretical construct
        for (factor_idx in factor_for_theoretical_construct) {
          if (factor_idx <= length(loadings)) {
            current_loading <- abs(loadings[factor_idx])
            # Either set the loading value or take the max if multiple matches
            if (is.na(loading_on_theoretical_factor) || current_loading > loading_on_theoretical_factor) {
              loading_on_theoretical_factor <- current_loading
            }
          }
        }

        # Now check if the loading is below threshold
        if (!is.na(loading_on_theoretical_factor) && loading_on_theoretical_factor < 0.4) {
          problem_items <- c(problem_items, item)
        }
      }
    }

    if (length(problem_items) > 0) {
      summary_text <- paste0(summary_text, "* Problematic items with cross-loadings or weak loadings on theoretical construct:\n")
      for (item in unique(problem_items)) {
        summary_text <- paste0(summary_text, "  * ", item, "\n")
      }
    } else {
      summary_text <- paste0(summary_text, "* All items loaded primarily on their theoretical constructs with no significant cross-loadings.\n")
    }
  } else {
    summary_text <- paste0(summary_text, "* EFA results not available.\n")
  }

  # Add CFA results section
  summary_text <- paste0(summary_text, "\n## Confirmatory Factor Analysis\n\n")

  if (!is.null(cfa_results) && "fit_indices" %in% names(cfa_results)) {
    # Extract key fit indices
    fit_indices <- cfa_results$fit_indices

    # Common fit indices to report
    key_indices <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr")

    summary_text <- paste0(summary_text, "* Model fit indices:\n")

    for (index in key_indices) {
      if (index %in% fit_indices$Fit_Index) {
        value <- fit_indices$Value[fit_indices$Fit_Index == index]

        # Format value based on index type
        if (index == "pvalue") {
          if (value < 0.001) {
            formatted_value <- "< 0.001"
          } else {
            formatted_value <- sprintf("%.3f", value)
          }
        } else {
          formatted_value <- sprintf("%.3f", value)
        }

        summary_text <- paste0(summary_text, "  * ", index, " = ", formatted_value, "\n")
      }
    }

    # Add interpretation of fit
    cfi_value <- fit_indices$Value[fit_indices$Fit_Index == "cfi"]
    rmsea_value <- fit_indices$Value[fit_indices$Fit_Index == "rmsea"]
    srmr_value <- fit_indices$Value[fit_indices$Fit_Index == "srmr"]

    if (!is.na(cfi_value) && !is.na(rmsea_value) && !is.na(srmr_value)) {
      if (cfi_value >= 0.95 && rmsea_value <= 0.06 && srmr_value <= 0.08) {
        fit_quality <- "excellent"
      } else if (cfi_value >= 0.90 && rmsea_value <= 0.08 && srmr_value <= 0.10) {
        fit_quality <- "acceptable"
      } else {
        fit_quality <- "suboptimal"
      }

      summary_text <- paste0(summary_text, "* The model shows ", fit_quality, " fit to the data.\n")
    }

    # Add loadings summary
    if ("loadings" %in% names(cfa_results)) {
      loadings <- cfa_results$loadings

      min_loading <- min(loadings$est.std)
      max_loading <- max(loadings$est.std)
      avg_loading <- mean(loadings$est.std)

      summary_text <- paste0(summary_text,
                           "* Standardized factor loadings range from ",
                           round(min_loading, 2), " to ", round(max_loading, 2),
                           " with an average of ", round(avg_loading, 2), ".\n")

      # Check for low loadings
      low_loadings <- loadings[loadings$est.std < 0.5, ]
      if (nrow(low_loadings) > 0) {
        summary_text <- paste0(summary_text, "* Items with low loadings (< 0.5):\n")
        for (i in seq_len(nrow(low_loadings))) {
          summary_text <- paste0(summary_text, "  * ", low_loadings$rhs[i],
                               " on ", low_loadings$lhs[i],
                               " (", round(low_loadings$est.std[i], 2), ")\n")
        }
      }
    }
  } else {
    summary_text <- paste0(summary_text, "* CFA results not available.\n")
  }

  # Add convergent validity section
  summary_text <- paste0(summary_text, "\n## Convergent Validity\n\n")

  if (nrow(convergent_validity) > 0) {
    valid_count <- sum(convergent_validity$Meets_Convergent_Validity, na.rm = TRUE)
    total_count <- nrow(convergent_validity)

    summary_text <- paste0(summary_text,
                         "* ", valid_count, " out of ", total_count,
                         " constructs demonstrate acceptable convergent validity.\n")

    summary_text <- paste0(summary_text, "* Convergent validity statistics:\n")

    for (i in seq_len(nrow(convergent_validity))) {
      summary_text <- paste0(summary_text,
                           "  * ", convergent_validity$Construct[i],
                           ": AVE = ", round(convergent_validity$AVE[i], 2),
                           ", CR = ", round(convergent_validity$CR[i], 2),
                           " (", ifelse(convergent_validity$Meets_Convergent_Validity[i], "Acceptable", "Problematic"), ")\n")
    }
  } else {
    summary_text <- paste0(summary_text, "* Convergent validity results not available.\n")
  }

  # Add discriminant validity section
  summary_text <- paste0(summary_text, "\n## Discriminant Validity\n\n")

  if (nrow(discriminant_validity) > 0) {
    valid_count <- sum(discriminant_validity$Meets_Discriminant_Validity, na.rm = TRUE)
    total_count <- nrow(discriminant_validity)

    summary_text <- paste0(summary_text,
                         "* ", valid_count, " out of ", total_count,
                         " construct pairs demonstrate acceptable discriminant validity.\n")

    # List problematic pairs
    problematic_pairs <- discriminant_validity[!discriminant_validity$Meets_Discriminant_Validity, ]

    if (nrow(problematic_pairs) > 0) {
      summary_text <- paste0(summary_text, "* Problematic construct pairs:\n")

      for (i in seq_len(nrow(problematic_pairs))) {
        summary_text <- paste0(summary_text,
                             "  * ", problematic_pairs$Construct1[i], " and ",
                             problematic_pairs$Construct2[i],
                             " (r = ", round(problematic_pairs$Correlation[i], 2),
                             ", r² = ", round(problematic_pairs$Squared_Correlation[i], 2), ")\n")
      }
    }
  } else {
    summary_text <- paste0(summary_text, "* Discriminant validity results not available.\n")
  }

  # Add known-groups validity section
  summary_text <- paste0(summary_text, "\n## Known-Groups Validity\n\n")

  if (nrow(known_groups_validity) > 0) {
    valid_count <- sum(known_groups_validity$Significant, na.rm = TRUE)
    total_count <- nrow(known_groups_validity)

    summary_text <- paste0(summary_text,
                         "* ", valid_count, " out of ", total_count,
                         " group comparisons showed significant differences.\n")

    # Summarize by construct
    constructs <- unique(known_groups_validity$Construct)

    for (construct in constructs) {
      construct_tests <- known_groups_validity[known_groups_validity$Construct == construct, ]
      construct_valid <- sum(construct_tests$Significant, na.rm = TRUE)
      construct_total <- nrow(construct_tests)

      summary_text <- paste0(summary_text,
                           "* ", construct, ": ", construct_valid, " out of ",
                           construct_total, " tests significant.\n")

      # List significant differences
      sig_tests <- construct_tests[construct_tests$Significant, ]

      if (nrow(sig_tests) > 0) {
        for (i in seq_len(nrow(sig_tests))) {
          p_value <- sig_tests$p_value[i]
          if (p_value < 0.001) {
            sig_level <- "p < 0.001"
          } else if (p_value < 0.01) {
            sig_level <- "p < 0.01"
          } else {
            sig_level <- "p < 0.05"
          }

          summary_text <- paste0(summary_text,
                               "  * ", sig_tests$Grouping_Variable[i], ": ",
                               sig_tests$Groups_Compared[i],
                               " (diff = ", round(sig_tests$Mean_Difference[i], 2),
                               ", ", sig_level, ")\n")
        }
      }
    }
  } else {
    summary_text <- paste0(summary_text, "* Known-groups validity results not available.\n")
  }

  # Add recommendations section
  summary_text <- paste0(summary_text, "\n## Recommendations\n\n")

  # Identify problematic constructs for convergent validity
  if (nrow(convergent_validity) > 0) {
    problematic_cv <- convergent_validity$Construct[!convergent_validity$Meets_Convergent_Validity]

    if (length(problematic_cv) > 0) {
      summary_text <- paste0(summary_text,
                           "* Revise items for constructs with low convergent validity: ",
                           paste(problematic_cv, collapse = ", "), ".\n")
    }
  }

  # Identify problematic construct pairs for discriminant validity
  if (nrow(discriminant_validity) > 0) {
    problematic_pairs <- discriminant_validity[!discriminant_validity$Meets_Discriminant_Validity, ]

    if (nrow(problematic_pairs) > 0) {
      unique_problematic_constructs <- unique(c(problematic_pairs$Construct1, problematic_pairs$Construct2))

      summary_text <- paste0(summary_text,
                           "* Clarify conceptual distinctions between related constructs: ",
                           paste(unique_problematic_constructs, collapse = ", "), ".\n")
    }
  }

  # If CFA fit is poor, recommend model modifications
  if (!is.null(cfa_results) && "fit_indices" %in% names(cfa_results)) {
    fit_indices <- cfa_results$fit_indices
    cfi_value <- fit_indices$Value[fit_indices$Fit_Index == "cfi"]
    rmsea_value <- fit_indices$Value[fit_indices$Fit_Index == "rmsea"]

    if (!is.na(cfi_value) && !is.na(rmsea_value) && (cfi_value < 0.90 || rmsea_value > 0.08)) {
      summary_text <- paste0(summary_text,
                           "* Consider model modifications to improve fit, such as adding error covariances or removing problematic items.\n")
    }
  }

  # General recommendations
  summary_text <- paste0(summary_text,
                       "* For future studies, consider collecting additional data to validate the measurement model.\n",
                       "* Perform test-retest reliability assessment to evaluate temporal stability.\n")

  return(summary_text)
}

# Generate summary
validity_summary <- generate_validity_summary(efa_results, cfa_results,
                                          convergent_validity, discriminant_validity,
                                          known_groups_validity)

# Save summary to text file
writeLines(validity_summary, file.path(result_dir, "validity_summary.txt"))
message("Validity summary saved to ", file.path(result_dir, "validity_summary.txt"))

# Create formatted tables for the paper
table_cv <- kable(convergent_validity, caption = "Convergent Validity Assessment") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

table_dv <- kable(discriminant_validity, caption = "Discriminant Validity Assessment") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

table_kg <- kable(known_groups_validity, caption = "Known-Groups Validity Assessment") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

# Save HTML tables
writeLines(table_cv, file.path(result_dir, "convergent_validity_table.html"))
writeLines(table_dv, file.path(result_dir, "discriminant_validity_table.html"))
writeLines(table_kg, file.path(result_dir, "known_groups_validity_table.html"))

message("\n=== CONSTRUCT VALIDITY ANALYSIS COMPLETE ===\n")
message("All results exported to the ", result_dir, " directory")
message("Files created:")
message("1. convergent_validity.csv")
message("2. discriminant_validity.csv")
message("3. construct_correlations.csv")
message("4. known_groups_validity.csv")
if (!is.null(efa_results) && "loadings" %in% names(efa_results)) {
  message("5. efa_loadings.csv")
}
if (!is.null(cfa_results)) {
  if ("loadings" %in% names(cfa_results)) {
    message("6. cfa_loadings.csv")
  }
  if ("fit_indices" %in% names(cfa_results)) {
    message("7. cfa_fit_indices.csv")
  }
}
message("8. validity_summary.txt")