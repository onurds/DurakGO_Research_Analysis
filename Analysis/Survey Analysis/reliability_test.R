library(psych)      # For psychometric analysis, Cronbach's alpha
library(ltm)        # For latent trait models and reliability measures
library(dplyr)      # For data manipulation
library(car)        # For data transformation
library(knitr)      # For table formatting
library(ggplot2)    # For visualizations
library(tidyr)      # For data reshaping

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

  # Create derived variables for analysis

  # Wait time perception scale (convert ordinal responses to numeric)
  survey_data$wait_perception <- survey_data$WAIT_3

  # Relaxation effect
  survey_data$relaxation_effect <- survey_data$WAIT_4

  # System satisfaction
  survey_data$system_satisfaction <- survey_data$IMP_3

  # App satisfaction
  survey_data$app_satisfaction <- survey_data$SAT_1

  # Recommendation likelihood
  survey_data$recommend_likelihood <- survey_data$SAT_5

  # Offline functionality importance
  survey_data$offline_importance <- survey_data$OFF_3

  # Offline functionality satisfaction
  survey_data$offline_satisfaction <- survey_data$OFF_1

  # Offline accuracy perception
  survey_data$offline_accuracy <- survey_data$OFF_2

  # Create multi-item scales

  # Wait time perception scale: WAIT_3, WAIT_4, and indirectly WAIT_1 vs WAIT_2
  survey_data$wait_exp_score <- rowMeans(
    cbind(scale(survey_data$WAIT_3),
          scale(survey_data$WAIT_4),
          scale(survey_data$wait_time_diff)),
    na.rm = TRUE)

  # App satisfaction scale: SAT_1, SAT_5, OFF_1
  survey_data$app_sat_score <- rowMeans(
    cbind(scale(survey_data$SAT_1),
          scale(survey_data$SAT_5),
          scale(survey_data$OFF_1)),
    na.rm = TRUE)

  # System satisfaction scale: IMP_3, IMP_2
  survey_data$sys_sat_score <- rowMeans(
    cbind(scale(survey_data$IMP_3),
          scale(survey_data$IMP_2)),
    na.rm = TRUE)

  # Offline functionality scale: OFF_1, OFF_2, OFF_3
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
# Add debugging information
print(paste("Current working directory:", getwd()))
file_path <- "Analysis/Survey_Data/2final_data.csv"
print(paste("Checking if file exists:", file.exists(file_path)))
print(paste("Absolute path:", normalizePath(file_path, mustWork = FALSE)))

# Try to load with relative path
if (file.exists(file_path)) {
  survey_data <- prepare_survey_data(file_path)
} else {
  # Try with absolute path as fallback
  abs_path <- file.path(getwd(), "Analysis/Survey_Data/2final_data.csv")
  abs_path <- normalizePath(abs_path, mustWork = FALSE)
  print(paste("Trying absolute path:", abs_path))
  print(paste("Absolute path exists:", file.exists(abs_path)))
  
  if (file.exists(abs_path)) {
    survey_data <- prepare_survey_data(abs_path)
  } else {
    # Try with a different approach to navigate to the data folder
    parent_dir <- dirname(getwd())
    data_path <- file.path(parent_dir, "Survey_Data/2final_data.csv")
    data_path <- normalizePath(data_path, mustWork = FALSE)
    print(paste("Trying parent directory path:", data_path))
    print(paste("Parent directory path exists:", file.exists(data_path)))
    
    if (file.exists(data_path)) {
      survey_data <- prepare_survey_data(data_path)
    } else {
      stop("Unable to find the data file with any method")
    }
  }
}

# Print dataset summary
message("Data loaded: ", nrow(survey_data), " rows and ", ncol(survey_data), " columns")

#------------------------------------------------------------------------------
# RELIABILITY ANALYSIS
#------------------------------------------------------------------------------

# Define multi-item constructs for reliability analysis
constructs <- list(
  wait_time_perception = c("WAIT_3", "WAIT_4", "wait_time_diff"),
  app_satisfaction = c("SAT_1", "SAT_5", "OFF_1", "IMP_2"),
  system_satisfaction = c("IMP_3", "IMP_1", "IMP_2"),
  offline_functionality = c("OFF_1", "OFF_2", "OFF_3"),
  feature_usefulness = c("USE_4_1", "USE_4_2", "USE_4_3", "USE_4_4", "USE_4_5")
)

#------------------------------------------------------------------------------
# FUNCTION 1: INTERNAL CONSISTENCY RELIABILITY (CRONBACH'S ALPHA)
#------------------------------------------------------------------------------

calculate_cronbach_alpha <- function(data, constructs) {
  results <- data.frame(
    Construct = character(),
    Items = integer(),
    Alpha = numeric(),
    Assessment = character(),
    stringsAsFactors = FALSE
  )

  for (construct_name in names(constructs)) {
    # Get the items for this construct
    items <- constructs[[construct_name]]

    # Subset the data to just these items
    construct_data <- data[, items, drop = FALSE]

    # Handle binary items (convert to numeric if needed)
    for (col in colnames(construct_data)) {
      if (is.logical(construct_data[[col]])) {
        construct_data[[col]] <- as.numeric(construct_data[[col]])
      }
    }

    # Calculate Cronbach's alpha
    alpha_result <- tryCatch({
      alpha(construct_data, check.keys = TRUE)
    }, error = function(e) {
      message("Error calculating alpha for ", construct_name, ": ", e$message)
      return(NULL)
    })

    if (!is.null(alpha_result)) {
      alpha_value <- alpha_result$total$raw_alpha

      # Determine assessment
      assessment <- case_when(
        alpha_value >= 0.9 ~ "Excellent",
        alpha_value >= 0.8 ~ "Good",
        alpha_value >= 0.7 ~ "Acceptable",
        alpha_value >= 0.6 ~ "Questionable",
        alpha_value >= 0.5 ~ "Poor",
        TRUE ~ "Unacceptable"
      )

      # Add to results
      results <- rbind(results, data.frame(
        Construct = construct_name,
        Items = length(items),
        Alpha = round(alpha_value, 2),
        Assessment = assessment,
        stringsAsFactors = FALSE
      ))
    }
  }

  return(results)
}

#------------------------------------------------------------------------------
# FUNCTION 2: SPLIT-HALF RELIABILITY WITH SPEARMAN-BROWN CORRECTION
#------------------------------------------------------------------------------

calculate_split_half_reliability <- function(data, constructs) {
  results <- data.frame(
    Construct = character(),
    Split_Half = numeric(),
    Spearman_Brown = numeric(),
    Assessment = character(),
    stringsAsFactors = FALSE
  )

  for (construct_name in names(constructs)) {
    # Get the items for this construct
    items <- constructs[[construct_name]]

    # Skip if less than 2 items
    if (length(items) < 2) {
      message("Skipping ", construct_name, " - needs at least 2 items for split-half")
      next
    }

    # Subset the data to just these items
    construct_data <- data[, items, drop = FALSE]

    # Handle binary items (convert to numeric if needed)
    for (col in colnames(construct_data)) {
      if (is.logical(construct_data[[col]])) {
        construct_data[[col]] <- as.numeric(construct_data[[col]])
      }
    }

    # Calculate split-half reliability
    split_half_result <- tryCatch({
      # Convert to matrix for splitHalf function
      construct_matrix <- as.matrix(construct_data)

      # Create even and odd halves
      n_items <- ncol(construct_matrix)
      even_indices <- seq(2, n_items, by = 2)
      odd_indices <- seq(1, n_items, by = 2)

      # Ensure items are available for both halves
      if (length(even_indices) == 0 || length(odd_indices) == 0) {
        # Alternate approach: split into first half and second half
        half_point <- ceiling(n_items / 2)
        first_half <- 1:half_point
        second_half <- (half_point + 1):n_items

        # If two proper halves cannot be created, skip
        if (length(first_half) == 0 || length(second_half) == 0) {
          return(NULL)
        }

        even_indices <- first_half
        odd_indices <- second_half
      }

      # Calculate scores for each half
      even_scores <- rowSums(construct_matrix[, even_indices, drop = FALSE], na.rm = TRUE)
      odd_scores <- rowSums(construct_matrix[, odd_indices, drop = FALSE], na.rm = TRUE)

      # Calculate correlation between halves
      split_half_coef <- cor(even_scores, odd_scores, use = "pairwise.complete.obs")

      # Apply Spearman-Brown correction
      spearman_brown_coef <- (2 * split_half_coef) / (1 + split_half_coef)

      list(split_half = split_half_coef, spearman_brown = spearman_brown_coef)
    }, error = function(e) {
      message("Error calculating split-half reliability for ", construct_name, ": ", e$message)
      return(NULL)
    })

    if (!is.null(split_half_result)) {
      # Determine assessment
      assessment <- case_when(
        split_half_result$spearman_brown >= 0.9 ~ "Excellent",
        split_half_result$spearman_brown >= 0.8 ~ "Good",
        split_half_result$spearman_brown >= 0.7 ~ "Acceptable",
        split_half_result$spearman_brown >= 0.6 ~ "Questionable",
        split_half_result$spearman_brown >= 0.5 ~ "Poor",
        TRUE ~ "Unacceptable"
      )

      # Add to results
      results <- rbind(results, data.frame(
        Construct = construct_name,
        Split_Half = round(split_half_result$split_half, 2),
        Spearman_Brown = round(split_half_result$spearman_brown, 2),
        Assessment = assessment,
        stringsAsFactors = FALSE
      ))
    }
  }

  return(results)
}

#------------------------------------------------------------------------------
# FUNCTION 3: ITEM-TOTAL CORRELATIONS
#------------------------------------------------------------------------------

calculate_item_total_correlations <- function(data, constructs) {
  results <- data.frame(
    Construct = character(),
    Item = character(),
    Item_Total_Correlation = numeric(),
    Alpha_if_Item_Deleted = numeric(),
    Below_Threshold = logical(),
    stringsAsFactors = FALSE
  )

  for (construct_name in names(constructs)) {
    # Get the items for this construct
    items <- constructs[[construct_name]]

    # Skip if only one item
    if (length(items) < 2) {
      message("Skipping ", construct_name, " - needs at least 2 items for item-total correlation")
      next
    }

    # Subset the data to just these items
    construct_data <- data[, items, drop = FALSE]

    # Handle binary items (convert to numeric if needed)
    for (col in colnames(construct_data)) {
      if (is.logical(construct_data[[col]])) {
        construct_data[[col]] <- as.numeric(construct_data[[col]])
      }
    }

    # Calculate item-total correlations
    alpha_result <- tryCatch({
      alpha(construct_data, check.keys = TRUE)
    }, error = function(e) {
      message("Error calculating item-total correlations for ", construct_name, ": ", e$message)
      return(NULL)
    })

    if (!is.null(alpha_result)) {
      # Extract item statistics
      item_stats <- alpha_result$item.stats

      # Add each item's stats to results
      for (i in seq_along(items)) {
        item <- items[i]
        r.drop <- item_stats$r.drop[i]
        alpha.without <- alpha_result$alpha.drop[i, "raw_alpha"]
        below_threshold <- r.drop < 0.4  # Conventional threshold

        results <- rbind(results, data.frame(
          Construct = construct_name,
          Item = item,
          Item_Total_Correlation = round(r.drop, 2),
          Alpha_if_Item_Deleted = round(alpha.without, 2),
          Below_Threshold = below_threshold,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  return(results)
}

#------------------------------------------------------------------------------
# FUNCTION 5: VISUAL ITEM ANALYSIS
#------------------------------------------------------------------------------

create_item_analysis_plots <- function(data, constructs) {
  # Create a list to store plots
  plots <- list()

  for (construct_name in names(constructs)) {
    items <- constructs[[construct_name]]

    # Skip if only one item
    if (length(items) < 2) {
      message("Skipping visual analysis for ", construct_name, " - needs at least 2 items")
      next
    }

    # Subset the data to just these items
    construct_data <- data[, items, drop = FALSE]

    # Handle binary items (convert to numeric if needed)
    for (col in colnames(construct_data)) {
      if (is.logical(construct_data[[col]])) {
        construct_data[[col]] <- as.numeric(construct_data[[col]])
      }
    }

    # Skip if empty data
    if (nrow(construct_data) == 0) {
      message("Skipping visual analysis for ", construct_name, " - no data available")
      next
    }

    # Calculate total score
    total_score <- rowSums(construct_data, na.rm = TRUE)

    # Prepare data for plotting
    plot_data <- data.frame(total_score = total_score)
    for (item in items) {
      plot_data[[item]] <- data[[item]]
    }

    # Reshape data for plotting
    plot_data_long <- pivot_longer(
      plot_data,
      cols = all_of(items),
      names_to = "item",
      values_to = "response"
    )

    # For dummy categorical data, convert to factor
    plot_data_long$item <- factor(plot_data_long$item)

    # Create boxplot showing distribution of responses for each item
    p1 <- ggplot(plot_data_long, aes(x = item, y = response)) +
      geom_boxplot() +
      labs(title = paste("Response Distribution for", construct_name),
           x = "Item", y = "Response") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # Save plot
    plots[[paste0(construct_name, "_boxplot")]] <- p1

    # Create correlation plot showing relationship between items
    # This would be more meaningful with more observations
    if (nrow(construct_data) > 3) {  # Need at least a few observations for correlation
      item_cors <- cor(construct_data, use = "pairwise.complete.obs")
      item_cors_long <- as.data.frame(as.table(item_cors))
      names(item_cors_long) <- c("Item1", "Item2", "Correlation")

      p2 <- ggplot(item_cors_long, aes(x = Item1, y = Item2, fill = Correlation)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                           limits = c(-1, 1)) +
        labs(title = paste("Inter-Item Correlation Matrix for", construct_name)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      # Save plot
      plots[[paste0(construct_name, "_corplot")]] <- p2
    }
  }

  return(plots)
}

#------------------------------------------------------------------------------
# EXECUTE RELIABILITY ANALYSES
#------------------------------------------------------------------------------

# 1. Calculate internal consistency (Cronbach's Alpha)
alpha_results <- calculate_cronbach_alpha(survey_data, constructs)
print("Internal Consistency Reliability (Cronbach's Alpha):")
print(kable(alpha_results))

# 2. Calculate split-half reliability with Spearman-Brown correction
split_half_results <- calculate_split_half_reliability(survey_data, constructs)
print("Split-Half Reliability with Spearman-Brown Correction:")
print(kable(split_half_results))

# 3. Calculate item-total correlations
item_total_results <- calculate_item_total_correlations(survey_data, constructs)
print("Item-Total Correlations:")
print(kable(item_total_results))

#------------------------------------------------------------------------------
# GENERATE SUMMARY REPORT
#------------------------------------------------------------------------------

generate_reliability_summary <- function(alpha_results, split_half_results, item_total_results) {
  summary_text <- "# RELIABILITY ANALYSIS SUMMARY\n\n"

  # Overall findings
  summary_text <- paste0(summary_text, "## Overall Findings\n\n")

  # Count constructs with acceptable or better reliability
  acceptable_constructs <- sum(alpha_results$Assessment %in% c("Acceptable", "Good", "Excellent"))
  total_constructs <- nrow(alpha_results)

  summary_text <- paste0(summary_text,
                         "* ", acceptable_constructs, " out of ", total_constructs,
                         " constructs demonstrated acceptable or better internal consistency reliability (α ≥ 0.70).\n")

  # Items below threshold
  items_below_threshold <- sum(item_total_results$Below_Threshold)
  total_items <- nrow(item_total_results)

  summary_text <- paste0(summary_text,
                         "* ", items_below_threshold, " out of ", total_items,
                         " items showed item-total correlations below the recommended threshold (r < 0.40).\n")

  # Construct-specific findings
  summary_text <- paste0(summary_text, "\n## Construct-Specific Findings\n\n")

  for (construct in unique(alpha_results$Construct)) {
    alpha_value <- alpha_results$Alpha[alpha_results$Construct == construct]
    assessment <- alpha_results$Assessment[alpha_results$Construct == construct]

    summary_text <- paste0(summary_text, "### ", construct, "\n\n")
    summary_text <- paste0(summary_text, "* Cronbach's α = ", alpha_value, " (", assessment, ")\n")

    # If split-half results exist for this construct
    if (construct %in% split_half_results$Construct) {
      sb_value <- split_half_results$Spearman_Brown[split_half_results$Construct == construct]
      summary_text <- paste0(summary_text, "* Spearman-Brown Coefficient = ", sb_value, "\n")
    }

    # Item-level information
    construct_items <- item_total_results[item_total_results$Construct == construct, ]
    if (nrow(construct_items) > 0) {
      problem_items <- construct_items[construct_items$Below_Threshold, ]

      if (nrow(problem_items) > 0) {
        summary_text <- paste0(summary_text, "* Problematic items:\n")
        for (i in seq_len(nrow(problem_items))) {
          summary_text <- paste0(summary_text, "  * ", problem_items$Item[i],
                               " (r = ", problem_items$Item_Total_Correlation[i],
                               ", α if deleted = ", problem_items$Alpha_if_Item_Deleted[i], ")\n")
        }
      } else {
        summary_text <- paste0(summary_text, "* All items show adequate item-total correlations (r ≥ 0.40)\n")
      }
    }

    summary_text <- paste0(summary_text, "\n")
  }

  # Recommendations
  summary_text <- paste0(summary_text, "## Recommendations\n\n")

  if (items_below_threshold > 0) {
    summary_text <- paste0(summary_text, "* Consider revising or removing items with low item-total correlations.\n")
  }

  low_reliability_constructs <- alpha_results[alpha_results$Alpha < 0.70, "Construct"]
  if (length(low_reliability_constructs) > 0) {
    summary_text <- paste0(summary_text, "* The following constructs would benefit from revision or additional items:\n")
    for (construct in low_reliability_constructs) {
      summary_text <- paste0(summary_text, "  * ", construct, "\n")
    }
  }

  if (acceptable_constructs == total_constructs && items_below_threshold == 0) {
    summary_text <- paste0(summary_text, "* All constructs demonstrate acceptable reliability. No immediate revisions needed.\n")
  }

  return(summary_text)
}

# Generate and print summary report
reliability_summary <- generate_reliability_summary(alpha_results, split_half_results, item_total_results)
cat(reliability_summary)

#------------------------------------------------------------------------------
# EXPORT RESULTS TO CSV
#------------------------------------------------------------------------------

# Create Result_reliability directory if it doesn't exist
result_dir <- "Survey_Results/Analysis Results/Result_csv"
if (!dir.exists(result_dir)) {
  dir.create(result_dir)
  message("Created directory: ", result_dir)
}

# Export all results
write.csv(alpha_results, file.path(result_dir, "internal_consistency.csv"), row.names = FALSE)
write.csv(split_half_results, file.path(result_dir, "split_half_reliability.csv"), row.names = FALSE)
write.csv(item_total_results, file.path(result_dir, "item_total_correlations.csv"), row.names = FALSE)
write.csv(demographic_results, file.path(result_dir, "demographic_invariance.csv"), row.names = FALSE)

# Save summary to text file
writeLines(reliability_summary, file.path(result_dir, "reliability_summary.txt"))

message("=== Reliability Analysis Complete ===")
message("All results exported to the ", result_dir, " directory")
message("Internal consistency results: ", nrow(alpha_results), " constructs analyzed")
message("Split-half reliability results: ", nrow(split_half_results), " constructs analyzed")
message("Item-total correlation results: ", nrow(item_total_results), " items analyzed")
message("Demographic invariance results: ", nrow(demographic_results), " comparisons analyzed")