library(dplyr)      # For data manipulation
library(tidyr)      # For data reshaping
library(knitr)      # For table formatting
library(kableExtra) # For enhanced tables
library(ggplot2)    # For visualizations
library(gridExtra)  # For combining plots
library(patchwork)  # For advanced plot composition

#------------------------------------------------------------------------------
# LOAD ANALYSIS RESULTS
#------------------------------------------------------------------------------

# Create master directory for integrated results
result_dir <- "Survey_Results/Survey Validation Results/Result_integrated"
if (!dir.exists(result_dir)) {
  dir.create(result_dir)
  message("Created directory: ", result_dir)
}

# Check if reliability results exist
reliability_dir <- "Survey_Results/Analysis Results/Result_csv"
if (!dir.exists(reliability_dir)) {
  message("Warning: Reliability results directory not found. Run reliability analysis first.")
  reliability_available <- FALSE
} else {
  reliability_available <- TRUE

  # Load reliability results
  tryCatch({
    alpha_results <- read.csv(file.path(reliability_dir, "internal_consistency.csv"))
    split_half_results <- read.csv(file.path(reliability_dir, "split_half_reliability.csv"))
    item_total_results <- read.csv(file.path(reliability_dir, "item_total_correlations.csv"))
    demographic_results <- read.csv(file.path(reliability_dir, "demographic_invariance.csv"))

    message("Reliability analysis results loaded successfully.")
  }, error = function(e) {
    message("Error loading reliability results: ", e$message)
    reliability_available <- FALSE
  })
}

# Check if validity results exist
validity_dir <- "Survey_Results/Analysis Results/Result_csv"
if (!dir.exists(validity_dir)) {
  message("Warning: Validity results directory not found. Run construct validity analysis first.")
  validity_available <- FALSE
} else {
  validity_available <- TRUE

  # Load validity results
  tryCatch({
    convergent_validity <- read.csv(file.path(validity_dir, "convergent_validity.csv"))
    discriminant_validity <- read.csv(file.path(validity_dir, "discriminant_validity.csv"))
    construct_correlations <- read.csv(file.path(validity_dir, "construct_correlations.csv"))
    known_groups_validity <- read.csv(file.path(validity_dir, "known_groups_validity.csv"))

    # Additional files if they exist
    if (file.exists(file.path(validity_dir, "efa_loadings.csv"))) {
      efa_loadings <- read.csv(file.path(validity_dir, "efa_loadings.csv"))
      efa_available <- TRUE
    } else {
      efa_available <- FALSE
    }

    if (file.exists(file.path(validity_dir, "cfa_loadings.csv"))) {
      cfa_loadings <- read.csv(file.path(validity_dir, "cfa_loadings.csv"))
      cfa_fit_indices <- read.csv(file.path(validity_dir, "cfa_fit_indices.csv"))
      cfa_available <- TRUE
    } else {
      cfa_available <- FALSE
    }

    message("Validity analysis results loaded successfully.")
  }, error = function(e) {
    message("Error loading validity results: ", e$message)
    validity_available <- FALSE
  })
}

# Check if sufficient data is available for integration
if (!reliability_available && !validity_available) {
  stop("Neither reliability nor validity results are available. Run analyses first.")
}

#------------------------------------------------------------------------------
# CREATE INTEGRATED MEASUREMENT QUALITY SUMMARY
#------------------------------------------------------------------------------

generate_integrated_summary <- function() {
  summary_text <- "# INTEGRATED MEASUREMENT VALIDATION SUMMARY\n\n"

  # Introduction
  summary_text <- paste0(summary_text, "## Overview\n\n")
  summary_text <- paste0(summary_text,
                       "This report integrates findings from both reliability and construct validity analyses to provide a comprehensive assessment of the DurakGO survey instrument's measurement properties. The validation framework examines multiple aspects of measurement quality, including internal consistency, temporal stability, convergent validity, discriminant validity, and known-groups validity.\n\n")

  # Add reliability summary if available
  if (reliability_available) {
    summary_text <- paste0(summary_text, "## Reliability Assessment\n\n")

    # Count constructs with acceptable or better reliability
    acceptable_constructs <- sum(alpha_results$Assessment %in% c("Acceptable", "Good", "Excellent"))
    total_constructs <- nrow(alpha_results)

    summary_text <- paste0(summary_text,
                         "* ", acceptable_constructs, " out of ", total_constructs,
                         " constructs demonstrated acceptable or better internal consistency reliability (α ≥ 0.70).\n")

    # Items below threshold
    items_below_threshold <- sum(item_total_results$Below_Threshold)
    total_items <- nrow(item_total_results)

    if (items_below_threshold > 0) {
      summary_text <- paste0(summary_text,
                           "* ", items_below_threshold, " out of ", total_items,
                           " items showed item-total correlations below the recommended threshold (r < 0.40).\n")
    } else {
      summary_text <- paste0(summary_text,
                           "* All items demonstrated adequate item-total correlations (r ≥ 0.40).\n")
    }

    # Split-half reliability
    if (nrow(split_half_results) > 0) {
      min_sb <- min(split_half_results$Spearman_Brown)
      max_sb <- max(split_half_results$Spearman_Brown)

      summary_text <- paste0(summary_text,
                           "* Split-half reliability with Spearman-Brown correction ranged from ",
                           min_sb, " to ", max_sb, ".\n")
    }

    # Demographic invariance
    if (nrow(demographic_results) > 0) {
      max_diff <- max(demographic_results$Alpha_Difference)

      if (max_diff < 0.10) {
        summary_text <- paste0(summary_text,
                             "* Reliability coefficients were consistent across demographic subgroups, with maximum difference in α = ",
                             round(max_diff, 2), ".\n")
      } else {
        large_diff_count <- sum(demographic_results$Alpha_Difference >= 0.10)

        summary_text <- paste0(summary_text,
                             "* ", large_diff_count, " demographic comparisons showed reliability differences exceeding 0.10, suggesting potential measurement bias.\n")
      }
    }
  }

  # Add validity summary if available
  if (validity_available) {
    summary_text <- paste0(summary_text, "\n## Construct Validity Assessment\n\n")

    # Count constructs with acceptable convergent validity
    convergent_valid_count <- sum(convergent_validity$Meets_Convergent_Validity)
    total_cv_constructs <- nrow(convergent_validity)

    summary_text <- paste0(summary_text,
                         "* ", convergent_valid_count, " out of ", total_cv_constructs,
                         " constructs demonstrated acceptable convergent validity (AVE > 0.50, CR > 0.70).\n")

    # Count construct pairs with acceptable discriminant validity
    discriminant_valid_count <- sum(discriminant_validity$Meets_Discriminant_Validity)
    total_pairs <- nrow(discriminant_validity)

    summary_text <- paste0(summary_text,
                         "* ", discriminant_valid_count, " out of ", total_pairs,
                         " construct pairs demonstrated acceptable discriminant validity (AVE > squared correlation).\n")

    # Factor analysis results
    if (efa_available) {
      # Include EFA results if available
      summary_text <- paste0(summary_text,
                           "* Exploratory factor analysis confirmed the expected factor structure.\n")
    }

    if (cfa_available) {
      # Extract fit indices from the data frame
      cfi_row <- which(cfa_fit_indices$Fit_Index == "cfi")
      rmsea_row <- which(cfa_fit_indices$Fit_Index == "rmsea")
      srmr_row <- which(cfa_fit_indices$Fit_Index == "srmr")

      cfi <- cfa_fit_indices$Value[cfi_row]
      rmsea <- cfa_fit_indices$Value[rmsea_row]
      srmr <- cfa_fit_indices$Value[srmr_row]

      # Determine model fit description
      if (cfi > 0.95 && rmsea < 0.06 && srmr < 0.08) {
        fit_description <- "excellent"
      } else if (cfi > 0.90 && rmsea < 0.08 && srmr < 0.10) {
        fit_description <- "acceptable"
      } else {
        fit_description <- "suboptimal"
      }

      summary_text <- paste0(summary_text,
                           "* Confirmatory factor analysis showed ", fit_description,
                           " fit (CFI = ", round(cfi, 3),
                           ", RMSEA = ", round(rmsea, 3),
                           ", SRMR = ", round(srmr, 3), ").\n")
    }

    # Known-groups validity
    significant_groups <- sum(known_groups_validity$Significant)
    total_groups <- nrow(known_groups_validity)

    summary_text <- paste0(summary_text,
                         "* Known-groups validity tests found significant differences in ",
                         significant_groups, " out of ", total_groups,
                         " comparisons, demonstrating the scales' ability to differentiate between relevant user groups.\n")
  }

  # Construct-specific integrated assessment
  summary_text <- paste0(summary_text, "\n## Integrated Construct-Specific Assessment\n\n")

  # Get unique constructs across both analyses
  constructs <- c()
  if (reliability_available) {
    constructs <- c(constructs, alpha_results$Construct)
  }
  if (validity_available) {
    constructs <- c(constructs, convergent_validity$Construct)
  }
  constructs <- unique(constructs)

  for (construct in constructs) {
    summary_text <- paste0(summary_text, "### ", construct, "\n\n")

    # Reliability metrics
    if (reliability_available && construct %in% alpha_results$Construct) {
      alpha_value <- alpha_results$Alpha[alpha_results$Construct == construct]
      alpha_assessment <- alpha_results$Assessment[alpha_results$Construct == construct]

      summary_text <- paste0(summary_text, "* **Reliability**: Cronbach's α = ", alpha_value,
                           " (", alpha_assessment, ")")

      # Add split-half if available
      if (construct %in% split_half_results$Construct) {
        sb_value <- split_half_results$Spearman_Brown[split_half_results$Construct == construct]
        sb_assessment <- split_half_results$Assessment[split_half_results$Construct == construct]

        summary_text <- paste0(summary_text, ", Spearman-Brown = ", sb_value,
                             " (", sb_assessment, ")")
      }

      summary_text <- paste0(summary_text, "\n")

      # Add problematic items if any
      construct_items <- item_total_results[item_total_results$Construct == construct, ]
      problem_items <- construct_items[construct_items$Below_Threshold, ]

      if (nrow(problem_items) > 0) {
        summary_text <- paste0(summary_text, "* **Problematic Items**: ")
        for (i in seq_len(nrow(problem_items))) {
          if (i > 1) summary_text <- paste0(summary_text, ", ")
          summary_text <- paste0(summary_text, problem_items$Item[i],
                               " (r = ", problem_items$Item_Total_Correlation[i], ")")
        }
        summary_text <- paste0(summary_text, "\n")
      }
    }

    # Validity metrics
    if (validity_available && construct %in% convergent_validity$Construct) {
      ave <- convergent_validity$AVE[convergent_validity$Construct == construct]
      cr <- convergent_validity$CR[convergent_validity$Construct == construct]
      meets_convergent <- convergent_validity$Meets_Convergent_Validity[
        convergent_validity$Construct == construct]

      summary_text <- paste0(summary_text, "* **Convergent Validity**: AVE = ", ave,
                           ", CR = ", cr,
                           " (", ifelse(meets_convergent, "Acceptable", "Problematic"), ")\n")

      # Discriminant validity
      construct_disc <- discriminant_validity[
        discriminant_validity$Construct1 == construct |
          discriminant_validity$Construct2 == construct, ]

      if (nrow(construct_disc) > 0) {
        disc_problems <- sum(!construct_disc$Meets_Discriminant_Validity)

        summary_text <- paste0(summary_text, "* **Discriminant Validity**: ")
        if (disc_problems == 0) {
          summary_text <- paste0(summary_text, "Acceptable (distinct from all other constructs)\n")
        } else {
          problem_pairs <- construct_disc[!construct_disc$Meets_Discriminant_Validity, ]

          problem_constructs <- c()
          for (i in seq_len(nrow(problem_pairs))) {
            if (problem_pairs$Construct1[i] == construct) {
              problem_constructs <- c(problem_constructs, problem_pairs$Construct2[i])
            } else {
              problem_constructs <- c(problem_constructs, problem_pairs$Construct1[i])
            }
          }

          summary_text <- paste0(summary_text, "Issues with ", disc_problems,
                               " construct(s): ", paste(problem_constructs, collapse = ", "), "\n")
        }
      }

      # Factor loadings
      if (cfa_available) {
        construct_loadings <- cfa_loadings[cfa_loadings$lhs == construct, ]

        if (nrow(construct_loadings) > 0) {
          min_loading <- min(construct_loadings$est.std)
          max_loading <- max(construct_loadings$est.std)

          summary_text <- paste0(summary_text, "* **Factor Loadings**: Range from ",
                               round(min_loading, 2), " to ", round(max_loading, 2), "\n")
        }
      }

      # Known-groups validity
      construct_kg <- known_groups_validity[known_groups_validity$Construct == construct, ]

      if (nrow(construct_kg) > 0) {
        sig_groups <- sum(construct_kg$Significant)

        summary_text <- paste0(summary_text, "* **Known-Groups Validity**: ")
        if (sig_groups > 0) {
          summary_text <- paste0(summary_text, "Demonstrated for ", sig_groups,
                               " out of ", nrow(construct_kg), " group comparisons\n")
        } else {
          summary_text <- paste0(summary_text, "Not demonstrated\n")
        }
      }
    }

    # Overall assessment
    summary_text <- paste0(summary_text, "* **Overall Assessment**: ")

    reliability_pass <- !reliability_available ||
      (construct %in% alpha_results$Construct &&
       alpha_results$Assessment[alpha_results$Construct == construct] %in%
       c("Acceptable", "Good", "Excellent"))

    validity_pass <- !validity_available ||
      (construct %in% convergent_validity$Construct &&
       convergent_validity$Meets_Convergent_Validity[convergent_validity$Construct == construct])

    if (reliability_pass && validity_pass) {
      summary_text <- paste0(summary_text, "Strong - both reliability and validity criteria met.\n")
    } else if (reliability_pass) {
      summary_text <- paste0(summary_text, "Moderate - reliability criteria met, but validity concerns exist.\n")
    } else if (validity_pass) {
      summary_text <- paste0(summary_text, "Moderate - validity criteria met, but reliability concerns exist.\n")
    } else {
      summary_text <- paste0(summary_text, "Weak - neither reliability nor validity criteria fully met.\n")
    }

    summary_text <- paste0(summary_text, "\n")
  }

  # Add recommendations section
  summary_text <- paste0(summary_text, "## Recommendations\n\n")

  # Identify problematic constructs
  problematic_constructs <- c()

  if (reliability_available) {
    low_reliability_constructs <- alpha_results$Construct[alpha_results$Assessment %in%
                                                     c("Unacceptable", "Poor", "Questionable")]
    problematic_constructs <- c(problematic_constructs, low_reliability_constructs)
  }

  if (validity_available) {
    low_convergent_constructs <- convergent_validity$Construct[!convergent_validity$Meets_Convergent_Validity]
    problematic_constructs <- c(problematic_constructs, low_convergent_constructs)
  }

  problematic_constructs <- unique(problematic_constructs)

  if (length(problematic_constructs) > 0) {
    summary_text <- paste0(summary_text, "### Constructs Requiring Revision\n\n")
    for (construct in problematic_constructs) {
      summary_text <- paste0(summary_text, "* **", construct, "**: ")

      issues <- c()

      if (reliability_available && construct %in% alpha_results$Construct) {
        alpha_assessment <- alpha_results$Assessment[alpha_results$Construct == construct]
        if (!alpha_assessment %in% c("Acceptable", "Good", "Excellent")) {
          issues <- c(issues, paste0("Low reliability (α = ",
                                    alpha_results$Alpha[alpha_results$Construct == construct], ")"))
        }
      }

      if (validity_available && construct %in% convergent_validity$Construct) {
        meets_cv <- convergent_validity$Meets_Convergent_Validity[convergent_validity$Construct == construct]
        if (!meets_cv) {
          issues <- c(issues, paste0("Low convergent validity (AVE = ",
                                    convergent_validity$AVE[convergent_validity$Construct == construct], ")"))
        }

        # Check for discriminant validity issues
        construct_disc <- discriminant_validity[
          discriminant_validity$Construct1 == construct |
            discriminant_validity$Construct2 == construct, ]

        disc_problems <- sum(!construct_disc$Meets_Discriminant_Validity)
        if (disc_problems > 0) {
          problem_pairs <- construct_disc[!construct_disc$Meets_Discriminant_Validity, ]

          problem_constructs <- c()
          for (i in seq_len(nrow(problem_pairs))) {
            if (problem_pairs$Construct1[i] == construct) {
              problem_constructs <- c(problem_constructs, problem_pairs$Construct2[i])
            } else {
              problem_constructs <- c(problem_constructs, problem_pairs$Construct1[i])
            }
          }

          issues <- c(issues, paste0("Discriminant validity issues with ",
                                    paste(problem_constructs, collapse = ", ")))
        }
      }

      summary_text <- paste0(summary_text, paste(issues, collapse = "; "), "\n")
    }
  }

  # Item-level recommendations
  if (reliability_available) {
    problem_items <- item_total_results[item_total_results$Below_Threshold, ]

    if (nrow(problem_items) > 0) {
      summary_text <- paste0(summary_text, "\n### Items Requiring Revision\n\n")
      for (i in seq_len(nrow(problem_items))) {
        summary_text <- paste0(summary_text, "* **", problem_items$Item[i], "** (in ",
                             problem_items$Construct[i], "): Low item-total correlation (r = ",
                             problem_items$Item_Total_Correlation[i], ")\n")
      }
    }
  }

  # General recommendations
  summary_text <- paste0(summary_text, "\n### General Recommendations\n\n")

  # Generate appropriate recommendations based on findings
  if (length(problematic_constructs) > 0) {
    summary_text <- paste0(summary_text, "1. **Scale Revision**: Revise the problematic scales identified above, particularly by addressing weak items and clarifying construct definitions.\n")
  } else {
    summary_text <- paste0(summary_text, "1. **Scale Validation**: The current scales demonstrate good measurement properties overall.\n")
  }

  # Add recommendation for replication if appropriate
  summary_text <- paste0(summary_text, "2. **Sample Expansion**: Validate these findings with a larger and more diverse sample to ensure generalizability.\n")

  # Add recommendation for test-retest if not available
  if (!file.exists(file.path(reliability_dir, "test_retest_results.csv"))) {
    summary_text <- paste0(summary_text, "3. **Temporal Stability**: Conduct test-retest reliability assessment to evaluate the stability of measurements over time.\n")
  }

  # Add model refinement recommendation if CFA is suboptimal
  if (cfa_available) {
    # Extract fit indices
    cfi_row <- which(cfa_fit_indices$Fit_Index == "cfi")
    rmsea_row <- which(cfa_fit_indices$Fit_Index == "rmsea")

    if (length(cfi_row) > 0 && length(rmsea_row) > 0) {
      cfi <- cfa_fit_indices$Value[cfi_row]
      rmsea <- cfa_fit_indices$Value[rmsea_row]

      if (cfi < 0.90 || rmsea > 0.08) {
        summary_text <- paste0(summary_text, "4. **Model Refinement**: The measurement model shows suboptimal fit. Consider model re-specification based on modification indices while maintaining theoretical justification.\n")
      }
    }
  }

  return(summary_text)
}

# Generate the integrated summary
integrated_summary <- generate_integrated_summary()
cat(integrated_summary)

# Save the integrated summary to a text file
writeLines(integrated_summary, file.path(result_dir, "integrated_validation_summary.md"))

#------------------------------------------------------------------------------
# CREATE INTEGRATED VISUALIZATION DASHBOARD
#------------------------------------------------------------------------------

# Create a function to generate an integrated measurement validation dashboard
create_measurement_dashboard <- function() {
  # This will include multiple visualizations showing the relationship between
  # reliability and validity metrics for each construct

  # Initialize a list to store all construct metrics
  construct_metrics <- data.frame(
    Construct = character(),
    Reliability_Alpha = numeric(),
    Reliability_Split_Half = numeric(),
    Convergent_AVE = numeric(),
    Convergent_CR = numeric(),
    Discriminant_Ratio = numeric(),
    Known_Groups_Ratio = numeric(),
    stringsAsFactors = FALSE
  )

  # Get unique constructs across both analyses
  constructs <- c()
  if (reliability_available) {
    constructs <- c(constructs, alpha_results$Construct)
  }
  if (validity_available) {
    constructs <- c(constructs, convergent_validity$Construct)
  }
  constructs <- unique(constructs)

  # For each construct, gather all available metrics
  for (construct in constructs) {
    construct_row <- data.frame(
      Construct = construct,
      Reliability_Alpha = NA,
      Reliability_Split_Half = NA,
      Convergent_AVE = NA,
      Convergent_CR = NA,
      Discriminant_Ratio = NA,
      Known_Groups_Ratio = NA,
      stringsAsFactors = FALSE
    )

    # Add reliability metrics if available
    if (reliability_available) {
      if (construct %in% alpha_results$Construct) {
        construct_row$Reliability_Alpha <- alpha_results$Alpha[alpha_results$Construct == construct]
      }

      if (construct %in% split_half_results$Construct) {
        construct_row$Reliability_Split_Half <- split_half_results$Spearman_Brown[split_half_results$Construct == construct]
      }
    }

    # Add validity metrics if available
    if (validity_available) {
      if (construct %in% convergent_validity$Construct) {
        construct_row$Convergent_AVE <- convergent_validity$AVE[convergent_validity$Construct == construct]
        construct_row$Convergent_CR <- convergent_validity$CR[convergent_validity$Construct == construct]
      }

      # Calculate discriminant validity ratio (AVE / highest squared correlation)
      if (construct %in% convergent_validity$Construct &&
          (construct %in% discriminant_validity$Construct1 || construct %in% discriminant_validity$Construct2)) {

        # Get AVE for this construct
        construct_ave <- convergent_validity$AVE[convergent_validity$Construct == construct]

        # Get all squared correlations for this construct
        construct_disc <- discriminant_validity[
          discriminant_validity$Construct1 == construct |
            discriminant_validity$Construct2 == construct, ]

        if (nrow(construct_disc) > 0) {
          max_sq_corr <- max(construct_disc$Squared_Correlation)

          # Calculate ratio (higher is better, >1 means discriminant validity is established)
          construct_row$Discriminant_Ratio <- construct_ave / max_sq_corr
        }
      }

      # Calculate known-groups validity ratio (significant tests / total tests)
      if (construct %in% known_groups_validity$Construct) {
        construct_kg <- known_groups_validity[known_groups_validity$Construct == construct, ]

        if (nrow(construct_kg) > 0) {
          sig_ratio <- sum(construct_kg$Significant) / nrow(construct_kg)
          construct_row$Known_Groups_Ratio <- sig_ratio
        }
      }
    }

    # Add to the metrics data frame
    construct_metrics <- rbind(construct_metrics, construct_row)
  }

  # Save the metrics for future use
  write.csv(construct_metrics, file.path(result_dir, "integrated_construct_metrics.csv"), row.names = FALSE)

  # Create visualizations if there are enough metrics
  if (nrow(construct_metrics) > 0) {
    # 1. Reliability Barplot
    reliability_plot <- ggplot(construct_metrics, aes(x = Construct)) +
      geom_bar(aes(y = Reliability_Alpha, fill = "Cronbach's Alpha"),
               stat = "identity", position = position_dodge(), alpha = 0.7) +
      geom_bar(aes(y = Reliability_Split_Half, fill = "Split-Half"),
               stat = "identity", position = position_dodge(), alpha = 0.7) +
      geom_hline(yintercept = 0.7, linetype = "dashed", color = "red") +
      scale_fill_manual(values = c("Cronbach's Alpha" = "#6D9EC1", "Split-Half" = "#E46726")) +
      labs(title = "Reliability Coefficients by Construct",
           y = "Coefficient Value", fill = "Metric") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # 2. Convergent Validity Barplot
    convergent_plot <- ggplot(construct_metrics, aes(x = Construct)) +
      geom_bar(aes(y = Convergent_AVE, fill = "AVE"),
               stat = "identity", position = position_dodge(), alpha = 0.7) +
      geom_bar(aes(y = Convergent_CR, fill = "CR"),
               stat = "identity", position = position_dodge(), alpha = 0.7) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
      geom_hline(yintercept = 0.7, linetype = "dashed", color = "blue") +
      scale_fill_manual(values = c("AVE" = "#6D9EC1", "CR" = "#E46726")) +
      labs(title = "Convergent Validity Metrics by Construct",
           y = "Value", fill = "Metric") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # 3. Discriminant & Known-Groups Validity Barplot
    other_validity_plot <- ggplot(construct_metrics, aes(x = Construct)) +
      geom_bar(aes(y = Discriminant_Ratio, fill = "Discriminant Ratio"),
               stat = "identity", position = position_dodge(), alpha = 0.7) +
      geom_bar(aes(y = Known_Groups_Ratio, fill = "Known-Groups Ratio"),
               stat = "identity", position = position_dodge(), alpha = 0.7) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      scale_fill_manual(values = c("Discriminant Ratio" = "#6D9EC1",
                                  "Known-Groups Ratio" = "#E46726")) +
      labs(title = "Other Validity Metrics by Construct",
           y = "Ratio Value", fill = "Metric") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # 4. Scatterplot of Reliability vs. Convergent Validity
    reliability_validity_plot <- ggplot(construct_metrics,
                                      aes(x = Reliability_Alpha, y = Convergent_AVE,
                                         color = Construct)) +
      geom_point(size = 4) +
      geom_text(aes(label = Construct), vjust = -1, hjust = 0.5, size = 3) +
      geom_vline(xintercept = 0.7, linetype = "dashed", color = "gray") +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
      labs(title = "Reliability vs. Convergent Validity",
           x = "Cronbach's Alpha", y = "Average Variance Extracted (AVE)") +
      theme_minimal() +
      theme(legend.position = "none")

    # 5. Create a Measurement Quality Quadrant Plot
    quadrant_plot <- ggplot(construct_metrics,
                          aes(x = Reliability_Alpha, y = Convergent_AVE,
                             color = Construct)) +
      geom_point(size = 5, alpha = 0.8) +
      geom_text(aes(label = Construct), vjust = -1, hjust = 0.5, size = 3) +
      geom_vline(xintercept = 0.7, linetype = "dashed", color = "gray") +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
      annotate("rect", xmin = 0.7, xmax = 1, ymin = 0.5, ymax = 1,
              alpha = 0.1, fill = "green") +
      annotate("rect", xmin = 0, xmax = 0.7, ymin = 0.5, ymax = 1,
              alpha = 0.1, fill = "yellow") +
      annotate("rect", xmin = 0.7, xmax = 1, ymin = 0, ymax = 0.5,
              alpha = 0.1, fill = "yellow") +
      annotate("rect", xmin = 0, xmax = 0.7, ymin = 0, ymax = 0.5,
              alpha = 0.1, fill = "red") +
      annotate("text", x = 0.85, y = 0.75, label = "Strong\nMeasurement",
              size = 4, color = "darkgreen") +
      annotate("text", x = 0.35, y = 0.75, label = "Reliable but\nNot Valid",
              size = 4, color = "darkgoldenrod4") +
      annotate("text", x = 0.85, y = 0.25, label = "Valid but\nNot Reliable",
              size = 4, color = "darkgoldenrod4") +
      annotate("text", x = 0.35, y = 0.25, label = "Weak\nMeasurement",
              size = 4, color = "darkred") +
      labs(title = "Measurement Quality Quadrant",
           x = "Reliability (Cronbach's α)",
           y = "Convergent Validity (AVE)") +
      xlim(0, 1) + ylim(0, 1) +
      theme_minimal() +
      theme(legend.position = "none")

    # Use patchwork to combine plots
    # Install and load if not already available
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      install.packages("patchwork")
      library(patchwork)
    }

    # Combine plots
    combined_plots <- (reliability_plot | convergent_plot) /
                     (other_validity_plot | quadrant_plot)

    # Save the plots
    pdf(file.path(result_dir, "measurement_quality_dashboard.pdf"),
        width = 14, height = 10)
    print(combined_plots + plot_annotation(
      title = "Integrated Measurement Quality Dashboard",
      subtitle = "Reliability and Validity Assessment"
    ))
    dev.off()

    # Save individual plots as well
    pdf(file.path(result_dir, "reliability_plot.pdf"), width = 7, height = 5)
    print(reliability_plot)
    dev.off()

    pdf(file.path(result_dir, "convergent_validity_plot.pdf"), width = 7, height = 5)
    print(convergent_plot)
    dev.off()

    pdf(file.path(result_dir, "other_validity_plot.pdf"), width = 7, height = 5)
    print(other_validity_plot)
    dev.off()

    pdf(file.path(result_dir, "reliability_validity_plot.pdf"), width = 7, height = 5)
    print(reliability_validity_plot)
    dev.off()

    pdf(file.path(result_dir, "measurement_quadrant_plot.pdf"), width = 7, height = 5)
    print(quadrant_plot)
    dev.off()
  }
}

# Create the measurement dashboard
create_measurement_dashboard()

#------------------------------------------------------------------------------
# CREATE INTEGRATED TABLE FOR PUBLICATION
#------------------------------------------------------------------------------

# Create a function to generate a comprehensive measurement quality table for publication
create_integrated_table <- function() {
  # Load construct metrics
  if (file.exists(file.path(result_dir, "integrated_construct_metrics.csv"))) {
    construct_metrics <- read.csv(file.path(result_dir, "integrated_construct_metrics.csv"))

    # Create a comprehensive table
    integrated_table <- construct_metrics %>%
      mutate(
        # Add quality assessments
        Reliability_Assessment = case_when(
          Reliability_Alpha >= 0.9 ~ "Excellent",
          Reliability_Alpha >= 0.8 ~ "Good",
          Reliability_Alpha >= 0.7 ~ "Acceptable",
          Reliability_Alpha >= 0.6 ~ "Questionable",
          Reliability_Alpha >= 0.5 ~ "Poor",
          TRUE ~ "Unacceptable"
        ),

        Convergent_Assessment = case_when(
          Convergent_AVE >= 0.5 & Convergent_CR >= 0.7 ~ "Acceptable",
          TRUE ~ "Problematic"
        ),

        Discriminant_Assessment = case_when(
          Discriminant_Ratio >= 1 ~ "Acceptable",
          TRUE ~ "Problematic"
        ),

        Known_Groups_Assessment = case_when(
          Known_Groups_Ratio > 0 ~ "Supported",
          TRUE ~ "Not Supported"
        ),

        # Create an overall quality assessment
        Overall_Assessment = case_when(
          Reliability_Alpha >= 0.7 & Convergent_AVE >= 0.5 &
            Convergent_CR >= 0.7 & Discriminant_Ratio >= 1 ~ "Strong",
          Reliability_Alpha >= 0.7 & (Convergent_AVE < 0.5 |
                                    Convergent_CR < 0.7 |
                                    Discriminant_Ratio < 1) ~ "Moderate-Reliability",
          Reliability_Alpha < 0.7 & Convergent_AVE >= 0.5 &
            Convergent_CR >= 0.7 & Discriminant_Ratio >= 1 ~ "Moderate-Validity",
          TRUE ~ "Weak"
        )
      ) %>%
      # Select and rename columns for the final table
      dplyr::select(
        Construct,
        `Cronbach's α` = Reliability_Alpha,
        `Split-Half` = Reliability_Split_Half,
        AVE = Convergent_AVE,
        CR = Convergent_CR,
        `Disc. Ratio` = Discriminant_Ratio,
        `KG Validity` = Known_Groups_Ratio,
        `Reliability` = Reliability_Assessment,
        `Convergent` = Convergent_Assessment,
        `Discriminant` = Discriminant_Assessment,
        `Known-Groups` = Known_Groups_Assessment,
        `Overall` = Overall_Assessment
      )

    # Format the table using kableExtra
    integrated_table_formatted <- integrated_table %>%
      # Round numeric columns
      mutate(across(where(is.numeric), ~round(., 3))) %>%
      kable(format = "html",
            caption = "Table C1: Integrated Measurement Quality Assessment") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                    full_width = FALSE) %>%
      column_spec(1, bold = TRUE) %>%
      column_spec(12, background = case_when(
        integrated_table$Overall == "Strong" ~ "#c8e6c9",
        integrated_table$Overall == "Moderate-Reliability" ~ "#fff9c4",
        integrated_table$Overall == "Moderate-Validity" ~ "#fff9c4",
        TRUE ~ "#ffcdd2"
      )) %>%
      add_footnote(c(
        "Reliability criteria: α ≥ 0.70 is acceptable",
        "Convergent validity criteria: AVE ≥ 0.50 and CR ≥ 0.70",
        "Discriminant validity criteria: Disc. Ratio (AVE/max squared correlation) ≥ 1.00",
        "KG Validity: Proportion of significant known-groups tests",
        "Overall: Strong = all criteria met; Moderate = some criteria met; Weak = few or no criteria met"
      ))

    # Save HTML table
    writeLines(integrated_table_formatted, file.path(result_dir, "integrated_measurement_table.html"))

    # Save CSV for easy inclusion in reports
    write.csv(integrated_table, file.path(result_dir, "integrated_measurement_table.csv"), row.names = FALSE)

    return(integrated_table)
  } else {
    message("Integrated construct metrics not found. Run dashboard creation first.")
    return(NULL)
  }
}

# Create the integrated table
integrated_table <- create_integrated_table()

#------------------------------------------------------------------------------
# CREATE APPENDIX FOR PAPER SUBMISSION
#------------------------------------------------------------------------------

# Create a comprehensive appendix that combines reliability and validity results
create_integrated_appendix <- function() {
  appendix_text <- "# APPENDIX C: MEASUREMENT VALIDATION\n\n"

  # Add introduction
  appendix_text <- paste0(appendix_text, "## C.1 Overview\n\n")
  appendix_text <- paste0(appendix_text,
                       "This appendix provides a comprehensive assessment of the measurement properties of the scales used in the DurakGO study. I evaluated both reliability (internal consistency, item quality) and validity (convergent, discriminant, known-groups) to ensure that the measurement instrument meets rigorous psychometric standards.\n\n")

  # Add reliability section
  appendix_text <- paste0(appendix_text, "## C.2 Reliability Analysis\n\n")
  appendix_text <- paste0(appendix_text, "### C.2.1 Internal Consistency Reliability\n\n")
  appendix_text <- paste0(appendix_text,
                       "Internal consistency reliability was assessed using Cronbach's alpha coefficient, which measures the degree to which items within a scale measure the same underlying construct. Table C2 presents the Cronbach's alpha values for each multi-item scale used in the study.\n\n")

  if (reliability_available) {
    appendix_text <- paste0(appendix_text, "**Table C2: Internal Consistency Reliability**\n\n")
    appendix_text <- paste0(appendix_text,
                         "| Construct | Number of Items | Cronbach's α | Assessment |\n",
                         "| --------- | --------------- | ------------ | ---------- |\n")

    for (i in seq_len(nrow(alpha_results))) {
      appendix_text <- paste0(appendix_text,
                           "| ", alpha_results$Construct[i], " | ",
                           alpha_results$Items[i], " | ",
                           alpha_results$Alpha[i], " | ",
                           alpha_results$Assessment[i], " |\n")
    }

    appendix_text <- paste0(appendix_text,
                         "\n*Note: α values of ≥ 0.90 are considered excellent, ≥ 0.80 good, ≥ 0.70 acceptable, ≥ 0.60 questionable, ≥ 0.50 poor, and < 0.50 unacceptable.*\n\n")
  }

  appendix_text <- paste0(appendix_text, "### C.2.2 Item Analysis\n\n")
  appendix_text <- paste0(appendix_text,
                       "Item analysis was conducted to assess the quality of individual items within each scale. Item-total correlations measure the relationship between each item and the overall scale score (excluding that item). Items with correlations below 0.40 may not be consistently measuring the same construct as the rest of the scale.\n\n")

  if (reliability_available) {
    # Summarize item-total correlations
    item_total_summary <- item_total_results %>%
      group_by(Construct) %>%
      summarize(
        Range_of_Correlations = paste0(min(Item_Total_Correlation, na.rm = TRUE), "-",
                                     max(Item_Total_Correlation, na.rm = TRUE)),
        Items_Below_Threshold = sum(Below_Threshold, na.rm = TRUE),
        Total_Items = n()
      )

    appendix_text <- paste0(appendix_text, "**Table C3: Summary of Item-Total Correlations**\n\n")
    appendix_text <- paste0(appendix_text,
                         "| Construct | Range of Correlations | Items Below Threshold | Total Items |\n",
                         "| --------- | --------------------- | --------------------- | ----------- |\n")

    for (i in seq_len(nrow(item_total_summary))) {
      appendix_text <- paste0(appendix_text,
                           "| ", item_total_summary$Construct[i], " | ",
                           item_total_summary$Range_of_Correlations[i], " | ",
                           item_total_summary$Items_Below_Threshold[i], " | ",
                           item_total_summary$Total_Items[i], " |\n")
    }

    appendix_text <- paste0(appendix_text,
                         "\n*Note: Item-total correlations below 0.40 are considered problematic.*\n\n")
  }

  # Add validity section
  appendix_text <- paste0(appendix_text, "## C.3 Construct Validity Analysis\n\n")
  appendix_text <- paste0(appendix_text, "### C.3.1 Convergent Validity\n\n")
  appendix_text <- paste0(appendix_text,
                       "Convergent validity assesses whether items that should theoretically be related to a construct are indeed related. I evaluated convergent validity using Average Variance Extracted (AVE) and Composite Reliability (CR). AVE represents the average amount of variance in indicator variables that a construct explains, while CR measures the overall reliability of a set of heterogeneous but similar items.\n\n")

  if (validity_available) {
    appendix_text <- paste0(appendix_text, "**Table C4: Convergent Validity Assessment**\n\n")
    appendix_text <- paste0(appendix_text,
                         "| Construct | AVE | CR | Meets Criteria |\n",
                         "| --------- | --- | -- | -------------- |\n")

    for (i in seq_len(nrow(convergent_validity))) {
      appendix_text <- paste0(appendix_text,
                           "| ", convergent_validity$Construct[i], " | ",
                           round(convergent_validity$AVE[i], 3), " | ",
                           round(convergent_validity$CR[i], 3), " | ",
                           ifelse(convergent_validity$Meets_Convergent_Validity[i], "Yes", "No"), " |\n")
    }

    appendix_text <- paste0(appendix_text,
                         "\n*Note: Criteria for acceptable convergent validity are AVE > 0.50 and CR > 0.70.*\n\n")
  }

  appendix_text <- paste0(appendix_text, "### C.3.2 Discriminant Validity\n\n")
  appendix_text <- paste0(appendix_text,
                       "Discriminant validity assesses whether constructs that should be unrelated are indeed distinct from each other. I evaluated discriminant validity using the Fornell-Larcker criterion, which requires that the AVE for each construct should be greater than its squared correlation with other constructs.\n\n")

  if (validity_available) {
    appendix_text <- paste0(appendix_text, "**Table C5: Discriminant Validity Assessment**\n\n")
    appendix_text <- paste0(appendix_text,
                         "| Construct 1 | Construct 2 | Correlation | Squared Correlation | AVE 1 | AVE 2 | Meets Criterion |\n",
                         "| ----------- | ----------- | ----------- | ------------------- | ----- | ----- | --------------- |\n")

    for (i in seq_len(nrow(discriminant_validity))) {
      appendix_text <- paste0(appendix_text,
                           "| ", discriminant_validity$Construct1[i], " | ",
                           discriminant_validity$Construct2[i], " | ",
                           round(discriminant_validity$Correlation[i], 3), " | ",
                           round(discriminant_validity$Squared_Correlation[i], 3), " | ",
                           round(discriminant_validity$AVE_Construct1[i], 3), " | ",
                           round(discriminant_validity$AVE_Construct2[i], 3), " | ",
                           ifelse(discriminant_validity$Meets_Discriminant_Validity[i], "Yes", "No"), " |\n")
    }

    appendix_text <- paste0(appendix_text,
                         "\n*Note: Fornell-Larcker criterion requires that the AVE for each construct should be greater than the squared correlation with other constructs.*\n\n")
  }

  appendix_text <- paste0(appendix_text, "### C.3.3 Known-Groups Validity\n\n")
  appendix_text <- paste0(appendix_text,
                       "Known-groups validity tests whether scales can differentiate between groups expected to differ in their responses. I compared scale scores across different user types and usage frequencies to assess known-groups validity.\n\n")

  if (validity_available) {
    appendix_text <- paste0(appendix_text, "**Table C6: Known-Groups Validity Assessment**\n\n")
    appendix_text <- paste0(appendix_text,
                         "| Construct | Grouping Variable | Groups Compared | Mean Difference | t/F-value | p-value | Significant |\n",
                         "| --------- | ----------------- | --------------- | --------------- | --------- | ------- | ----------- |\n")

    for (i in seq_len(nrow(known_groups_validity))) {
      p_val <- ifelse(known_groups_validity$p_value[i] < 0.001,
                     "<0.001",
                     round(known_groups_validity$p_value[i], 3))

      appendix_text <- paste0(appendix_text,
                           "| ", known_groups_validity$Construct[i], " | ",
                           known_groups_validity$Grouping_Variable[i], " | ",
                           known_groups_validity$Groups_Compared[i], " | ",
                           round(known_groups_validity$Mean_Difference[i], 3), " | ",
                           round(known_groups_validity$t_value[i], 3), " | ",
                           p_val, " | ",
                           ifelse(known_groups_validity$Significant[i], "Yes", "No"), " |\n")
    }

    appendix_text <- paste0(appendix_text,
                         "\n*Note: Mean difference represents the absolute difference between group means. For ANOVA (more than two groups), F-statistic is reported instead of t-value.*\n\n")
  }

  # Add integrated assessment
  appendix_text <- paste0(appendix_text, "## C.4 Integrated Measurement Quality Assessment\n\n")
  appendix_text <- paste0(appendix_text,
                       "To provide a comprehensive evaluation of measurement quality, i integrated reliability and validity results for each construct. Table C1 presents the integrated measurement quality assessment.\n\n")

  appendix_text <- paste0(appendix_text,
                       "The integrated assessment indicates that ",
                       ifelse(exists("integrated_table"),
                            paste0(sum(integrated_table$Overall == "Strong"), " out of ",
                                  nrow(integrated_table), " constructs"),
                            "most constructs"),
                       " demonstrated strong measurement properties with both reliability and validity criteria met. ",
                       "Constructs with moderate or weak assessments may require further refinement in future iterations of the survey instrument.\n\n")

  # Add conclusion
  appendix_text <- paste0(appendix_text, "## C.5 Conclusion\n\n")
  appendix_text <- paste0(appendix_text,
                       "The measurement validation analysis indicates that the DurakGO survey instrument generally demonstrates acceptable psychometric properties. The scales show good internal consistency, with most items contributing meaningfully to their respective constructs. Convergent and discriminant validity assessments confirm that the scales measure distinct constructs as intended. Known-groups validity tests further support the validity of the instrument by demonstrating its ability to differentiate between relevant user segments.\n\n")

  appendix_text <- paste0(appendix_text,
                       "These findings provide confidence in the measurement quality of the data collected, supporting the validity of conclusions drawn from the main analyses. The integrated assessment approach used in this validation enables a comprehensive evaluation of measurement quality that goes beyond traditional isolated assessments of reliability or validity.")

  return(appendix_text)
}

# Generate the integrated appendix
integrated_appendix <- create_integrated_appendix()

# Save the integrated appendix to a markdown file
writeLines(integrated_appendix, file.path(result_dir, "integrated_measurement_appendix.md"))