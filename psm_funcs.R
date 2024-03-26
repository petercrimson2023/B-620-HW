library(magrittr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)


# Estimate propensity scores
estimate_propensity <- function(formula, data, family = binomial) {
  prop_model <- glm(formula, data = data, family = family)
  data$propensity <- predict(prop_model, type = "response")
  return(data)
}

# Sort data by propensity scores
sort_by_propensity <- function(data) {
  treated <- data[data$treat == 1, ] %>% arrange(desc(propensity))
  control <- data[data$treat == 0, ] %>% arrange(desc(propensity))
  return(list(treated = treated, control = control))
}

# Perform nearest neighbor matching
nearest_neighbor_matching <- function(treated, control, replace = FALSE) {
  treated_matched <- NULL
  control_matched <- NULL
  
  if (replace) {
    for (i in 1:nrow(treated)) {
      match_index <- which.min(abs(control$propensity - treated$propensity[i]))
      treated_matched <- rbind(treated_matched, treated[i, ])
      control_matched <- rbind(control_matched, control[match_index, ])
    }
  } else {
    for (i in 1:nrow(treated)) {
      match_index <- which.min(abs(control$propensity - treated$propensity[i]))
      treated_matched <- rbind(treated_matched, treated[i, ])
      control_matched <- rbind(control_matched, control[match_index, ])
      control <- control[-match_index, ]
    }
  }
  
  return(list(treated_matched = treated_matched, control_matched = control_matched))
}

# Perform propensity score matching
perform_psm <- function(formula, data, family = binomial, replace = FALSE) {
  data <- estimate_propensity(formula, data, family)
  sorted_data <- sort_by_propensity(data)
  matched_data <- nearest_neighbor_matching(sorted_data$treated, sorted_data$control, replace = replace)
  return(matched_data)
}

matched_data <- perform_psm(treat ~ hcover + pcdocs, data = data, replace = TRUE)
ate <- mean(matched_data$treated_matched$mrate) - mean(matched_data$control_matched$mrate)

# Plotting results
plot_data <- data.frame(
  group = c(rep("treat", nrow(matched_data$treated_matched)), rep("control", nrow(matched_data$control_matched))),
  propensity = c(matched_data$treated_matched$propensity, matched_data$control_matched$propensity)
)

# KDE plot

plot_distribution <- function(matched_data)
{
  # constructing the plot_data
  plot_data <- data.frame(
    group = c(rep("treat", nrow(matched_data$treated_matched)), rep("control", nrow(matched_data$control_matched))),
    propensity = c(matched_data$treated_matched$propensity, matched_data$control_matched$propensity)
  )
  # KDE plot
  
  kde_p = ggplot(plot_data, aes(x = propensity, color = group)) +
    geom_density(size = 1) +
    labs(title = "Propensity Score Distribution", x = "Propensity", y = "Density")
  
  # Histogram
  
  hist_p = ggplot(plot_data, aes(x = propensity, fill = group)) +
    geom_histogram(binwidth = 0.005, alpha = 0.5, position = "identity") +
    labs(title = "Propensity Score Distribution", x = "Propensity", y = "Freq", fill = "Group")
  
  kde_p+hist_p+plot_layout(ncol = 1)
}

# ggplot(plot_data, aes(x = propensity, color = group)) +
#   geom_density(size = 1) +
#   labs(title = "Propensity Score Distribution", x = "Propensity", y = "Density")
# 
# # Histogram
# ggplot(plot_data, aes(x = propensity, fill = group)) +
#   geom_histogram(binwidth = 0.005, alpha = 0.5, position = "identity") +
#   labs(title = "Propensity Score Distribution", x = "Propensity", y = "Freq", fill = "Group")


#plot_distribution(matched_data)


# Calculate standardized mean differences (SMD)
smd_anon <- function(x, y) (mean(x) - mean(y)) / sqrt((var(x) + var(y)) / 2)

calculate_smd <- function(data, treat, covariates, group) {
  if (!all(c(treat, covariates, group) %in% names(data))) {
    stop("One or more specified columns do not exist in the input data frame.")
  }
  
  if (!all(treat %in% names(data))) {
    warning("The 'treat' variable is not found in the data. SMD will be returned as NA.")
    return(tibble(group = unique(data[[group]]),
                  variable = covariates,
                  smd = NA_real_))
  }
  
  smd_data <- data %>%
    group_by(across(all_of(group))) %>%
    summarise(across(all_of(covariates), ~ {
      smd_anon(.x[.data[[treat]] == 1], .x[.data[[treat]] == 0])
    }, .names = "{.col}_smd")) %>%
    pivot_longer(cols = ends_with("_smd"), names_to = "variable", values_to = "smd") %>%
    mutate(variable = gsub("_smd", "", variable))
  
  return(smd_data)
}

# Plot love plot
plot_love <- function(smd_data, title = "Love Plot: Standardized Mean Differences", 
                      smd_threshold = 0.1, colors = c("red", "blue"), 
                      shapes = c("circle", "triangle")) {
  plot <- ggplot(smd_data, aes(x = abs(smd), y = variable, color = match_status, shape = match_status)) +
    geom_point(size = 3) +
    geom_vline(xintercept = smd_threshold, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    labs(
      title = title,
      x = "Absolute Standardized Mean Difference",
      y = "Variable",
      color = "Matching Status",
      shape = "Matching Status"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(plot)
}

