library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)



propensity_score_matching <- function(data, treatment_col, covariate_cols) {
  # Estimate Propensity Scores
  formula <- as.formula(paste(treatment_col, "~", paste(covariate_cols, collapse = "+")))
  propensity_model <- glm(formula, data=data, family=binomial(link="logit"))
  scores <- predict(propensity_model, type="response")
  
  # Data with Scores
  data_with_scores <- cbind(data, PropensityScore=scores)
  
  # Separate treatment and control
  treated <- data_with_scores[data_with_scores[[treatment_col]] == 1,]
  control <- data_with_scores[data_with_scores[[treatment_col]] == 0,]
  
  #available_control_indices <- 1:nrow(control)
  
  # Greedy 1:1 Matching without Replacement
  matched_indices <- sapply(treated$PropensityScore, function(score) {
    differences <- abs(control$PropensityScore - score)
    if (length(differences) > 0) {
      matched_index <- which.min(differences)
      control <- control[-matched_index,] # Remove matched control
      # matched_index <- which.min(differences)
      # matched_control_index <- available_control_indices[matched_index]
      # available_control_indices <- available_control_indices[-matched_index] # Remove matched control
      # return(matched_control_index)
      return(matched_index)
    } else {
      return(NA)
    }
  }, simplify = "array")
  
  # Results
  matched_control <- control[matched_indices, , drop = FALSE]
  matched_data <- rbind(treated, matched_control)
  return(matched_data)
}


calculate_ATE_manual <- function(matched_data, outcome_col, treatment_col) {
  n_pairs <- nrow(matched_data) / 2
  treated_outcomes <- matched_data[1:n_pairs, outcome_col]
  control_outcomes <- matched_data[(n_pairs + 1):(2*n_pairs), outcome_col]
  
  # Calculate differences
  differences <- treated_outcomes - control_outcomes
  mean_diff <- mean(differences)
  sd_diff <- sd(differences)
  n <- length(differences)
  
  
  ATE <- mean_diff
  t_statistic <- mean_diff / (sd_diff / sqrt(n))
  df <- n - 1
  p_value <- 2 * (1 - pt(abs(t_statistic), df))
  
  # Output
  list(
    ATE = ATE,
    t_statistic = t_statistic,
    p_value = p_value
  )
}



smd_anon <- function(x, y) (mean(x) - mean(y)) / sqrt((var(x) + var(y)+0.001) / 2)

calculate_smd = function(matched_data,outcome_col,treatment_col,covariates,matched_data_set = TRUE)
{
  if(matched_data_set == TRUE)
  {
    n_pairs <- nrow(matched_data) / 2
    # treated_outcomes <- matched_data[1:n_pairs, outcome_col]
    # control_outcomes <- matched_data[(n_pairs + 1):(2*n_pairs), outcome_col]
    n_end = nrow(matched_data)
    smd_list = list()
    
    for (i in covariates)
    {
      treated_covariate <- matched_data[1:n_pairs, i] 
      control_covariate <- matched_data[(n_pairs + 1):n_end, i] 
      smd <- smd_anon(treated_covariate, control_covariate)
      smd_list[[i]] <- smd
    }
    
  }
  else
  {
    # matched_data is the original dataset used to calculate smd for unadjsted scenario
    n_pairs = sum(matched_data[treatment_col] == 1)
    n_end = nrow(matched_data)
    smd_list = list()
    
    for (i in covariates)
    {
      treated_covariate <- matched_data[1:n_pairs, i] %>% unlist() %>% as.numeric()
      control_covariate <- matched_data[(n_pairs + 1):n_end, i] %>% unlist() %>% as.numeric()
      smd <- smd_anon(treated_covariate, control_covariate)
      smd_list[[i]] <- smd
    }
    
  }
  # renaming the SMDs
  
  names(smd_list) <- covariates
  
  return(smd_list)
}

smd = function(matched_data,data,outcome_col,treatment_col,covariates)
{
  SMD_results = calculate_smd(matched_data,outcome_col,treatment_col, covariates) %>% as.data.frame() 
  SMD_before_match = calculate_smd(data,outcome_col,treatment_col ,covariates, matched_data_set = FALSE) %>% as.data.frame() 
  smd_frame = rbind(SMD_before_match,SMD_results) %>% t() %>% as.data.frame()
  names(smd_frame) <- c("before","after") 
  smd_frame
}




plotting_propensity_score=function(matched_data)
{
  matched_data$Treatment = as.factor(matched_data$Treatment)
  ggplot(matched_data, aes(x=PropensityScore, fill=Treatment)) + geom_histogram(position="dodge", bins=20) + labs(title="Histogram of Propensity Score", x="Propensity Score", y="Frequency") + theme_minimal()
}

kde_propensity_score=function(matched_data)
{
  matched_data$Treatment = as.factor(matched_data$Treatment)
  ggplot(matched_data, aes(x=PropensityScore, fill=Treatment)) + geom_density(alpha=0.5) + labs(title="Density Plot of Propensity Score", x="Propensity Score", y="Density") + theme_minimal()
}


hcover <- c(0.06, 0.07, 0.06, 0.07, 0.07, 0.06, 0.02, 0.02, 0.01)
pcdocs <- c(0.02, 0.01, 0.02, 0.01, 0.02, 0.01, 0.04, 0.04, 0.05)
treat <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
mrate <- c(11, 14, 24, 20, 26, 20, 3, 7, 8)
id <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)

data <- data.frame(id, treat, mrate, hcover, pcdocs)


covariates = c("hcover", "pcdocs")

matched_data = propensity_score_matching(data,"treat", covariates)
calculate_ATE_manual(matched_data, "mrate", "treat")

smd(matched_data,data,"mrate","treat",covariates)


# _______________________________MatchIt version____________________________________

library(MatchIt)
library(cobalt)

match_it_model = matchit(treat~hcover+pcdocs,data = data,method = "nearest",
                         replace=FALSE,ratio=1)

bal.tab(match_it_model,data=data,un=TRUE)







