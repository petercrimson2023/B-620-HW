library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(R6)

# Define the PropensityScoreMatching class

PSM = R6Class(
  
  classname = "Propensity_Score_Matching",
  
  public = list(
    data = NULL,
    treatment_col = NULL,
    covariate_cols = NULL,
    outcome_col = NULL,
    matched_data = NULL,
    ATE_list = NULL,
    smd_result = NULL,
    
    initialize = function(data, treatment_col, covariate_cols,outcome_col) {
      self$data <- data
      self$treatment_col <- treatment_col
      self$covariate_cols <- covariate_cols
      self$matched_data <- -Inf 
      self$outcome_col <- outcome_col
      self$ATE_list <- list()
      self$smd_result <- list()
    },
    
    Match = function(){
      # Estimate Propensity Scores
      formula <- as.formula(paste(self$treatment_col, "~", paste(self$covariate_cols, collapse = "+")))
      propensity_model <- glm(formula, data=self$data, family=binomial(link="logit"))
      scores <- predict(propensity_model, type="response")
      
      
      # Data with Scores
      data_with_scores <- cbind(self$data, PropensityScore=scores)
      self$data$PropensityScore <- scores
      # Separate treatment and control
      treated <- data_with_scores[data_with_scores[[self$treatment_col]] == 1,]
      control <- data_with_scores[data_with_scores[[self$treatment_col]] == 0,]
      
      # Greedy 1:1 Matching without Replacement
      matched_indices <- sapply(treated$PropensityScore, function(score) {
        differences <- abs(control$PropensityScore - score)
        if (length(differences) > 0) {
          matched_index <- which.min(differences)
          control <- control[-matched_index,] 
          return(matched_index)
        } else {
          return(NA)
        }
      }, simplify = "array")
      
      # Results
      matched_control <- control[matched_indices, , drop = FALSE]
      self$matched_data <- rbind(treated, matched_control) %>% as.data.frame()
      #return(matched_data)
    },
    
    calculate_ATE = function() {
      
      # What this function do:
      # 1. Check if Match method has been called
      
      if(typeof(self$matched_data) == "double"){
        stop("Match method must be called before calculating ATE")
      }
      
      # 2. Calculate ATE
      
      n_pairs <- nrow(self$matched_data) / 2
      treated_outcomes <- self$matched_data[1:n_pairs, self$outcome_col]
      control_outcomes <- self$matched_data[(n_pairs + 1):(2*n_pairs), self$outcome_col]
      
      # Calculate differences
      differences <- treated_outcomes - control_outcomes
      mean_diff <- mean(differences)
      ATE <- mean_diff
      t_test_result = t.test(treated_outcomes, control_outcomes)
      
      # Output
      self$ATE_list = list(
        ATE = ATE,
        t_statistic = t_test_result$statistic,
        t_test_method = t_test_result$method,
        p_value = t_test_result$p.value,
        confidence_interval = t_test_result$conf.int,
        df = t_test_result$parameter
      )
    },
    
    # Standardized Mean Difference calculation function 1
    smd_anon = function(x, y) {(mean(x) - mean(y)) / sqrt((var(x) + var(y)+0.00001) / 2)},
    
    # Standardized Mean Difference calculation function 2
    calculate_smd = function(target_data,covariate_cols,treatment_col,matched_data_set = TRUE)
    {
      
      # What this function does:
      # 1. Check if Match method has been called
      
      if(matched_data_set == TRUE)
      {
        #matched_data is used to calculate smd for adjusted scenario
        n_pairs <- nrow(target_data) / 2
        n_end = nrow(target_data)
        smd_list = list()
        
        for (i in covariate_cols)
        {
          treated_covariate <- target_data[1:n_pairs, i] 
          control_covariate <- target_data[(n_pairs + 1):n_end, i] 
          smd <- self$smd_anon(treated_covariate, control_covariate)
          smd_list[[i]] <- smd
        }
        
      }
      else
      {
        # matched_data is the original dataset used to calculate smd for unadjsted scenario
        n_pairs = sum(target_data[treatment_col] == 1)
        n_end = nrow(target_data)
        smd_list = list()
        
        for (i in covariate_cols)
        {
          treated_covariate <- target_data[1:n_pairs, i] %>% unlist() %>% as.numeric()
          control_covariate <- target_data[(n_pairs + 1):n_end, i] %>% unlist() %>% as.numeric()
          smd <- self$smd_anon(treated_covariate, control_covariate)
          smd_list[[i]] <- smd
        }
        
      }
      # renaming the SMDs
      
      names(smd_list) <- covariate_cols
      
      return(smd_list)
    },
    
    smd = function()
    {
      SMD_results = self$calculate_smd(self$matched_data,
                                       self$covariate_cols,
                                       self$treatment_col) %>% as.data.frame()
      
      SMD_before_match = self$calculate_smd(self$data,
                                            self$covariate_cols,
                                            self$treatment_col,
                                            matched_data_set = FALSE) %>% as.data.frame()
      smd_frame = rbind(SMD_before_match, SMD_results) %>% t() %>% as.data.frame()
      names(smd_frame) <- c("before", "after") 
      self$smd_result = smd_frame
      print("SMD Results")
      smd_frame
    },
    
    plot = function()
    {
      # Plotting the distribution of propensity scores before and after matching
      
      before_match = ggplot(self$data,aes(x = PropensityScore,fill = as.factor(Treatment))) + 
        geom_density(alpha = 0.5) + 
        ggtitle("Before Matching") + 
        theme_minimal()
      
      after_match  =ggplot(self$matched_data ,aes(x = PropensityScore,fill=as.factor(Treatment))) + 
        geom_density(alpha = 0.5) + 
        ggtitle("After Matching") + 
        theme_minimal()
      
      before_match_histogram = ggplot(self$data,aes(x = PropensityScore,fill = as.factor(Treatment))) + 
        geom_histogram(alpha = 0.5) + 
        ggtitle("Histogram: Before Matching") + 
        theme_minimal()
      
      after_match_histogram = ggplot(self$matched_data ,aes(x = PropensityScore,fill=as.factor(Treatment))) +
        geom_histogram(alpha = 0.5) + 
        ggtitle("Histogram: After Matching") + 
        theme_minimal()
      
      # Love plot
      
      love_plot = ggplot(self$smd_result, aes(x = before, y = rownames(self$smd_result), color = "before")) +
        geom_point(size = 3) +
        geom_point(aes(x = after, color = "after"), size = 3) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
        geom_vline(xintercept = c(-0.1, 0.1), linetype = "twodash", color = "black") +
        scale_color_manual(name = "Stage", values = c("before" = "red", "after" = "blue")) +
        labs(x = "Standardized Mean Difference (SMD)", y = "Covariate",
             title = "Love Plot") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "top",
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank())
      
      
      before_match+after_match+before_match_histogram+after_match_histogram+love_plot+
        plot_layout(ncol = 2)
      
    },
    
    psm_analysis = function()
    {
      self$Match()
      self$calculate_ATE()
      print(self$ATE_list)
      self$smd()
      print(self$smd_result)
      self$plot()
      
    }
    
  )
)


