library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)
# prepare the toy data

hcover =c(0.06,0.07,0.06,0.07,0.07,0.06, 0.02,0.02,0.01)
pcdocs =c(0.02, 0.01,0.02,0.01,0.02,0.01,0.04,0.04,0.05)
A =c(1,1,1,1,0,0,0,0,0)
mrate =c(11,14, 24, 20, 26, 20, 3, 7,8)
ID = c(1,2,3,4,5,6,7,8,9)

data = cbind(ID,A, mrate,hcover, pcdocs ) %>% as.data.frame()

data

# propensity model

prop_model = glm(A~hcover+pcdocs, data = data, family = binomial)
data$propensity = predict(prop_model, type = "response")


# treated 

treated = data[data$A==1,] %>% arrange(desc(propensity))
control = data[data$A==0,] %>% arrange(desc(propensity))

treated_matched = NULL
control_matched = NULL


# nearest neighbour matching

for (i in 1:nrow(treated)){
  # print(i)
  match_index = which.min(abs(control$propensity - treated$propensity[i]))
  treated_matched = rbind(treated_matched, treated[i,])
  control_matched = rbind(control_matched, control[match_index,])
  control = control[-match_index,]
  # print(list(matched_treated = treated_matched, 
  #            matched_control = control_matched))
  #            #control_remain = control))
}

# ate

ate = mean(treated_matched$mrate) - mean(control_matched$mrate)



# _________________________________________________________functioning______________________________________________________


estimate_propensity <- function(formula, data, family = binomial) {
  prop_model <- glm(formula, data = data, family = family)
  data$propensity <- predict(prop_model, type = "response")
  return(data)
}

sort_by_propensity <- function(data) {
  treated <- data[data$A == 1, ] %>% arrange(desc(propensity))
  control <- data[data$A == 0, ] %>% arrange(desc(propensity))
  return(list(treated = treated, control = control))
}


nearest_neighbor_matching <- function(treated, control,Repeate = FALSE) {
  treated_matched <- NULL
  control_matched <- NULL
  
  if(Repeate)
  {
    for (i in 1:nrow(treated)) {
      match_index <- which.min(abs(control$propensity - treated$propensity[i]))
      treated_matched <- rbind(treated_matched, treated[i, ])
      control_matched <- rbind(control_matched, control[match_index, ])
      #control <- control[-match_index, ]
    }
  }else{
    for (i in 1:nrow(treated)) {
      match_index <- which.min(abs(control$propensity - treated$propensity[i]))
      treated_matched <- rbind(treated_matched, treated[i, ])
      control_matched <- rbind(control_matched, control[match_index, ])
      control <- control[-match_index, ]
    }
  }
  return(list(treated_matched = treated_matched, 
              control_matched = control_matched))
}


perform_psm <- function(formula, data, family = binomial,Repeate = FALSE) {
  data <- estimate_propensity(formula, data, family)
  sorted_data <- sort_by_propensity(data)
  matched_data <- nearest_neighbor_matching(sorted_data$treated, sorted_data$control,Repeate = Repeate)
  return(matched_data)
}


matched_data = perform_psm(A~hcover+pcdocs, data = data,Repeat = TRUE)

ate = mean(matched_data$treated_matched$mrate) - mean(matched_data$control_matched$mrate)

#ate


#____________ Plotting results __________________



matched_data

data.frame(
  Group = c(rep("treat", nrow(matched_data$treated_matched)), rep("control", nrow(matched_data$control_matched))),
  Propensity = c(matched_data$treated_matched$propensity, matched_data$control_matched$propensity)
) -> plot_data

# kde plot


ggplot(plot_data,aes(x=Propensity,color=Group))+
  geom_density(size=1)+
  labs(title="Propensity Score Distribution",x="Propensity",y="Density")


ggplot(plot_data,aes(x=Propensity,fill=Group))+
  geom_histogram(binwidth = 0.005,alpha = 0.5, position = "identity" )+
  labs(title="Propensity Score Distribution",x="Propensity",y="Freq",fill="Group")



# Calculating SMD and plotting the love plot

library(cobalt)


data_list = list(
  Before = data %>% mutate(treat=A),
  After = rbind(matched_data$treated_matched, matched_data$control_matched)%>% mutate(treat=A)
)

data_list = bind_rows(
  data_list$Before %>% mutate(match_status = "Before"),
  data_list$After %>% mutate(match_status = "After")
)

matched_result = rbind(
  matched_data$treated_matched, matched_data$control_matched
) %>% mutate(treat=A)

smd_anon <- function(x, y) (mean(x) - mean(y)) / sqrt((var(x) + var(y)) / 2)

# 计算标准化均值差(SMD)的函数

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



# 绘制 Love Plot 的函数
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


covariates <- c("hcover", "pcdocs")
smd_data <- calculate_smd(data_list, treat = "treat", covariates = covariates, group = "match_status")
love_plot <- plot_love(smd_data)
print(love_plot)
