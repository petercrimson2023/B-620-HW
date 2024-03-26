library(magrittr)
library(dplyr)
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

ate

