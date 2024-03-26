# Prepare the toy data
hcover <- c(0.06, 0.07, 0.06, 0.07, 0.07, 0.06, 0.02, 0.02, 0.01)
pcdocs <- c(0.02, 0.01, 0.02, 0.01, 0.02, 0.01, 0.04, 0.04, 0.05)
treat <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
mrate <- c(11, 14, 24, 20, 26, 20, 3, 7, 8)
id <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)

data <- data.frame(id, treat, mrate, hcover, pcdocs)



# Prepare the toy data
hcover <- c(0.06, 0.07, 0.06, 0.07, 0.07, 0.06, 0.02, 0.02, 0.01)
pcdocs <- c(0.02, 0.01, 0.02, 0.01, 0.02, 0.01, 0.04, 0.04, 0.05)
treat <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
mrate <- c(11, 14, 24, 20, 26, 20, 3, 7, 8)
id <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)

data <- data.frame(id, treat, mrate, hcover, pcdocs)



data_list <- bind_rows(
  data %>% mutate(match_status = "Before"),
  rbind(matched_data$treated_matched, matched_data$control_matched) %>% mutate(match_status = "After")
)

covariates <- c("hcover", "pcdocs")
smd_data <- calculate_smd(data_list, treat = "treat", covariates = covariates, group = "match_status")
love_plot <- plot_love(smd_data)
print(love_plot)
