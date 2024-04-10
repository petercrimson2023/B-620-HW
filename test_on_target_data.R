
#test on the target dataset

source("psm_R6.R")
library(readxl)

sheet1 = read_excel("ScreenTime-hw3Q3.xlsx",sheet=1)
sheet2 = read_excel("ScreenTime-hw3Q3.xlsx",sheet=2)

sheet1$pseudo_id <- as.character(sheet1$pseudo_id)
sheet2$pseudo_id <- as.character(sheet2$pseudo_id)

merged_data <- left_join(sheet1, sheet2, by = "pseudo_id")


data_A = merged_data %>% filter(Treatment != "B") %>% mutate(Treatment = ifelse(Treatment == "A", 1, 0),
                                                             Phase = ifelse(Phase == "Treatment", 1, 0),
                                                             Exp_Age = exp(0.2*age),
                                                             Age2 = age^2,
                                                             Age_Pickup = age*Pickups)

data_B = merged_data %>% filter(Treatment != "A") %>% mutate(Treatment = ifelse(Treatment == "B", 1, 0),
                                                             Phase = ifelse(Phase == "Treatment", 1, 0),
                                                             Exp_Age = exp(0.2*age),
                                                             Age2 = age^2,
                                                             Age_Pickup = age*Pickups,
                                                             Pickups2= Pickups^2,
                                                             log_Tot_Scr= log(Tot.Scr.Time),
                                                             sex_Tot_Scr = sex*Tot.Scr.Time,
                                                             age_Tot_Scr = age*Tot.Scr.Time,
                                                             Day_num = as.factor(Day)%>% as.numeric())


covariates_A = c("sex","Phase","Age_Pickup","Pickups")
psm_A = PSM$new(data_A,"Treatment",covariates_A,"Tot.Soc.Time")
psm_A$psm_analysis()


covariates_B = c("sex","age","pets","siblings")
psm_B = PSM$new(data_B,"Treatment",covariates_B,"Tot.Soc.Time")
psm_B$psm_analysis()


data_3 = merged_data %>% filter(Treatment != "P") %>%
  mutate(Treatment = ifelse(Treatment == "A",1,0),
         Phase = ifelse(Phase == "Treatment",1,0),
         Exp_Age = exp(0.2*age),
         Age2 = age^2,
         Age_Pickup = age*Pickups,
         Pickups2 = Pickups^2,
         log_Tot_Scr = log(Tot.Scr.Time))

covariates_3 = c("Pickups","Age_Pickup","Phase")

psm_3 = PSM$new(data_3,"Treatment",covariates_3,"Tot.Soc.Time")
psm_3$psm_analysis()








