library(MatchIt)
library(cobalt)

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
                                                             Age_Totscr = age*Tot.Scr.Time,
                                                             Sex_Totscr = sex*Tot.Scr.Time)


matchit_model_A = matchit(Treatment~sex+Age_Pickup+Phase+Pickups,
                          data=data_A,method="nearest",distance="glm",
                          caliper=0.1,ratio=1)

bal.tab(matchit_model_A, data = data_A, un = TRUE)
matched_data_A <- match.data(matchit_model_A)
model_A <- lm(Tot.Scr.Time ~ Treatment, data = matched_data_A)
coef(model_A)[2]
matched_data_A$distance %>% summary()

# Treatment 
# -261.8194 

ggplot(matched_data_A, aes(x = distance, fill = factor(Treatment))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red"), 
                    labels = c("Control", "Treatment"),
                    name = "Group") +
  xlab("Propensity Score") +
  ylab("Density") +
  ggtitle("Propensity Score Distribution (Treatment A)") +
  theme_minimal()



matchit_model_B = matchit(Treatment~sex+Tot.Soc.Time+Phase,
                          data=data_B,method="nearest",distance="glm",
                          caliper=0.1,ratio=1)

bal.tab(matchit_model_B, data = data_B, un = TRUE)
matched_data_B <- match.data(matchit_model_B)

model_B <- lm(Pickups ~ Treatment, data = matched_data_B)
coef(model_B)[2]
matched_data_B$distance %>% summary()


# Treatment 
# 40.62353 

ggplot(matched_data_B, aes(x = distance, fill = factor(Treatment))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red"), 
                    labels = c("Control", "Treatment"),
                    name = "Group") +
  xlab("Propensity Score") +
  ylab("Density") +
  ggtitle("Propensity Score Distribution (Treatment B)") +
  theme_minimal()

