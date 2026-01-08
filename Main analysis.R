###########################
rm(list = ls())

library(tidyverse)
library(vegan)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(stringr)
###################################
DD_results <- read.csv("Multitrophic_Stability_Results.csv", check.names = F)

############## Check for difference between two trophic levels
DD_long <- DD_results %>%
  pivot_longer(cols = c(spe_sta, com_sta, spe_phi, alpha_richness), 
               names_to = "variable", values_to = "value")
DD_wide <- DD_long %>%
  pivot_wider(id_cols = c(siteID, variable), 
              names_from = FunGroup, 
              values_from = value)
logt.test_results <- DD_wide %>%
  group_by(variable) %>%
  summarise(
    t_test = list(t.test(log(Consumer), log(Producer), paired = TRUE))
  ) %>%
  mutate(
    statistic = map_dbl(t_test, ~.$statistic),
    p_value = map_dbl(t_test, ~.$p.value),
    mean_difference = map_dbl(t_test, ~.$estimate)
  ) %>%
  dplyr::select(-t_test)
print(logt.test_results)

################ Then we check bottom-up and top-down effects of diversity
DD_producer <- DD_results[DD_results$FunGroup == "Producer",]
DD_consumer <- DD_results[DD_results$FunGroup == "Consumer",]

########## From the BOTTOM-UP Perspective: how producer diversity affect consumer stability?
## combine the data
DD_combined <- left_join(DD_producer, DD_consumer, by = c("siteID", "habitat_type", "sitetype","y_length")) %>%
  rename(prod_spesta = spe_sta.x, prod_comsta = com_sta.x, prod_spephi = spe_phi.x, 
         prod_richness = alpha_richness.x,prod_shannon = alpha_shannon.x, prod_simpson = alpha_simpson.x,
         cons_spesta = spe_sta.y, cons_comsta = com_sta.y, cons_spephi = spe_phi.y, 
         cons_richness = alpha_richness.y,cons_shannon = alpha_shannon.y, cons_simpson = alpha_simpson.y,
         tmp = tmp.x, sd_temp = sd_temp.x) %>%
  dplyr::select(-tmp.y, -FunGroup.y, -FunGroup.x, -sd_temp.y)

##########Convert the  data to Log scale
cols_to_transform <- c(4:7, 13:16)
DD_log <- DD_combined
DD_log[, cols_to_transform] <- lapply(DD_log[, cols_to_transform], function(x) {
  if(!is.numeric(x)) {
    x <- as.numeric(as.character(x))
  }
  return(log(x, base = exp(1))) 
})
DD_log <- as.data.frame(DD_log)
DD_log$habitat_type <- as.factor(DD_log$habitat_type)

###### For cosumer community stability using log-transferred data
options(na.action = "na.fail")
full_cons <- lmer(cons_comsta ~ prod_richness + cons_richness + tmp + sd_temp
                  + (1|habitat_type), data = DD_log)
summary(full_cons)
anova(full_cons)

##### For producer commuity stability
full_prod <- lmer(prod_comsta ~ prod_richness + cons_richness + tmp + sd_temp
                  + (1|habitat_type), data = DD_log)
summary(full_prod)
anova(full_prod)

#############################################################################################
################ SEM model
DD_sem_lmer <- psem(
  lm(cons_comsta ~  cons_spesta + cons_spephi, data = DD_log),
  lm(prod_comsta ~    prod_spesta  + prod_spephi, data = DD_log),
  lmer(cons_spesta ~  cons_richness + prod_richness + sd_temp + (1|habitat_type), data = DD_log),
  lmer(cons_spephi ~  cons_richness + (1|habitat_type), data = DD_log),
  lmer(prod_spesta ~  prod_richness + (1|habitat_type), data = DD_log),
  lmer(prod_spephi ~  prod_richness + cons_richness + sd_temp +tmp +(1|habitat_type), data = DD_log),
  
  cons_richness %~~% prod_richness, 
  cons_comsta %~~% prod_comsta,

  data = DD_log 
)
dSep(DD_sem_lmer)
summary(DD_sem_lmer)

###################### Full models With Richness
DD_sem_lmer_full <- psem(
  lm(cons_comsta ~  cons_spesta + cons_spephi, data = DD_log),
  lm(prod_comsta ~    prod_spesta  + prod_spephi, data = DD_log),
  lmer(cons_spesta ~  cons_richness + prod_richness + tmp + sd_temp + (1|habitat_type), data = DD_log),
  lmer(cons_spephi ~  cons_richness + prod_richness + tmp + sd_temp + (1|habitat_type), data = DD_log),
  lmer(prod_spesta ~   cons_richness + prod_richness + tmp + sd_temp  + (1|habitat_type), data = DD_log),
  lmer(prod_spephi ~  cons_richness + prod_richness + tmp + sd_temp + (1|habitat_type), data = DD_log),
  
  cons_richness %~~% prod_richness, 
  cons_comsta %~~% prod_comsta,
  #prod_spephi %~~% prod_spesta,
  #cons_spephi %~~% cons_spesta,
  
  data = DD_log 
)
dSep(DD_sem_lmer_full)
summary(DD_sem_lmer_full)

