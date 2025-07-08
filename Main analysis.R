###########################
rm(list = ls())

library(tidyverse)
library(vegan)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(stringr)

###################################

DD_results <- read.csv(file.choose(), check.names = F) #s3r
DD_temp <- read.csv("DD_temp.csv", check.names = F)
unique(DD_results$siteID)
unique(DD_temp$siteID)
##temperature data
DD_temp_1 <- DD_temp %>%
  group_by(siteID) %>%
  summarise(mean_tmp = mean(tmp, na.rm = T),
            sd_temp = sd(tmp, na.rm = T)) %>%
  ungroup() %>%
  mutate(t_com = mean_tmp/sd_temp) %>%
  rename(tmp = mean_tmp)
DD_results <- left_join(DD_results, DD_temp_1, by = c("siteID" = "siteID"))
#DD_results <- DD_results %>%
#  filter(!siteID %in% c("Kasumigaura"))

############### Check for difference between two trophic levels
DD_long_1 <- DD_results %>%
  pivot_longer(cols = c(spe_sta, com_sta, spe_phi, alpha_simpson), 
               names_to = "variable", values_to = "value")
DD_wide <- DD_long_1 %>%
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
  select(-t_test)
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
         #prod_evenness = alpha_evenness.x, cons_evenness = alpha_evenness.y,
         #prod_usd_slope = Species_u_sd.x, prod_u = u.x, prod_sd = sd.x, prod_totbio = tot_abund.x,
         #cons_usd_slope = Species_u_sd.y, cons_u = u.y, cons_sd = sd.y, cons_totbio = tot_abund.y,
         tmp = tmp.x) %>%
  dplyr::select(-tmp.y, -group.x, -group.y, -FunGroup.y, -FunGroup.x)

##########Convert the  data to Log scale
cols_to_transform <- c(4:6, 15:17)
DD_log <- DD_combined
DD_log[, cols_to_transform] <- lapply(DD_log[, cols_to_transform], function(x) {
  if(!is.numeric(x)) {
    x <- as.numeric(as.character(x))
  }
  return(log(x, base = exp(1))) 
})
DD_log <- as.data.frame(DD_log)
DD_log$habitat_type <- as.factor(DD_log$habitat_type)

###### For cosumer community stability
options(na.action = "na.fail")
full_cons <- lmer(cons_comsta ~ prod_simpson + cons_simpson + tmp + sd_temp.y
                    + (1|habitat_type), data = DD_log)
summary(full_cons)
anova(full_cons)

##### For producer commuity stability
full_prod <- lmer(prod_comsta ~ prod_simpson + cons_simpson + tmp + sd_temp.x
                    + (1|habitat_type), data = DD_log)
summary(full_prod)
anova(full_prod)

#############################################################################################
################ SEM model
log_data_sem_lmer <- psem(
  lm(cons_comsta ~  cons_spesta + cons_spephi, data = DD_log),
  lm(prod_comsta ~    prod_spesta  + prod_spephi, data = DD_log),
  lmer(cons_spesta ~  cons_simpson + prod_simpson + sd_temp.y + (1|habitat_type), data = DD_log),
  lmer(cons_spephi ~  cons_simpson + (1|habitat_type), data = DD_log),
  lmer(prod_spesta ~   cons_simpson +  prod_simpson + (1|habitat_type), data = DD_log),
  lmer(prod_spephi ~   prod_simpson + sd_temp.y +(1|habitat_type), data = DD_log),
  
  cons_simpson %~~% prod_simpson, 
  cons_comsta %~~% prod_comsta,
  
  data = DD_log 
  
)
dSep(log_data_sem_lmer)
summary(log_data_sem_lmer)

###################### Full models With Simpson diversity
log_data_sem_lmer_full <- psem(
  lm(cons_comsta ~  cons_spesta + cons_spephi, data = DD_log),
  lm(prod_comsta ~    prod_spesta  + prod_spephi, data = DD_log),
  lmer(cons_spesta ~  cons_simpson + prod_simpson + tmp + sd_temp.y + (1|habitat_type), data = DD_log),
  lmer(cons_spephi ~  cons_simpson + prod_simpson + tmp + sd_temp.y + (1|habitat_type), data = DD_log),
  lmer(prod_spesta ~   cons_simpson + prod_simpson + tmp + sd_temp.y  + (1|habitat_type), data = DD_log),
  lmer(prod_spephi ~  cons_simpson + prod_simpson + tmp + sd_temp.y + (1|habitat_type), data = DD_log),
  
  cons_simpson %~~% prod_simpson, 
  cons_comsta %~~% prod_comsta,
  prod_spephi %~~% prod_spesta,
  cons_spephi %~~% cons_spesta,
  
  data = DD_log 
)
dSep(log_data_sem_lmer_full)
summary(log_data_sem_lmer_full)
