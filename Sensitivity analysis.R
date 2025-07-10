###########################
rm(list = ls())

library(tidyverse)
library(lme4)
library(lmerTest)
library(car)

################### Sentivity analysis taking habitat type as fixed factors
setwd("C:/周礼斌/Data Collection/Data preserve/River for 2km/Manipulated_Data")

### Multiple regression with habitat type as fixed factors
full_cons_multivar <- lm(cons_comsta ~  habitat_type*prod_simpson + habitat_type*cons_simpson + sd_temp + tmp
                       , data = DD_log)
Anova(full_cons_multivar, type = "III")
summary(full_cons_multivar)

full_prod_multivar <- lm(prod_comsta ~  habitat_type*prod_simpson + habitat_type*cons_simpson + sd_temp + tmp
                       , data = DD_log)
Anova(full_prod_multivar, type = "III")
summary(full_prod_multivar)

###################################################################################################
################ Sensitivity analysis for different diversity indices
## shannon index
full_cons_shannon <- lmer(cons_comsta ~ prod_shannon + cons_shannon + tmp + sd_temp + 
                    + (1|habitat_type), data = DD_log)
summary(full_cons_shannon)
anova(full_cons_shannon)

full_prod_shannon <- lmer(prod_comsta ~ prod_shannon + cons_shannon + tmp + sd_temp + 
                    + (1|habitat_type), data = DD_log)
summary(full_prod_shannon)
anova(full_prod_shannon)

## richness with rare species
full_cons_richness <- lmer(cons_comsta ~  (prod_richness) + (cons_richness) + tmp + sd_temp + 
                    + (1|habitat_type), data = DD_log)
summary(full_cons_richness)
anova(full_cons_richness)

full_prod_richness <- lmer(prod_comsta ~  (prod_richness) + (cons_richness) + tmp + sd_temp + 
                             + (1|habitat_type), data = DD_log)
summary(full_prod_richness)
anova(full_prod_richness)

### richness without rare species
DD_results_norare <- read.csv("Multitrophic_Stability_Results(WithoutRareSpecies).csv", check.names = F)
DD_producer_norare <- DD_results_norare[DD_results_norare$FunGroup == "Producer",]
DD_consumer_norare <- DD_results_norare[DD_results_norare$FunGroup == "Consumer",]

########## From the BOTTOM-UP Perspective: how producer diversity affect consumer stability?
## combine the data
DD_combined_norare <- left_join(DD_producer_norare, DD_consumer_norare, by = c("siteID", "habitat_type", "sitetype","y_length")) %>%
  rename(prod_spesta = spe_sta.x, prod_comsta = com_sta.x, prod_spephi = spe_phi.x, 
         prod_richness = alpha_richness.x,prod_shannon = alpha_shannon.x, prod_simpson = alpha_simpson.x,
         cons_spesta = spe_sta.y, cons_comsta = com_sta.y, cons_spephi = spe_phi.y, 
         cons_richness = alpha_richness.y,cons_shannon = alpha_shannon.y, cons_simpson = alpha_simpson.y,
         tmp = tmp.x, sd_temp = sd_temp.x) %>%
  dplyr::select(-tmp.y, -FunGroup.y, -FunGroup.x, -sd_temp.y)

##########Convert the  data to Log scale
cols_to_transform <- c(4:7, 13:16)
DD_log_norare <- DD_combined_norare
DD_log_norare[, cols_to_transform] <- lapply(DD_log_norare[, cols_to_transform], function(x) {
  if(!is.numeric(x)) {
    x <- as.numeric(as.character(x))
  }
  return(log(x, base = exp(1))) 
})
DD_log_norare <- as.data.frame(DD_log_norare)
DD_log_norare$habitat_type <- as.factor(DD_log_norare$habitat_type)

full_cons_richness_norare <- lmer(cons_comsta ~ prod_richness + cons_richness + tmp + sd_temp + 
                                    (1|habitat_type), data = DD_log_norare)
summary(full_cons_richness_norare)
anova(full_cons_richness_norare)

full_prod_richness_norare <- lmer(prod_comsta ~ prod_richness + cons_richness + tmp + sd_temp + 
                                        + (1|habitat_type), data = DD_log_norare)
summary(full_prod_richness_norare)
anova(full_prod_richness_norare)

###################################################################################################
####################################### Sensitivity analysis for year length
##### For a minimum of 5 years
DD_mim5 <- DD_log %>%
  filter(y_length > 4) 

### Simpson index
options(na.action = "na.fail")
full_cons_mim5 <- lmer(cons_comsta ~ prod_simpson + cons_simpson + tmp + sd_temp
                  + (1|habitat_type), data = DD_mim5)
summary(full_cons_mim5)
anova(full_cons_mim5)

full_prod_mim5 <- lmer(prod_comsta ~ prod_simpson + cons_simpson + tmp + sd_temp
                  + (1|habitat_type), data = DD_mim5)
summary(full_prod_mim5)
anova(full_prod_mim5)

### Shannon index
full_cons_shannon_mim5 <- lmer(cons_comsta ~ prod_shannon + cons_shannon + tmp + sd_temp
                          + (1|habitat_type), data = DD_mim5)
summary(full_cons_shannon_mim5)
anova(full_cons_shannon_mim5)

full_prod_shannon_mim5 <- lmer(prod_comsta ~ prod_shannon + cons_shannon + tmp + sd_temp
                          + (1|habitat_type), data = DD_mim5)
summary(full_prod_shannon_mim5)
anova(full_prod_shannon_mim5)

### Richness with rare species
full_cons_richness_mim5 <- lmer(cons_comsta ~ prod_richness + cons_richness + tmp + sd_temp
                           + (1|habitat_type), data = DD_mim5)
summary(full_cons_richness_mim5)
anova(full_cons_richness_mim5)

full_prod_richness_mim5 <- lmer(prod_comsta ~ prod_richness + cons_richness + tmp + sd_temp
                           + (1|habitat_type), data = DD_mim5)
summary(full_prod_richness_mim5)
anova(full_prod_richness_mim5)

### Richness without rare species
DD_mim5_norare <- DD_log_norare %>%
  filter(y_length > 4)

full_cons_richness_mim5_norare <- lmer(cons_comsta ~ prod_richness + cons_richness + tmp + sd_temp
                                       + (1|habitat_type), data = DD_mim5_norare)
summary(full_cons_richness_mim5_norare)
anova(full_cons_richness_mim5_norare)

full_prod_richness_mim5_norare <- lmer(prod_comsta ~ prod_richness + cons_richness + tmp + sd_temp
                                       + (1|habitat_type), data = DD_mim5_norare)
summary(full_prod_richness_mim5_norare)
anova(full_prod_richness_mim5_norare)

###################################################################################################
####################################### Sensitivity analysis for seasonality
### sampling sites covering 4 seasons
DD_seasons <- read.csv("Sampling_Seasons.csv", check.names = F)
DD_log <- left_join(DD_log, DD_seasons, by = "siteID") 

DD_log_4seasons <- DD_log %>%
  filter(season > 3) 

full_cons_4seasons <- lmer(cons_comsta ~ prod_simpson + cons_simpson + tmp + sd_temp
                  + (1|habitat_type), data = DD_log_4seasons)
summary(full_cons_4seasons)
anova(full_cons_4seasons)

full_prod_4seasons <- lmer(prod_comsta ~   cons_simpson + prod_simpson + tmp + sd_temp
                  + (1|habitat_type), data = DD_log_4seasons)
summary(full_prod_4seasons)
anova(full_prod_4seasons)

### sampling sites covering 3 and 4 seasons
DD_log_mim3seasons <- DD_log %>%
  filter(season > 2) 

full_cons_mim3seasons <- lmer(cons_comsta ~ prod_simpson + cons_simpson + tmp + sd_temp
                           + (1|habitat_type), data = DD_log_mim3seasons)
summary(full_cons_mim3seasons)
anova(full_cons_mim3seasons)

full_prod_mim3seasons <- lmer(prod_comsta ~   cons_simpson + prod_simpson + tmp + sd_temp
                           + (1|habitat_type), data = DD_log_mim3seasons)
summary(full_prod_mim3seasons)
anova(full_prod_mim3seasons)

### sampling sites covering 2, 3 and 4 seasons
DD_log_mim2seasons <- DD_log %>%
  filter(season > 1) 

full_cons_mim2seasons <- lmer(cons_comsta ~ prod_simpson + cons_simpson + tmp + sd_temp
                              + (1|habitat_type), data = DD_log_mim2seasons)
summary(full_cons_mim2seasons)
anova(full_cons_mim2seasons)

full_prod_mim2seasons <- lmer(prod_comsta ~   cons_simpson + prod_simpson + tmp + sd_temp
                              + (1|habitat_type), data = DD_log_mim2seasons)
summary(full_prod_mim2seasons)
anova(full_prod_mim2seasons)

###################################################################################################
####################################### Sensitivity analysis for spatial autocorrelation
DD_location <- read.csv("Sampling_locations.csv", check.names = F)
DD_log_spatial <- left_join(DD_log, DD_location, by = "siteID" )

library(mgcv)

gamm_prod <- gamm(prod_comsta ~ prod_simpson + cons_simpson + tmp + sd_temp + 
                                  s(Latitude, Longitude, bs = "tp"),
                                random = list(habitat_type = ~ 1),
                                data = DD_log_spatial,
                                method = "ML")
summary(gamm_prod$gam)
anova(gamm_prod$gam)

gamm_cons <- gamm(cons_comsta ~ prod_simpson + cons_simpson + tmp + sd_temp + 
                                  s(Latitude, Longitude, bs = "tp"),
                                random = list(habitat_type = ~ 1),
                                data = DD_log_spatial,
                                method = "ML")
summary(gamm_cons$gam)
anova(gamm_cons$gam)
