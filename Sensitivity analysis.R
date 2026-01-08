###########################
rm(list = ls())

library(tidyverse)
library(lme4)
library(lmerTest)
library(car)

################### Sentivity analysis taking habitat type as fixed factors
### Multiple regression with habitat type as fixed factors
full_cons_multivar <- lm(cons_comsta ~  habitat_type*prod_richness + habitat_type*cons_richness + sd_temp + tmp
                         , data = DD_log)
anova(full_cons_multivar)
summary(full_cons_multivar)

full_prod_multivar <- lm(prod_comsta ~  habitat_type*prod_richness + habitat_type*cons_richness+ sd_temp + tmp
                         , data = DD_log)
anova(full_prod_multivar)
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

## Simpson index
full_cons_simpson <- lmer(cons_comsta ~ prod_simpson+ cons_simpson + tmp + sd_temp + 
                            + (1|habitat_type), data = DD_log)
summary(full_cons_simpson)
anova(full_cons_simpson)

full_prod_simpson <- lmer(prod_comsta ~ prod_simpson + cons_simpson + tmp + sd_temp + 
                            + (1|habitat_type), data = DD_log)
summary(full_prod_simpson)
anova(full_prod_simpson)
########## From the BOTTOM-UP Perspective: how producer diversity affect consumer stability?
###################################################################################################
####################################### Sensitivity analysis for year length
##### For a minimum of 5 years
DD_mim5 <- DD_log %>%
  filter(y_length > 4) 

### Simpson index
options(na.action = "na.fail")
full_cons_mim5 <- lmer(cons_comsta ~ prod_richness + cons_richness + tmp + sd_temp
                       + (1|habitat_type), data = DD_mim5)
summary(full_cons_mim5)
anova(full_cons_mim5)

full_prod_mim5 <- lmer(prod_comsta ~ prod_richness + cons_richness + tmp + sd_temp
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
full_cons_simpson_mim5 <- lmer(cons_comsta ~ prod_simpson + cons_simpson + tmp + sd_temp
                                + (1|habitat_type), data = DD_mim5)
summary(full_cons_simpson_mim5)
anova(full_cons_simpson_mim5)

full_prod_simpson_mim5 <- lmer(prod_comsta ~ prod_simpson + cons_simpson + tmp + sd_temp
                                + (1|habitat_type), data = DD_mim5)
summary(full_prod_simpson_mim5)
anova(full_prod_simpson_mim5)

###################################################################################################
####################################### Sensitivity analysis for seasonality
### sampling sites covering 4 seasons
DD_seasons <- read.csv("Sampling_Seasons.csv", check.names = F)
DD_log <- left_join(DD_log, DD_seasons, by = "siteID") 

DD_log_4seasons <- DD_log %>%
  filter(season > 3) 

full_cons_4seasons <- lmer(cons_comsta ~ prod_richness + cons_richness + tmp + sd_temp
                           + (1|habitat_type), data = DD_log_4seasons)
summary(full_cons_4seasons)
anova(full_cons_4seasons)

full_prod_4seasons <- lmer(prod_comsta ~   prod_richness + cons_richness + tmp + sd_temp
                           + (1|habitat_type), data = DD_log_4seasons)
summary(full_prod_4seasons)
anova(full_prod_4seasons)

### sampling sites covering 3 and 4 seasons
DD_log_mim3seasons <- DD_log %>%
  filter(season > 2) 

full_cons_mim3seasons <- lmer(cons_comsta ~ prod_richness + cons_richness + tmp + sd_temp
                              + (1|habitat_type), data = DD_log_mim3seasons)
summary(full_cons_mim3seasons)
anova(full_cons_mim3seasons)

full_prod_mim3seasons <- lmer(prod_comsta ~ prod_richness + cons_richness + tmp + sd_temp
                              + (1|habitat_type), data = DD_log_mim3seasons)
summary(full_prod_mim3seasons)
anova(full_prod_mim3seasons)

### sampling sites covering 2, 3 and 4 seasons
DD_log_mim2seasons <- DD_log %>%
  filter(season > 1) 

full_cons_mim2seasons <- lmer(cons_comsta ~  prod_richness + cons_richness + tmp + sd_temp
                              + (1|habitat_type), data = DD_log_mim2seasons)
summary(full_cons_mim2seasons)
anova(full_cons_mim2seasons)

full_prod_mim2seasons <- lmer(prod_comsta ~  prod_richness + cons_richness + tmp + sd_temp
                              + (1|habitat_type), data = DD_log_mim2seasons)
summary(full_prod_mim2seasons)
anova(full_prod_mim2seasons)

###################################################################################################
####################################### Sensitivity analysis for spatial autocorrelation
DD_location <- read.csv("Sampling_locations.csv", check.names = F)
DD_log_spatial <- left_join(DD_log, DD_location, by = "siteID" )

library(mgcv)

gamm_prod <- gamm(prod_comsta ~ prod_richness+ cons_richness + tmp + sd_temp + 
                    s(Latitude, Longitude, bs = "tp"),
                  random = list(habitat_type = ~ 1),
                  data = DD_log_spatial,
                  method = "ML")
summary(gamm_prod$gam)
anova(gamm_prod$gam)

gamm_cons <- gamm(cons_comsta ~ prod_richness + cons_richness + tmp + sd_temp + 
                    s(Latitude, Longitude, bs = "tp"),
                  random = list(habitat_type = ~ 1),
                  data = DD_log_spatial,
                  method = "ML")
summary(gamm_cons$gam)
anova(gamm_cons$gam)

