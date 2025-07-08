###########################
rm(list = ls())

library(tidyverse)
library(lme4)
library(lmerTest)
library(car)

################### Sentivity analysis taking habitat type as fixed factors
setwd("C:/周礼斌/Data Collection/Data preserve/River for 2km/Manipulated_Data")
DD_log <- read.csv("DD_log.csv", check.names = F) 
DD_rareRemove <- read.csv("DD_log_rareRemove0707.csv", check.names = F)

### Multiple regression with habitat type as fixed factors
full_cons_multi_var <- lm(cons_comsta ~  habitat_type*prod_simpson + habitat_type*cons_simpson + sd_temp.y + tmp
                       , data = DD_log)
Anova(full_cons_multi_var, type = "III")
summary(full_cons_multi_var)

full_prod_multi_var <- lm(prod_comsta ~  habitat_type*prod_simpson + habitat_type*cons_simpson + sd_temp.y + tmp
                       , data = DD_log)
Anova(full_prod_multi_var, type = "III")
summary(full_prod_multi_var)

################ Sensitivity analysis for different diversity indices
### For consumer community stability
## shannon index
full_cons_shannon <- lmer(cons_comsta ~ prod_shannon + cons_shannon + tmp + sd_temp.y + 
                    + (1|habitat_type), data = DD_log)
summary(full_cons_shannon)
anova(full_cons_shannon)

## richness with rare species
full_cons_richness <- lmer(cons_comsta ~  log(prod_richness) + log(cons_richness) + tmp + sd_temp.y + 
                    + (1|habitat_type), data = DD_log)
summary(full_cons_richness)
anova(full_cons_richness)
### richness without rare species
full_cons_richness_rareRemove <- lm(cons_comsta ~ habitat_type*log(prod_richness) + habitat_type*log(cons_richness) + tmp + sd_temp.y 
                   , data = DD_log_rareRemove)
summary(full_cons_richness_rareRemove)
anova(full_cons_richness_rareRemove)

### For producer community stability
## shannon index
full_prod <- lmer(prod_comsta ~ prod_shannon + cons_shannon + tmp + sd_temp.y + 
                    + (1|habitat_type), data = DD_log)
summary(full_prod)
anova(full_prod)
## richness
full_prod_richness <- lmer(prod_comsta ~  log(prod_richness) + log(cons_richness) + tmp + sd_temp.y + 
                    + (1|habitat_type), data = DD_log)
summary(full_prod_richness)
anova(full_prod_richness)
## richness without rare species
full_prod_richness_rareRemove <- lmer(prod_comsta ~ log(prod_richness) + log(cons_richness) + tmp + sd_temp.y + 
                    + (1|habitat_type), data = DD_log_rareRemove)
summary(full_prod_richness_rareRemove)
anova(full_prod_richness_rareRemove)

####################### Sensitivity analysis for year length
DD_5years <- DD_log %>%
  filter(y_length > 4) 
### Simpson index
options(na.action = "na.fail")
full_cons <- lmer(cons_comsta ~ prod_simpson + cons_simpson + tmp + sd_temp.y
                  + (1|habitat_type), data = DD_5years)
summary(full_cons)
anova(full_cons)

full_prod <- lmer(prod_comsta ~ prod_simpson + cons_simpson + tmp + sd_temp.x
                  + (1|habitat_type), data = DD_5years)
summary(full_prod)
anova(full_prod)

### Shannon index
options(na.action = "na.fail")
full_cons_shannon <- lmer(cons_comsta ~ prod_shannon + cons_shannon + tmp + sd_temp.y
                          + (1|habitat_type), data = DD_5years)
summary(full_cons_shannon)
anova(full_cons_shannon)

full_prod_shannon <- lmer(prod_comsta ~ prod_shannon + cons_shannon + tmp + sd_temp.x
                          + (1|habitat_type), data = DD_5years)
summary(full_prod_shannon)
anova(full_prod_shannon)

### Richness with rare species
options(na.action = "na.fail")
full_cons_richness <- lmer(cons_comsta ~ log(prod_richness) + log(cons_richness) + tmp + sd_temp.y
                           + (1|habitat_type), data = DD_5years)
summary(full_cons_richness)
anova(full_cons_richness)
full_prod_richness <- lmer(prod_comsta ~ log(prod_richness) + log(cons_richness) + tmp + sd_temp.x
                           + (1|habitat_type), data = DD_5years)
summary(full_prod_richness)
anova(full_prod_richness)
### Richness without rare species
DD_5years_rareRemove <- DD_rareRemove %>%
  filter(y_length > 4)

full_cons_richness_rareRemove <- lmer(cons_comsta ~ log(prod_richness) + log(cons_richness) + tmp + sd_temp.y
                                       + (1|habitat_type), data = DD_5years_rareRemove)
summary(full_cons_richness_rareRemove)
anova(full_cons_richness_rareRemove)

full_prod_richness_rareRemove <- lmer(prod_comsta ~ log(prod_richness) + log(cons_richness) + tmp + sd_temp.x
                                       + (1|habitat_type), data = DD_5years_rareRemove)
summary(full_prod_richness_rareRemove)
anova(full_prod_richness_rareRemove)
