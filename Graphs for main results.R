###########################
rm(list = ls())

library(ggplot2)
library(patchwork)
library(cowplot)
library(r2glmm)
library(ppcor)

DD_results <- read.csv("Results_Genus(remove2023Tai)@3years_PredatorRemoval.csv", check.names = F) 

###Preserve my theme
my_theme <- theme_bw() +
  theme(panel.grid=element_blank(), 
        legend.position = "none",
        axis.text = element_text(size = 20),
        axis.title.x = element_text(size = 25), 
        axis.title.y = element_text(size = 25))

my_scales <- list(scale_color_manual(values = c("#4FC3F7","#006064","purple","blue")),
                  scale_fill_manual(values = c("#4FC3F7","#006064","purple","blue")))

###### Community stability
### Averages
DD_results$FunGroup <- factor(DD_results$FunGroup, levels = c("Producer", "Consumer"))
TL_comsta <- ggplot() +
   stat_summary(data = DD_results,
               mapping = aes(x = FunGroup, y = log(com_sta), color = FunGroup),
               fun.data = function(x) {
                 se <- sd(x)/sqrt(length(x))
                 return(data.frame(
                   y = mean(x),
                   ymin = mean(x) - 2*se,
                   ymax = mean(x) + 2*se
                 ))
               },
               geom = "errorbar",
               width = 0.08,
               size = 1.5) +
  stat_summary(data = DD_results,
               mapping = aes(x = FunGroup, y = log(com_sta), color = FunGroup),
               fun = "mean",
               geom = "point",
               size = 10) +
  labs(x = "Trophic level", y = "Ln (Community stability)", TL = "Trophic level") + 
  scale_color_manual(values = c("#228b22", "darkorange")) +  
  scale_fill_manual(values = c("#228b22", "darkorange")) +  
  scale_x_discrete(labels = c("Coms" = "Consumers", "Prod" = "Producers")) +
  scale_y_continuous(breaks = seq(-0.2, 0.8, 0.2), labels = scales::number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(-0.2, 0.8), clip = "off")+
  my_theme +
  geom_segment(aes(x = 1, xend = 2, y = 0.63, yend = 0.63), linewidth = 0.7, color = "black") +
  geom_segment(aes(x = 1, xend = 1, y = 0.63, yend = 0.61), linewidth = 0.7, color = "black") +
  geom_segment(aes(x = 2, xend = 2, y = 0.63, yend = 0.61), linewidth = 0.7, color = "black") +
  geom_text(aes(x = 1.5, y = 0.69), label = "Mean~Difference == 0.509^'***'", parse = TRUE, size = 7) 

TL_comsta <- ggdraw(TL_comsta) + 
  draw_label("a",  x = 0, y = 1, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
TL_comsta

##Raw data points
TL_comsta_point <- ggplot() +
  geom_line(data = DD_results, mapping = aes(x = FunGroup, y = log(com_sta), group = siteID),
            linewidth =1, color = "grey", position = position_dodge(0.15), alpha = 0.3) +
  geom_violin(data = DD_results, 
              mapping = aes(x = FunGroup, y = log(com_sta), color = FunGroup),
              alpha = 0.1, size = 1) +
  geom_point(data = DD_results, 
             mapping = aes(x = FunGroup, y = log(com_sta),shape = FunGroup, color = FunGroup,group = siteID),
             size = 2, alpha = 1, position = position_dodge(0.15), stroke = 1.5) +
  geom_violin(data = DD_results, 
              mapping = aes(x = FunGroup, y = log(com_sta), color = FunGroup),
              alpha = 0,  
              fill = NA,  
              size = 1) +
  
  labs(x = "Trophic level", y = "Ln (Community stability)", TL = "Trophic level") + 
  scale_color_manual(values = c("#228b22", "darkorange")) +  
  scale_fill_manual(values = c("#228b22", "darkorange")) +  
  scale_shape_manual(values = c(1, 1)) +
  scale_x_discrete(labels = c("Coms" = "Consumers", "Prod" = "Producers")) +
  scale_y_continuous(limits = c(-1.5, 2.5), breaks = seq(-1.5,2.5,1), labels = scales::number_format(accuracy = 0.1)) + 
  my_theme +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)) 
TL_comsta_point

P_comsta <- ggdraw(TL_comsta) +
  draw_plot(TL_comsta_point, 
            x = 0.55,  
            y = 0.1,  
            width = 0.45,  
            height = 0.45,
            scale = 0.9)
P_comsta

### Species stability
TL_spesta <- ggplot() +
  stat_summary(data = DD_results,
               mapping = aes(x = FunGroup, y = log(spe_sta), color = FunGroup),
               fun.data = function(x) {
                 se <- sd(x)/sqrt(length(x))
                 return(data.frame(
                   y = mean(x),
                   ymin = mean(x) - 2*se,
                   ymax = mean(x) + 2*se
                 ))
               },
               geom = "errorbar",
               width = 0.08,
               size = 1.5) +
  stat_summary(data = DD_results,
               mapping = aes(x = FunGroup, y = log(spe_sta), color = FunGroup),
               fun = "mean",
               geom = "point",
               size = 10) +
  labs(x = "Trophic level", y = "Ln (Population stability)", TL = "Trophic level") +
  scale_color_manual(values = c("#228b22", "darkorange")) +  
  scale_fill_manual(values = c("#228b22", "darkorange")) +  
  scale_x_discrete(labels = c("Coms" = "Consumers", "Prod" = "Producers")) +
  scale_y_continuous(breaks = seq(-0.8, 0.2, 0.2), labels = scales::number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(-0.8, 0.2), clip = "off")+
  my_theme +
  geom_segment(aes(x = 1, xend = 2, y = 0.05, yend = 0.05), linewidth = 0.7, color = "black") +
  geom_segment(aes(x = 1, xend = 1, y = 0.05, yend = 0.03), linewidth = 0.7, color = "black") +
  geom_segment(aes(x = 2, xend = 2, y = 0.05, yend = 0.03), linewidth = 0.7, color = "black") +
  geom_text(aes(x = 1.5, y = 0.1), label = "Mean~Difference == 0.401^'***'", parse = TRUE, size = 7) 
TL_spesta <- ggdraw(TL_spesta) + 
  draw_label("b",  x = 0, y = 1, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
TL_spesta

TL_spesta_point <- ggplot() +
  geom_line(data = DD_results, mapping = aes(x = FunGroup, y = log(spe_sta), group = siteID),
            linewidth =1, color = "grey", position = position_dodge(0.15), alpha = 0.3) +
  geom_violin(data = DD_results, 
              mapping = aes(x = FunGroup, y = log(spe_sta), color = FunGroup),
              alpha = 0.1, size = 1) +
  geom_point(data = DD_results, 
             mapping = aes(x = FunGroup, y = log(spe_sta),shape = FunGroup, color = FunGroup,group = siteID),
             size = 2, alpha = 1, position = position_dodge(0.15), stroke = 1.5) +
  geom_violin(data = DD_results, 
              mapping = aes(x = FunGroup, y = log(spe_sta), color = FunGroup),
              alpha = 0,  
              fill = NA,  
              size = 1) +
  
  labs(x = "Trophic level", y = "Ln (Population stability)", TL = "Trophic level") + 
  scale_color_manual(values = c("#228b22", "darkorange")) +  
  scale_fill_manual(values = c("#228b22", "darkorange")) +  
  scale_shape_manual(values = c(1, 1)) +
  scale_x_discrete(labels = c("Coms" = "Consumers", "Prod" = "Producers")) +
  scale_y_continuous(limits = c(-1.5, 1.5), breaks = seq(-1.5,1.5,1), labels = scales::number_format(accuracy = 0.1)) +
  my_theme +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)) 
TL_spesta_point  

P_spesta <- ggdraw(TL_spesta) +
  draw_plot(TL_spesta_point, 
            x = 0.55,  
            y = 0.1,  
            width = 0.45,  
            height = 0.45,
            scale = 0.9)
P_spesta

### Species asynchrony
TL_spephi <- ggplot() +
  stat_summary(data = DD_results,
               mapping = aes(x = FunGroup, y = log(spe_phi), color = FunGroup),
               fun.data = function(x) {
                 se <- sd(x)/sqrt(length(x))
                 return(data.frame(
                   y = mean(x),
                   ymin = mean(x) - 2*se,
                   ymax = mean(x) + 2*se
                 ))
               },
               geom = "errorbar",
               width = 0.08,
               size = 1.5) +
  stat_summary(data = DD_results,
               mapping = aes(x = FunGroup, y = log(spe_phi), color = FunGroup),
               fun = "mean",
               geom = "point",
               size = 10) +
  labs(x = "Trophic level", y = "Ln(Population asynchrony)", TL = "Trophic level") +  
  scale_color_manual(values = c("#228b22", "darkorange")) +
  scale_fill_manual(values = c("#228b22", "darkorange")) +
  scale_x_discrete(labels = c("Coms" = "Consumers", "Prod" = "Producers")) +
  scale_y_continuous(breaks = seq(0.2, 0.7, 0.1), labels = scales::number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(0.2, 0.7), clip = "off") +
  my_theme +
  geom_segment(aes(x = 1, xend = 2, y = 0.63, yend = 0.63), linewidth = 0.7, color = "black") +
  geom_segment(aes(x = 1, xend = 1, y = 0.63, yend = 0.62), linewidth = 0.7, color = "black") +
  geom_segment(aes(x = 2, xend = 2, y = 0.63, yend = 0.62), linewidth = 0.7, color = "black") +
  geom_text(aes(x = 1.5, y = 0.65), label = "Mean~Difference == 0.108^'*'", parse = TRUE, size = 7)
TL_spephi <- ggdraw(TL_spephi) +
  draw_label("c",  x = 0, y = 1, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
TL_spephi

TL_spephi_point <- ggplot() +
  geom_line(data = DD_results, mapping = aes(x = FunGroup, y = log(spe_phi), group = siteID),
            linewidth =1, color = "grey", position = position_dodge(0.15), alpha = 0.3) +
  geom_violin(data = DD_results, 
              mapping = aes(x = FunGroup, y = log(spe_phi), color = FunGroup),
              alpha = 0.1, size = 1) +
  geom_point(data = DD_results, 
             mapping = aes(x = FunGroup, y = log(spe_phi),shape = FunGroup, color = FunGroup,group = siteID),
             size = 2, alpha = 1, position = position_dodge(0.15), stroke = 1.5) +
  geom_violin(data = DD_results, 
              mapping = aes(x = FunGroup, y = log(spe_phi), color = FunGroup),
              alpha = 0,  
              fill = NA,  
              size = 1) +
  labs(x = "Trophic level", y = "Log10 (Population asynchrony)", TL = "Trophic level") +
  scale_color_manual(values = c("#228b22", "darkorange")) +  
  scale_fill_manual(values = c("#228b22", "darkorange")) +  
  scale_shape_manual(values = c(1, 1)) +
  scale_x_discrete(labels = c("Coms" = "Consumers", "Prod" = "Producers")) +
  scale_y_continuous(limits = c(0, 2), breaks = seq(0,2,0.5), labels = scales::number_format(accuracy = 0.1)) +
  my_theme +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_text(size=10),
    axis.text.x=element_text(size=10))
TL_spephi_point

P_spephi <- ggdraw(TL_spephi) +
  draw_plot(TL_spephi_point, 
            x = 0.55,  
            y = 0.1,  
            width = 0.45,  
            height = 0.45,
            scale = 0.9)
P_spephi

######## Figure 2
Figure_2 <-  P_comsta + P_spesta +  P_spephi
Figure_2

########################################################################################
### For consumer community stability
options(na.action = "na.fail")
full_cons <- lmer(cons_comsta ~  prod_simpson + cons_simpson + sd_temp + tmp
                  + (1|habitat_type), data = DD_log)

summary(full_cons)
anova(full_cons)
r2beta(full_cons, partial = TRUE, method = "nsj")

############# For consumer stability as responses to producer and consumer simpson diversity
### Consumer community stability in response to producer simpson diversity
coef_bottomup <- data.frame(
  cons_simpson = mean(DD_log$cons_simpson),  
  prod_simpson = seq(min(DD_log$prod_simpson), max(DD_log$prod_simpson), length.out = 98),
  sd_temp = mean(DD_log$sd_temp, na.rm = T),
  tmp = mean(DD_log$tmp)
)

X_bottomup <- model.matrix(~ prod_simpson + cons_simpson + sd_temp + tmp, data = coef_bottomup)
prod_bottomup <- predict(full_cons, newdata = coef_bottomup, re.form = NA)

vcov_matrix <- vcov(full_cons, useScale = FALSE)
df <- df.residual(full_cons)
sigma <- sigma(full_cons)
vcov_adjusted <- vcov_matrix * (sigma^2)  
se_bottomup <- sqrt(diag(X_bottomup %*% vcov_adjusted %*% t(X_bottomup)))

t_value <- qt(0.975, df = df.residual(full_cons))
bottomup_int <- data.frame(
  fit = prod_bottomup,
  lwr = prod_bottomup - t_value * se_bottomup,
  upr = prod_bottomup + t_value * se_bottomup
)
result_bottomup <- cbind(coef_bottomup, bottomup_int)

habitat_types <- unique(DD_log$habitat_type)
pred_data_list <- list()

for(habitat in habitat_types) {
  habitat_data <- DD_log[DD_log$habitat_type == habitat,]
  new_data <- data.frame(
    prod_simpson = seq(min(habitat_data$prod_simpson, na.rm = TRUE), 
                       max(habitat_data$prod_simpson, na.rm = TRUE), 
                       length.out = 100),
    cons_simpson = mean(habitat_data$cons_simpson, na.rm = TRUE),
    sd_temp = mean(habitat_data$sd_temp, na.rm = TRUE),
    tmp = mean(habitat_data$tmp, na.rm = TRUE),
    habitat_type = habitat
  )
  new_data$fit <- predict(full_cons, newdata = new_data)
  pred_data_list[[habitat]] <- new_data
}

pred_data_facet <- do.call(rbind, pred_data_list)
pred_data_enhanced <- pred_data_facet

## Draw the graph
DSR_bottomup <- ggplot() +
  geom_point(data = DD_log, mapping = aes(x = prod_simpson, y = (cons_comsta), color = habitat_type), 
             size = 10, alpha = 0.3) +
    geom_line(data = pred_data_enhanced, 
            mapping = aes(x = prod_simpson, y = fit, color = habitat_type), 
            linewidth = 1.0) +
  
  geom_smooth(data= result_bottomup, mapping = aes(x =  prod_simpson, y = fit,color = FunGroup), 
              method = "lm", linetype = "solid", color = "black", linewidth = 2, se = T) +
  geom_ribbon(data= result_bottomup, mapping = aes(x = prod_simpson, ymin = lwr, ymax = upr), alpha = 0.1) +
  labs(x = "Producer diversity", y = "Ln (Consumer community stability)", color = "Habitat type") + 
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  guides(fill = "none") + 
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta), na.rm = TRUE), 
           label = expression(italic("P")~"= 0.010"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta), na.rm = TRUE) - 0.1, 
           label = expression(italic(R)^2~"= 0.070"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  my_scales + 
  my_theme 
DSR_bottomup <- ggdraw(DSR_bottomup) + 
  draw_label("c",  x = 0, y = 1.01, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
DSR_bottomup

### Consumer community stability as responnse to consumer simpson diversity
coef_cons <- data.frame(
  prod_simpson = mean(DD_log$prod_simpson), 
  cons_simpson = seq(min(DD_log$cons_simpson), max(DD_log$cons_simpson), length.out = 98),
  sd_temp = mean(DD_log$sd_temp, na.rm = T),
  tmp = mean(DD_log$tmp)
)

X_cons <- model.matrix(~ cons_simpson + prod_simpson + tmp +  sd_temp , data = coef_cons)
pred_cons <- predict(full_cons, newdata = coef_cons, re.form = NA)

vcov_matrix <- vcov(full_cons, useScale = FALSE)  #
df <- df.residual(full_cons)
sigma <- sigma(full_cons)
vcov_adjusted <- vcov_matrix * (sigma^2)  
se_cons <- sqrt(diag(X_cons %*% vcov_adjusted %*% t(X_cons)))

t_value <- qt(0.975, df = df.residual(full_cons))
cons_int <- data.frame(
  fit = pred_cons,
  lwr = pred_cons - t_value * se_cons,
  upr = pred_cons + t_value * se_cons
)

result_cons <- cbind(coef_cons, cons_int)

habitat_types <- unique(DD_log$habitat_type)
pred_cons_list <- list()

for(habitat in habitat_types) {
    habitat_data <- DD_log[DD_log$habitat_type == habitat,]
  
    habitat_min <- min(habitat_data$cons_simpson, na.rm = TRUE)
    habitat_max <- max(habitat_data$cons_simpson, na.rm = TRUE)
  
    new_data <- data.frame(
    cons_simpson = seq(habitat_min, habitat_max, length.out = 100),
    prod_simpson = mean(habitat_data$prod_simpson, na.rm = TRUE),
    sd_temp = mean(habitat_data$sd_temp, na.rm = TRUE),
    tmp = mean(habitat_data$tmp, na.rm = TRUE),
    habitat_type = habitat
  )

  new_data$fit <- predict(full_cons, newdata = new_data)
  pred_cons_list[[habitat]] <- new_data
}
pred_cons_facet_1 <- do.call(rbind, pred_cons_list)

### Draw the graph
DSR_cons <- ggplot() +
  geom_point(data = DD_log, mapping = aes(x = cons_simpson, y = (cons_comsta),  color = habitat_type), 
             size = 10, alpha = 0.3) +
  geom_line(data = pred_cons_facet_1, 
            mapping = aes(x = cons_simpson, y = fit, color = habitat_type), 
            linewidth = 1.0) +
  geom_smooth(data= result_cons, mapping = aes(x =  cons_simpson, y = fit,color = FunGroup), 
              method = "lm", linetype = "solid", color = "black", linewidth = 2, se = T) +
  geom_ribbon(data= result_cons, mapping = aes(x = cons_simpson, ymin = lwr, ymax = upr),  alpha = 0.1) +
  labs(x = "Consumer diversity", y = "Ln (Consumer community stability)", color = "Trophic level", shape = "Site type") + 
  scale_x_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  guides(fill = "none") + 
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta), na.rm = TRUE), 
           label = expression(italic("P")~"< 0.001"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta), na.rm = TRUE) - 0.1, 
           label = expression(italic(R)^2~"= 0.282"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  my_scales + my_theme +
  coord_cartesian(clip = "off")
DSR_cons <- ggdraw(DSR_cons) + 
  draw_label("a",  x = 0, y = 1.01, hjust = -0.5, vjust = 2, size = 30, fontface = "bold")
DSR_cons

########## Producer community stability as response to consumer and producer diversity
## Producer community stability in response to consumer simpson diversity
options(na.action = "na.fail")
full_prod <- lmer(prod_comsta ~  cons_simpson + prod_simpson+ tmp + sd_temp + 
                    (1|habitat_type), data = DD_log)
summary(full_prod)
anova(full_prod)
r2beta(full_prod, partial = TRUE, method = "nsj")

####### Producer stability as response to consumer diversity 
coef_topdown <- data.frame(
  prod_simpson = mean(DD_log$prod_simpson),  
  cons_simpson = seq(min(DD_log$cons_simpson), max(DD_log$cons_simpson), length.out = 98),
  sd_temp = mean(DD_log$sd_temp, na.rm = T),
  tmp = mean(DD_log$tmp)
)

X_topdown <- model.matrix(~ cons_simpson + prod_simpson  + tmp + sd_temp, data = coef_topdown)
pred_topdown <- predict(full_prod, newdata = coef_topdown, re.form = NA)

vcov_matrix <- vcov(full_prod, useScale = FALSE)  #
df <- df.residual(full_prod)
sigma <- sigma(full_prod)
vcov_adjusted <- vcov_matrix * (sigma^2)  
se_topdown <- sqrt(diag(X_topdown %*% vcov_adjusted %*% t(X_topdown)))

t_value <- qt(0.975, df = df.residual(full_prod))
topdown_int <- data.frame(
  fit = pred_topdown,
  lwr = pred_topdown - t_value * se_topdown,
  upr = pred_topdown + t_value * se_topdown
)

result_topdown <- cbind(coef_topdown, topdown_int)

habitat_types <- unique(DD_log$habitat_type)
pred_topdown_list <- list()
for(habitat in habitat_types) {
  habitat_data <- DD_log[DD_log$habitat_type == habitat,]
  
  habitat_min <- min(habitat_data$cons_simpson, na.rm = TRUE)
  habitat_max <- max(habitat_data$cons_simpson, na.rm = TRUE)
  
  new_data <- data.frame(
    cons_simpson = seq(habitat_min, habitat_max, length.out = 100),
    prod_simpson = mean(habitat_data$prod_simpson, na.rm = TRUE),
    sd_temp = mean(habitat_data$sd_temp, na.rm = TRUE),
    tmp = mean(habitat_data$tmp, na.rm = TRUE),
    habitat_type = habitat
  )
  
  new_data$fit <- predict(full_prod, newdata = new_data)
  pred_topdown_list[[habitat]] <- new_data
}
pred_topdown_facet <- do.call(rbind, pred_topdown_list)

### Draw the graph
DSR_topdown <- ggplot() +
  geom_point(data = DD_log, mapping = aes(x = cons_simpson, y = (prod_comsta),  color = habitat_type), 
             size = 10, alpha = 0.3) +
  geom_line(data = pred_topdown_facet, 
            mapping = aes(x = cons_simpson, y = fit, color = habitat_type), 
            linewidth = 1.0) +
  geom_smooth(data= result_topdown, mapping = aes(x =  cons_simpson, y = fit,color = FunGroup), 
              method = "lm", linetype = "solid", color = "black", linewidth = 2, se = T) +
  geom_ribbon(data= result_topdown, mapping = aes(x = cons_simpson, ymin = lwr, ymax = upr), alpha = 0.1) +
  labs(x = "Comsumer diversity", y = "Ln (Producer community stability)", color = "Habitat type") + 
  scale_x_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  guides(fill = "none") + 
  annotate("text", x = 0.2, y = max(log(DD_producer$com_sta)-0.1, na.rm = TRUE), 
           label = expression(italic("P")~"= 0.008"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  annotate("text", x = 0.2, y = max(log(DD_producer$com_sta)-0.1, na.rm = TRUE) - 0.1, 
           label = expression(italic(R)^2~"= 0.067"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  my_scales + my_theme

DSR_topdown <- ggdraw(DSR_topdown) + 
  draw_label("d",  x = 0, y = 1, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
DSR_topdown

########## Producer community stability as response to producer simpson diversity
coef_prod <- data.frame(
  cons_simpson = mean(DD_log$cons_simpson), 
  prod_simpson = seq(min(DD_log$prod_simpson), max(DD_log$prod_simpson), length.out = 98),
  sd_temp = mean(DD_log$sd_temp, na.rm = T),
  tmp = mean(DD_log$tmp)
)
X_prod <- model.matrix(~ cons_simpson + prod_simpson + tmp + sd_temp, data = coef_prod)
pred_prod <- predict(full_prod, newdata = coef_prod, re.form = NA)
se_prod <- sqrt(diag(X_prod %*% vcov(full_prod, useScale = FALSE) %*% t(X_prod)))

t_value <- qt(0.975, df = df.residual(full_prod))
prod_int <- data.frame(
  fit = pred_prod,
  lwr = pred_prod - t_value * se_prod,
  upr = pred_prod + t_value * se_prod
)
result_prod <- cbind(coef_prod, prod_int)
############################################################
habitat_types <- unique(DD_log$habitat_type)
pred_prod_list <- list()
for(habitat in habitat_types) {
    habitat_data <- DD_log[DD_log$habitat_type == habitat,]
    new_data <- data.frame(
    prod_simpson = seq(min(habitat_data$prod_simpson, na.rm = TRUE), 
                       max(habitat_data$prod_simpson, na.rm = TRUE), 
                       length.out = 100),
    cons_simpson = mean(habitat_data$cons_simpson, na.rm = TRUE),
    sd_temp = mean(habitat_data$sd_temp, na.rm = TRUE),
    tmp = mean(habitat_data$tmp, na.rm = TRUE),
    habitat_type = habitat
  )
  new_data$fit <- predict(full_prod, newdata = new_data)
  pred_prod_list[[habitat]] <- new_data
}
pred_prod_facet_1 <- do.call(rbind, pred_prod_list)

### Draw the graph
DSR_prod <- ggplot() +
  geom_point(data = DD_log, mapping = aes(x = prod_simpson, y = (prod_comsta),  color = habitat_type), 
             size = 10, alpha = 0.3) +
  geom_line(data = pred_prod_facet_1, 
            mapping = aes(x = prod_simpson, y = fit, color = habitat_type), 
            linewidth = 1.0) +
  labs(x = "Producer diversity", y = "Ln (Producer community stability)", color = "Habitat type") + 
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 1)) +
  guides(fill = "none") + 
  my_scales + 
  my_theme
DSR_prod <- ggdraw(DSR_prod) + 
  draw_label("b",  x = 0, y = 1, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
DSR_prod

### Combine the graphs
DSR_graph <- (DSR_cons + DSR_bottomup)/(DSR_prod + DSR_topdown)
DSR_graph

### Add legend
legend <- get_legend(
  ggplot() +
    geom_point(data = DD_log, 
               mapping = aes(x = prod_simpson, y = (prod_comsta), color = habitat_type),
               size = 4) +
    scale_color_manual(
      breaks = c("Lentic", "Lotic", "Estuary", "Marine"),
      labels = c(
        "Lentic" = "Lentic freshwaters",
        "Lotic" = "Lotic freshwaters",
        "Estuary" = "Estuary",
        "Marine" = "Marine"
      ),
      values = c("Lentic" = "#006064", 
                 "Lotic" = "purple",
                 "Estuary" = "#4FC3F7",
                 "Marine" = "blue")  
    ) +
    theme(legend.position = "right",
          legend.justification = "center",
          legend.box.just = "center",
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(0, 0, 0, 0),
          legend.background = element_blank(),
          legend.title = element_text(
            size = 25,
            face = "plain"),
          legend.text = element_text(
            size = 20)
    ) +
    labs(color = "Habitat type")
)

#### Figure 3
Figure_3 <- cowplot::plot_grid(
  DSR_graph, legend, 
  ncol = 2, 
  rel_widths = c(1, .2),
  align = 'h',
  axis = 'l'
)
Figure_3
