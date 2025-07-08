library(ggplot2)
library(patchwork)
library(cowplot)
library(r2glmm)
library(ppcor)

#########
DD_results <- read.csv("Results_Genus(remove2023Tai)@3years_PredatorRemoval.csv", check.names = F) 
DD_producer <- DD_results[DD_results$FunGroup == "Producer",]
DD_consumer <- DD_results[DD_results$FunGroup == "Consumer",]

DD_log <- read.csv("DD_log.csv", check.names = F) 

########################### Sensitivity analysis for datasets with  minimum of 5 years
DD_log_sensitive <- DD_log %>%
  filter(y_length > 4)

DD_results_sensitive <- DD_results %>%
  filter(y_length > 4)
DD_producer_sensitive <- DD_results_sensitive[DD_results_sensitive$FunGroup == "Producer",]
DD_consumer_sensitive <- DD_results_sensitive[DD_results_sensitive$FunGroup == "Consumer",]

############For bottom-up processes
full_cons_sensitive <- lmer(cons_comsta ~ cons_simpson + prod_simpson + sd_temp.y + tmp + (1|habitat_type), data = DD_log_sensitive)
summary(full_cons_sensitive)
anova(full_cons_sensitive)
r2beta(full_cons_sensitive, partial = TRUE, method = "nsj")

coef_bottomup_sen <- data.frame(
  cons_simpson = mean(DD_log_sensitive$cons_simpson), 
  prod_simpson = seq(min(DD_log_sensitive$prod_simpson), max(DD_log_sensitive$prod_simpson), length.out = 98),
  sd_temp.y = mean(DD_log_sensitive$sd_temp.y, na.rm = T),
  tmp = mean(DD_log_sensitive$tmp)
)

X_bottomup_sen <- model.matrix(~  cons_simpson + prod_simpson + sd_temp.y + tmp, data = coef_bottomup_sen)
prod_bottomup_sen <- predict(full_cons_sensitive, newdata = coef_bottomup_sen, re.form = NA)

vcov_matrix <- vcov(full_cons_sensitive, useScale = FALSE)  #
df <- df.residual(full_cons_sensitive)
sigma <- sigma(full_cons_sensitive)
vcov_adjusted <- vcov_matrix * (sigma^2)  
se_bottomup_sen <- sqrt(diag(X_bottomup_sen %*% vcov_adjusted %*% t(X_bottomup_sen)))

t_value_sen <- qt(0.975, df = df.residual(full_cons_sensitive))
bottomup_int_sen <- data.frame(
  fit = prod_bottomup_sen,
  lwr = prod_bottomup_sen - t_value_sen * se_bottomup_sen,
  upr = prod_bottomup_sen + t_value_sen * se_bottomup_sen
)
result_bottomup_sen <- cbind(coef_bottomup_sen, bottomup_int_sen)

habitat_types <- unique(DD_log_sensitive$habitat_type)
pred_bottomup_list_sen <- list()
for(habitat in habitat_types) {
  habitat_data <- DD_log_sensitive[DD_log_sensitive$habitat_type == habitat,]
  new_data <- data.frame(
    prod_simpson = seq(min(habitat_data$prod_simpson, na.rm = TRUE), 
                       max(habitat_data$prod_simpson, na.rm = TRUE), 
                       length.out = 100),
    cons_simpson = mean(habitat_data$cons_simpson, na.rm = TRUE),
    sd_temp.y = mean(habitat_data$sd_temp.y, na.rm = TRUE),
    tmp = mean(habitat_data$tmp, na.rm = TRUE),
    habitat_type = habitat
  )
  new_data$fit <- predict(full_cons_sensitive, newdata = new_data)
  pred_bottomup_list_sen[[habitat]] <- new_data
}
pred_bottomup_facet_sen <- do.call(rbind, pred_bottomup_list_sen)

### Draw the graph
DSR_bottomup_sen <- ggplot() +
  geom_point(data = DD_log_sensitive, mapping = aes(x = prod_simpson, y = (cons_comsta), color = habitat_type), 
             size = 10, alpha = 0.3) +
  geom_line(data = pred_bottomup_facet_sen, 
            mapping = aes(x = prod_simpson, y = fit, color = habitat_type), 
            linewidth = 1.0) +
  geom_smooth(data= result_bottomup_sen, mapping = aes(x = prod_simpson, y = fit,color = FunGroup), 
              method = "lm", linetype = "solid", color = "black", linewidth = 2, se = T) +
  geom_ribbon(data= result_bottomup_sen, mapping = aes(x = prod_simpson, ymin = lwr, ymax = upr), alpha = 0.1) +
  labs(x = "Producer diversity", y = "Ln (Consumer community stability)", color = "Habitat type") + 
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  guides(fill = "none") + 
  annotate("text", x = 0.2, y = max(log(DD_consumer_sensitive$com_sta), na.rm = TRUE), 
           label = expression(italic("P")~"= 0.038"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  annotate("text", x = 0.2, y = max(log(DD_consumer_sensitive$com_sta), na.rm = TRUE) - 0.1, 
           label = expression(italic(R)^2~"= 0.053"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  my_scales + 
  my_theme 
DSR_bottomup_sen <- ggdraw(DSR_bottomup_sen) + 
  draw_label("c",  x = 0, y = 1.01, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
DSR_bottomup_sen

################################## For consumer stability vs. consumer diversity 
coef_cons_sen <- data.frame(
  prod_simpson = mean(DD_log_sensitive$prod_simpson), 
  cons_simpson = seq(min(DD_log_sensitive$cons_simpson), max(DD_log_sensitive$cons_simpson), length.out = 98),
  sd_temp.y = mean(DD_log_sensitive$sd_temp.y, na.rm = T),
  tmp = mean(DD_log_sensitive$tmp)
)
X_cons_sen <- model.matrix(~ cons_simpson + prod_simpson + tmp +  sd_temp.y, data = coef_bottomup_sen)
pred_cons_sen <- predict(full_cons_sensitive, newdata = coef_cons_sen, re.form = NA)

vcov_matrix <- vcov(full_cons_sensitive, useScale = FALSE)
df <- df.residual(full_cons_sensitive)
sigma <- sigma(full_cons_sensitive)
vcov_adjusted <- vcov_matrix * (sigma^2)  
se_cons_sen <- sqrt(diag(X_cons_sen %*% vcov_adjusted %*% t(X_cons_sen)))

t_value_sen <- qt(0.975, df = df.residual(full_cons_sensitive))
cons_int_sen <- data.frame(
  fit = pred_cons_sen,
  lwr = pred_cons_sen - t_value_sen * se_cons_sen,
  upr = pred_cons_sen + t_value_sen * se_cons_sen
)
result_cons_sen <- cbind(coef_cons_sen, cons_int_sen)

habitat_types <- unique(DD_log_sensitive$habitat_type)
pred_cons_sen_list <- list()
for(habitat in habitat_types) {
  habitat_data <- DD_log_sensitive[DD_log_sensitive$habitat_type == habitat,]
  new_data <- data.frame(
    cons_simpson = seq(min(habitat_data$cons_simpson, na.rm = TRUE), 
                       max(habitat_data$cons_simpson, na.rm = TRUE), 
                       length.out = 100),
    prod_simpson = mean(habitat_data$prod_simpson, na.rm = TRUE),
    sd_temp.y = mean(habitat_data$sd_temp.y, na.rm = TRUE),
    tmp = mean(habitat_data$tmp, na.rm = TRUE),
    habitat_type = habitat
  )
  new_data$fit <- predict(full_cons_sensitive, newdata = new_data)
  pred_cons_sen_list[[habitat]] <- new_data
}
pred_cons_sen_facet_1 <- do.call(rbind, pred_cons_sen_list)

##Draw the graph
DSR_cons_sen <- ggplot() +
  geom_point(data = DD_log_sensitive, mapping = aes(x = cons_simpson, y = (cons_comsta),  color = habitat_type), 
             size = 10, alpha = 0.3) +
  geom_line(data = pred_cons_sen_facet_1, 
            mapping = aes(x = cons_simpson, y = fit, color = habitat_type), 
            linewidth = 1.0) +
  geom_smooth(data= result_cons_sen, mapping = aes(x =  cons_simpson, y = fit,color = FunGroup), 
              method = "lm", linetype = "solid", color = "black", linewidth = 1.5, se = T) +
  geom_ribbon(data= result_cons_sen, mapping = aes(x = cons_simpson, ymin = lwr, ymax = upr),  alpha = 0.1) +
  labs(x = "Consumer diversity", y = "Ln (Consumer community stability)", color = "Trophic level", shape = "Site type") + 
  scale_x_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  guides(fill = "none") + 
  annotate("text", x = 0.2, y = max(log(DD_consumer_sensitive$com_sta), na.rm = TRUE), 
           label = expression(italic("P")~"< 0.001"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  annotate("text", x = 0.2, y = max(log(DD_consumer_sensitive$com_sta), na.rm = TRUE) - 0.1, 
           label = expression(italic(R)^2~"= 0.272"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  my_scales + 
  my_theme +
  coord_cartesian(clip = "off")
DSR_cons_sen <- ggdraw(DSR_cons_sen) + 
  draw_label("a",  x = 0, y = 1.01, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
DSR_cons_sen

#### Producer stability as responses to consumer and producer diversity
options(na.action = "na.fail")
full_prod_sensitive <- lmer(prod_comsta ~  prod_simpson+ cons_simpson + tmp + sd_temp.y + (1|habitat_type), data = DD_log_sensitive)
summary(full_prod_sensitive)
anova(full_prod_sensitive)
r2beta(full_prod_sensitive, partial = TRUE, method = "nsj")

####### car####### For producer stability vs. consuer diversity 
coef_topdown_sen <- data.frame(
  prod_simpson = mean(DD_log_sensitive$prod_simpson),  
  cons_simpson = seq(min(DD_log_sensitive$cons_simpson), max(DD_log_sensitive$cons_simpson), length.out = 98),
  sd_temp.y = mean(DD_log_sensitive$sd_temp.y, na.rm = T),
  tmp = mean(DD_log_sensitive$tmp)
)
X_topdown_sen <- model.matrix(~ cons_simpson + prod_simpson  + tmp + sd_temp.y, data = coef_topdown_sen)
pred_topdown_sen <- predict(full_prod_sensitive, newdata = coef_topdown_sen, re.form = NA)

vcov_matrix <- vcov(full_prod_sensitive, useScale = FALSE)
df <- df.residual(full_prod_sensitive)
sigma <- sigma(full_prod_sensitive)
vcov_adjusted <- vcov_matrix * (sigma^2)  
se_topdown_sen <- sqrt(diag(X_topdown_sen %*% vcov_adjusted %*% t(X_topdown_sen)))

t_value_sen <- qt(0.975, df = df.residual(full_prod_sensitive))
topdown_int_sen <- data.frame(
  fit = pred_topdown_sen,
  lwr = pred_topdown_sen - t_value_sen * se_topdown_sen,
  upr = pred_topdown_sen + t_value_sen * se_topdown_sen
)

result_topdown_sen <- cbind(coef_topdown_sen, topdown_int_sen)

habitat_types <- unique(DD_log_sensitive$habitat_type)
pred_topdown_sen_list <- list()

for(habitat in habitat_types) {
  habitat_data <- DD_log_sensitive[DD_log_sensitive$habitat_type == habitat,]
  new_data <- data.frame(
    cons_simpson = seq(min(habitat_data$cons_simpson, na.rm = TRUE), 
                       max(habitat_data$cons_simpson, na.rm = TRUE), 
                       length.out = 100),
    prod_simpson = mean(habitat_data$prod_simpson, na.rm = TRUE),
    sd_temp.y = mean(habitat_data$sd_temp.y, na.rm = TRUE),
    tmp = mean(habitat_data$tmp, na.rm = TRUE),
    habitat_type = habitat
  )
  new_data$fit <- predict(full_prod_sensitive, newdata = new_data)
  pred_topdown_sen_list[[habitat]] <- new_data
}
pred_topdown_sen_facet_1 <- do.call(rbind, pred_topdown_sen_list)

### Draw the graph
DSR_topdown_sen <- ggplot() +
  geom_point(data = DD_log_sensitive, mapping = aes(x = cons_simpson, y = (prod_comsta),  color = habitat_type), 
             size = 10, alpha = 0.3) +
  geom_line(data = pred_topdown_sen_facet_1, 
            mapping = aes(x = cons_simpson, y = fit, color = habitat_type), 
            linewidth = 1.0) +
  geom_smooth(data= result_topdown_sen, mapping = aes(x =  cons_simpson, y = fit,color = FunGroup), 
              method = "lm", linetype = "solid", color = "black", linewidth = 1.5, se = T) +
  geom_ribbon(data= result_topdown_sen, mapping = aes(x = cons_simpson, ymin = lwr, ymax = upr), alpha = 0.1) +
  labs(x = "Comsumer diversity", y = "Ln (Producer community stability)", color = "Habitat type") + 
  scale_x_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  guides(fill = "none") + 
  annotate("text", x = 0.2, y = max(log(DD_consumer_sensitive$com_sta)-0.1, na.rm = TRUE), 
           label = expression(italic("P")~"= 0.003"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  annotate("text", x = 0.2, y = max(log(DD_consumer_sensitive$com_sta)-0.1, na.rm = TRUE) - 0.1, 
           label = expression(italic(R)^2~"= 0.099"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  my_scales + 
  my_theme
DSR_topdown_sen <- ggdraw(DSR_topdown_sen) + 
  draw_label("d",  x = 0, y = 1, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
DSR_topdown_sen

####### Producer stability as response to producer diversity 
coef_prod <- data.frame(
  cons_simpson = mean(DD_log$cons_simpson), 
  prod_simpson = seq(min(DD_log$prod_simpson), max(DD_log$prod_simpson), length.out = 98),
  sd_temp.y = mean(DD_log$sd_temp.y, na.rm = T),
  tmp = mean(DD_log$tmp)
)
X_prod <- model.matrix(~ cons_simpson + prod_simpson + tmp + sd_temp.y, data = coef_prod)
pred_prod <- predict(full_prod, newdata = coef_prod, re.form = NA)

vcov_matrix <- vcov(full_prod_sensitive, useScale = FALSE)  #
df <- df.residual(full_prod_sensitive)
sigma <- sigma(full_prod_sensitive)
vcov_adjusted <- vcov_matrix * (sigma^2)  
se_prod <- sqrt(diag(X_prod %*% vcov_adjusted %*% t(X_prod)))

t_value <- qt(0.975, df = df.residual(full_prod))
prod_int <- data.frame(
  fit = pred_prod,
  lwr = pred_prod - t_value * se_prod,
  upr = pred_prod + t_value * se_prod
)
result_prod <- cbind(coef_prod, prod_int)
habitat_types <- unique(DD_log_sensitive$habitat_type)
pred_prod_list <- list()

for(habitat in habitat_types) {
  habitat_data <- DD_log_sensitive[DD_log_sensitive$habitat_type == habitat,]
  new_data <- data.frame(
    prod_simpson = seq(min(habitat_data$prod_simpson, na.rm = TRUE), 
                       max(habitat_data$prod_simpson, na.rm = TRUE), 
                       length.out = 100),
    cons_simpson = mean(habitat_data$cons_simpson, na.rm = TRUE),
    sd_temp.y = mean(habitat_data$sd_temp.y, na.rm = TRUE),
    tmp = mean(habitat_data$tmp, na.rm = TRUE),
    habitat_type = habitat
  )
  new_data$fit <- predict(full_prod_sensitive, newdata = new_data)
  pred_prod_list[[habitat]] <- new_data
}
pred_prod_facet_2 <- do.call(rbind, pred_prod_list)

### Draw the graph
DSR_prod_sen <- ggplot() +
  geom_point(data = DD_log_sensitive, mapping = aes(x = prod_simpson, y = (prod_comsta),  color = habitat_type), 
             size = 10, alpha = 0.3) +
  geom_line(data = pred_prod_facet_2, 
            mapping = aes(x = prod_simpson, y = fit, color = habitat_type), 
            linewidth = 1.0) +
  labs(x = "Producer diversity", y = "Ln (Producer community stability)", color = "Habitat type") + 
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, 0.5)) +
  guides(fill = "none") + 
  my_scales + 
  my_theme
DSR_prod_sen <- ggdraw(DSR_prod_sen) + 
  draw_label("b",  x = 0, y = 1, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
DSR_prod_sen

DSR_graph_5years <- (DSR_cons_sen + DSR_bottomup_sen)/(DSR_prod_sen + DSR_topdown_sen)
DSR_graph_5years

legend_sen <- get_legend(
  ggplot() +
    geom_point(data = DD_log_sensitive, 
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
          #legend.key = element_blank(),      ## delete the background of the legend  
          legend.background = element_blank(),
          legend.title = element_text(
            size = 25,
            face = "plain"),
          legend.text = element_text(
            size = 20)
    ) +
    labs(color = "Habitat type")
)

Extended_Figure_2 <- cowplot::plot_grid(
  DSR_graph_5years, legend_sen, 
  ncol = 2, 
  rel_widths = c(1, .2),
  align = 'h',
  axis = 'l'
)
Extended_Figure_2


######### Check the relationship between Simpson and richness (without rare species) and community stability
#### Shannon index
full_cons_shannon<- lmer(cons_comsta ~  prod_shannon + cons_shannon +  sd_temp.y + tmp + (1|habitat_type), data = DD_log)
summary(full_cons_shannon)
anova(full_cons_shannon)
r2beta(full_cons_shannon, partial = TRUE, method = "nsj")

### Consumer stability as responses to producer diversity 
coef_bottomup_shannon <- data.frame(
  cons_shannon = mean(DD_log$cons_shannon), 
  prod_shannon = seq(min(DD_log$prod_shannon), max(DD_log$prod_shannon), length.out = 98),
  sd_temp.y = mean(DD_log$sd_temp.y, na.rm = T),
  tmp = mean(DD_log$tmp)
)
X_bottomup_shannon <- model.matrix(~ prod_shannon + cons_shannon + sd_temp.y + tmp , data = coef_bottomup_shannon)
prod_bottomup_shannon <- predict(full_cons_shannon, newdata = coef_bottomup_shannon, re.form = NA)

vcov_matrix <- vcov(full_cons_shannon, useScale = FALSE)  #
df <- df.residual(full_cons_shannon)
sigma <- sigma(full_cons_shannon)
vcov_adjusted <- vcov_matrix * (sigma^2)  
se_bottomup_shannon <- sqrt(diag(X_bottomup_shannon %*% vcov_adjusted %*% t(X_bottomup_shannon)))

t_value_shannon <- qt(0.975, df = df.residual(full_cons_shannon))
bottomup_int_shannon <- data.frame(
  fit = prod_bottomup_shannon,
  lwr = prod_bottomup_shannon - t_value_shannon * se_bottomup_shannon,
  upr = prod_bottomup_shannon + t_value_shannon * se_bottomup_shannon
)
result_bottomup_shannon <- cbind(coef_bottomup_shannon, bottomup_int_shannon)

habitat_types <- unique(DD_log$habitat_type)
pred_bottomup_list_shannon <- list()
for(habitat in habitat_types) {
  habitat_data <- DD_log[DD_log$habitat_type == habitat,]
  new_data <- data.frame(
    prod_shannon = seq(min(habitat_data$prod_shannon, na.rm = TRUE), 
                       max(habitat_data$prod_shannon, na.rm = TRUE), 
                       length.out = 100),
    cons_shannon = mean(habitat_data$cons_shannon, na.rm = TRUE),
    sd_temp.y = mean(habitat_data$sd_temp.y, na.rm = TRUE),
    tmp = mean(habitat_data$tmp, na.rm = TRUE),
    habitat_type = habitat
  )
  
  new_data$fit <- predict(full_cons_shannon, newdata = new_data)
  pred_bottomup_list_shannon[[habitat]] <- new_data
}
pred_bottomup_facet_shannon <- do.call(rbind, pred_bottomup_list_shannon)

### Draw the graph
DSR_bottomup_shannon <- ggplot() +
  geom_point(data = DD_log, mapping = aes(x = prod_shannon, y = (cons_comsta), color = habitat_type), 
             size = 10, alpha = 0.3) +
  geom_line(data = pred_bottomup_facet_shannon, 
            mapping = aes(x = prod_shannon, y = fit, color = habitat_type), 
            linewidth = 1.0) +
  geom_smooth(data= result_bottomup_shannon, mapping = aes(x =  prod_shannon, y = fit,color = FunGroup), 
              method = "lm", linetype = "solid", color = "black", linewidth = 2, se = T) +
  geom_ribbon(data= result_bottomup_shannon, mapping = aes(x = prod_shannon, ymin = lwr, ymax = upr), alpha = 0.1) +
  labs(x = "Producer diversity", y = "Ln (Consumer community stability)", color = "Habitat type") + 
  guides(fill = "none") + 
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta)-0.1, na.rm = TRUE), 
           label = expression(italic("P")~"= 0.092"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta)-0.1, na.rm = TRUE) - 0.1, 
           label = expression(italic(R)^2~"= 0.029"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  my_scales + my_theme 
DSR_bottomup_shannon <- ggdraw(DSR_bottomup_shannon) + 
  draw_label("c",  x = 0, y = 1.01, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
DSR_bottomup_shannon

###Consumer stability as response to consumer diversity 
coef_cons_shannon <- data.frame(
  prod_shannon = mean(DD_log$prod_shannon), 
  cons_shannon = seq(min(DD_log$cons_shannon), max(DD_log$cons_shannon), length.out = 98),
  sd_temp.y = mean(DD_log$sd_temp.y, na.rm = T),
  tmp = mean(DD_log$tmp)
)
X_cons_shannon <- model.matrix(~ cons_shannon + prod_shannon + tmp +  sd_temp.y, data = coef_bottomup_shannon)
pred_cons_shannon <- predict(full_cons_shannon, newdata = coef_cons_shannon, re.form = NA)

vcov_matrix <- vcov(full_cons_shannon, useScale = FALSE)
df <- df.residual(full_cons_shannon)
sigma <- sigma(full_cons_shannon)
vcov_adjusted <- vcov_matrix * (sigma^2)  
se_cons_shannon <- sqrt(diag(X_cons_shannon %*% vcov_adjusted %*% t(X_cons_shannon)))

t_value_shannon <- qt(0.975, df = df.residual(full_cons_shannon))
cons_int_shannon <- data.frame(
  fit = pred_cons_shannon,
  lwr = pred_cons_shannon - t_value_shannon * se_cons_shannon,
  upr = pred_cons_shannon + t_value_shannon * se_cons_shannon
)
result_cons_shannon <- cbind(coef_cons_shannon, cons_int_shannon)

habitat_types <- unique(DD_log$habitat_type)
pred_cons_shannon_list <- list()

for(habitat in habitat_types) {
   habitat_data <- DD_log[DD_log$habitat_type == habitat,]

  new_data <- data.frame(
    cons_shannon = seq(min(habitat_data$cons_shannon, na.rm = TRUE), 
                       max(habitat_data$cons_shannon, na.rm = TRUE), 
                       length.out = 100),
    prod_shannon = mean(habitat_data$prod_shannon, na.rm = TRUE),
    sd_temp.y = mean(habitat_data$sd_temp.y, na.rm = TRUE),
    tmp = mean(habitat_data$tmp, na.rm = TRUE),
    habitat_type = habitat
  )
  new_data$fit <- predict(full_cons_shannon, newdata = new_data)
  pred_cons_shannon_list[[habitat]] <- new_data
}
pred_cons_shannon_facet_1 <- do.call(rbind, pred_cons_shannon_list)

### Draw the graph
DSR_cons_shannon <- ggplot() +
  geom_point(data = DD_log, mapping = aes(x = cons_shannon, y = (cons_comsta),  color = habitat_type), 
             size = 10, alpha = 0.3) +
  geom_line(data = pred_cons_shannon_facet_1, 
            mapping = aes(x = cons_shannon, y = fit, color = habitat_type), 
            linewidth = 1.0) +
  geom_smooth(data= result_cons_shannon, mapping = aes(x =  cons_shannon, y = fit,color = FunGroup), 
              method = "lm", linetype = "solid", color = "black", linewidth = 2, se = T) +
  geom_ribbon(data= result_cons_shannon, mapping = aes(x = cons_shannon, ymin = lwr, ymax = upr),  alpha = 0.1) +
  labs(x = "Consumer diversity", y = "LN (Consumer community stability)", color = "Trophic level", shape = "Site type") + 
  guides(fill = "none") + 
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta), na.rm = TRUE), 
           label = expression(italic("P")~"< 0.001"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta), na.rm = TRUE) - 0.1, 
           label = expression(italic(R)^2~"= 0.233"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  my_scales + my_theme +
  coord_cartesian(clip = "off")
DSR_cons_shannon <- ggdraw(DSR_cons_shannon) + 
  draw_label("a",  x = 0, y = 1.01, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
DSR_cons_shannon

###
options(na.action = "na.fail")
full_prod_shannon <- lmer(prod_comsta ~  prod_shannon+ cons_shannon + tmp + sd_temp.y + (1|habitat_type), data = DD_log)
summary(full_prod_shannon)
anova(full_prod_shannon)
r2beta(full_prod_shannon, partial = TRUE, method = "nsj")


### Producer stability as response to consumer diversity 
coef_topdown_shannon <- data.frame(
  prod_shannon = mean(DD_log$prod_shannon),  
  cons_shannon = seq(min(DD_log$cons_shannon), max(DD_log$cons_shannon), length.out = 98),
  sd_temp.y = mean(DD_log$sd_temp.y, na.rm = T),
  tmp = mean(DD_log$tmp)
)

X_topdown_shannon <- model.matrix(~ cons_shannon + prod_shannon  + tmp + sd_temp.y, data = coef_topdown_shannon)
pred_topdown_shannon <- predict(full_prod_shannon, newdata = coef_topdown_shannon, re.form = NA)

vcov_matrix <- vcov(full_prod_shannon, useScale = FALSE)  #
df <- df.residual(full_prod_shannon)
sigma <- sigma(full_prod_shannon)
vcov_adjusted <- vcov_matrix * (sigma^2)  
se_topdown_shannon <- sqrt(diag(X_topdown_shannon %*% vcov_adjusted %*% t(X_topdown_shannon)))

t_value_shannon <- qt(0.975, df = df.residual(full_prod_shannon))
topdown_int_shannon <- data.frame(
  fit = pred_topdown_shannon,
  lwr = pred_topdown_shannon - t_value_shannon * se_topdown_shannon,
  upr = pred_topdown_shannon + t_value_shannon * se_topdown_shannon
)
result_topdown_shannon <- cbind(coef_topdown_shannon, topdown_int_shannon)

habitat_types <- unique(DD_log$habitat_type)
pred_topdown_shannon_list <- list()
for(habitat in habitat_types) {
  habitat_data <- DD_log[DD_log$habitat_type == habitat,]
  new_data <- data.frame(
    cons_shannon = seq(min(habitat_data$cons_shannon, na.rm = TRUE), 
                       max(habitat_data$cons_shannon, na.rm = TRUE), 
                       length.out = 100),
    prod_shannon = mean(habitat_data$prod_shannon, na.rm = TRUE),
    sd_temp.y = mean(habitat_data$sd_temp.y, na.rm = TRUE),
    tmp = mean(habitat_data$tmp, na.rm = TRUE),
    habitat_type = habitat
  )
  
  new_data$fit <- predict(full_prod_shannon, newdata = new_data)
  pred_topdown_shannon_list[[habitat]] <- new_data
}
pred_topdown_shannon_facet_1 <- do.call(rbind, pred_topdown_shannon_list)
### Draw the graph
DSR_topdown_shannon <- ggplot() +
  geom_point(data = DD_log, mapping = aes(x = cons_shannon, y = (prod_comsta),  color = habitat_type), 
             size = 10, alpha = 0.3) +
  geom_line(data = pred_topdown_shannon_facet_1, 
            mapping = aes(x = cons_shannon, y = fit, color = habitat_type), 
            linewidth = 1.0) +
  geom_smooth(data= result_topdown_shannon, mapping = aes(x =  cons_shannon, y = fit,color = FunGroup), 
              method = "lm", linetype = "solid", color = "black", linewidth = 2, se = T) +
  geom_ribbon(data= result_topdown_shannon, mapping = aes(x = cons_shannon, ymin = lwr, ymax = upr), alpha = 0.1) +
  labs(x = "Comsumer diversity", y = "Ln (Producer community stability)", color = "Habitat type") + 
  guides(fill = "none") + 
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta)-0.1, na.rm = TRUE), 
           label = expression(italic("P")~"= 0.005"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta)-0.1, na.rm = TRUE) - 0.1, 
           label = expression(italic(R)^2~"= 0.077"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  my_scales + 
  my_theme
DSR_topdown_shannon <- ggdraw(DSR_topdown_shannon) + 
  draw_label("d",  x = 0, y = 1, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
DSR_topdown_shannon

### Producer stability as response to producer diversity 
coef_prod_shannon <- data.frame(
  cons_shannon = mean(DD_log$cons_shannon), 
  prod_shannon = seq(min(DD_log$prod_shannon), max(DD_log$prod_shannon), length.out = 98),
  sd_temp.y = mean(DD_log$sd_temp.y, na.rm = T),
  tmp = mean(DD_log$tmp)
)

X_prod_shannon <- model.matrix(~ cons_shannon + prod_shannon + tmp + sd_temp.y, data = coef_prod_shannon)
pred_prod_shannon <- predict(full_prod_shannon, newdata = coef_prod_shannon, re.form = NA)

vcov_matrix <- vcov(full_prod_shannon, useScale = FALSE)  #
df <- df.residual(full_prod_shannon)
sigma <- sigma(full_prod_shannon)
vcov_adjusted <- vcov_matrix * (sigma^2)  
se_prod_shannon <- sqrt(diag(X_prod_shannon %*% vcov_adjusted %*% t(X_prod_shannon)))

t_value_shannon <- qt(0.975, df = df.residual(full_prod_shannon))
prod_int_shannon <- data.frame(
  fit = pred_prod_shannon,
  lwr = pred_prod_shannon - t_value_shannon * se_prod_shannon,
  upr = pred_prod_shannon + t_value_shannon * se_prod_shannon
)

result_prod_shannon <- cbind(coef_prod_shannon, prod_int_shannon)
habitat_types <- unique(DD_log$habitat_type)
pred_prod_list_shannon <- list()
for(habitat in habitat_types) {
   habitat_data <- DD_log[DD_log$habitat_type == habitat,]
  
   new_data <- data.frame(
    prod_shannon = seq(min(habitat_data$prod_shannon, na.rm = TRUE), 
                       max(habitat_data$prod_shannon, na.rm = TRUE), 
                       length.out = 100),
    cons_shannon = mean(habitat_data$cons_shannon, na.rm = TRUE),
    sd_temp.y = mean(habitat_data$sd_temp.y, na.rm = TRUE),
    tmp = mean(habitat_data$tmp, na.rm = TRUE),
    habitat_type = habitat
  )
  
  new_data$fit <- predict(full_prod_shannon, newdata = new_data)
  pred_prod_list_shannon[[habitat]] <- new_data
}
pred_prod_facet_shannon_1 <- do.call(rbind, pred_prod_list_shannon)

### Draw the graph
DSR_prod_shannon <- ggplot() +
  geom_point(data = DD_log, mapping = aes(x = prod_shannon, y = (prod_comsta),  color = habitat_type), 
             size = 10, alpha = 0.3) +
  geom_line(data = pred_prod_facet_shannon_1, 
            mapping = aes(x = prod_shannon, y = fit, color = habitat_type), 
            linewidth = 1.0) +
  labs(x = "Producer diversity", y = "Ln (Producer community stability)", color = "Habitat type") + 
  scale_y_continuous(limits = c(-0.8, 1), breaks = seq(-0.8, 1, 0.5)) +
  guides(fill = "none") + 
  my_scales + 
  my_theme
DSR_prod_shannon <- ggdraw(DSR_prod_shannon) + 
  draw_label("b",  x = 0, y = 1, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
DSR_prod_shannon

DSR_graph_shannon <- (DSR_cons_shannon + DSR_bottomup_shannon)/(DSR_prod_shannon + DSR_topdown_shannon)
DSR_graph_shannon

legend_sen <- get_legend(
  ggplot() +
    geom_point(data = DD_log, 
               mapping = aes(x = prod_shannon, y = log10(prod_comsta), color = habitat_type),
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
          #legend.key = element_blank(),      ## delete the background of the legend  
          legend.background = element_blank(),
          legend.title = element_text(
            size = 25,
            face = "plain"),
          legend.text = element_text(
            size = 20)
    ) +
    labs(color = "Habitat type")
)

Extended_Figure_3 <- cowplot::plot_grid(
  DSR_graph_shannon, legend_sen, 
  ncol = 2, 
  rel_widths = c(1, .2),
  align = 'h',
  axis = 'l'
)
Extended_Figure_3

####### Relationship between richness (without rare species) and community stability
#### Consumer stability as responses to producer diversity
DD_log_rareRemoval <- read.csv("DD_log(withoutRareSpecies0.1%)_0707.csv", check.names = F) 

full_cons_richness <- lmer(cons_comsta ~  log(prod_richness) + log(cons_richness) + sd_temp.y + tmp + 
                             (1|habitat_type), data = DD_log_rareRemoval)
summary(full_cons_richness)
anova(full_cons_richness)
r2beta(full_cons_richness, partial = TRUE, method = "nsj")

####### Consumer stability as response to producer diversity 
coef_bottomup_richness <- data.frame(
  cons_richness = mean(DD_log_rareRemoval$cons_richness), 
  prod_richness = seq(min(DD_log_rareRemoval$prod_richness), max(DD_log_rareRemoval$prod_richness), length.out = 98),
  sd_temp.y = mean(DD_log_rareRemoval$sd_temp.y, na.rm = T),
  tmp = mean(DD_log_rareRemoval$tmp)
)
X_bottomup_richness <- model.matrix(~ log(prod_richness) + log(cons_richness) + sd_temp.y + tmp, data = coef_bottomup_richness)
prod_bottomup_richness <- predict(full_cons_richness, newdata = coef_bottomup_richness, re.form = NA)

vcov_matrix <- vcov(full_cons_richness, useScale = FALSE)  #
df <- df.residual(full_cons_richness)
sigma <- sigma(full_cons_richness)
vcov_adjusted <- vcov_matrix * (sigma^2)  
se_bottomup_richness <- sqrt(diag(X_bottomup_richness %*% vcov_adjusted %*% t(X_bottomup_richness)))

t_value_richness <- qt(0.975, df = df.residual(full_cons_richness))
bottomup_int_richness <- data.frame(
  fit = prod_bottomup_richness,
  lwr = prod_bottomup_richness - t_value_richness * se_bottomup_richness,
  upr = prod_bottomup_richness + t_value_richness * se_bottomup_richness
)
result_bottomup_richness <- cbind(coef_bottomup_richness, bottomup_int_richness)

habitat_types <- unique(DD_log$habitat_type)
pred_bottomup_richness_list <- list()
for(habitat in habitat_types) {
  habitat_data <- DD_log_rareRemoval[DD_log_rareRemoval$habitat_type == habitat,]
  new_data <- data.frame(
    prod_richness = seq(min((habitat_data$prod_richness), na.rm = TRUE), 
                        max((habitat_data$prod_richness), na.rm = TRUE), 
                        length.out = 100),
    cons_richness = mean((habitat_data$cons_richness), na.rm = TRUE),
    sd_temp.y = mean(habitat_data$sd_temp.y, na.rm = TRUE),
    tmp = mean(habitat_data$tmp, na.rm = TRUE),
    habitat_type = habitat
  )
  new_data$fit <- predict(full_cons_richness, newdata = new_data)
  pred_bottomup_richness_list[[habitat]] <- new_data
}
pred_bottomup_richness_facet_1 <- do.call(rbind, pred_bottomup_richness_list)

### Draw the graph
DSR_bottomup_richness <- ggplot() +
  geom_point(data = DD_log_rareRemoval, mapping = aes(x = log(prod_richness), y = (cons_comsta), color = habitat_type), 
             size = 10, alpha = 0.3) +
  geom_line(data = pred_bottomup_richness_facet_1, 
            mapping = aes(x = log(prod_richness), y = fit, color = habitat_type), 
            linewidth = 1.0) +
  geom_smooth(data= result_bottomup_richness, mapping = aes(x =  log(prod_richness), y = fit,color = FunGroup), 
             method = "lm", linetype = "solid", color = "black", linewidth = 1.5, se = T) +
  geom_ribbon(data= result_bottomup_richness, mapping = aes(x = log(prod_richness), ymin = lwr, ymax = upr), alpha = 0.2) +
  labs(x = "Producer diversity", y = "Ln (Consumer community stability)", color = "Habitat type") + 
  guides(fill = "none") + 
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta), na.rm = TRUE), 
           label = expression(italic("P")~"= 0.004"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta), na.rm = TRUE) - 0.12, 
           label = expression(italic(R)^2~"= 0.085"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  my_scales + my_theme 
DSR_bottomup_richness <- ggdraw(DSR_bottomup_richness) + 
  draw_label("c",  x = 0, y = 1.01, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
DSR_bottomup_richness

####### Consumer stability as response to consumer diversity 
coef_cons_richness <- data.frame(
  prod_richness = mean(DD_log_rareRemoval$prod_richness),  # 添加这一行
  cons_richness = seq(min(DD_log_rareRemoval$cons_richness), max(DD_log_rareRemoval$cons_richness), length.out = 98),
  sd_temp.y = mean(DD_log_rareRemoval$sd_temp.y, na.rm = T),
  tmp = mean(DD_log_rareRemoval$tmp)
)
X_cons_richness <- model.matrix(~ log(cons_richness) + log(prod_richness) + tmp +  sd_temp.y, data = coef_bottomup_richness)
pred_cons_richness <- predict(full_cons_richness, newdata = coef_cons_richness, re.form = NA)

vcov_matrix <- vcov(full_cons_richness, useScale = FALSE)  #
df <- df.residual(full_cons_richness)
sigma <- sigma(full_cons_richness)
vcov_adjusted <- vcov_matrix * (sigma^2)  
se_cons_richness <- sqrt(diag(X_cons_richness %*% vcov_adjusted %*% t(X_cons_richness)))

t_value_richness <- qt(0.975, df = df.residual(full_cons_richness))
cons_int_richness <- data.frame(
  fit = pred_cons_richness,
  lwr = pred_cons_richness - t_value_richness * se_cons_richness,
  upr = pred_cons_richness + t_value_richness * se_cons_richness
)
result_cons_richness <- cbind(coef_cons_richness, cons_int_richness)

habitat_types <- unique(DD_log_rareRemoval$habitat_type)
pred_cons_richness_list <- list()
for(habitat in habitat_types) {
  habitat_data <- DD_log_rareRemoval[DD_log_rareRemoval$habitat_type == habitat,]
  new_data <- data.frame(
    cons_richness = seq(min(habitat_data$cons_richness, na.rm = TRUE), 
                        max(habitat_data$cons_richness, na.rm = TRUE), 
                        length.out = 100),
    prod_richness = mean(habitat_data$prod_richness, na.rm = TRUE),
    sd_temp.y = mean(habitat_data$sd_temp.y, na.rm = TRUE),
    tmp = mean(habitat_data$tmp, na.rm = TRUE),
    habitat_type = habitat
  )
  new_data$fit <- predict(full_cons_richness, newdata = new_data)
  pred_cons_richness_list[[habitat]] <- new_data
}
pred_cons_richness_facet_1 <- do.call(rbind, pred_cons_richness_list)

### Draw the graph
DSR_cons_richness <- ggplot() +
  geom_point(data = DD_log_rareRemoval, mapping = aes(x = log(cons_richness), y = (cons_comsta),  color = habitat_type), 
             size = 10, alpha = 0.3) +
  #geom_smooth(data= DD_log_rareRemoval, mapping = aes(x = log(cons_richness), y = (cons_comsta),color = habitat_type), 
   #          method = "lm", linetype = "solid",  linewidth = 0.8, se = F) +
  geom_line(data = pred_cons_richness_facet_1, 
            mapping = aes(x = log(cons_richness), y = fit, color = habitat_type), 
            linewidth = 1.0) +
  geom_smooth(data= result_cons_richness, mapping = aes(x =  log(cons_richness), y = fit,color = FunGroup), 
             method = "lm", linetype = "solid", color = "black", linewidth = 2, se = T) +
  geom_ribbon(data= result_cons_richness, mapping = aes(x = log(cons_richness), ymin = lwr, ymax = upr),  alpha = 0.1) +
  labs(x = "Consumer diversity", y = "Ln (Consumer community stability)", color = "Trophic level", shape = "Site type") + 
  guides(fill = "none") + 
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta), na.rm = TRUE), 
           label = expression(italic("P")~"< 0.001"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta), na.rm = TRUE) - 0.12, 
           label = expression(italic(R)^2~"= 0.335"), 
           parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  my_scales + my_theme +
  coord_cartesian(clip = "off")
DSR_cons_richness <- ggdraw(DSR_cons_richness) + 
  draw_label("a",  x = 0, y = 1.01, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
DSR_cons_richness

### Producer stability as response to producer and consumer diversity
options(na.action = "na.fail")
full_prod_richness <- lmer(prod_comsta ~  log(cons_richness) + log(prod_richness) +  tmp + sd_temp.y + (1|habitat_type), data = DD_log_rareRemoval)
summary(full_prod_richness)
anova(full_prod_richness)
r2beta(full_prod_richness, partial = TRUE, method = "nsj")

coef_topdown_richness <- data.frame(
  prod_richness = mean(DD_log_rareRemoval$prod_richness),  
  cons_richness = seq(min(DD_log_rareRemoval$cons_richness), max(DD_log_rareRemoval$cons_richness), length.out = 98),
  sd_temp.y = mean(DD_log_rareRemoval$sd_temp.y, na.rm = T),
  tmp = mean(DD_log_rareRemoval$tmp)
)
X_topdown_richness <- model.matrix(~ log(cons_richness) + log(prod_richness)  + tmp + sd_temp.y, data = coef_topdown_richness)
pred_topdown_richness <- predict(full_prod_richness, newdata = coef_topdown_richness, re.form = NA)

vcov_matrix <- vcov(full_prod_richness, useScale = FALSE) 
df <- df.residual(full_prod_richness)
sigma <- sigma(full_prod_richness)
vcov_adjusted <- vcov_matrix * (sigma^2)  
se_topdown_richness <- sqrt(diag(X_topdown_richness %*% vcov_adjusted %*% t(X_topdown_richness)))

t_value_richness <- qt(0.975, df = df.residual(full_prod_richness))
topdown_int_richness <- data.frame(
  fit = pred_topdown_richness,
  lwr = pred_topdown_richness - t_value_richness * se_topdown_richness,
  upr = pred_topdown_richness + t_value_richness * se_topdown_richness
)
result_topdown_richness <- cbind(coef_topdown_richness, topdown_int_richness)

habitat_types <- unique(DD_log_rareRemoval$habitat_type)
pred_topdown_richness_list <- list()
for(habitat in habitat_types) {
  habitat_data <- DD_log_rareRemoval[DD_log_rareRemoval$habitat_type == habitat,]
  new_data <- data.frame(
    cons_richness = seq(min(habitat_data$cons_richness, na.rm = TRUE), 
                        max(habitat_data$cons_richness, na.rm = TRUE), 
                        length.out = 100),
    prod_richness = mean(habitat_data$prod_richness, na.rm = TRUE),
    sd_temp.y = mean(habitat_data$sd_temp.y, na.rm = TRUE),
    tmp = mean(habitat_data$tmp, na.rm = TRUE),
    habitat_type = habitat
  )
  new_data$fit <- predict(full_prod_richness, newdata = new_data)
  pred_topdown_richness_list[[habitat]] <- new_data
}
pred_topdown_richness_facet <- do.call(rbind, pred_topdown_richness_list)

### Draw the graph
DSR_topdown_richness <- ggplot() +
  geom_point(data = DD_log_rareRemoval, mapping = aes(x = log(cons_richness), y = (prod_comsta),  color = habitat_type), 
             size = 10, alpha = 0.3) +
  geom_line(data = pred_topdown_richness_facet, 
            mapping = aes(x = log(cons_richness), y = fit, color = habitat_type), 
            linewidth = 1.0) +
  geom_smooth(data= result_topdown_richness, mapping = aes(x = log(cons_richness), y = fit,color = FunGroup), 
             method = "lm", linetype = "solid", color = "black", linewidth = 1.5, se = T) +
  geom_ribbon(data= result_topdown_richness, mapping = aes(x = log(cons_richness), ymin = lwr, ymax = upr), alpha = 0.2) +
  labs(x = "Comsumer diversity", y = "Ln (Producer community stability)", color = "Habitat type") + 
  #scale_x_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  #scale_y_continuous(limits = c(-0.8, 1), breaks = seq(-0.8, 1, 0.5)) +
  guides(fill = "none") + 
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta)-0.1, na.rm = TRUE), 
          label = expression(italic("P")~"= 0.007"), 
         parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta)-0.1, na.rm = TRUE) - 0.12, 
          label = expression(italic(R)^2~"= 0.080"), 
         parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  my_scales + 
  my_theme
DSR_topdown_richness <- ggdraw(DSR_topdown_richness) + 
  draw_label("d",  x = 0, y = 1, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
DSR_topdown_richness

####### Producer stability as response to producer diversity 
coef_prod_richness <- data.frame(
  cons_richness = mean(DD_log_rareRemoval$cons_richness), 
  prod_richness = seq(min(DD_log_rareRemoval$prod_richness), max(DD_log_rareRemoval$prod_richness), length.out = 98),
  sd_temp.y = mean(DD_log_rareRemoval$sd_temp.y, na.rm = T),
  y_length = mean(DD_log_rareRemoval$y_length),
  tmp = mean(DD_log_rareRemoval$tmp)
)
X_prod_richness <- model.matrix(~ log(cons_richness) + log(prod_richness) + tmp + sd_temp.y, data = coef_prod_richness)
pred_prod_richness <- predict(full_prod_richness, newdata = coef_prod_richness, re.form = NA)

vcov_matrix <- vcov(full_prod_richness, useScale = FALSE)  #
df <- df.residual(full_prod_richness)
sigma <- sigma(full_prod_richness)
vcov_adjusted <- vcov_matrix * (sigma^2)  
se_prod_richness <- sqrt(diag(X_prod_richness %*% vcov_adjusted %*% t(X_prod_richness)))

t_value_richness <- qt(0.975, df = df.residual(full_prod_richness))
prod_int_richness <- data.frame(
  fit = pred_prod_richness,
  lwr = pred_prod_richness - t_value_richness * se_prod_richness,
  upr = pred_prod_richness + t_value_richness * se_prod_richness
)
result_prod_richness <- cbind(coef_prod_richness, prod_int_richness)

habitat_types <- unique(DD_log_rareRemoval$habitat_type)
pred_prod_richness_list <- list()
for(habitat in habitat_types) {
  habitat_data <- DD_log_rareRemoval[DD_log_rareRemoval$habitat_type == habitat,]
  new_data <- data.frame(
    prod_richness = seq(min(habitat_data$prod_richness, na.rm = TRUE), 
                        max(habitat_data$prod_richness, na.rm = TRUE), 
                        length.out = 100),
    cons_richness = mean(habitat_data$cons_richness, na.rm = TRUE),
    sd_temp.y = mean(habitat_data$sd_temp.y, na.rm = TRUE),
    tmp = mean(habitat_data$tmp, na.rm = TRUE),
    habitat_type = habitat
  )
  new_data$fit <- predict(full_prod_richness, newdata = new_data)
  pred_prod_richness_list[[habitat]] <- new_data
}
pred_prod_richness_facet_1 <- do.call(rbind, pred_prod_richness_list)

### Draw the graph
DSR_prod_richness <- ggplot() +
  geom_point(data = DD_log_rareRemoval, mapping = aes(x = log(prod_richness), y = (prod_comsta),  color = habitat_type), 
             size = 10, alpha = 0.3) +
  geom_line(data = pred_prod_richness_facet_1, 
            mapping = aes(x = log(prod_richness), y = fit, color = habitat_type), 
            linewidth = 1.0) +
  geom_smooth(data= result_prod_richness, mapping = aes(x = log(prod_richness), y = fit,color = FunGroup), 
              method = "lm", linetype = "solid", color = "black", linewidth = 1.5, se = T) +
  geom_ribbon(data= result_prod_richness, mapping = aes(x = log(prod_richness), ymin = lwr, ymax = upr), alpha = 0.1) +
  labs(x = "Producer diversity", y = "Ln (Producer community stability)", color = "Habitat type") + 
  guides(fill = "none") + 
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta), na.rm = TRUE), 
           label = expression(italic("P")~"< 0.001"), 
          parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  annotate("text", x = 0.2, y = max(log(DD_consumer$com_sta), na.rm = TRUE) - 0.12, 
          label = expression(italic(R)^2~" = 0.191"), 
         parse = TRUE, hjust = 0, vjust = 1, size = 6) +
  my_scales + 
  my_theme
DSR_prod_richness <- ggdraw(DSR_prod_richness) + 
  draw_label("b",  x = 0, y = 1, hjust = -0.5, vjust = 1.5, size = 30, fontface = "bold")
DSR_prod_richness

DSR_richness <- (DSR_cons_richness + DSR_bottomup_richness)/(DSR_prod_richness + DSR_topdown_richness)
DSR_richness

legend_sen <- get_legend(
  ggplot() +
    geom_point(data = DD_log_rareRemoval, 
               mapping = aes(x = log(prod_richness), y = log(prod_comsta), color = habitat_type),
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
          #legend.key = element_blank(),      ## delete the background of the legend  
          legend.background = element_blank(),
          legend.title = element_text(
            size = 25,
            face = "plain"),
          legend.text = element_text(
            size = 20)
    ) +
    labs(color = "Habitat type")
)

Exteded_Figure_4 <- cowplot::plot_grid(
  DSR_richness, legend_sen, 
  ncol = 2, 
  rel_widths = c(1, .2),
  align = 'h',
  axis = 'l'
)
Exteded_Figure_4




