################################
rm(list = ls())

library(tidyverse)      
library(sf)     
library(ggplot2)
library(rnaturalearth) 
library(rnaturalearthdata)

##########
DD_sites <- DD_log_spatial %>%
  dplyr::select(siteID, habitat_type, Longitude, Latitude)

library(rnaturalearth)
sf_use_s2(FALSE)
world_map <- ne_countries(scale = "large", returnclass = "sf") %>%
  st_make_valid() %>%
  st_union()

Main_plot <- ggplot() +
  geom_sf(data = world_map, fill = "antiquewhite1") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    ) +
  geom_point(data = DD_sites, aes(x = Longitude, y = Latitude, color = habitat_type), size = 3.5, alpha = 0.6) +
  theme(legend.position = "right",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15)) +
  scale_color_manual(values =  c("#4FC3F7","#006064","purple","blue"),
                     name = "Habitat type",
                     labels = c("Estuary (n=28)", "Lentic freshwaters(n=37)",
                                "Lotic freshwaters(n=21)", "Marine(n=11)")) +
  coord_sf(expand = FALSE) 
Main_plot

################################# For marine systems
library(rnaturalearthdata)
DD_marine_data <- DD_sites %>%
  filter(habitat_type == "Marine")
calif_coast <- ne_states(country = "united states of america", 
                         returnclass = "sf") %>%
  st_crop(xmin = -121, xmax = -119,
          ymin = 33, ymax = 35)

P_marine <- ggplot() +
  geom_sf(data = calif_coast, fill = "antiquewhite1", color = "black") +
  coord_sf(xlim = c(-120.8, -119), 
           ylim = c(33.8, 35)) +
  geom_point(data = DD_marine_data,
             aes(x = Longitude, y = Latitude),
             color = "blue",
             size = 2.5,
             alpha = 0.7) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue", color = NA),
    panel.grid.major = element_line(color = "aliceblue", linetype = "dashed"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)
  ) +
  labs(x = "Longitude",
       y = "Latitude")
P_marine

Marine_grob <- ggplotGrob(P_marine)
Main_plot_1 <- Main_plot +
  annotation_custom(grob = Marine_grob,
                    xmin = -80, xmax = 0,
                    ymin = -90, ymax = 0)
Main_plot_1

#########################################For SFS estuary
DD_sfs_estuary <- DD_sites %>%
  filter(grepl("Cluster_", siteID))
sf_use_s2(FALSE)
california <- ne_states(country = "united states of america", returnclass = "sf") %>%
  filter(name == "California")

DD_sfs_estuary_sf <- DD_sfs_estuary %>%
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = 4326)  
water_sf <- ne_download(scale = "large", type = "rivers_lake_centerlines", category = "physical", returnclass = "sf") %>%
  st_crop(xmin = -122.5, xmax = -121.1, ymin = 37.6, ymax = 38.4)

sfs <- ggplot() +
  geom_sf(data = california, fill = "antiquewhite1", color = "black") +
  geom_point(data = DD_sfs_estuary, aes(x = Longitude, y = Latitude), color = "#4FC3F7", size = 2.5, alpha = 1) +
  coord_sf(xlim = c(-122.5, -121.1), ylim = c(37.6, 38.4)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "white", linetype = "dashed"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)
  ) +
  labs(x = "Longitude",
       y = "Latitude")
sfs
sfs_grob <- ggplotGrob(sfs)
Main_plot_2 <- Main_plot_1 +
  annotation_custom(grob = sfs_grob,
                    xmin = -170, xmax = -80,
                    ymin = -90, ymax = 0)
Main_plot_2

######## For Danish lakes
DD_Danish <- DD_sites %>%
  filter(grepl("^\\d+$", siteID))

denmark <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name == "Denmark")

danish_lakes_sf <- st_as_sf(DD_Danish, coords = c("Longitude", "Latitude"), crs = 4326)

Danish_lakes <- ggplot() +
  geom_sf(data = denmark, fill = "antiquewhite1", color = "black") +
  geom_sf(data = danish_lakes_sf, color = "#006064", size = 2.5) +
  coord_sf(xlim = c(8, 13), ylim = c(54.5, 57.7)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue", color = NA),
    panel.grid.major = element_line(color = "aliceblue", linetype = "dashed"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(x = "Longitude",
       y = "Latitude")
Danish_lakes

danish_grob <- ggplotGrob(Danish_lakes)

Main_plot_3 <- Main_plot_2 +
  annotation_custom(grob = danish_grob,
                    xmin = -20, xmax = 90,
                    ymin = -79, ymax = -12)
Main_plot_3

#################### Link the site clusters to the small plot
Main_plot_4 <- Main_plot_3 +
  geom_segment(aes(x = -120, y = 35, xend = -75, yend = -15), 
               color = "blue", linewidth  = 0.6, alpha = 1, linetype = "dashed") +
  geom_segment(aes(x = -120, y = 35, xend = -3, yend = -15), 
               color = "blue", linewidth  = 0.6, alpha = 1, linetype = "dashed") +
  geom_segment(aes(x = -122, y = 38, xend = -165.5, yend = -14), 
               color = "#4FC3F7", linewidth = 0.6, alpha = 1, linetype = "dashed") +
  geom_segment(aes(x = -122, y = 38, xend = -83, yend = -14), 
               color = "#4FC3F7", linewidth = 0.6, alpha = 1, linetype = "dashed") +
  geom_segment(aes(x = 10, y = 55, xend = 9.9, yend = -15), 
               color = "#006064", linewidth = 0.6, alpha = 1, linetype = "dashed")+
  geom_segment(aes(x = 10, y = 55, xend = 62, yend = -15), 
               color = "#006064", linewidth = 0.6, alpha = 1, linetype = "dashed")
Main_plot_4 + 
  coord_sf(expand = FALSE)
Main_plot_4

Extended_Figure_1 <- Main_plot_4
