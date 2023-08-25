#-----------------------------------------Map--------------------------------------#
#-Author: Francisca Castro -------------------------------- Created: June 30, 2023-#
#-R Version: 4.3.1 -------------------------------------- Revised: August 25, 2023-#

# Preparation

## Install packages

pacman::p_load(biscale, cowplot, sf, ggplot2, tidyverse, magrittr, chilemapas, stringr, rgdal, rgdal, here,
               microbenchmark, viridis)

## Load shapefile

### Shapefiles can be downloaded from https://www.bcn.cl/siit/mapas_vectoriales/index_html

chile_municipalities <- sf::st_read(here("01-data/map-data/comunas.shp"))
chile_regions <- sf::st_read(here("/01-data/map-data/Regional.shp"))


# Data preparation

## Modify `final_df` so it's grouped by municipalities
final_df_comunas <- final_df %>%
  group_by(nombre_comuna, nombre_provincia) %>%
  summarize(contentious_total = sum(contentious_total),
            repression_total = sum(repression_total))

rm(final_df)

## Include codigo_comuna to final_df
map_df <- left_join(final_df_comunas, codigos_territoriales %>% 
                      dplyr::select(nombre_comuna, codigo_comuna), 
                    by = "nombre_comuna")

## Remove leading zero from codigo_comuna and create cod_comuna in `map_df`
map_df$cod_comuna = str_remove(map_df$codigo_comuna, "^0+")

map_df$cod_comuna <- as.numeric(map_df$cod_comuna)
class(map_df$cod_comuna)

chile_municipalities$cod_comuna <- as.numeric(chile_municipalities$cod_comuna)
class(chile_municipalities$cod_comuna)

## Merge both datasets

map_df_final <- chile_municipalities %>%
  right_join(map_df, by = c("cod_comuna"))

# Create map
map <- ggplot() +
  geom_sf(data = map_df_final, aes(fill = contentious_total), color = "black") +
  scale_fill_gradient(name = "Contentious Total", low = "green", high = "red") +
  theme_minimal() +
  coord_sf(xlim = c(-9000000, -7393642), ylim = c(-7554436, -1978920), expand = FALSE)

microbenchmark(ggsave("test1.pdf", map), times = 1)

## Three-part map

# Define the latitude and longitude ranges for each region
norte <- list(xlim = c(-8493642, -7393642), ylim = c(-1899900, -3784745))
centro <- list(xlim = c(-8493642, -7393642), ylim = c(-3784745, -5669589))
sur <- list(xlim = c(-8493642, -7393642), ylim = c(-5669589, -7554436))
rm <- list(xlim = c(-7903099, -7835011), ylim = c(-3929671, -3999134))


# Create three separate maps for each region
map_region1 <- ggplot() +
  geom_sf(data = map_df_final, aes(fill = contentious_total), color = NA) +  # Set border color 
  geom_sf(data = chile_regions, colour = "gray50", fill = NA, size = 0.5) +
  scale_fill_continuous(type = "viridis", name = "Contentious Total", option = "magma", direction = -1, 
                        breaks = seq(0, 150, 50), labels = seq(0, 150, 50), limits = c(0, 150)) +  
  theme_minimal() +
  coord_sf(xlim = norte$xlim, ylim = norte$ylim, expand = FALSE) +
  labs(title = "North")

map_region2 <- ggplot() +
  geom_sf(data = map_df_final, aes(fill = contentious_total), color = NA) +  # Set border color 
  geom_sf(data = chile_regions, colour = "gray50", fill = NA, size = 0.5) +
  scale_fill_continuous(type = "viridis", name = "Contentious Total", option = "magma", direction = -1, 
                        breaks = seq(0, 150, 50), labels = seq(0, 150, 50), limits = c(0, 150)) +  
  theme_minimal() +
  theme(legend.position = "none") +
  coord_sf(xlim = centro$xlim, ylim = centro$ylim, expand = FALSE) +
  labs(title = "Center")

map_region3 <- ggplot() +
  geom_sf(data = map_df_final, aes(fill = contentious_total), color = NA) +  # Set border color 
  geom_sf(data = chile_regions, colour = "gray50", fill = NA, size = 0.5) +
  scale_fill_continuous(type = "viridis", name = "Contentious Total", option = "magma", direction = -1, 
                        breaks = seq(0, 150, 50), labels = seq(0, 150, 50), limits = c(0, 150)) +  
  theme_minimal() +
  theme(legend.position = "none") +
  coord_sf(xlim = sur$xlim, ylim = sur$ylim, expand = FALSE) +
  labs(title = "South")

map_region4 <- ggplot() +
  geom_sf(data = map_df_final, aes(fill = contentious_total), color = NA) +  # Set border color 
  geom_sf(data = chile_regions, colour = "gray50", fill = NA, size = 0.5) +
  scale_fill_continuous(type = "viridis", name = "Contentious Total", option = "magma", direction = -1, 
                        breaks = seq(0, 150, 50), labels = seq(0, 150, 50), limits = c(0, 150)) +  
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf(xlim = rm$xlim, ylim = rm$ylim) +
  labs(title = "Province of Santiago")


# Combine the maps into a grid of plots with a shared legend
legend <- get_legend(map_region1)

grid_map1 <- plot_grid(map_region1 + theme(legend.position = "none"), 
                       map_region2 + theme(legend.position = "none"),
                       map_region3 + theme(legend.position = "none"),
                       nrow = 1, rel_widths = c(1, 1, 1))

grid_map2 <- plot_grid(map_region4 + theme(legend.position = "none"), NULL, legend, NULL,
                       nrow = 4, rel_heights = c(0.55, -0.1, 0.2, 0.15),
                       rel_widths = c(1,1))

grid_combined <- plot_grid(grid_map1, grid_map2, nrow = 1, rel_widths = c(1.8, 0.5), align = "h", axis = "t", scale = c(1, .95))

ggsave("combined_map.pdf", grid_combined, width = 15, height = 8)

ggsave("combined_map.png", grid_combined, width = 15, height = 8)

ggsave("combined_map.jpeg", grid_combined, width = 15, height = 8)


# Create map for repressive events
map_region1_2 <- ggplot() +
  geom_sf(data = map_df_final, aes(fill = repression_total), color = NA) +  # Set border color 
  geom_sf(data = chile_regions, colour = "gray50", fill = NA, size = 0.5) +
  scale_fill_continuous(type = "viridis", name = "Repressive Actions", option = "magma", direction = -1, 
                        breaks = seq(0, 150, 50), labels = seq(0, 150, 50), limits = c(0, 150)) +  
  theme_minimal() +
  coord_sf(xlim = norte$xlim, ylim = norte$ylim, expand = FALSE) +
  labs(title = "North")

map_region2_2 <- ggplot() +
  geom_sf(data = map_df_final, aes(fill = repression_total), color = NA) +  # Set border color 
  geom_sf(data = chile_regions, colour = "gray50", fill = NA, size = 0.5) +
  scale_fill_continuous(type = "viridis", name = "Repressive Actions", option = "magma", direction = -1, 
                        breaks = seq(0, 150, 50), labels = seq(0, 150, 50), limits = c(0, 150)) +  
  theme_minimal() +
  theme(legend.position = "none") +
  coord_sf(xlim = centro$xlim, ylim = centro$ylim, expand = FALSE) +
  labs(title = "Center")

map_region3_2 <- ggplot() +
  geom_sf(data = map_df_final, aes(fill = repression_total), color = NA) +  # Set border color 
  geom_sf(data = chile_regions, colour = "gray50", fill = NA, size = 0.5) +
  scale_fill_continuous(type = "viridis", name = "Repressive Actions", option = "magma", direction = -1, 
                        breaks = seq(0, 150, 50), labels = seq(0, 150, 50), limits = c(0, 150)) +  
  theme_minimal() +
  theme(legend.position = "none") +
  coord_sf(xlim = sur$xlim, ylim = sur$ylim, expand = FALSE) +
  labs(title = "South")

map_region4_2 <- ggplot() +
  geom_sf(data = map_df_final, aes(fill = repression_total), color = NA) +  # Set border color 
  geom_sf(data = chile_regions, colour = "gray50", fill = NA, size = 0.5) +
  scale_fill_continuous(type = "viridis", name = "Repressive Actions", option = "magma", direction = -1, 
                        breaks = seq(0, 150, 50), labels = seq(0, 150, 50), limits = c(0, 150)) +  
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf(xlim = rm$xlim, ylim = rm$ylim) +
  labs(title = "Province of Santiago")


# Combine the maps into a grid of plots with a shared legend
legend_2 <- get_legend(map_region1_2)

grid_map1_2 <- plot_grid(map_region1_2 + theme(legend.position = "none"), 
                         map_region2_2 + theme(legend.position = "none"),
                         map_region3_2 + theme(legend.position = "none"),
                         nrow = 1, rel_widths = c(1, 1, 1))

grid_map2_2 <- plot_grid(map_region4_2 + theme(legend.position = "none"), NULL, legend_2, NULL,
                         nrow = 4, rel_heights = c(0.55, -0.1, 0.2, 0.15),
                         rel_widths = c(1,1))

grid_combined_2 <- plot_grid(grid_map1_2, grid_map2_2, nrow = 1, rel_widths = c(1.8, 0.5), align = "h", axis = "t", scale = c(1, .95))

ggsave("combined_map_2.pdf", grid_combined_2, width = 15, height = 8)

ggsave("combined_map_2.png", grid_combined_2, width = 15, height = 8)

ggsave("combined_map_2.jpeg", grid_combined_2, width = 15, height = 8)


### Chile divided in two parts instead of three

norte2 <- list(xlim = c(-8493642, -7393642), ylim = c(-1899900, -4727167))
sur2 <- list(xlim = c(-8493642, -7393642), ylim = c(-4727167, -7554436))

# Create map for repressive events
map_region1_3 <- ggplot() +
  geom_sf(data = map_df_final, aes(fill = repression_total), color = NA) +  # Set border color 
  geom_sf(data = chile_regions, colour = "gray50", fill = NA, size = 0.5) +
  scale_fill_continuous(type = "viridis", name = "Repressive Actions", option = "magma", direction = -1, 
                        breaks = seq(0, 150, 50), labels = seq(0, 150, 50), limits = c(0, 150)) +  
  theme_minimal() +
  coord_sf(xlim = norte2$xlim, ylim = norte2$ylim, expand = FALSE) +
  labs(title = "North") + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

map_region3_3 <- ggplot() +
  geom_sf(data = map_df_final, aes(fill = repression_total), color = NA) +  # Set border color 
  geom_sf(data = chile_regions, colour = "gray50", fill = NA, size = 0.5) +
  scale_fill_continuous(type = "viridis", name = "Repressive Actions", option = "magma", direction = -1, 
                        breaks = seq(0, 150, 50), labels = seq(0, 150, 50), limits = c(0, 150)) +  
  theme_minimal() +
  theme(legend.position = "none") +
  coord_sf(xlim = sur2$xlim, ylim = sur2$ylim, expand = FALSE) +
  labs(title = "South") + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

map_region4_3 <- ggplot() +
  geom_sf(data = map_df_final, aes(fill = repression_total), color = NA) +  # Set border color 
  geom_sf(data = chile_regions, colour = "gray50", fill = NA, size = 0.5) +
  scale_fill_continuous(type = "viridis", name = "Repressive Actions", option = "magma", direction = -1, 
                        breaks = seq(0, 150, 50), labels = seq(0, 150, 50), limits = c(0, 150)) +  
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf(xlim = rm$xlim, ylim = rm$ylim) +
  labs(title = "Province of Santiago") + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Combine the maps into a grid of plots with a shared legend
legend_3 <- get_legend(map_region1_3)

grid_map1_3 <- plot_grid(map_region1_3 + theme(legend.position = "none"), 
                         map_region3_3 + theme(legend.position = "none"),
                         nrow = 1, rel_widths = c(1, 1))

grid_map2_3 <- plot_grid(map_region4_3 + theme(legend.position = "none"), NULL, legend_3, NULL,
                         nrow = 4, rel_heights = c(0.55, 0.1, 0.2, 0.15),
                         rel_widths = c(1,1))

grid_combined_3 <- plot_grid(grid_map1_3, grid_map2_3, nrow = 1, rel_widths = c(1.3,0.7), 
                             align = "h", axis = "t", scale = c(1, .95))

ggsave("combined_map_3.pdf", grid_combined_3, width = 11, height = 8)

ggsave("combined_map_3.png", grid_combined_3, width = 11, height = 8)

ggsave("combined_map_3.jpeg", grid_combined_3, width = 11, height = 8)

