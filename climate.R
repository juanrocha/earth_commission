library(tidyverse)
library(fs)
library(sf)
library(spData)
library(terra)
library(biscale)
library(tictoc)
library(tidync)
library(patchwork)

## load processed datasets
load(file = "data/subnational_poverty_vars.RData") # 243Mb
load(file = "data/subnational_hdi_vars.RData") # 557Mb

wet1 <- rast("~/Documents/Projects/DATA/EC_additional_datasets/max_daily_tw_onepoint2_degrees.tif") 

wet2 <- rast("~/Documents/Projects/DATA/EC_additional_datasets/max_daily_tw_two_degrees.tif")
wet2
plot(wet1)

wet1 <- rotate(wet1)
wet2 <- rotate(wet2)

### Composed map for just:
## Because there is two temperature scenarions, I need to duplicate the color scale construction block
## For 1.2C
povsn <- bi_class(povsn, x = GSAP2_poor, y = max_t1_2, dim = 4, style = "quantile") 

# rename is not working, doing it in base.
names(povsn)[names(povsn) == "bi_class"] <- "bi_poverty"

povsn <- bi_class(povsn, x = population, y = max_t1_2, dim = 4, style = "quantile") 
names(povsn)[names(povsn) == "bi_class"] <- "bi_pop"

hdi_shp <-  bi_class(hdi_shp, x = shdi, y = max_t1_2, dim = 4, style = "quantile") 
names(hdi_shp)[names(hdi_shp) == "bi_class"] <- "bi_hdi" 

# For 2C
povsn <- bi_class(povsn, x = GSAP2_poor, y = max_t2, dim = 4, style = "quantile") 

# rename is not working, doing it in base.
names(povsn)[names(povsn) == "bi_class"] <- "bi_poverty2"

povsn <- bi_class(povsn, x = population, y = max_t2, dim = 4, style = "quantile") 
names(povsn)[names(povsn) == "bi_class"] <- "bi_pop2"

hdi_shp <-  bi_class(hdi_shp, x = shdi, y = max_t2, dim = 4, style = "quantile") 
names(hdi_shp)[names(hdi_shp) == "bi_class"] <- "bi_hdi2"


## Combine them all
tic()
ggsave(
    plot = (
        (povsn |> 
             ggplot() +
             geom_sf(aes(fill = bi_pop), size = 0.01, color = "white", show.legend = FALSE) +
             bi_scale_fill(pal = "BlueOr", dim = 4) +
             annotation_custom(
                 grob = ggplotGrob(
                     bi_legend(
                         pal = "BlueOr", dim = 4, 
                         xlab = "Population", ylab = "Max daily temperature",
                         flip_axes = FALSE, rotate_pal = FALSE) +
                         bi_theme(base_family = "Helvetica", base_size = 4)
                 ), 
                 xmin = -180, ymin = -60, xmax = -90, ymax = 20) +
             labs(tag = "A") +
             theme_void(base_size = 6)) +
            (povsn |> 
                 ggplot() +
                 geom_sf(aes(fill = bi_poverty), size = 0.01, color = "white", show.legend = FALSE) +
                 bi_scale_fill(pal = "DkViolet2", dim = 4) +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "DkViolet2", dim = 4, 
                             xlab = "Poverty", ylab = "Max daily temperature",
                             flip_axes = FALSE, rotate_pal = FALSE) +
                             bi_theme(base_family = "Helvetica", base_size = 4)
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 20) +
                 labs(tag = "C") +
                 theme_void(base_size = 6)) +
            (hdi_shp |> 
                 ggplot() +
                 geom_sf(aes(fill = bi_hdi), size = 0.01, color = "white", show.legend = FALSE) +
                 bi_scale_fill(pal = "GrPink2", dim = 4) +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "GrPink2", dim = 4, 
                             xlab = "HDI", ylab = "Max daily temperature",
                             flip_axes = FALSE, rotate_pal = FALSE) +
                             bi_theme(base_family = "Helvetica", base_size = 4)
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 20)+
                 labs(tag = "E") +
                 theme_void(base_size = 6)) +
            (povsn |> 
                 ggplot() +
                 geom_sf(aes(fill = bi_pop2), size = 0.01, color = "white", show.legend = FALSE) +
                 bi_scale_fill(pal = "BlueOr", dim = 4) +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "BlueOr", dim = 4, 
                             xlab = "Population", ylab = "Max daily temperature",
                             flip_axes = FALSE, rotate_pal = FALSE) +
                             bi_theme(base_family = "Helvetica", base_size = 4)
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 20) +
                 labs(tag = "B") +
                 theme_void(base_size = 6)) +
            (povsn |> 
                 ggplot() +
                 geom_sf(aes(fill = bi_poverty2), size = 0.01, color = "white", show.legend = FALSE) +
                 bi_scale_fill(pal = "DkViolet2", dim = 4) +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "DkViolet2", dim = 4, 
                             xlab = "Poverty", ylab = "Max daily temperature",
                             flip_axes = FALSE, rotate_pal = FALSE) +
                             bi_theme(base_family = "Helvetica", base_size = 4)
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 20) +
                 labs(tag = "D") +
                 theme_void(base_size = 6)) +
            (hdi_shp |> 
                 ggplot() +
                 geom_sf(aes(fill = bi_hdi), size = 0.01, color = "white", show.legend = FALSE) +
                 bi_scale_fill(pal = "GrPink2", dim = 4) +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "GrPink2", dim = 4, 
                             xlab = "HDI", ylab = "Max daily temperature",
                             flip_axes = FALSE, rotate_pal = FALSE) +
                             bi_theme(base_family = "Helvetica", base_size = 4)
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 20)+
                 labs(tag = "F") +
                 theme_void(base_size = 6)) +
            plot_layout(nrow = 3, ncol = 2, byrow = FALSE)
    ) ,
    filename = "just_climate_temp.png", path = "figures/", 
    device = "png", width = 4, height = 6, dpi = 400, bg = "white")
toc() #20s









#### old code ####
#### Now being calculated in load_data
# tic()
# wet_df <- terra::extract(wet1, vect(povsn), fun = mean, na.rm = TRUE)
# toc() #44 sec elapsed
# 
# head(wet_df)
# 
# povsn <- povsn |> 
#     mutate(ID = row_number()) |> 
#     left_join(wet_df |> rename(max_t1_2 = max_daily_tw_onepoint2_degrees))
# 
# povsn <- bi_class(povsn, x = GSAP2_poor, y = max_t1_2, dim = 3)
# 
# leg <- bi_legend(
#     pal = "DkViolet", dim = 3, 
#     xlab = "Poverty rate", ylab = "Max daily temperature", size = 5)
# 
# tic()
# ggsave(
#     plot = (povsn |> 
#                 ggplot() +
#                 geom_sf(aes(fill = bi_class), size = 0.01, color = "white", show.legend = FALSE) +
#                 bi_scale_fill(pal = "DkViolet", dim = 3) +
#                 annotation_custom(
#                     grob = ggplotGrob(leg), 
#                     xmin = -180, ymin = -50, xmax = -90, ymax = 0) +
#                 theme_void(base_size = 6)) ,
#     filename = "Temp1point2C_poverty.png", path = "figures/", 
#     device = "png", width = 6, height = 4, dpi = 400, bg = "white")
# toc() #20s
# 
# 
# plot(wet1)
# 
# ggplot(povsn) +
#     geom_sf(aes(fill = max_t1_2),, size = 0.01, color = "white") +
#     scico::scale_fill_scico()
#     theme_void()
# 
# 
