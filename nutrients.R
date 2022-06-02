library(tidyverse)
library(fs)
library(sf)
library(spData)
library(terra)
library(biscale)
library(tictoc)
library(tidync)
library(patchwork)


## Datasets
load("data/subnational_poverty_vars.RData")
load("data/subnational_hdi_vars.RData")

nut <- rast("~/Documents/Projects/DATA/EC_additional_datasets/exc_nsur_crit_mi_all_ph.asc")
p <- rast("~/Documents/Projects/DATA/EC_additional_datasets/MH2018_Pconcn_runoffcutoff5_concngt100k.nc")


terra::plot(
    nut, axes = FALSE, plg = list(ext = c(-150,-145,-60,30), cex = 0.7, horiz = TRUE),
    grid = FALSE, col = viridisLite::viridis(n = 20, option = "D",direction = -1),
    mar = c(1,1,1,1)
) 


### Composed map for just:

povsn <- bi_class(povsn, x = GSAP2_poor, y = N_surplus, dim = 4, style = "quantile") 

# rename is not working, doing it in base.
names(povsn)[names(povsn) == "bi_class"] <- "bi_poverty"

povsn <- bi_class(povsn, x = population, y = N_surplus, dim = 4, style = "quantile") 
names(povsn)[names(povsn) == "bi_class"] <- "bi_pop"

hdi_shp <-  bi_class(hdi_shp, x = shdi, y = N_surplus, dim = 4, style = "quantile") 
names(hdi_shp)[names(hdi_shp) == "bi_class"] <- "bi_hdi" 


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
                         xlab = "Population", ylab = "Relative N surplus",
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
                             xlab = "Poverty", ylab = "Relative N surplus",
                             flip_axes = FALSE, rotate_pal = FALSE) +
                             bi_theme(base_family = "Helvetica", base_size = 4)
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 20) +
                 labs(tag = "B") +
                 theme_void(base_size = 6)) +
            (hdi_shp |> 
                 ggplot() +
                 geom_sf(aes(fill = bi_hdi), size = 0.01, color = "white", show.legend = FALSE) +
                 bi_scale_fill(pal = "GrPink2", dim = 4) +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "GrPink2", dim = 4, 
                             xlab = "HDI", ylab = "Relative N surplus",
                             flip_axes = FALSE, rotate_pal = FALSE) +
                             bi_theme(base_family = "Helvetica", base_size = 4)
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 20)+
                 labs(tag = "C") +
                 theme_void(base_size = 6)) +
            plot_layout(nrow = 3)
    ) ,
    filename = "just_nitrogen.png", path = "figures/", 
    device = "png", width = 4, height = 6, dpi = 400, bg = "white")
toc() #90s


## Composed map for Phosphorous


povsn <- bi_class(povsn, x = GSAP2_poor, y = P.concentration, dim = 4, style = "quantile") 

# rename is not working, doing it in base.
names(povsn)[names(povsn) == "bi_class"] <- "bi_poverty"

povsn <- bi_class(povsn, x = population, y = P.concentration, dim = 4, style = "quantile") 
names(povsn)[names(povsn) == "bi_class"] <- "bi_pop"

hdi_shp <-  bi_class(hdi_shp, x = shdi, y = P.concentration, dim = 4, style = "quantile") 
names(hdi_shp)[names(hdi_shp) == "bi_class"] <- "bi_hdi" 


tic()
ggsave(
    plot = (
        (povsn |> 
             ggplot() +
             geom_sf(aes(fill = bi_poverty), size = 0.01, color = "white", show.legend = FALSE) +
             bi_scale_fill(pal = "BlueOr", dim = 4) +
             annotation_custom(
                 grob = ggplotGrob(
                     bi_legend(
                         pal = "BlueOr", dim = 4, 
                         xlab = "Population", ylab = "P concentration",
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
                             xlab = "Poverty", ylab = "P concentration",
                             flip_axes = FALSE, rotate_pal = FALSE) +
                             bi_theme(base_family = "Helvetica", base_size = 4)
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 20) +
                 labs(tag = "B") +
                 theme_void(base_size = 6)) +
            (hdi_shp |> 
                 ggplot() +
                 geom_sf(aes(fill = bi_hdi), size = 0.01, color = "white", show.legend = FALSE) +
                 bi_scale_fill(pal = "GrPink2", dim = 4) +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "GrPink2", dim = 4, 
                             xlab = "Human Development Index", ylab = "P concentration",
                             flip_axes = FALSE, rotate_pal = FALSE) +
                             bi_theme(base_family = "Helvetica", base_size = 4)
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 20)+
                 labs(tag = "C") +
                 theme_void(base_size = 6)) +
            plot_layout(nrow = 3)
    ) ,
    filename = "just_phosphorous.png", path = "figures/", 
    device = "png", width = 4, height = 6, dpi = 400, bg = "white")
toc() #20s


