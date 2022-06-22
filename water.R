library(tidyverse)
library(fs)
library(sf)
library(spData)
library(terra)
library(biscale)
library(tictoc)
library(tidync)
library(patchwork)
library(tidyterra)

library(rworldmap)
data("coastsCoarse")


## Datasets
load("data/subnational_poverty_vars.RData")
#load("data/subnational_hdi_vars.RData")

wtr <- rast("~/Documents/Projects/DATA/EC_additional_datasets/RiskMap_13 May 2022.tif")
# cubic km


## Number of months per year outside the 20% boundary: aggregate average number of months outside the boundary per year
estress <- rast("~/Documents/Projects/DATA/EC_additional_datasets/Estress_WBM_TerraClimate_2000-2020_LTM_estress_chg20.tif")
est <- st_read("~/Documents/Projects/DATA/EC_additional_datasets/HydroSTN30_Subbasin_ESTRESS_Chg20/HydroSTN30_Subbasin_ESTRESS_Chg20.shp")

## population
pop <- rast("~/Documents/Projects/DATA/Population_density_NASA/gpw-v4-population-density-rev11_2020_15_min_tif/gpw_v4_population_density_rev11_2020_15_min.tif")

area <- cellSize(pop, unit = "km")
## adjust population: from people / km to number of people
pop_adj <- (pop * area)



### Composed map for just:
## Because there is two water variables, I need to duplicate the color scale construction block
## For risk_volume
povsn <- bi_class(povsn, x = GSAP2_poor, y = risk_volume, dim = 4, style = "quantile") 

# rename is not working, doing it in base.
names(povsn)[names(povsn) == "bi_class"] <- "bi_poverty"
breaksA <- bi_class_breaks(povsn, x = GSAP2_poor, y = risk_volume, dim = 4, 
                           style = "quantile", dig_lab = 1, split = TRUE)

povsn <- bi_class(povsn, x = poplog, y = risk_volume, dim = 4, style = "quantile") 
names(povsn)[names(povsn) == "bi_class"] <- "bi_pop"
breaksB <- bi_class_breaks(povsn, x = poplog, y = risk_volume, dim = 4, 
                           style = "quantile", dig_lab = 1, split = TRUE)


# For num_months
povsn <- bi_class(povsn, x = GSAP2_poor, y = num_months, dim = 4, style = "jenks") 
names(povsn)[names(povsn) == "bi_class"] <- "bi_poverty2"
breaksD <- bi_class_breaks(povsn, x = GSAP2_poor, y = num_months, dim = 4, 
                           style = "jenks", dig_lab = 1, split = TRUE)

povsn <- bi_class(povsn, x = poplog, y = num_months, dim = 4, style = "jenks") 
names(povsn)[names(povsn) == "bi_class"] <- "bi_pop2"
breaksE <- bi_class_breaks(povsn, x = poplog, y = num_months, dim = 4, 
                           style = "jenks", dig_lab = 1, split = TRUE)


## correct manually
bi_class_breaks(povsn, x = GSAP2_poor, y = risk_volume, dim = 4, 
                style = "quantile", dig_lab = 1, split = FALSE)
breaksA$bi_y <- breaksA$bi_y[-1]
breaksB$bi_y <- breaksB$bi_y[-1]
breaksA$bi_y[1] <- -200
breaksA$bi_y[2] <- -6

breaksB$bi_y[1] <- -200
breaksB$bi_y[2] <- -6


breaksB

## test individual variables
tic()
ggplot() +
    geom_spatraster(data = wtr) +
    geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
    # scale_fill_viridis_c(
    #     "Risk volume",option = "D", direction = 1, na.value = "white",
    #     guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"),
    #                            barheight = unit(2,"mm"))) +
    # scale_fill_gradient2(
    #     "Risk volume",  na.value = "white", midpoint = 0,
    #     guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"),
    #                            barheight = unit(2,"mm"))) +
    scico::scale_fill_scico(
        "Risk volume",  na.value = "white", midpoint = 0, palette = "vik",
        guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"),
                               barheight = unit(2,"mm"))) +
    labs(tag = "A") + lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
    theme_void(base_size = 6) +
    theme(legend.position = c(0.11, 0.1),
          legend.direction = "horizontal")
toc() #3s

ggplot() +
    geom_spatraster(data = estress) +
    geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
    scico::scale_fill_scico(
        "Number of months with\n +/-20% flow alteration",  na.value = "white", 
        palette = "bamako", direction = -1,
        guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"),
                               barheight = unit(2,"mm"))) +
    labs(tag = "A") + lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
    theme_void(base_size = 6) +
    theme(legend.position = c(0.11, 0.1),
          legend.direction = "horizontal")

ggplot() +
    geom_sf(data = est, aes(color = PointSampl)) +
    scale_color_viridis_c(
        "Number of months with\n +/-20% flow alteration",option = "D", direction = 1, na.value = "white",
        guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"), 
                               barheight = unit(2,"mm"))) +
    geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
    labs(tag = "A") + lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
    theme_void(base_size = 6)+
    theme(legend.position = c(0.11, 0.1),
          legend.direction = "horizontal")

## Combine them all
tic()
ggsave(
    plot = (
        (ggplot() +
             geom_sf(data = est, aes(color = PointSampl), size = 0.1) +
             scale_color_viridis_c(
                 "Number of months with\n +/-20% flow alteration",
                 option = "D", direction = 1, na.value = "white",
                 guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"), 
                                        barheight = unit(2,"mm"))) +
             geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
             labs(tag = "A") + lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
             theme_void(base_size = 6)+
             theme(legend.position = c(0.15, 0.15),
                   legend.direction = "horizontal"))+
        (ggplot() +
             geom_spatraster(data = wtr) +
             geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
             scico::scale_fill_scico(
                 "Trend in ground\nwater depth",  na.value = "white", midpoint = 0, palette = "vik",
                 guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"),
                                        barheight = unit(2,"mm"))) +
             labs(tag = "B") + lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
             theme_void(base_size = 6) +
             theme(legend.position = c(0.15, 0.15),
                   legend.direction = "horizontal")) +
            
            (povsn |> 
                 ggplot() +
                 geom_sf(aes(fill = bi_pop2), size = 0.01, color = "white", show.legend = FALSE) +
                 bi_scale_fill(pal = "BlueOr", dim = 4) +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "BlueOr", dim = 4, arrows = FALSE, breaks = breaksD,
                             xlab = "Population [log]", ylab = "Months with +/-20% flow",
                             flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                             theme(plot.margin = unit(rep(1,4),"mm"))
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
                 labs(tag = "C") +
                 theme_void(base_size = 5)) +
            (povsn |> 
                 ggplot() +
                 geom_sf(aes(fill = bi_pop), size = 0.01, color = "white", show.legend = FALSE) +
                 bi_scale_fill(pal = "BlueOr", dim = 4) +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "BlueOr", dim = 4,  arrows = FALSE, breaks = breaksB,
                             xlab = "Population [log]", ylab = "Trend in ground\nwater depth",
                             flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                             theme(plot.margin = unit(rep(1,4),"mm"))
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
                 labs(tag = "D") +
                 theme_void(base_size = 5)) +
            (povsn |> 
                 ggplot() +
                 geom_sf(aes(fill = bi_poverty2), size = 0.01, color = "white", show.legend = FALSE) +
                 bi_scale_fill(pal = "DkViolet2", dim = 4) +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "DkViolet2", dim = 4, arrows = FALSE, breaks = breaksE,
                             xlab = "Poverty", ylab = "Months with +/-20% flow",
                             flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                             theme(plot.margin = unit(rep(1,4),"mm"))
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
                 labs(tag = "E") +
                 theme_void(base_size = 5)) +
            (povsn |> 
                 ggplot() +
                 geom_sf(aes(fill = bi_poverty), size = 0.01, color = "white", show.legend = FALSE) +
                 bi_scale_fill(pal = "DkViolet2", dim = 4) +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "DkViolet2", dim = 4, arrows = FALSE, breaks = breaksA,
                             xlab = "Poverty", ylab = "Trend in ground\nwater depth",
                             flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                             theme(plot.margin = unit(rep(1,4),"mm"))
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
                 labs(tag = "F") +
                 theme_void(base_size = 5)) +
            plot_layout(nrow = 3, ncol = 2, byrow = TRUE)
    ) ,
    filename = "just_water.png", path = "figures/", 
    device = "png", width = 7, height = 6, dpi = 400, bg = "white")
toc() #209s




#### Leftovers ####

(hdi_shp |> 
     ggplot() +
     geom_sf(aes(fill = bi_hdi), size = 0.01, color = "white", show.legend = FALSE) +
     bi_scale_fill(pal = "GrPink2", dim = 4) +
     annotation_custom(
         grob = ggplotGrob(
             bi_legend(
                 pal = "GrPink2", dim = 4, arrows = FALSE, breaks = breaksC,
                 xlab = "HDI", ylab = "Risk volume",
                 flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                 theme(plot.margin = unit(rep(1,4),"mm"))
         ), 
         xmin = -180, ymin = -60, xmax = -90, ymax = 10)+
     labs(tag = "D") +
     theme_void(base_size = 5)) 

(hdi_shp |> 
        ggplot() +
        geom_sf(aes(fill = bi_hdi), size = 0.01, color = "white", show.legend = FALSE) +
        bi_scale_fill(pal = "GrPink2", dim = 4) +
        annotation_custom(
            grob = ggplotGrob(
                bi_legend(
                    pal = "GrPink2", dim = 4, arrows = FALSE, breaks = breaksF,
                    xlab = "HDI", ylab = "Months with +/-20% flow",
                    flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                    theme(plot.margin = unit(rep(1,4),"mm"))
            ), 
            xmin = -180, ymin = -60, xmax = -90, ymax = 10)+
        labs(tag = "H") +
        theme_void(base_size = 5)) 





(ggplot() +
        geom_spatraster(data = estress) +
        geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
        scico::scale_fill_scico(
            "Number of months with\n +/-20% flow alteration",  na.value = "white", 
            palette = "bamako", direction = -1,
            guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"),
                                   barheight = unit(2,"mm"))) +
        labs(tag = "E") + lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
        theme_void(base_size = 6) +
        theme(legend.position = c(0.15, 0.15),
              legend.direction = "horizontal")) 

hdi_shp <-  bi_class(hdi_shp, x = shdi, y = risk_volume, dim = 4, style = "quantile") 
names(hdi_shp)[names(hdi_shp) == "bi_class"] <- "bi_hdi" 
breaksC <- bi_class_breaks(hdi_shp, x = shdi, y = risk_volume, dim = 4, 
                           style = "quantile", dig_lab = 1, split = TRUE)


hdi_shp <-  bi_class(hdi_shp, x = shdi, y = num_months, dim = 4, style = "jenks") 
names(hdi_shp)[names(hdi_shp) == "bi_class"] <- "bi_hdi2"
breaksF <- bi_class_breaks(hdi_shp, x = shdi, y = num_months, dim = 4, 
                           style = "jenks", dig_lab = 1, split = TRUE)

# clean_breaks <- function(x) {
#     ifelse(x$bi_y |> is.na() |> any(),
#            x$bi_y <- x$bi_y[-1], #remove the NA
#            x$bi_y)
#     return(x)
# }
# 
# breaksA <- clean_breaks(breaksA)
# breaksB <- clean_breaks(breaksB)
# breaksC <- clean_breaks(breaksC)
# breaksD <- clean_breaks(breaksD)
# breaksE <- clean_breaks(breaksE)
# breaksF <- clean_breaks(breaksF)