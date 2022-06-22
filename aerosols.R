## aerosols

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

ars <- rast("~/Documents/Projects/DATA/NASA_SEDAC/sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019-geotiff/sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019.tif")

pop <- rast("~/Documents/Projects/DATA/Population_density_NASA/gpw-v4-population-density-rev11_2020_15_min_tif/gpw_v4_population_density_rev11_2020_15_min.tif")

area <- cellSize(pop, unit = "km")
## adjust population: from people / km to number of people
pop_adj <- log1p(pop * area) # in logarithmic units for plotting

### Note: if we need barcharts, I'll need to work again with the aerosols and 
# population grids. Aerosols is at 0.01 degree grid, and population 0.25, which 
# means we need to come up with sensitive choices to aggregate aerosols to 1/4.

# ars25 <- aggregate(ars, fact = res(pop)[1]/res(ars)[1], fun = "mean",
#                    filename = "data/aerosols_025deg.tif")
ars25 <- terra::rast("data/aerosols_025deg.tif")
ars25 <- extend(ars25, pop_adj)



tic()
ggplot() +
    geom_spatraster(data = ars) +
    geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
    #geom_sf(data = world, size = 0.1, fill = NA, color = "gray10") +
    # scico::scale_fill_scico(
    #     "Mean PPM 2.5", palette = "vikO", na.value = "white",
    #     guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"), 
    #                            barheight = unit(2,"mm"))) +
    scale_fill_viridis_c(
        "PPM 2.5",option = "D", direction = 1, na.value = "white",
        guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"), 
                               barheight = unit(2,"mm"))) +
    labs(tag = "A") + lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
    theme_void(base_size = 6) +
    theme(legend.position = c(0.11, 0.1),
          legend.direction = "horizontal")
toc() #3s

## The new exposure map needs data frame objects
add(ars25) <- pop_adj # adds the population layer to the aerosols dataset

ars_df <- as.data.frame(ars25, xy = TRUE) |> as_tibble()

ars_df <- ars_df |> 
    rename(mean_ppm25 = `sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019`,
           pop2020 = gpw_v4_population_density_rev11_2020_15_min) 

## compute the color scale
ars_df <- bi_class(ars_df, x = mean_ppm25, y = pop2020, dim = 4, style = "quantile")


### Composed map for just:

povsn <- bi_class(povsn, x = GSAP2_poor, y = mean_ppm25, dim = 4, style = "quantile") 
# rename is not working, doing it in base.
names(povsn)[names(povsn) == "bi_class"] <- "bi_poverty"
breaksA <- bi_class_breaks(povsn, x = GSAP2_poor, y = mean_ppm25, dim = 4, 
                           style = "quantile", dig_lab = 1, split = TRUE)

# povsn <- bi_class(povsn, x = poplog, y = mean_ppm25, dim = 4, style = "quantile") 
# names(povsn)[names(povsn) == "bi_class"] <- "bi_pop"
breaksB <- bi_class_breaks(ars_df, x = mean_ppm25, y = pop2020, dim = 4, 
                           style = "quantile", dig_lab = 1, split = TRUE)

## get the number of people affected
tic()
ars_df2 <- terra::extract(
    ((ars25$`sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019` > 15) * (pop * area)), 
    vect(world), fun = sum, na.rm = TRUE) |> 
    rename(exposure_ppm25 = sdei.global.annual.gwr.pm2.5.modis.misr.seawifs.aod.v4.gl.03.2019) # people exposed
toc() #635.124 sec elapsed

pop_df <- terra::extract(
    (pop * area), 
    vect(world), fun = sum, na.rm = TRUE) |> 
    rename(pop2020 = gpw_v4_population_density_rev11_2020_15_min)

world <- world |> 
    mutate(ID = row_number()) |> 
    left_join(ars_df2) |> 
    left_join(pop_df ) |> 
    mutate(prop_exposed = exposure_ppm25 / pop2020)

da <- world |> 
    arrange(desc(exposure_ppm25)) |> 
    top_n(15, wt = exposure_ppm25) |> 
    mutate(name_long = str_replace(
        name_long, pattern = "Democratic Republic of the Congo", replacement = "DR Congo")) |> 
    mutate(name_long = as_factor(name_long) |> fct_rev()) |> 
    ggplot(aes(exposure_ppm25, name_long)) +
    geom_col() +
    labs(tag = "D", x = "Number of people exposed", y = "") +
    theme_minimal(base_size = 6)

db <-  world |> 
    arrange(desc(prop_exposed)) |> 
    top_n(15, wt = prop_exposed) |> 
    mutate(name_long = as_factor(name_long) |> fct_rev()) |> 
    ggplot(aes(prop_exposed, name_long)) +
    geom_col() +
    labs(tag = "E", x = "Proportion of population", y = "") +
    theme_minimal(base_size = 6)

d <- da/db

## ensamble
lyt <- '
AAAD
BBBD
CCCD'

tic()
ggsave(
    plot = (
        ((ggplot() +
             geom_spatraster(data = ars) +
             geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
             scale_fill_viridis_c(
                 "PPM 2.5",option = "D", direction = 1, na.value = "white",
                 guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"), 
                                        barheight = unit(2,"mm"))) +
             labs(tag = "A") + lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
             theme_void(base_size = 6) +
             theme(legend.position = c(0.11, 0.1),
                   legend.direction = "horizontal")) +
            (ggplot() +
                 geom_tile(data = ars_df, aes(x = x, y = y, fill = bi_class), show.legend = FALSE) +
                 geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
                 bi_scale_fill(pal = "BlueOr", dim = 4) +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "BlueOr", dim = 4, breaks = breaksB, arrows = FALSE,
                             ylab = "Population [log]", xlab = "Mean PPM 2.5",
                             flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                             theme(plot.margin = unit(rep(1,4),"mm"))
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
                 labs(tag = "B") +  lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
                 theme_void(base_size = 6)
                 ) +
            (povsn |> 
                 ggplot() +
                 geom_sf(aes(fill = bi_poverty), size = 0.01, color = "white", show.legend = FALSE) +
                 bi_scale_fill(pal = "DkViolet2", dim = 4) +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "DkViolet2", dim = 4,  breaks = breaksA, arrows = FALSE,
                             xlab = "Poverty", ylab = "Mean PPM 2.5",
                             flip_axes = FALSE, rotate_pal = FALSE, size = 4) +
                             theme(plot.margin = unit(rep(1,4),"mm"))
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
                 labs(tag = "C") +
                 theme_void(base_size = 6))) +
            (d) +
            plot_layout(design = lyt, ncol = 2, widths = c(2,1))
    ) ,
    filename = "just_aerosols.png", path = "figures/", 
    device = "png", width = 6, height = 5, dpi = 400, bg = "white")
toc() #90s





#### Left overs ####
# HDI will not be part of the paper.

hdi_shp <-  bi_class(hdi_shp, x = shdi, y = mean_ppm25, dim = 4, style = "quantile") 
names(hdi_shp)[names(hdi_shp) == "bi_class"] <- "bi_hdi" 
breaksC <- bi_class_breaks(hdi_shp, x = shdi, y = mean_ppm25, dim = 4, 
                           style = "quantile", dig_lab = 1, split = TRUE)


hdi_shp |> 
    ggplot() +
    geom_sf(aes(fill = bi_hdi), size = 0.01, color = "white", show.legend = FALSE) +
    bi_scale_fill(pal = "GrPink2", dim = 4) +
    annotation_custom(
        grob = ggplotGrob(
            bi_legend(
                pal = "GrPink2", dim = 4,  breaks = breaksC, arrows = FALSE,
                xlab = "HDI", ylab = "Mean PPM 2.5",
                flip_axes = FALSE, rotate_pal = FALSE, size = 4)+
                theme(plot.margin = unit(rep(1,4),"mm"))
        ), 
        xmin = -180, ymin = -60, xmax = -90, ymax = 10)+
    labs(tag = "D") +
    theme_void(base_size = 6)

povsn |> 
    ggplot() +
    geom_sf(aes(fill = bi_pop), size = 0.01, color = "white", show.legend = FALSE) +
    bi_scale_fill(pal = "BlueOr", dim = 4) +
    annotation_custom(
        grob = ggplotGrob(
            bi_legend(
                pal = "BlueOr", dim = 4, breaks = breaksA, arrows = FALSE,
                xlab = "Population", ylab = "Mean PPM 2.5",
                flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                theme(plot.margin = unit(rep(1,4),"mm"))
        ), 
        xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
    labs(tag = "B") +
    theme_void(base_size = 6)