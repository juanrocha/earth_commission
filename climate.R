
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

## load processed datasets
load(file = "data/subnational_poverty_vars.RData") # 243Mb
# load(file = "data/subnational_hdi_vars.RData") # 557Mb

wet1 <- rast("~/Documents/Projects/DATA/EC_additional_datasets/max_daily_tw_onepoint2_degrees.tif") 
wet2 <- rast("~/Documents/Projects/DATA/EC_additional_datasets/max_daily_tw_two_degrees.tif")
wet2
plot(wet1)

mat <- rast("~/Documents/Projects/DATA/EC_additional_datasets/exposuregrid/Warming15C_mat29.tif")

wet1 <- rotate(wet1)
wet2 <- rotate(wet2)

pop <- rast("~/Documents/Projects/DATA/Population_density_NASA/gpw-v4-population-density-rev11_2020_15_min_tif/gpw_v4_population_density_rev11_2020_15_min.tif")

area <- cellSize(pop, unit = "km")
## adjust population: from people / km to number of people
pop_adj <- (pop * area)


tic()
ggplot() +
    geom_spatraster(data = wet1) +
    geom_sf(data = st_as_sf(coastsCoarse), linewidth = 0.1, color = "gray25") +
    #geom_sf(data = world, size = 0.1, fill = NA, color = "gray10") +
    # scico::scale_fill_scico(
    #     "Mean PPM 2.5", palette = "vikO", na.value = "white",
    #     guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"), 
    #                            barheight = unit(2,"mm"))) +
    scale_fill_viridis_c(
        "Wet bulb temperature",option = "D", direction = 1, na.value = "white",
        guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"), 
                               barheight = unit(2,"mm"))) +
    labs(tag = "A") + #lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
    theme_void(base_size = 6) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")
toc() #3s

## The new exposure map needs data frame objects
## pop_adj is at 0.25deg and wet1 is at 1.25deg, needs adjustment
pop_adj125 <- aggregate(pop_adj, fact = res(wet1)[1]/res(pop_adj)[1], fun = "sum")
pop_adj125 <- log1p(pop_adj125) # logarithmic scale for ploting
## MAT (mean average temp) is at 0.083deg, needs to be on the 0.25deg scale of pop
## The assumption is if there is pixel crossed at 0.083, that is a pixel crossed at 0.25
mat_025 <- aggregate(mat, fact = 3.5, fun = "max") # not sure why the fact does not work well, fixed manually

## correct extent
wet1 <- project(wet1, pop_adj125)
wet2 <- project(wet2, pop_adj125)
mat_025 <- project(mat_025, pop_adj)

add(wet1) <- pop_adj125 # adds the population layer to the dataset
add(wet2) <- pop_adj125
add(mat_025) <- pop_adj

wet1_df <- as.data.frame(wet1, xy = TRUE)|> as_tibble() |> 
    rename(maxT1 = max_daily_tw_onepoint2_degrees, pop2020 = gpw_v4_population_density_rev11_2020_15_min)
wet2_df <- as.data.frame(wet2, xy = TRUE)|> as_tibble()|> 
    rename(maxT2 = max_daily_tw_two_degrees, pop2020 = gpw_v4_population_density_rev11_2020_15_min)

mat_df <- as.data.frame(mat_025, xy = TRUE) |> as_tibble() |> 
    rename(pop2020 = gpw_v4_population_density_rev11_2020_15_min, cross_29C = Rowid) |> 
    mutate(cross_29C = cross_29C > 0.5, cross_29C = as.factor(cross_29C),
           poplog = log1p(pop2020)) # make sure it is dichotomous

wet1_df <- bi_class(wet1_df, x = maxT1, y = pop2020, dim = 4, style = "quantile")
wet2_df <- bi_class(wet2_df, x = maxT2, y = pop2020, dim = 4, style = "quantile")
mat_df <- bi_class(mat_df, x = cross_29C, y = poplog, dim = 2, style = "quantile")

brks <- bi_class_breaks(mat_df, x = cross_29C, y = poplog, dim = 2, style = "quantile",
                         dig_lab = 1, split = TRUE)


### Composed map for just:
## Because there is two temperature scenarions, I need to duplicate the color scale construction block
## For 1.2C
povsn <- bi_class(povsn, x = GSAP2_poor, y = max_t1_2, dim = 4, style = "quantile") 
names(povsn)[names(povsn) == "bi_class"] <- "bi_poverty"
breaksE <- bi_class_breaks(povsn, x = GSAP2_poor, y = max_t1_2, dim = 4, 
                           style = "quantile", dig_lab = 1, split = TRUE)
breaksC <- bi_class_breaks(wet1_df, x = maxT1, y = pop2020, dim = 4, 
                           style = "quantile", dig_lab = 1, split = TRUE)

# For 2C
povsn <- bi_class(povsn, x = GSAP2_poor, y = max_t2, dim = 4, style = "quantile") 
names(povsn)[names(povsn) == "bi_class"] <- "bi_poverty2"
breaksF <- bi_class_breaks(povsn, x = GSAP2_poor, y = max_t2, dim = 4, 
                           style = "quantile", dig_lab = 1, split = TRUE)
breaksD <- bi_class_breaks(wet2_df, x = maxT2, y = pop2020, dim = 4, 
                           style = "quantile", dig_lab = 1, split = TRUE)

breaksC$bi_x <- breaksC$bi_x[-c(1)] # remove NA
breaksD$bi_x <- breaksD$bi_x[-c(1)] 


## Population affected
## get the number of people affected
tic()
wet_df1 <- terra::extract(
    ((wet1$max_daily_tw_onepoint2_degrees > 32) * (expm1(wet1$gpw_v4_population_density_rev11_2020_15_min))),
    vect(world), fun = sum, na.rm = TRUE) |>
    rename(exposed = max_daily_tw_onepoint2_degrees) # people exposed
toc() #635.124 sec elapsed

tic()
wet_df2 <- terra::extract(
    ((wet2$max_daily_tw_two_degrees > 32) * (expm1(wet1$gpw_v4_population_density_rev11_2020_15_min))),
    vect(world), fun = sum, na.rm = TRUE) |>
    rename(exposed2 = max_daily_tw_two_degrees) # people exposed
toc() #635.124 sec elapsed

pop_df <- terra::extract(
    expm1(wet1$gpw_v4_population_density_rev11_2020_15_min), 
    vect(world), fun = sum, na.rm = TRUE) |> 
    rename(pop2020 = gpw_v4_population_density_rev11_2020_15_min)

world <- world |> 
    mutate(ID = row_number()) |> 
    left_join(wet_df1) |>
    left_join(wet_df2) |> 
    left_join(pop_df ) |> 
    mutate(prop_exposed1 = exposed / pop2020,
           prop_exposed2 = exposed2/ pop2020)


ga <- world |> 
    top_n(15, wt = exposed) |> 
    arrange(exposed) |> 
    mutate(name_long = str_replace(
        name_long, pattern = "Democratic Republic of the Congo", replacement = "DR Congo")) |> 
    mutate(name_long = as_factor(name_long)) |> #select(name_long, exposed)
    ggplot(aes((exposed/1e06), name_long)) +
    geom_col() +
    labs(y = "", x = "Number of people exposed\n[millions]", tag = "E") +
    theme_minimal(base_size = 6)

gb <- world |> 
    top_n(15, wt = prop_exposed1) |> 
    arrange(prop_exposed1) |> 
    mutate(name_long = str_replace(
        name_long, pattern = "Democratic Republic of the Congo", replacement = "DR Congo")) |> 
    mutate(name_long = as_factor(name_long)) |> 
    ggplot(aes((prop_exposed1), name_long)) +
    geom_col() +
    labs(y = "", x = "Proportion exposed", tag = "F") +
    theme_minimal(base_size = 6)

fa <- world |> 
    top_n(15, wt = exposed2) |> 
    arrange(exposed2) |> 
    mutate(name_long = str_replace(
        name_long, pattern = "Democratic Republic of the Congo", replacement = "DR Congo")) |> 
    mutate(name_long = as_factor(name_long)) |> #select(name_long, exposed)
    ggplot(aes((exposed2/1e06), name_long)) +
    geom_col() +
    labs(y = "", x = "Number of people exposed\n[millions]", tag = "G") +
    theme_minimal(base_size = 6)

fb <- world |> 
    top_n(15, wt = prop_exposed2) |> 
    arrange(prop_exposed2) |> 
    mutate(name_long = str_replace(
        name_long, pattern = "Democratic Republic of the Congo", replacement = "DR Congo")) |> 
    mutate(name_long = as_factor(name_long)) |> 
    ggplot(aes((prop_exposed2), name_long)) +
    geom_col() +
    labs(y = "", x = "Proportion exposed", tag = "H") +
    theme_minimal(base_size = 6)

## Combine them all
## J220622: Tim requests deleting the top two maps with the safe proxies. Adding also population aggregates.
tic()
ggsave(
    plot = (
        # If new file comes, this could be number of days above threshold per year.
        # (ggplot() +
        #      geom_spatraster(data = wet1$max_daily_tw_onepoint2_degrees) +
        #      geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
        #      scale_fill_viridis_c(
        #          "Maximum wet bulb\ntemperature",option = "D", direction = 1, na.value = "white",
        #          guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"), 
        #                                 barheight = unit(2,"mm"))) +
        #      labs(tag = "A") + #lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
        #      theme_void(base_size = 6) +
        #      theme(legend.position = "bottom",
        #            legend.direction = "horizontal")) +
        # (ggplot() +
        #          geom_spatraster(data = wet2$max_daily_tw_two_degrees) +
        #          geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
        #          scale_fill_viridis_c(
        #              "Maximum wet bulb\ntemperature",option = "D", direction = 1, na.value = "white",
        #              guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"), 
        #                                     barheight = unit(2,"mm"))) +
        #          labs(tag = "B") + #lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
        #          theme_void(base_size = 6) +
        #          theme(legend.position = "bottom",
        #                legend.direction = "horizontal")) +
        (ggplot() +
                 geom_tile(data = wet1_df, aes(x = x, y = y, fill = bi_class), show.legend = FALSE) +
                 geom_sf(data = st_as_sf(coastsCoarse), linewidth = 0.1, color = "gray25") +
                 bi_scale_fill(pal = "BlueOr", dim = 4, na.value = "white") +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "BlueOr", dim = 4, breaks = breaksC, arrows = FALSE,
                             ylab = "Population [log]", xlab = "Maximum wet bulb\ntemperature",
                             flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                             theme(plot.margin = unit(rep(1,4),"mm"))
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
                 labs(tag = "A") +  lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
                 theme_void(base_size = 6) ) +
        (ggplot() +
                 geom_tile(data = wet2_df, 
                           aes(x = x, y = y, fill = bi_class), show.legend = FALSE) +
                 geom_sf(data = st_as_sf(coastsCoarse), linewidth = 0.1, color = "gray25") +
                 bi_scale_fill(pal = "BlueOr", dim = 4,  na.value = "white") +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "BlueOr", dim = 4, breaks = breaksD, arrows = FALSE,
                             ylab = "Population [log]", xlab = "Maximum wet bulb\ntemperature",
                             flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                             theme(plot.margin = unit(rep(1,4),"mm"))
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
                 labs(tag = "B") +  lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
                 theme_void(base_size = 6) ) +
        (povsn |> 
                 ggplot() +
                 geom_sf(aes(fill = bi_poverty), linewidth = 0.01, color = "white", show.legend = FALSE) +
                 bi_scale_fill(pal = "DkViolet2", dim = 4) +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "DkViolet2", dim = 4, breaks = breaksE, arrows = FALSE,
                             xlab = "Poverty rate", ylab = "Maximum wet bulb\ntemperature",
                             flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                             theme(plot.margin = unit(rep(1,4),"mm"))
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
                 labs(tag = "C") +
                 theme_void(base_size = 6)) +
        (povsn |> 
                 ggplot() +
                 geom_sf(aes(fill = bi_poverty2), linewidth = 0.01, color = "white", show.legend = FALSE) +
                 bi_scale_fill(pal = "DkViolet2", dim = 4) +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "DkViolet2", dim = 4, breaks = breaksF, arrows = FALSE,
                             xlab = "Poverty rate", ylab ="Maximum wet bulb\ntemperature",
                             flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                             theme(plot.margin = unit(rep(1,4),"mm"))
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
                 labs(tag = "D") +
                 theme_void(base_size = 6)) +
            plot_layout(nrow = 2, ncol = 2, byrow = TRUE)
    ) / (ga+gb +fa+fb + plot_layout(nrow = 1, ncol = 4, byrow = TRUE)) +
        plot_layout(heights = c(3,1)),
    filename = "just_climate.pdf", path = "figures/", 
    device = "pdf", width = 7, height = 5, dpi = 300, bg = "white")
toc() #204s

ggsave(
    ga+gb +fa+fb + plot_layout(nrow = 1, ncol = 4, byrow = TRUE) ,
    filename = "just_climate_temp_b.pdf", path = "figures/", 
    device = "pdf", width = 7, height = 1.5, dpi = 400, bg = "white"
)

## Exposed people to MAT > 29C
ggplot() +
    geom_tile(data = mat_df, aes(x = x, y = y, fill = bi_class), show.legend = FALSE) +
    geom_sf(data = st_as_sf(coastsCoarse), linewidth = 0.1, color = "gray25") +
    bi_scale_fill(pal = "BlueOr", dim = 4) +
    annotation_custom(
        grob = ggplotGrob(
            bi_legend(
                pal = "BlueOr", dim = 2,arrows = FALSE,
                ylab = "Population [log]", xlab = "Max daily temperature",
                flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                theme(plot.margin = unit(rep(1,4),"mm"))
        ), 
        xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
    labs(tag = "A") +  lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
    theme_void(base_size = 6)

## get the number of people affected
tic()
mat_df2 <- terra::extract(
    ((mat_025$Rowid>0.5) * (pop_adj)),
    vect(world), fun = sum, na.rm = TRUE) |>
    rename(exposed = Rowid) # people exposed
toc() #635.124 sec elapsed

pop_df <- terra::extract(
    (pop * area), 
    vect(world), fun = sum, na.rm = TRUE) |> 
    rename(pop2020 = gpw_v4_population_density_rev11_2020_15_min)

world <- world |> 
    mutate(ID = row_number()) |> 
    left_join(mat_df2) |> 
    left_join(pop_df ) |> 
    mutate(prop_exposed = exposed / pop2020)

ba <- world |> 
    top_n(15, wt = exposed) |> 
    arrange(exposed) |> 
    mutate(name_long = as_factor(name_long)) |> 
    ggplot(aes((exposed/1e06), name_long)) +
    geom_col() +
    labs(y = "", x = "Number of people exposed\n[millions]", tag = "B") +
    theme_minimal(base_size = 6)

bb <- world |> 
    top_n(15, wt = prop_exposed) |> 
    arrange(prop_exposed) |> 
    mutate(name_long = as_factor(name_long)) |> 
    ggplot(aes((prop_exposed), name_long)) +
    geom_col() +
    labs(y = "", x = "Proportion exposed", tag = "C") +
    theme_minimal(base_size = 6)

tic()
ggsave(
    plot = (
        (ggplot() +
            geom_tile(data = mat_df, aes(x = x, y = y, fill = bi_class), show.legend = FALSE) +
            geom_sf(data = st_as_sf(coastsCoarse), linewidth = 0.1, color = "gray25") +
            bi_scale_fill(pal = "BlueOr", dim = 2) +
            annotation_custom(
                grob = ggplotGrob(
                    bi_legend(
                        pal = "BlueOr", dim = 2,arrows = FALSE,
                        ylab = "Population [log]", xlab = ">29C",
                        flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                        theme(plot.margin = unit(rep(1,4),"mm"))
                ), 
                xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
            labs(tag = "A") +  lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
            theme_void(base_size = 6)) +
            (ba + bb) + plot_layout(ncol = 2)
        ),
    filename = "just_climate_mat.pdf", path = "figures/", 
    device = "pdf", width = 7, height = 2, dpi = 400, bg = "white")
toc()

#### old code + leftovers ####
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
# hdi_shp <-  bi_class(hdi_shp, x = shdi, y = max_t1_2, dim = 4, style = "quantile") 
# names(hdi_shp)[names(hdi_shp) == "bi_class"] <- "bi_hdi" 


hdi_shp <-  bi_class(hdi_shp, x = shdi, y = max_t2, dim = 4, style = "quantile") 
names(hdi_shp)[names(hdi_shp) == "bi_class"] <- "bi_hdi2"

(povsn |> 
        ggplot() +
        geom_sf(aes(fill = bi_pop), linewidth = 0.01, color = "white", show.legend = FALSE) +
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
        theme_void(base_size = 5)) +
    
    (hdi_shp |> 
         ggplot() +
         geom_sf(aes(fill = bi_hdi), linewidth = 0.01, color = "white", show.legend = FALSE) +
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
         theme_void(base_size = 5)) +
    
    (hdi_shp |> 
         ggplot() +
         geom_sf(aes(fill = bi_hdi), linewidth = 0.01, color = "white", show.legend = FALSE) +
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
         theme_void(base_size = 5)) +
    
    (povsn |> 
         ggplot() +
         geom_sf(aes(fill = bi_pop2), linewidth = 0.01, color = "white", show.legend = FALSE) +
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
         theme_void(base_size = 5)) +