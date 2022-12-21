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
# load("data/subnational_hdi_vars.RData")

nut <- rast("~/Documents/Projects/DATA/EC_additional_datasets/exc_nsur_crit_mi_all_ph.asc")
p <- rast("~/Documents/Projects/DATA/EC_additional_datasets/MH2018_Pconcn_runoffcutoff5_concngt10k.nc")

hpx <- readxl::read_xls(path = "~/Downloads/wri_eutrophic_hypoxic_dataset_2011-03.xls") |> 
    janitor::clean_names()

pop <- rast("~/Documents/Projects/DATA/Population_density_NASA/gpw-v4-population-density-rev11_2020_15_min_tif/gpw_v4_population_density_rev11_2020_15_min.tif")

area <- cellSize(pop, unit = "km")
## adjust population: from people / km to number of people
pop_adj <- (pop * area)

## get number of people affected.\
## ## The new exposure map needs data frame objects
## pop_adj is at 0.25deg and nut is at 0.5deg, needs adjustment
pop_adj05 <- aggregate(pop_adj, fact = res(nut)[1]/res(pop_adj)[1], fun = "sum")
pop_adj05 <- log1p(pop_adj05)
p025 <- aggregate(p, fact = (res(pop_adj)[1]/res(p)[1]), fun = "mean", na.rm = TRUE)

## correct extends
ext(nut) == ext(pop_adj05)
ext(p025) == ext(pop_adj)

p025 <- project(p025, pop_adj)

## Add the population layer
add(nut) <- pop_adj05
add(p025) <- log1p(pop_adj)

n_df <- as.data.frame(nut, xy = TRUE) |> as_tibble() |> 
    rename(n_surplus = exc_nsur_crit_mi_all_ph, 
           people = gpw_v4_population_density_rev11_2020_15_min)
p_df <- as.data.frame(p025, xy = TRUE) |> as_tibble() |> 
    rename(p_concentration = `P concentration`, 
           people = gpw_v4_population_density_rev11_2020_15_min)

n_df <- bi_class(n_df, x = n_surplus, y = people, dim = 4, style = "quantile")
p_df <- bi_class(p_df, x = p_concentration, people, dim = 4, style = "quantile")

#### Nitrogen ####
tic()
ggplot() +
    geom_spatraster(data = nut$exc_nsur_crit_mi_all_ph) +
    geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
    scico::scale_fill_scico(
        "N surplus", palette = "vikO", na.value = "white",
        guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"), 
                               barheight = unit(2,"mm"))) +
    geom_point(data = hpx |> rename(x = long, y = lat), 
               aes(x,y), color = "orange", size = 0.25, alpha = 0.25) +
    labs(tag = "A") + lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
    theme_void(base_size = 6) +
    theme(legend.position = c(0.11, 0.1),
          legend.direction = "horizontal")
toc() #3s

## calculate bar plots
## get the number of people affected
tic()
n_df2 <- terra::extract(
    ((nut$exc_nsur_crit_mi_all_ph > 0) * (expm1(nut$gpw_v4_population_density_rev11_2020_15_min))),
    vect(world), fun = sum, na.rm = TRUE) |>
    rename(exposed = exc_nsur_crit_mi_all_ph) # people exposed
toc() #635.124 sec elapsed

pop_df <- terra::extract(
    expm1(nut$gpw_v4_population_density_rev11_2020_15_min), 
    vect(world), fun = sum, na.rm = TRUE) |> 
    rename(pop2020 = gpw_v4_population_density_rev11_2020_15_min)

world <- world |> 
    mutate(ID = row_number()) |> 
    left_join(n_df2) |> 
    left_join(pop_df ) |> 
    mutate(prop_exposed = exposed / pop2020)

ba <- world |> 
    top_n(15, wt = exposed) |> 
    arrange(exposed) |> 
    mutate(name_long = as_factor(name_long)) |> #select(name_long, exposed)
    ggplot(aes((exposed/1e06), name_long)) +
    geom_col() +
    labs(y = "", x = "Number of people exposed\n[millions]", tag = "D") +
    theme_minimal(base_size = 6)

bb <- world |> 
    top_n(15, wt = prop_exposed) |> 
    arrange(prop_exposed) |> 
    mutate(name_long = as_factor(name_long)) |> 
    ggplot(aes((prop_exposed), name_long)) +
    geom_col() +
    labs(y = "", x = "Proportion exposed", tag = "E") +
    theme_minimal(base_size = 6)

### Composed map for just:

povsn <- bi_class(povsn, x = GSAP2_poor, y = N_surplus, dim = 4, style = "quantile") 
# rename is not working, doing it in base.
names(povsn)[names(povsn) == "bi_class"] <- "bi_poverty"
breaksA <- bi_class_breaks(povsn, x = GSAP2_poor, y = N_surplus, dim = 4, 
                           style = "quantile", dig_lab = 1, split = TRUE)

# povsn <- bi_class(povsn, x = poplog, y = N_surplus, dim = 4, style = "quantile") 
# names(povsn)[names(povsn) == "bi_class"] <- "bi_pop"
breaksB <- bi_class_breaks(n_df, x = n_surplus, y = people, dim = 4, 
                           style = "quantile", dig_lab = 1, split = TRUE)
#Correct breaks manually
breaksA$bi_y <- breaksA$bi_y[-1]
breaksA$bi_y[1] <- -1 * breaksA$bi_y[1]

breaksB$bi_x <- c(-400, -9, 3, 30, 400) # correcting manually

lyt <- '
AAAD
BBBD
CCCD'

tic()
ggsave(
    plot = (
        (ggplot() +
             geom_spatraster(data = nut$exc_nsur_crit_mi_all_ph) +
             geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
             scico::scale_fill_scico(
                 "Excess N surplus", palette = "vikO", na.value = "white",
                 guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"), 
                                        barheight = unit(2,"mm"))) +
             geom_point(data = hpx |> rename(x = long, y = lat), 
                        aes(x,y), color = "orange", size = 0.25, alpha = 0.25) +
             labs(tag = "A") + lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
             theme_void(base_size = 6) +
             theme(legend.position = c(0.11, 0.1),
                   legend.direction = "horizontal")) +
        (ggplot() +
             geom_tile(data = n_df, aes(x = x, y = y, fill = bi_class), show.legend = FALSE) +
             geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
             bi_scale_fill(pal = "BlueOr", dim = 4) +
             annotation_custom(
                 grob = ggplotGrob(
                     bi_legend(
                         pal = "BlueOr", dim = 4, breaks = breaksB, arrows = FALSE,
                         ylab = "Population [log]", xlab = "Excess N surplus",
                         flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                         theme(plot.margin = unit(rep(1,4),"mm"))
                 ), 
                 xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
             labs(tag = "B") +  lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
             theme_void(base_size = 6)) +
            (povsn |> 
                 ggplot() +
                 geom_sf(aes(fill = bi_poverty), size = 0.01, color = "white", show.legend = FALSE) +
                 bi_scale_fill(pal = "DkViolet2", dim = 4) +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "DkViolet2", dim = 4,  breaks = breaksA, arrows = FALSE,
                             xlab = "Poverty", ylab = "Excess N surplus",
                             flip_axes = FALSE, rotate_pal = FALSE, size = 4) +
                             theme(plot.margin = unit(rep(1,4),"mm"))
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
                 labs(tag = "C") +
                 theme_void(base_size = 6)) +
            (ba / bb) +
            plot_layout(design = lyt, ncol = 2, widths = c(2,1))
    ) ,
    filename = "just_nitrogen.png", path = "figures/", 
    device = "png", width = 6, height = 5, dpi = 400, bg = "white")
toc() #23s


#### Phosphorous ####
# P.concentration needs transformation
ggplot(povsn, aes(P.concentration)) + geom_density() + scale_x_log10()

povsn <- povsn |> 
    mutate(p_log = log1p(P.concentration))

ggplot() +
    geom_spatraster(data = p) +
    scale_fill_continuous(
        "P concentration",type = "viridis", trans = "log1p",  na.value = "white",
        breaks = c(0, 100, 1000, 10000),
        guide = guide_colorbar(title.position = "top", barwidth = unit(2,"mm"), 
                               barheight = unit(20,"mm"))) +
    geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
    labs(tag = "A") + lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
    theme_void(base_size = 6) +
    theme(legend.position = c(0.1, 0.3), legend.text = element_text(size = 4),
          legend.direction = "vertical")

## calculate people exposed
## get the number of people affected
tic()
p_df2 <- terra::extract(
    ((p025$`P concentration` > 50) * (expm1(p025$gpw_v4_population_density_rev11_2020_15_min))),
    vect(world), fun = sum, na.rm = TRUE) |>
    rename(exposed = P.concentration) # people exposed
toc() #635.124 sec elapsed

pop_df <- terra::extract(
    expm1(p025$gpw_v4_population_density_rev11_2020_15_min), 
    vect(world), fun = sum, na.rm = TRUE) |> 
    rename(pop2020 = gpw_v4_population_density_rev11_2020_15_min)


world <- world |> select(-exposed, -prop_exposed, -pop2020)

world <- world |> 
    mutate(ID = row_number()) |> 
    left_join(p_df2) |> 
    left_join(pop_df ) |> 
    mutate(prop_exposed = exposed / pop2020)

ba <- world |> 
    top_n(15, wt = exposed) |> 
    arrange(exposed) |> 
    mutate(name_long = as_factor(name_long)) |> #select(name_long, exposed)
    ggplot(aes((exposed/1e06), name_long)) +
    geom_col() +
    labs(y = "", x = "Number of people exposed\n[millions]", tag = "D") +
    theme_minimal(base_size = 6)

bb <- world |> 
    top_n(15, wt = prop_exposed) |> 
    arrange(prop_exposed) |> 
    mutate(name_long = as_factor(name_long)) |> 
    ggplot(aes((prop_exposed), name_long)) +
    geom_col() +
    labs(y = "", x = "Proportion exposed", tag = "E") +
    theme_minimal(base_size = 6)



## Composed map for Phosphorous
povsn <- bi_class(povsn, x = GSAP2_poor, y = p_log, dim = 4, style = "quantile") 
# rename is not working, doing it in base.
names(povsn)[names(povsn) == "bi_class"] <- "bi_poverty2"
breaksA <- bi_class_breaks(povsn, x = GSAP2_poor, y = p_log, dim = 4, 
                           style = "quantile", dig_lab = 1, split = TRUE)

breaksB <- bi_class_breaks(p_df, x = p_concentration, people, dim = 4, 
                           style = "quantile", dig_lab = 1, split = TRUE)

## tune the inner legend
ggplot() + geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
    geom_point(data = tibble(x = -90,y = 10), aes(x,y), color = "red")

tic()
ggsave(
    plot = (
        (ggplot() +
             geom_spatraster(data = p) +
             scale_fill_continuous(
                 "P concentration",type = "viridis", trans = "log1p",  na.value = "white",
                 breaks = c(0, 100, 1000, 10000, 50000),
                 guide = guide_colorbar(title.position = "top", barwidth = unit(1,"mm"), 
                                        barheight = unit(10,"mm"))) +
             geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
             geom_point(data = hpx |> rename(x = long, y = lat), 
                        aes(x,y), fill = "orange", color = "orange", size = 0.05, alpha = 0.25) +
             labs(tag = "A") + lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
             theme_void(base_size = 6) +
             theme(legend.position = c(0.1, 0.25), legend.text = element_text(size = 4),
                   legend.title = element_text(size = 4), legend.direction = "vertical")) +
        (ggplot() +
             geom_tile(data = p_df, aes(x = x, y = y, fill = bi_class), show.legend = FALSE) +
             geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
             bi_scale_fill(pal = "BlueOr", dim = 4) +
             annotation_custom(
                 grob = ggplotGrob(
                     bi_legend(
                         pal = "BlueOr", dim = 4, breaks = breaksB, arrows = FALSE,
                         ylab = "Population [log]", xlab = "P concentration",
                         flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                         theme(plot.margin = unit(rep(1,4),"mm"))
                 ), 
                 xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
             labs(tag = "B") +  lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
             theme_void(base_size = 6)) +
            (povsn |> 
                 ggplot() +
                 geom_sf(aes(fill = bi_poverty2), size = 0.01, color = "white", show.legend = FALSE) +
                 bi_scale_fill(pal = "DkViolet2", dim = 4) +
                 annotation_custom(
                     grob = ggplotGrob(
                         bi_legend(
                             pal = "DkViolet2", dim = 4,  breaks = breaksA, arrows = FALSE,
                             xlab = "Poverty", ylab = "P concentration",
                             flip_axes = FALSE, rotate_pal = FALSE, size = 4) +
                             theme(plot.margin = unit(rep(1,4),"mm"))
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
                 labs(tag = "C") +
                 theme_void(base_size = 6)) +
            (ba/bb) +
            plot_layout(design = lyt, ncol = 2, widths = c(2,1))
    ) ,
    filename = "just_phosphorous.png", path = "figures/", 
    device = "png", width = 6, height = 5, dpi = 400, bg = "white")
toc() #87s


### Leftovers
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
# 
# hdi_shp <-  bi_class(hdi_shp, x = shdi, y = N_surplus, dim = 4, style = "quantile") 
# names(hdi_shp)[names(hdi_shp) == "bi_class"] <- "bi_hdi" 
# breaksC <- bi_class_breaks(hdi_shp, x = shdi, y = N_surplus, dim = 4, 
#                            style = "quantile", dig_lab = 1, split = TRUE)
hdi_shp <- hdi_shp |> 
    mutate(p_log = log1p(P.concentration))


povsn <- bi_class(povsn, x = poplog, y = p_log, dim = 4, style = "quantile") 
names(povsn)[names(povsn) == "bi_class"] <- "bi_pop"
breaksB <- bi_class_breaks(povsn, x = poplog, y = p_log, dim = 4, 
                           style = "quantile", dig_lab = 1, split = TRUE)

hdi_shp <-  bi_class(hdi_shp, x = shdi, y = p_log, dim = 4, style = "quantile") 
names(hdi_shp)[names(hdi_shp) == "bi_class"] <- "bi_hdi" 
breaksC <- bi_class_breaks(hdi_shp, x = shdi, y = p_log, dim = 4, 
                           style = "quantile", dig_lab = 1, split = TRUE)
