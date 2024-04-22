## Biodiversity
## Juan Rocha
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


#### Datasets ####
## current:
bio_wl <- rast("data/working_lands_025degree.tif")
bio_nl <- rast("data/all_integrity_025degree.tif")
bio_comb <- min(bio_wl, bio_nl, na.rm = T)

load("data/subnational_poverty_vars.RData")
#load("data/subnational_hdi_vars.RData")


pop <- rast("~/Documents/Projects/DATA/Population_density_NASA/gpw-v4-population-density-rev11_2020_15_min_tif/gpw_v4_population_density_rev11_2020_15_min.tif")

area <- cellSize(pop, unit = "km")
## adjust population: from people / km to number of people
pop_adj <- log1p(pop * area)
bio_comb <- project(bio_comb, pop_adj)
add(bio_comb) <- pop_adj

bio_df <- as.data.frame(bio_comb, xy=TRUE) |> as_tibble() |> 
    rename(integrity = Map_mean, pop2020 = gpw_v4_population_density_rev11_2020_15_min)

#### Figures ####

tic()
ggsave(
    # (ggplot() +
    #     geom_spatraster(data = bio_nl) +
    #     geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
    #     scale_fill_viridis_c(
    #         "Integrity natural lands",option = "D", direction = -1, na.value = "white",
    #         guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"), 
    #                                barheight = unit(2,"mm"))) +
    #     labs(tag = "A") + lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
    #     theme_void(base_size = 6) +
    #     theme(legend.position = c(0.11, 0.1),
    #           legend.direction = "horizontal")) +
    (ggplot() +
         geom_spatraster(data = bio_wl) +
         geom_sf(data = st_as_sf(coastsCoarse), linewidth = 0.1, color = "gray25") +
         scale_fill_viridis_c(
             "Integrity working lands",option = "D", direction = -1, na.value = "white",
             guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"), 
                                    barheight = unit(2,"mm"))) +
         #labs(tag = "B") + 
         lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
         theme_void(base_size = 6) +
         theme(legend.position = c(0.11, 0.1),
               legend.direction = "horizontal")),
        #plot_layout(nrow = 1, ncol = 2),
    filename = "safe_biodiversity.pdf", path = "figures/", 
    device = "pdf", width = 7, height = 3, dpi = 400, bg = "white"
)
toc() #5.1s

## get the number of people affected
tic()
plp_df <- terra::extract(
    ((bio_comb$Map_mean <= 0.2) * (pop * area)), 
    vect(world), fun = sum, na.rm = TRUE) |> 
    rename(people = Map_mean) # people exposed
toc() #1.3sec elapsed
pop_df <- terra::extract(
    (pop * area), 
    vect(world), fun = sum, na.rm = TRUE) |> 
    rename(pop2020 = gpw_v4_population_density_rev11_2020_15_min)

world <- world |> 
    mutate(ID = row_number()) |> 
    left_join(plp_df) |> 
    left_join(pop_df ) |> 
    mutate(prop_exposed = people / pop2020)

da <- world |> 
    arrange(desc(people)) |> 
    top_n(15, wt = people) |> 
    mutate(name_long = as_factor(name_long) |> fct_rev()) |> 
    ggplot(aes(people, name_long)) +
    geom_col() + 
    scale_x_continuous(n.breaks = 3) +
    labs(tag = "D", x = "People living with < 20% integrity", y = "") +
    theme_minimal(base_size = 5)

db <-  world |> 
    arrange(desc(prop_exposed)) |> 
    top_n(15, wt = prop_exposed) |> 
    mutate(name_long = as_factor(name_long) |> fct_rev()) |> 
    ggplot(aes(prop_exposed, name_long)) +
    geom_col() +
    labs(tag = "E", x = "Proportion of population", y = "") +
    theme_minimal(base_size = 5)

d <- da/db

### Composed map for just:

bio_df <- bi_class(bio_df, x = integrity, y = pop2020, dim = 4, style = "quantile")

povsn <- bi_class(povsn, x = GSAP2_poor, y = mean_integrity, dim = 4, style = "quantile") 
# rename is not working, doing it in base.
names(povsn)[names(povsn) == "bi_class"] <- "bi_poverty"
breaksC <- bi_class_breaks(
    povsn, x = GSAP2_poor, y = mean_integrity, dim = 4, 
    style = "quantile", dig_lab = 1, split = TRUE)

## correct manually
breaksC$bi_y <-  breaksC$bi_y[-c(1)]
breaksC$bi_y[1] <- 0

breaksB <- bi_class_breaks(bio_df, x = integrity, y = pop2020, dim = 4, 
                           style = "quantile", dig_lab = 1, split = TRUE)

lyt <- '
AAAD
BBBD
CCCD'

tic()
ggsave(
    plot = (
        (ggplot() +
             geom_spatraster(data = bio_comb$Map_mean) +
             geom_sf(data = st_as_sf(coastsCoarse), linewidth = 0.1, color = "gray25") +
             scale_fill_viridis_c(
                 "Functional integrity", option = "D", direction = -1, na.value = "white",
                 guide = guide_colorbar(title.position = "top", barwidth = unit(2,"cm"), 
                                        barheight = unit(2,"mm"))) +
             labs(tag = "A") + lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
             theme_void(base_size = 6) +
             theme(legend.position = c(0.11, 0.1),
                   legend.direction = "horizontal")) +
        (ggplot() +
             geom_tile(data = bio_df, aes(x = x, y = y, fill = bi_class), show.legend = FALSE) +
             geom_sf(data = st_as_sf(coastsCoarse), linewidth = 0.1, color = "gray25") +
             bi_scale_fill(pal = "BlueOr", dim = 4) +
             annotation_custom(
                 grob = ggplotGrob(
                     bi_legend(
                         pal = "BlueOr", dim = 4, breaks = breaksB, arrows = FALSE,
                         ylab = "Population [log]", xlab = "Functional integrity",
                         flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                         theme(plot.margin = unit(rep(1,4),"mm"))
                 ), 
                 xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
             labs(tag = "B") +  lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
             theme_void(base_size = 6))  +
        ( povsn |> 
             ggplot() +
             geom_sf(aes(fill = bi_poverty), linewidth = 0.01, color = "white", show.legend = FALSE) +
             bi_scale_fill(pal = "DkViolet2", dim = 4) +
             annotation_custom(
                 grob = ggplotGrob(
                     bi_legend(
                         pal = "DkViolet2", dim = 4,  breaks = breaksC, arrows = FALSE,
                         xlab = "Poverty", ylab = "Mean integrity",
                         flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                         theme(plot.margin = unit(rep(1,4),"mm"))
                 ), 
                 xmin = -180, ymin = -60, xmax = -90, ymax = 10) +
             labs(tag = "C") +
             theme_void(base_size = 6) ) +
        (d) +
            plot_layout(design = lyt, ncol = 2, widths = c(2,1))
            ) ,
    filename = "just_biodiversity.png", path = "figures/", 
    device = "png", width = 6, height = 5, dpi = 300, bg = "white")
toc() #24s




#### Old code and left overs ####

# quartz.save(
#     file = "figures/working_lands_025d.png", 
#     device = dev.cur() ,width = 6, height = 4, 
#     pointsize = 6, dpi = 400, bg = "white")
# 







+ ggtitle("A") +
    terra::plot(
        bio_comb, axes = FALSE, plg = list(ext = c(-150,-145,-60,30), cex = 0.7, horiz = TRUE),
        grid = FALSE, col = viridisLite::viridis(n = 20, option = "D",direction = -1),
        mar = c(1,1,1,1)
    ) + ggtitle("B") + plot_layout(ncol = 2)

library(tmap)

bio_comb |> 
    tm_shape()+
    tm_raster(palette = viridisLite::viridis(n = 20, option = "D",direction = -1)) +
    tm_layout(frame = FALSE, legend.position = c(0.1, 0.1))






#### Integrity processing ####
#### No need to run this part again, here only for archiving of how the different intergrity files
#### were stitched together.

fls <- dir_ls(
    path = "~/Documents/Projects/DATA/EC_additional_datasets/working_lands/", 
    recurse = TRUE)

fls <- fls |> str_subset(pattern = ".tif")

out <-  terra::rast(fls[5])
out
plot(out)

## the mean of the mean is not equal to the mean of all.
## x <- runif(100)
# mean(
#     mean(x[1:10]), mean(x[11:20]), mean(x[21:30]), mean(x[31:40]), mean(x[41:50]), 
#     mean(x[51:60]), mean(x[61:70]), mean(x[71:80]), mean(x[81:90]), mean(x[91:100]) 
# )

tic()
out <- terra::rast(fls[1]) |> 
    aggregate(fact = 278, fun = mean, na.rm = TRUE, filename = "data/tmp/integrity_1.tif")
toc() # 13.87s for one file

test <- terra::rast("data/tmp/integrity_1.tif")
plot(test)
plot(out)

### set up a loop:
fct <- 0.25/res(out)[1] # the factor to rescale to 0.25 degree
tic()
for (i in seq_along(fls)){
    nn <- paste0("data/tmp/working_lands_", i, ".tif")
    terra::rast(fls[i]) |>
        aggregate(fact = fct, fun = mean, na.rm = TRUE, 
                  filename = nn, overwrite = TRUE)
}
toc() #967s

fls2 <- dir_ls("data/tmp/")

# test:
length(fls2) == length(fls) # TRUE

out <- map(fls2, rast)
out2 <- sprc(out) 

tic()
merge(out2, filename = 'data/working_lands_025degree.tif')
toc() #1.9s with warning: rasters did not align and were resampled

bio <- rast("data/working_lands_025degree.tif")
bio
terra::plot(
    bio, axes = FALSE, plg = list(ext = c(-150,-145,-60,30), cex = 0.7, horiz = TRUE),
    grid = FALSE, col = viridisLite::viridis(n = 20, option = "D",direction = -1),
    mar = c(1,1,1,1)
)

# old_par <- par()
# 
# quartz(width = 6.5, height = 3, pointsize = 6, dpi = 400, bg = "white")
# 
# par(mar=c(1,1,1,1), mfrow = c(1,2))
# 
# terra::plot(
#     bio_nl, axes = FALSE, plg = list(ext = c(-150,-145,-60,30), cex = 0.7, horiz = TRUE),
#     grid = FALSE, col = viridisLite::viridis(n = 20, option = "D",direction = -1),
#     mar = c(1,1,1,1)
# ) 
# title("A", adj = 0)
# 
# terra::plot(
#     bio_wl, axes = FALSE, plg = list(ext = c(-150,-145,-60,30), cex = 0.7, horiz = TRUE),
#     grid = FALSE, col = viridisLite::viridis(n = 20, option = "D",direction = -1),
#     mar = c(1,1,1,1)
# ) 
# title("B", adj = 0)
# 
# quartz.save(
#     file = "figures/integrity_combined_025d.png",
#     device = dev.cur() ,width = 6.5, height = 2,
#     pointsize = 6, dpi = 400, bg = "white")
#     
# leftovers


povsn <- bi_class(povsn, x = poplog, y = mean_integrity, dim = 4, style = "quantile") 
names(povsn)[names(povsn) == "bi_class"] <- "bi_pop"
breaksB <- bi_class_breaks(povsn, x = poplog, y = mean_integrity, dim = 4, 
                           style = "quantile", dig_lab = 1, split = F)

hdi_shp <-  bi_class(hdi_shp, x = shdi, y = mean_integrity, dim = 4, style = "quantile") 
names(hdi_shp)[names(hdi_shp) == "bi_class"] <- "bi_hdi" 
breaksD <- bi_class_breaks(hdi_shp, x = shdi, y = mean_integrity, dim = 4, 
                           style = "quantile", dig_lab = 1, split = F)

# clean_breaks <- function(x) {
#     ifelse(x$bi_y |> is.na() |> any(),
#            x$bi_y <- x$bi_y[-1], #remove the NA
#            x$bi_y)
#     return(x)
# }
# 
# breaksD <- clean_breaks(breaksD)
# breaksB <- clean_breaks(breaksB)
# breaksC <- clean_breaks(breaksC)
povsn |> 
    ggplot() +
    geom_sf(aes(fill = bi_pop), size = 0.01, color = "white", show.legend = FALSE) +
    bi_scale_fill(pal = "BlueOr", dim = 4) +
    annotation_custom(
        grob = ggplotGrob(
            bi_legend(
                pal = "BlueOr", dim = 4,  breaks = breaksB, arrows = FALSE,
                xlab = "Population", ylab = "Mean integrity",
                flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                theme(plot.margin = unit(rep(1,4),"mm"), 
                      axis.text.x = element_text(angle = 90))

hdi_shp |> 
    ggplot() +
    geom_sf(aes(fill = bi_hdi), size = 0.01, color = "white", show.legend = FALSE) +
    bi_scale_fill(pal = "GrPink2", dim = 4) +
    annotation_custom(
        grob = ggplotGrob(
            bi_legend(
                pal = "GrPink2", dim = 4,  breaks = breaksD, arrows = FALSE,
                xlab = "HDI", ylab = "Mean integrity",
                flip_axes = FALSE, rotate_pal = FALSE, size =4) +
                theme(plot.margin = unit(rep(1,4),"mm"),
                      axis.text.x = element_text(angle = 90))
        ), 
        xmin = -180, ymin = -60, xmax = -90, ymax = 20)+
    labs(tag = "D") +
    theme_void(base_size = 6)
