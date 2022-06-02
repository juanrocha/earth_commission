## Biodiversity: intactness area vs species 
## Is there a tipping point on intactness area that triggers extinction risk?
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

#### Datasets ####
## current:
bio_wl <- rast("data/working_lands_025degree.tif")
bio_nl <- rast("data/all_integrity_025degree.tif")
bio_comb <- min(bio_wl, bio_nl, na.rm = T)

load("data/subnational_poverty_vars.RData")
load("data/subnational_hdi_vars.RData")

## Old
## Number of species in each cell relative to pristine baseline (%) and changes relative to 1900
## Purvis et al 2018
fls <-  "~/Documents/Projects/DATA/Biodiversity/PREDICTS/purvis_comcom_id6_20220208_v1_4d.nc"
bio <- tidync(fls) 
spp <- terra::rast(fls)

## GlobES: useless, it does not have global coverage, only parts of Africa and middle east
# fls <- "~/Documents/Projects/DATA/Biodiversity/GlobES/globES.nc"
# bio <- tidync(fls)
# bio <- terra::rast(fls)

## some exploratory queries to understand the dataset
# ncmeta::nc_grids(fls) |> unnest(cols = c(variables))
# ncmeta::nc_vars(fls)
# ncmeta::nc_dims(fls)
# ncmeta::nc_var(fls, "var_entity")
# ncmeta::nc_atts(fls, "var_entity") |> unnest(cols = c(value))
# ncmeta::nc_atts(fls) |> print(n=45)
# ncmeta::nc_atts(fls, "time") |> unnest(cols = c(value))
## doesnt' work nicely because there is no clear named variables
bio2 <- bio |> hyper_array()

## Fragmentation
fls <- "~/Documents/Projects/DATA/Biodiversity/RMF-fragmentation/RMF_002.nc"
frg <- terra::rast(fls, lyrs = 27) # lyr is the year here, 27 is the latest year 2018

frg

frg_025 <- 
terra::plot(frg)
## each layer is a year I think, see info here: https://portal.geobon.org/ebv-detail?id=4
# the resolution is 0.0027 degree ~ 300m. To combine with the spp dataset it needs to be
# rescaled to 0.25 degree. 
frg |> range(na.rm = TRUE) # this should be 0-1

## New GeoBON datasets:

intact <- read

fls <-  "~/Documents/Projects/DATA/Biodiversity/BES-SIM PREDICTS/pereira_comcom_id28_20220328_v1_4d.nc"
bio <- tidync(fls) 
spp <- terra::rast(fls)

#### Integrity ####

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


#### Figures ####
old_par <- par()

quartz(width = 6.5, height = 3, pointsize = 6, dpi = 400, bg = "white")

par(mar=c(1,1,1,1), mfrow = c(1,2))

terra::plot(
    bio_nl, axes = FALSE, plg = list(ext = c(-150,-145,-60,30), cex = 0.7, horiz = TRUE),
    grid = FALSE, col = viridisLite::viridis(n = 20, option = "D",direction = -1),
    mar = c(1,1,1,1)
) 
title("A", adj = 0)

terra::plot(
    bio_wl, axes = FALSE, plg = list(ext = c(-150,-145,-60,30), cex = 0.7, horiz = TRUE),
    grid = FALSE, col = viridisLite::viridis(n = 20, option = "D",direction = -1),
    mar = c(1,1,1,1)
) 
title("B", adj = 0)

quartz.save(
    file = "figures/integrity_combined_025d.png",
    device = dev.cur() ,width = 6.5, height = 2,
    pointsize = 6, dpi = 400, bg = "white")


### Composed map for just:

povsn <- bi_class(povsn, x = GSAP2_poor, y = mean_integrity, dim = 4, style = "quantile") 

# rename is not working, doing it in base.
names(povsn)[names(povsn) == "bi_class"] <- "bi_poverty"

povsn <- bi_class(povsn, x = population, y = mean_integrity, dim = 4, style = "quantile") 
names(povsn)[names(povsn) == "bi_class"] <- "bi_pop"

hdi_shp <-  bi_class(hdi_shp, x = shdi, y = mean_integrity, dim = 4, style = "quantile") 
names(hdi_shp)[names(hdi_shp) == "bi_class"] <- "bi_hdi" 


# leg <- bi_legend(
#     pal = "DkViolet", dim = 3, # "DkViolet"
#     xlab = "Poverty", ylab = "Mean N surplus",
#     flip_axes = FALSE, rotate_pal = FALSE) +
#     bi_theme(base_family = "Helvetica", base_size = 4)
# leg

#par(old_par) 

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
                        xlab = "Population", ylab = "Mean integrity",
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
                         xlab = "Poverty", ylab = "Mean integrity",
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
                             xlab = "HDI", ylab = "Mean integrity",
                             flip_axes = FALSE, rotate_pal = FALSE) +
                             bi_theme(base_family = "Helvetica", base_size = 4)
                     ), 
                     xmin = -180, ymin = -60, xmax = -90, ymax = 20)+
                 labs(tag = "C") +
                 theme_void(base_size = 6)) +
            plot_layout(nrow = 3)
            ) ,
    filename = "just_biodiversity.png", path = "figures/", 
    device = "png", width = 4, height = 6, dpi = 400, bg = "white")
toc() #20s



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

