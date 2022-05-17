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

#### Datasets ####

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
    path = "~/Documents/Projects/DATA/EC_additional_datasets/integrity/", 
    recurse = TRUE)

fls <- fls |> str_subset(pattern = ".tif")

out <-  terra::rast(fls[3])
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


### set up a loop:
fct <- 0.25/res(out)[1] # the factor to rescale to 0.25 degree
tic()
for (i in seq_along(fls)){
    nn <- paste0("data/tmp/integrity_", i, ".tif")
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
merge(out2, filename = 'data/integrity_025degree.tif')
toc() #1.9s with warning: rasters did not align and were resampled

bio <- rast("data/integrity_025degree.tif")
bio
terra::plot(
    bio, axes = FALSE, plg = list(ext = c(-150,-145,-60,30), cex = 0.7, horiz = TRUE),
     grid = FALSE, col = viridisLite::viridis(n = 20, option = "D",direction = -1),
     mar = c(1,1,1,1)
      )

quartz(width = 6, height = 4, pointsize = 6, dpi = 400, bg = "white")

quartz.save(
    file = "figures/integrity_025d.png", 
    device = dev.cur() ,width = 6, height = 4, 
    pointsize = 6, dpi = 400, bg = "white")
