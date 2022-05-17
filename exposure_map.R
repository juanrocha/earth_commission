## Maps for the Earth Commission
## Juan Rocha, 20220428
library(tidyverse)
library(fs)
library(sf)
library(spData)
library(terra)
library(scico)
library("patchwork")
library(biscale)
library(tictoc)


#### Aerosols ####

ars <- rast("~/Documents/Projects/DATA/NASA_SEDAC/sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019-geotiff/sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019.tif")

st_crs(ars)

#### Water ####
wtr <- rast("~/Documents/Projects/DATA/EC_additional_datasets/RiskMapVolume-1.tif")

st_crs(wtr) == st_crs(ars)
