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

