library(tidyverse)
library(fs)
library(sf)
library(spData)
library(terra)
library(scico)
library("patchwork")
library(biscale)
library(tictoc)
library(tidync)
library(raster)
# library(future)
# library(furrr)

#### Subnational poverty:  world bank ####
povsn <- st_read("~/Documents/Projects/DATA/WorldBank/gsap-maps/GSAP2.shp")

povsn <- povsn |> 
    mutate(across(.cols = GSAP2_poor:GSAP2_thei , .fns = as.numeric)) |> #str()
    mutate(across(.cols = GSAP2_base:GSAP2_line, .fns = as.numeric))

povsn |> as_tibble() |> select(-geometry) |> skimr::skim()


#### Aerosols ####

ars <- rast("~/Documents/Projects/DATA/NASA_SEDAC/sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019-geotiff/sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019.tif")

tic()
ars_df <- terra::extract(ars, vect(povsn), fun = mean, na.rm = TRUE)
toc() #635.124 sec elapsed

ars_df |> head()

povsn <- povsn |> 
    mutate(ID = row_number()) |> 
    left_join(
        ars_df |> 
            rename(mean_ppm25 = sdei.global.annual.gwr.pm2.5.modis.misr.seawifs.aod.v4.gl.03.2019))


rm(ars)

#### Biodiversity ####
# % of species left 0.25deg
fls <-  "~/Documents/Projects/DATA/Biodiversity/PREDICTS/purvis_comcom_id6_20220208_v1_4d.nc"
fls <- "~/Documents/Projects/DATA/EC_additional_datasets/hummod_3-1.tif"
fls <- dir_ls(
    path = "~/Documents/Projects/DATA/EC_additional_datasets/integrity/", 
    recurse = TRUE)

fls <- fls |> str_subset(pattern = ".tif")

bio <- terra::rast(fls)
#bio3 <- bio3$`ebv_cube_entity=1_12` # 12 time slice which is 2020
# ## I need to change the CRS to WGS84 == "epsg:4326"
tic()
bio2 <- project(bio, crs(povsn), method = "bilinear", filename = "data/hummod_projected.tif")
toc() ## this takes over an hour
cat(crs(bio2))

tic()
bio_df <- terra::extract(bio, vect(povsn), fun = mean, na.rm = TRUE)
toc() #44 sec elapsed


povsn <- povsn |> 
    mutate(ID = row_number()) |> 
    left_join(bio_df |> rename(mean_integrity = Map_mean))

rm(bio3)

#### Water ####
wtr <- rast("~/Documents/Projects/DATA/EC_additional_datasets/RiskMapVolume-1.tif")
# cubic km
tic()
wtr_df <- terra::extract(wtr, vect(povsn), fun = mean, na.rm = TRUE)
toc() #24 sec elapsed

povsn <- povsn |> 
    mutate(ID = row_number()) |> 
    left_join(wtr_df |> rename(risk_volume = RiskMapVolume.1))



## Number of months per year outside the 20% boundary: aggregate average number of months outside the boundary per year
estress <- rast("~/Documents/Projects/DATA/EC_additional_datasets/Estress_WBM_TerraClimate_2000-2020_LTM_estress_chg20.tif")

tic()
est_df <- terra::extract(estress, vect(povsn), fun = mean, na.rm = TRUE)
toc() 

povsn <- povsn |> 
    mutate(ID = row_number()) |> 
    left_join(est_df |> rename(num_months = Estress_WBM_TerraClimate_2000.2020_LTM_estress_chg20))

#### Nutrients ####
## Nitrogen
nut <- rast("~/Documents/Projects/DATA/EC_additional_datasets/nsur_all_perha_ag.asc")
class(nut)
nut

tic()
nut_df <- terra::extract(nut, vect(povsn), fun = mean, na.rm = TRUE)
toc() 

povsn <- povsn |> 
    mutate(ID = row_number()) |> 
    left_join(nut_df |> rename(N_surplus = nsur_all_perha_ag))

## Phosphorous
p <- rast("~/Documents/Projects/DATA/EC_additional_datasets/MekonnenPdischarge.tif")
p

tic()
p_df <- terra::extract(p, vect(povsn), fun = mean, na.rm = TRUE)
toc() 

povsn <- povsn |> 
    mutate(ID = row_number()) |> 
    left_join(p_df |> rename(P_surplus = MekonnenPdischarge))


#### Population ####

pop <- rast("~/Documents/Projects/DATA/Population_density_NASA/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals-rev11_2000_15_min_tif/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2000_15_min.tif")

# change the projection of the poverty data to the same as population:
#povsn <- st_transform(povsn, crs = st_crs(pop))

st_crs(povsn)

# ## I need to change the CRS to WGS84 == "epsg:4326"
# tic()
# pop_wgs84 <- project(pop, "epsg:4326", method = "bilinear", filename = "pop_wgs84_220429.tif")
# toc() ## this takes over an hour
# cat(crs(pop_wgs84))
# 
# pop_wgs84 <- rast("pop_wgs84_220429.tif")

tic()
pop_df <- terra::extract(pop, vect(povsn), fun = sum, na.rm = TRUE)
toc() #84s/ 9846.044 Error: vector memory exhausted (limit reached?)

povsn <- povsn |> 
    mutate(ID = row_number()) |> 
    left_join(pop_df |> rename(population = gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2000_15_min ))


save(povsn, file = 'data/subnational_poverty_vars.RData')


#### HDI combination ####
#### #### HDI subnational ####
## The shape file has a statistic to it, I assume the mean of the time series
hdi_shp <- st_read("~/Documents/Projects/DATA/HDI_subnational/GDL Shapefiles V4/GDL Shapefiles V4.shp")    

#### Aerosols ####

tic()
ars_df <- terra::extract(ars, vect(hdi_shp), fun = mean, na.rm = TRUE)
toc() #635.124 sec elapsed

ars_df |> head()

hdi_shp <- hdi_shp |> 
    mutate(ID = row_number()) |> 
    left_join(
        ars_df |> 
            rename(mean_ppm25 = sdei.global.annual.gwr.pm2.5.modis.misr.seawifs.aod.v4.gl.03.2019))


rm(ars)

#### Biodiversity ####
# % of species left 0.25deg
fls <-  "~/Documents/Projects/DATA/Biodiversity/PREDICTS/purvis_comcom_id6_20220208_v1_4d.nc"

bio3 <- terra::rast(fls)
bio3 <- bio3$`ebv_cube_entity=1_12` # 12 time slice which is 2020


tic()
bio_df <- terra::extract(bio3, vect(hdi_shp), fun = mean, na.rm = TRUE)
toc() #44 sec elapsed


hdi_shp <- hdi_shp |> 
    mutate(ID = row_number()) |> 
    left_join(bio_df |> rename(mean_integrity = Map_mean))

rm(bio3)

#### Water ####
wtr <- rast("~/Documents/Projects/DATA/EC_additional_datasets/RiskMapVolume-1.tif")

tic()
wtr_df <- terra::extract(wtr, vect(hdi_shp), fun = mean, na.rm = TRUE)
toc() #24 sec elapsed


hdi_shp <- hdi_shp |> 
    mutate(ID = row_number()) |> 
    left_join(wtr_df |> rename(risk_volume = RiskMapVolume.1))

tic()
est_df <- terra::extract(estress, vect(hdi_shp), fun = mean, na.rm = TRUE)
toc() # 45s

hdi_shp <- hdi_shp |> 
    mutate(ID = row_number()) |> 
    left_join(est_df |> rename(num_months = Estress_WBM_TerraClimate_2000.2020_LTM_estress_chg20))

save(hdi_shp, file = 'data/subnational_hdi_vars.RData')


