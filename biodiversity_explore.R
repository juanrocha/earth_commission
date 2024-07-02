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
## current integrity:
bio_wl <- rast("data/working_lands_025degree.tif")
bio_nl <- rast("data/all_integrity_025degree.tif")
bio_comb <- min(bio_wl, bio_nl, na.rm = T)

bio_comb 

## % of species loss
## Predicts dataset
fl <- "~/Documents/Projects/DATA/Biodiversity/BES-SIM PREDICTS/pereira_comcom_id28_20220328_v1_4d.nc"


dat <- raster::brick(fl, varname = "scenario_1/metric_4/ebv_cube") # this one access the scenarios and metrics at least! Check readme file for options, but metric_2 is delta_S where S is species richness, and the delta is the change between 1900 and 2015.

dat$X2015.01.01 |> #class()
    image() # plots the raster, it is a 1deg res

tic()
bio1d <- aggregate(bio_comb,  fact = res(dat)[1]/res(bio_comb)[1], fun = "mean",
                   filename = "data/integrity_1degree.tif", overwrite = TRUE)
toc() #0.125s

## correct the extend:
ext(bio1d) <-  ext(dat)
ext(bio1d) == ext(dat) #TRUE

## correcting the resolution here is super important, otherwhise the datasets do not align!
bio1d <- project(bio1d, crs(dat) |> as.character(), res = 1)
#bio1d <- extend(bio1d, y = ext(dat))


bio_df <- as.data.frame(bio1d, xy = TRUE) |> as_tibble()
sps_df <- as.data.frame(dat$X2015.01.01, xy = TRUE) |> as_tibble()

bio_df <- bio_df |> 
    left_join(sps_df) |> 
    rename(delta_S = X2015.01.01, integrity = Map_mean)

## categorical variables: biomes
terr_eco <- st_read("~/Documents/Projects/DATA/terr-ecoregions-TNC/tnc_terr_ecoregions.shp")
terr_eco["WWF_MHTNAM"]

# use the ext(bio1d) to fill up values
r <- raster::raster(nrows = 180, ncol = 360, xmn = -180, xmx = 180, ymn = -90, ymx = 90)

biomes <- fasterize::fasterize(terr_eco, r, field = "WWF_MHTNUM")
biomes <- as.data.frame(biomes, xy = TRUE) |> as_tibble()

# extract the key from shape file:
key_terrestrial <- terr_eco %>%
    as_tibble() %>%
    dplyr::select("WWF_MHTNAM", "WWF_MHTNUM") %>%
    unique() %>%
    rename(biome = 1, biome_code = 2)

biomes <- biomes |> 
    rename(biome_code = layer) |> 
    left_join(key_terrestrial)

bio_df <- bio_df |> 
    left_join(biomes)

bio_df |> 
    filter(!biome %in% c("Inland Water", "Mangroves", "Rock and Ice") & !is.na(biome)) |> 
    ggplot(aes(integrity, delta_S)) + 
    geom_point(aes(color = biome), show.legend = FALSE, size = 1, alpha = 0.5) +
    geom_hline(yintercept = 0, color ="orange", linetype = 2) +
    geom_smooth(method = "lm") +
    scico::scale_color_scico_d(palette = "romaO", na.value = "grey50") +
    facet_wrap(~biome) +
    theme_light(base_size = 6)

#### Segmented regression ####

fit <- lm(delta_S ~ integrity + biome,
       data = bio_df |> 
           # remove biomes with too little observations
           filter(!biome %in% c("Inland Water", "Mangroves", "Rock and Ice") & !is.na(biome)) )
sfit <- segmented::segmented(fit, ~integrity)
davies <- segmented::davies.test(fit, ~integrity, k = 10)


summary(fit)
summary(sfit)
davies


#### Other datasets ####
#### Birds

birds <- raster::brick("~/Documents/Projects/DATA/Biodiversity/Local_bird_diversity/martins_comcom_id1_20220208_v1.nc", varname = "metric_2/ebv_cube")

birds <- as.data.frame(birds, xy = TRUE) |> as_tibble()
birds |> skimr::skim()

all(birds |> 
    filter(!is.na(all.birds)) |> 
    pull(all.birds) == 0)

# it's all zeroes


#### Left overs ####
# bio_pred <- rast("~/Documents/Projects/DATA/Biodiversity/BES-SIM PREDICTS/pereira_comcom_id28_20220328_v1_4d.nc") # fail wiht raster
# Fail with tidync
# # bio_pred <- tidync(fl)
# bio_pred
# ncmeta::nc_vars(fl); 
# ncmeta::nc_grids(fl) |> unnest(cols = c(variables))
# ncmeta::nc_atts(fl, "entity") |> 
#     unnest(cols = c(value))
# 
# ncmeta::nc_atts(fl, "time") |> 
#     unnest(cols = c(value))
# 
# dat <- bio_pred |>
#     activate("D4,D3") |> 
#     hyper_array()
# dat$entity # It does not get the data out!
# Fail with stars
# # dat <- stars::read_ncdf(fl) # doesnt' work neither