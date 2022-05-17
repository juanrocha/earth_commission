library(tidyverse)
library(fs)
library(sf)
library(spData)
library(terra)
# library("rnaturalearth")
# library("rnaturalearthdata")
library(scico)
library("patchwork")
# library(rworldmap)
# library(maps)
library(biscale)
library(tictoc)
library(tidync)
library(RColorBrewer)



#### Subnational poverty:  world bank ####
povsn <- st_read("~/Documents/Projects/DATA/WorldBank/gsap-maps/GSAP2.shp")

povsn <- povsn |> 
    mutate(across(.cols = GSAP2_poor:GSAP2_thei , .fns = as.numeric)) |> #str()
    mutate(across(.cols = GSAP2_base:GSAP2_line, .fns = as.numeric))

povsn |> as_tibble() |> select(-geometry) |> skimr::skim()


#### HDI subnational ####
hdi <- read_csv("~/Documents/Projects/DATA/HDI_subnational/GDL-Sub-national-HDI-data.csv") |> 
    janitor::clean_names()

hdi <- hdi |> 
    rename(y2019 = x2019) |> 
    select(-starts_with("x")) |> 
    filter(region != "Total")

## The shape file has a statistic to it, I assume the mean of the time series
hdi_shp <- st_read("~/Documents/Projects/DATA/HDI_subnational/GDL Shapefiles V4/GDL Shapefiles V4.shp")    



#### Aerosols ####

ars <- rast("~/Documents/Projects/DATA/NASA_SEDAC/sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019-geotiff/sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019.tif")

tic()
ars_df <- terra::extract(ars, vect(povsn), fun = mean, na.rm = TRUE)
toc() #635.124 sec elapsed

ars_df |> head()

# quartz(width = 5, height = 3.5, pointsize = 6)
# plot(ars, legend = "bottomleft")
# quartz.save(file = "figures/aerosols_ppm25.png", dpi = 200, width = 5, height = 3.5, pointsize = 6)

povsn <- povsn |> 
    mutate(ID = row_number()) |> 
    left_join(ars_df |> rename(mean_ppm25 = sdei.global.annual.gwr.pm2.5.modis.misr.seawifs.aod.v4.gl.03.2019)) |> 
    mutate(biclass = case_when(
        
    ))

povsn <- bi_class(povsn, x = GSAP2_poor, y = mean_ppm25, dim = 3)

leg <- bi_legend(pal = "DkViolet", dim = 3, rotate_pal = TRUE,
                 xlab = "Poverty rate", ylab = "Aerosols ppm 2.5", size = 6) +
    annotate("text", x = c(1.5,2.5), y = 0.4, label = c("33%", "66%") ) +
    annotate("text", y = c(1.5,2.5), x = 0.4, label = c("33%", "66%"), angle = 90 ) 

povsn |> ggplot(aes(GSAP2_poor, mean_ppm25)) + 
    geom_density2d(alpha = 0.5) +
    geom_hline(
        yintercept = quantile(povsn$mean_ppm25,c(0.3,0.6), na.rm = TRUE),
        color = "grey50", linetype = 2) +
    geom_vline(
        xintercept = quantile(povsn$GSAP2_poor,c(0.3,0.6), na.rm = TRUE),
        color = "grey50", linetype = 2) +
    geom_vline(
        xintercept = c(15, 25, 35), color = "orange", linetype = 2) +
    labs(x = "Mean PPM 2.5", y = "Poverty rate at 1.9 US$") +
    theme_light()


tic()
povsn |> 
    ggplot() +
    geom_sf(aes(fill = bi_class), size = 0.01, color = "white", show.legend = FALSE) +
    bi_scale_fill(pal = "DkViolet", dim = 3) +
    annotation_custom(
        grob = ggplotGrob(leg), 
        xmin = -180, ymin = -50, xmax = -90, ymax = 0) +
    theme_void(base_size = 6)
toc()

ggsave(
    plot = last_plot(), 
    filename = "aerosols_poverty.png", path = "figures/", 
    device = "png", width = 6, height = 4, dpi = 400, bg = "white"
)

interaction(1:3,1:3)
dat <- tibble(x = 1:3, y = 1:3)
dat |> ggplot(aes(x,y)) +
    geom_tile()
