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



#### base world map with countries ####
## update for a more recent version of the world
# base <- maps::map("world", plot = FALSE, fill = TRUE) |> 
#     sf::st_as_sf()
# 
# data(world)
# 
# world |> 
#     ggplot() +
#     geom_sf() +
#     theme_minimal()


## there are problems with naming when merging data sets.
wb_countries <- read_csv("~/Documents/Projects/DATA/WorldBank/WDI_csv/WDICountry.csv") |> 
    janitor::clean_names() 

## Add iso codes for easy joining with other datasets
world <- world |> 
    left_join(wb_countries |> select(iso_a2 = x2_alpha_code, country_code)) 

world$country_code[world$name_long == "Namibia"] <- "NAM" # correcting for Namibia

#### National level harm ####

# pop <- read_csv(
#     file = "~/Documents/Projects/DATA/WorldBank/hnp_stats_csv/HNP_StatsData.csv") |> 
#     janitor::clean_names()

risk <- readxl::read_xlsx(
    path = "data/INFORM_Risk_2022_v062_adjusted.xlsx",
    sheet = 1) |> 
    janitor::clean_names()

# poverty <- readxl::read_xlsx(
#     path = "data/API_11_DS2_en_excel_v2_3469552.xlsx",
#     sheet = 1, skip = 3) |>
#     janitor::clean_names()
# 
# wdi <- read_csv(
#     file = "~/Documents/Projects/DATA/WorldBank/WDI_csv/WDIData.csv") |> 
#     janitor::clean_names()
# 
# sdg <- read_csv(
#     file = "~/Documents/Projects/DATA/WorldBank/SDG_csv/SDGData.csv") |> 
#     janitor::clean_names()

pov_indicators <- sdg$indicator_name |> unique() |> str_subset("poverty")

wdi$country_name |> unique()

base$ID [!base$ID %in% risk$country] |> sort()
risk$country [!risk$country %in% base$ID] |> sort()




p1 <- left_join(world,risk, by = c("country_code" = "code")) |> 
    ggplot() +
    geom_sf(aes(fill = inform_risk_adjusted), size = 0.05, color = "white") + 
    scale_fill_scico("Inform Risk Index", palette = "berlin") +
    theme_void(base_size = 6) +
    theme(legend.position = "top", legend.direction = "horizontal",
          legend.key.width = unit(10, "mm"), legend.key.height = unit(1.5,"mm"))

ggsave(
    filename = "risk_index.png", path = "figures/", device = "png", 
    width = 4, height = 2.5, dpi = 400, bg = "white"
)


left_join(
    base, 
    pop |> 
        filter(indicator_name == "Population, total"
               ) |> 
        select(country_code, indicator_name, x2018), # |>  print(n=266) ,
    by = c("code" = "country_code") ) |> #filter(is.na(x2018))
    ggplot() +
    geom_sf(aes(fill = x2018), size = 0.05, color = "white") + 
    scale_fill_scico("Population, total"  , palette = "berlin", guide = guide_colorbar(title.position = "top")) +
    theme_void(base_size = 6) +
    labs(caption = "Source: Health, Nutrition and Population Statistics, World Bank (2018)") +
    theme(legend.position = "top", legend.direction = "horizontal",
          legend.key.width = unit(10, "mm"), legend.key.height = unit(1.5,"mm"))

ggsave(
    plot = last_plot(),
    filename = "population_HNP_WorldBank.png", path = "figures/", device = "png", 
    width = 4, height = 2.5, dpi = 400, bg = "white"
)


poverty |> pull(indicator_name) |> unique()

# there is a lot of NAs, so what indicator to use?
poverty |> 
    pivot_longer(starts_with("x"), names_to = "year", values_to = "value") |> 
    mutate(year = str_remove(year, "x"), year = as.numeric(year)) |> 
    mutate(is_na = is.na(value)) |> 
    group_by(year, indicator_name) |> 
    summarize(nas = sum(is_na) / 266) |> # divided by number of countries
    ggplot(aes(year, indicator_name)) +
    geom_tile(aes(fill = nas)) +
    scale_fill_viridis_c('Number of NAs', option = "C") + 
    theme_light(base_size = 6)

wdi |> 
    filter(indicator_name %in% pov_indicators) |> 
    pivot_longer(starts_with("x"), names_to = "year", values_to = "value") |> 
    mutate(year = str_remove(year, "x"), year = as.numeric(year)) |> 
    mutate(is_na = is.na(value)) |> 
    filter(year > 1960) |>  
    group_by(year, indicator_name) |> 
    summarize(nas = sum(is_na) / 266) |> # divided by number of countries
    ggplot(aes(year, indicator_name)) +
    geom_tile(aes(fill = nas)) +
    scale_fill_viridis_c()

sdg |> 
    filter(indicator_name %in% pov_indicators) |> 
    pivot_longer(starts_with("x"), names_to = "year", values_to = "value") |> 
    mutate(year = str_remove(year, "x"), year = as.numeric(year)) |> 
    mutate(is_na = is.na(value)) |> 
    filter(year > 1960) |>  
    group_by(year, indicator_name) |> 
    summarize(nas = sum(is_na) / 266) |> # divided by number of countries
    ggplot(aes(year, indicator_name)) +
    geom_tile(aes(fill = nas)) +
    scale_fill_viridis_c()


pov2016 <- poverty |> 
    pivot_longer(starts_with("x"), names_to = "year", values_to = "value") |>
    mutate(year = str_remove(year, "x"), year = as.numeric(year)) |> 
    filter(indicator_name == "Poverty gap at $1.90 a day (2011 PPP) (%)", year == 2016) |> 
    select(poverty_2016 = value, ID = country, country_code)

left_join(base, pov2016) |> 
    ggplot() +
    geom_sf(aes(fill = poverty_2016), size = 0.05, color = "white") + 
    scale_fill_scico("Poverty gap at $1.90 a day (2011 PPP) (%)", 
                     palette = "berlin" , 
                     guide = guide_colorbar(title.position = "top")) +
    theme_void(base_size = 6) +
    theme(legend.position = "top", legend.direction = "horizontal",
          legend.key.width = unit(20, "mm"), legend.key.height = unit(3,"mm"))

left_join(
    base, 
    sdg |> 
        filter(indicator_name == 
                   "Poverty headcount ratio at national poverty lines (% of population)") |> 
        pivot_longer(starts_with("x"), names_to = "year", values_to = "value") |>
        mutate(year = str_remove(year, "x"), year = as.numeric(year)) |> 
        filter(year == 2018),
    by = c("code" = "country_code")
    ) |> 
    ggplot() +
    geom_sf(aes(fill = value), size = 0.05, color = "white") + 
    scale_fill_scico(
        "Poverty headcount ratio at national poverty lines (% of population)", 
        palette = "berlin", guide = guide_colorbar(title.position = "top")) +
    labs(caption = "Source: Sustainable development statistics, World Bank (2018)") +
    theme_void(base_size = 6) +
    theme(legend.position = "top", legend.direction = "horizontal",
          legend.key.width = unit(10, "mm"), legend.key.height = unit(1.5,"mm"))

ggsave(
    plot = last_plot(),
    filename = "poverty_SDG_WorldBank.png", path = "figures/", device = "png", 
    width = 4, height = 2.5, dpi = 400, bg = "white"
)

#### Subnational poverty:  world bank ####

povsn <- st_read("~/Documents/Projects/DATA/WorldBank/gsap-maps/GSAP2.shp")

povsn <- povsn |> 
    mutate(across(.cols = GSAP2_poor:GSAP2_thei , .fns = as.numeric)) |> #str()
    mutate(across(.cols = GSAP2_base:GSAP2_line, .fns = as.numeric))

povsn |> as_tibble() |> select(-geometry) |> skimr::skim()


povsn <- left_join(povsn, risk |> select(code, inform_risk_adjusted))
povsn <- bi_class(povsn, x = GSAP2_poor, y = inform_risk_adjusted, dim = 3)

leg <- bi_legend(pal = "DkViolet", dim = 3, 
                 xlab = "Poverty rate", ylab = "Inform Risk", size = 6)
 
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
    filename = "poverty_risk-inform.png", path = "figures/", 
    device = "png", width = 7, height = 4.5, dpi = 600, bg = "white"
)

a <- povsn |> 
    ggplot(aes(GSAP2_poor)) +
    geom_density()

rm(a)
## Code for ploting individual variables of povsn 
povsn |>  
    ggplot() +
    geom_sf(aes(fill = GSAP2_po_1), size = 0.01, color = "white") +
    scale_fill_viridis_c(
        "Poverty headcount ratio at %1.90 a day (lineup, 2011 PPP)",
        option = "B", guide = guide_colorbar(title.position = "top")) +
    labs(caption = "Source: World Bank") +
    theme_void(base_size = 6) +
    theme(legend.position = "top", legend.direction = "horizontal",
          legend.key.width = unit(10, "mm"), legend.key.height = unit(1.5,"mm"))

ggsave(
    plot = last_plot(), 
    filename = "poverty_190US_subnational_WB.png", path = "figures/", 
    device = "png", width = 6, height = 4, dpi = 400, bg = "white"
)


#### Grid data: ####

#### Poverty ####
fls <- fs::dir_ls("~/Documents/Projects/DATA/Relative_Wealth_Index/relative-wealth-index-april-2021_Version_May2021/")

pvt <- map(fls, read_csv, col_types = "ddddd") 
pvt <- pvt |> bind_rows()
pvt <- pvt |> rename(x = longitude, y = latitude)

base |> 
    ggplot() +
    geom_sf(size = 0.05, color = "gray50") + 
    geom_tile(data = pvt, aes(x,y, fill = rwi)) +
    scale_fill_viridis_c(
        "Relative Wealth Index", 
        option = "C", guide = guide_colorbar(title.position = "top")) +
    labs(caption = "Source: Chi et al PNAS (2022), 2.4km resolution grid for low and middle income countries") +
    theme_void(base_size = 6) +
    theme(legend.position = "top", legend.direction = "horizontal",
          legend.key.width = unit(10, "mm"), legend.key.height = unit(1.5,"mm"))


ggsave(
    plot = last_plot(),
    filename = "poverty_RWI.png", path = "figures/", device = "png", 
    width = 4, height = 2.5, dpi = 400, bg = "white"
)

#### Population layers #### 
pop <- raster::raster("~/Documents/Projects/DATA/Population_GHSL/GHS_POP_E2015_GLOBE_R2019A_54009_250_V1_0/GHS_POP_E2015_GLOBE_R2019A_54009_250_V1_0.tif")

pop
# library(raster)
# plot(pop)

#### Aerosols ####

ars <- rast("~/Documents/Projects/DATA/NASA_SEDAC/sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019-geotiff/sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019.tif")

tic()
ars_df <- terra::extract(ars, vect(povsn), fun = mean, na.rm = TRUE)
toc() #635.124 sec elapsed

ars_df |> head()

povsn <- povsn |> 
    mutate(ID = row_number()) |> 
    left_join(ars_df |> rename(mean_ppm25 = sdei.global.annual.gwr.pm2.5.modis.misr.seawifs.aod.v4.gl.03.2019))

bi_povsn <- bi_class(povsn, x = GSAP2_poor, y = mean_ppm25, dim = 3)

leg <- bi_legend(pal = "DkViolet", dim = 3, 
                 xlab = "Poverty rate", ylab = "Aerosols ppm 2.5", size = 6)

povsn |> ggplot(aes(mean_ppm25, GSAP2_poor)) + 
    geom_density2d(alpha = 0.5) +
    geom_vline(
        xintercept = quantile(povsn$mean_ppm25,c(0.3,0.6), na.rm = TRUE),
        color = "red", linetype = 2) +
    geom_hline(
        yintercept = quantile(povsn$GSAP2_poor,c(0.3,0.6), na.rm = TRUE),
        color = "red", linetype = 2)


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

## Extract summary stat for aerosols
povsn |>  
    ggplot() +
    geom_sf(aes(fill = mean_ppm25), size = 0.01, color = "white") +
    scale_fill_viridis_c(
        "Mean ground-level fine particulate matter of 2.5 micrometers or smaller (PM2.5)",
        option = "C", guide = guide_colorbar(title.position = "top")) +
    labs(caption = "Source: NASA, data for 2019") +
    theme_void(base_size = 6) +
    theme(legend.position = "top", legend.direction = "horizontal",
          legend.key.width = unit(10, "mm"), legend.key.height = unit(1.5,"mm"))

ggsave(
    plot = last_plot(), 
    filename = "aerosols_PM25_NASA.png", path = "figures/", 
    device = "png", width = 6, height = 4, dpi = 400, bg = "white"
)

#### Biodiversity ####
fls <-  "~/Documents/Projects/DATA/Biodiversity/PREDICTS/purvis_comcom_id6_20220208_v1_4d.nc"
bio <- tidync(fls) 

## some exploratory queries to understand the dataset
ncmeta::nc_grids(fls) |> unnest(cols = c(variables))
ncmeta::nc_vars(fls)
ncmeta::nc_dims(fls)
ncmeta::nc_var(fls, "entity")
ncmeta::nc_atts(fls, "entity") |> unnest(cols = c(value))
ncmeta::nc_atts(fls) 
ncmeta::nc_atts(fls, "time") |> unnest(cols = c(value))

## see here for example how to change to readable time: https://ropensci.org/blog/2019/11/05/tidync/

ncmeta::nc_axes(fls)

# completely fail at extracting values with tidync
# bio2 <- bio |> activate(time) |> 
#     hyper_filter(time = time == 58439) |> # this is the layer for 2020
#     activate(entity, select_var = ebv_entity_scope) |> 
#     hyper_array(drop = T)
# 
# bio2

bio3 <- raster::brick(fls, varname = "metric_1/ebv_cube")
bio3 <- bio3$X2020.01.01 #reduce to the year of interest


# alternative with terra:
bio3 <- terra::rast(fls)
bio3 <- bio3$`ebv_cube_entity=1_12` # 12 time slice which is 2020


tic()
bio_df <- terra::extract(bio, vect(povsn), fun = mean, na.rm = TRUE)
toc() #44 sec elapsed


povsn <- povsn |> 
    mutate(ID = row_number()) |> 
    left_join(bio_df |> rename(mean_spp = ebv_cube_entity.1_12))

povsn <- bi_class(povsn, x = GSAP2_poor, y = mean_spp, dim = 3)

leg <- bi_legend(pal = "DkViolet", dim = 3, 
                 xlab = "Poverty rate", ylab = "% of species left", size = 6)

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
    filename = "species_poverty.png", path = "figures/", 
    device = "png", width = 6, height = 4, dpi = 400, bg = "white"
)

#### Water ####

wtr <- rast("~/Documents/Projects/DATA/EC_additional_datasets/RiskMapVolume-1.tif")

tic()
wtr_df <- terra::extract(wtr, vect(povsn), fun = mean, na.rm = TRUE)
toc() #24 sec elapsed

wtr_df |> head()

povsn <- povsn |> 
    mutate(ID = row_number()) |> 
    left_join(wtr_df |> rename(risk_volume = RiskMapVolume.1))

povsn <- bi_class(povsn, x = GSAP2_poor, y = risk_volume, dim = 3)

leg <- bi_legend(pal = "DkViolet", dim = 3, 
                 xlab = "Poverty rate", ylab = "Risk volume", size = 6)

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
    filename = "water_poverty.png", path = "figures/", 
    device = "png", width = 6, height = 4, dpi = 400, bg = "white"
)

####
####
#### left overs ####
base$ID [!base$ID %in% pop$country] |> sort()
pop$country_name [!pop$country_name %in% base$ID] |> unique() |> sort()

# when renamed:
pop$country [!pop$country %in% base$ID] |> unique() |> sort()

pop <- pop |> 
    rename(country = country_name) |> 
    mutate(country = case_when(
        country == "Antigua and Barbuda" ~ "Antigua",
        country == "Bahamas, The" ~ "Bahamas",
        country == "Brunei Darussalam" ~ "Brunei",
        country == "Cabo Verde"~"Cape Verde",
        country == "Congo, Rep."~ "Republic of Congo" ,
        country == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo" ,
        country == "Cote d'Ivoire" ~ "Ivory Coast", 
        country == "Eswatini" ~ "Swaziland" , 
        country == "Korea, Dem. People's Rep." ~ "North Korea",
        country == "Korea, Rep." ~ "South Korea", 
        country == "Kyrgyz Republic" ~ "Kyrgyzstan", 
        country == "Lao PDR"  ~ "Laos" , 
        country == "Moldova Republic of"~ "Moldova",
        country == "Russian Federation" ~ "Russia" ,
        country == "St. Kitts and Nevis" ~ "Saint Kitts" ,
        country == "Saint Vincent and the Grenadines" ~ "Saint Vincent",
        country == "Trinidad and Tobago" ~ "Trinidad", 
        #country == "Tuvalu" ~ , 
        country == "United Kingdom" ~ "UK", 
        country == "United States"  ~ "USA",
        country == "Viet Nam" ~ "Vietnam" , 
        country == "Venezuela, RB" ~ "Venezuela" , 
        country == "Gambia, The"  ~ "Gambia",
        country == "Iran, Islamic Rep." ~ "Iran",
        country == "Egypt, Arab Rep." ~ "Egypt",
        country == "Yemen, Rep." ~ "Yemen",
        country == "Syrian Arab Republic"  ~ "Syria",
        country == "Slovak Republic"  ~ "Slovakia", 
        TRUE ~ country
    )
    )


base$ID [!base$ID %in% poverty$country_name] |> sort()
poverty$country_name [!poverty$country_name %in% base$ID] |> unique() |> sort()

# when renamed:
poverty$country [!poverty$country %in% base$ID] |> unique() |> sort()

poverty <- poverty |> 
    rename(country = country_name) |> 
    mutate(country = case_when(
        country == "Antigua and Barbuda" ~ "Antigua",
        country == "Bahamas, The" ~ "Bahamas",
        country == "Brunei Darussalam" ~ "Brunei",
        country == "Cabo Verde"~"Cape Verde",
        country == "Congo, Rep."~ "Republic of Congo" ,
        country == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo" ,
        country == "Cote d'Ivoire" ~ "Ivory Coast", 
        country == "Eswatini" ~ "Swaziland" , 
        country == "Korea, Dem. People's Rep." ~ "North Korea",
        country == "Korea, Rep." ~ "South Korea", 
        country == "Kyrgyz Republic" ~ "Kyrgyzstan", 
        country == "Lao PDR"  ~ "Laos" , 
        country == "Moldova Republic of"~ "Moldova",
        country == "Russian Federation" ~ "Russia" ,
        country == "St. Kitts and Nevis" ~ "Saint Kitts" ,
        country == "Saint Vincent and the Grenadines" ~ "Saint Vincent",
        country == "Trinidad and Tobago" ~ "Trinidad", 
        #country == "Tuvalu" ~ , 
        country == "United Kingdom" ~ "UK", 
        country == "United States"  ~ "USA",
        country == "Viet Nam" ~ "Vietnam" , 
        country == "Venezuela, RB" ~ "Venezuela" , 
        country == "Gambia, The"  ~ "Gambia",
        country == "Iran, Islamic Rep." ~ "Iran",
        country == "Egypt, Arab Rep." ~ "Egypt",
        country == "Yemen, Rep." ~ "Yemen",
        country == "Syrian Arab Republic"  ~ "Syria",
        country == "Slovak Republic"  ~ "Slovakia", 
        TRUE ~ country
    )
    )

base$ID [!base$ID %in% sdg$country_name] |> sort()
sdg$country_name [!sdg$country_name %in% base$ID] |> unique() |> sort()

# when renamed:
poverty$country [!poverty$country %in% base$ID] |> unique() |> sort()



risk <- risk |> 
    mutate(
        country = case_when(
            country == "Antigua and Barbuda" ~ "Antigua",
            country == "Brunei Darussalam" ~ "Brunei",
            country == "Cabo Verde"~"Cape Verde",
            country == "Congo"~ "Republic of Congo" ,
            country == "Congo DR" ~ "Democratic Republic of the Congo" ,
            country == "Côte d'Ivoire" ~ "Ivory Coast", 
            country == "Eswatini" ~ "Swaziland" , 
            country == "Korea DPR" ~ "North Korea",
            country == "Korea Republic of" ~ "South Korea", 
            country == "Lao PDR"  ~ "Laos" , 
            country == "Moldova Republic of"~ "Moldova",
            country == "Russian Federation" ~ "Russia" ,
            country == "Saint Kitts and Nevis" ~ "Saint Kitts" ,
            country == "Saint Vincent and the Grenadines" ~ "Saint Vincent",
            country == "Trinidad and Tobago" ~ "Trinidad", 
            country == "United Kingdom" ~ "UK", 
            country == "United States of America"  ~ "USA",
            country == "Viet Nam" ~ "Vietnam" , 
            TRUE ~ country
        )
    )


nut <- rast("~/Documents/Projects/DATA/EC_additional_datasets/nsur_all.asc")
class(nut)
pl