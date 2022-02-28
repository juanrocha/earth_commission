library(tidyverse)
library(fs)
library(sf)
# library("rnaturalearth")
# library("rnaturalearthdata")
library(scico)
library("patchwork")
# library(rworldmap)
# library(maps)
library(biscale)

#### base world map with countries ####

base <- maps::map("world", plot = FALSE, fill = TRUE) |> 
    sf::st_as_sf()

base |> 
    ggplot() +
    geom_sf() +
    theme_void()
## there are problems with naming when merging data sets.

#### National level harm ####

pop <- read_csv(
    file = "~/Documents/Projects/DATA/WorldBank/hnp_stats_csv/HNP_StatsData.csv") |> 
    janitor::clean_names()

risk <- readxl::read_xlsx(
    path = "data/INFORM_Risk_2022_v062_adjusted.xlsx",
    sheet = 1) |> 
    janitor::clean_names()


poverty <- readxl::read_xlsx(
    path = "data/API_11_DS2_en_excel_v2_3469552.xlsx",
    sheet = 1, skip = 3) |>
    janitor::clean_names()

wdi <- read_csv(
    file = "~/Documents/Projects/DATA/WorldBank/WDI_csv/WDIData.csv") |> 
    janitor::clean_names()

sdg <- read_csv(
    file = "~/Documents/Projects/DATA/WorldBank/SDG_csv/SDGData.csv") |> 
    janitor::clean_names()

pov_indicators <- sdg$indicator_name |> unique() |> str_subset("poverty")

wdi$country_name |> unique()

base$ID [!base$ID %in% risk$country] |> sort()
risk$country [!risk$country %in% base$ID] |> sort()


## Correct names so the join works better: you only need to do it once on the base layer

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

base <- left_join(base, risk |> select(country, code), by = c("ID" = "country"))


p1 <- left_join(base,risk) |> 
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



base |> 
    ggplot() +
    geom_sf(aes(fill = value), size = 0.05, color = "white") + 
    scale_fill_scico(palette = "berlin") +
    theme_void(base_size = 6) +
    theme(legend.position = "top", legend.direction = "horizontal",
          legend.key.width = unit(20, "mm"), legend.key.height = unit(3,"mm"))


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
