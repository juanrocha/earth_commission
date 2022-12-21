library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(fs)
library(rworldmap)

## load world map and transform it into an sf object
data(countriesLow)
class(countriesLow)
world <- st_as_sf(countriesLow)

# Import layers
fls <- dir_ls("data/for_wef/") |> 
    str_subset(".tif$")

ebs <- map(fls, rast)
ebs[[8]] ## this is the total already calculated

plot(ebs[[8]]) # check values

# check the raster and vector layers are aligned
ggplot() +
    geom_spatraster(data = ebs[[8]]) +
    geom_sf(data = world, size = 0.1, color = "gray25", fill = NA) +
    theme_void()

df_trans <- extract(ebs[[8]] > 0, world, sum, na.rm = TRUE) # pixels transgressed
df_count <- extract(!is.na(ebs[[8]]), world, sum, na.rm = TRUE) # total pixels
df_max <- extract(ebs[[8]], world, max, na.rm = TRUE) # max transgressions

world <- world |>
    mutate(total_territory = df_count$total_transgression_WEF,
           pxls_affected = df_trans$total_transgression_WEF,
           max_trans = df_max$total_transgression_WEF) |> 
    mutate(prop_affected = pxls_affected / total_territory)

## return result on the same file already created:
file <- dir_ls("data/for_wef/") |> str_subset(".csv$")
dat <- read_csv(file)

dat <- dat |> left_join(
    world |> as_tibble() |> 
        select(ISO3, prop_affected, pxls_affected, max_trans))

write_csv(dat, file = file)

a <- ggplot(dat, aes(transgressions, AREA)) +
    geom_point() +
    geom_smooth() +
    scale_y_log10() + labs(tag = "A") +
    theme_light(base_size = 7)

b <- ggplot(dat, aes(transgressions, prop_affected)) +
    geom_point(size = 1.5) +
    geom_text(aes(label = ISO3), size = 1.5, nudge_x = 0.25) + labs(tag = "B") +
    theme_light(base_size = 7)

c <- ggplot(dat, aes(max_trans, transgressions)) +
    geom_point()+ labs(tag = "C") +
    theme_light(base_size = 7)

library(patchwork)

ggsave(
    filename = "transgressions.png", 
    plot = a+b+c, device = "png", dpi = 300,
    width = 7, height = 3,
    path = "figures/"
)

a+b
