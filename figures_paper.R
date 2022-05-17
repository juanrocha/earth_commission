library(tidyverse)
library("rnaturalearth")
library("rnaturalearthdata")
library(scico)
library("patchwork")


dat <- googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1rNeYoHa9GbCkaaw1R-aVUImWAPQZyUxHY46mjaQVOFU/edit?usp=sharing",
    sheet = 1
)

dat <- dat %>% 
    filter(!is.na(lon)) %>% 
    janitor::clean_names()

dat %>% 
    mutate()

#### Map with countries ####
map_data("world") %>% 
    rename(lon = long) %>% 
    ggplot(aes(lon,lat)) +
    geom_path( aes(map_id = region, group = group), size = 0.15 ) +
    geom_point(
        data = dat,
        aes(lon,lat, fill = tipping_elements_names),
        size = 7, alpha = 0.5) +
    theme(legend.position = "bottom")



## Map without
base_map <- ne_coastline( returnclass = "sp")

base_map %>% 
    ggplot() +
    geom_sf(size = 0.1) +
    geom_point(
        data = dat,
        aes(lon,lat, color = tipping_elements_names),
        size = 3, alpha = 0.5) + 
    scale_color_scico_d(palette = "romaO") +
    guides(
        color = guide_legend(
            ncol = 4, title.position = "top", title = "Tipping Elements")) +
    theme_void(base_size = 6) +
    theme(legend.position = "bottom")


ggsave(
    filename = "tipping_map.png",
    plot = last_plot(),
    device = "png",
    path = "figures/",
    width = 7, height = 4, units = "in",
    dpi = 450, bg = "white"
)

#### starfish ####

dat <- googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1l_KVmONg3L0vmfPQnIdjPaanRDVNIw70AShl8kwuHQ0/edit#gid=141907550",
    sheet = 3
)

dat <- dat |> 
    slice(7:11) |> 
    select(-9, -10, -11) |> 
    pivot_longer(cols = 2:last_col(), names_to = "domain", values_to = "estimate") |> 
    mutate(estimate = as.character(estimate)) |> 
    mutate(estimate = str_remove(estimate, "<|>|%") |> str_trim("both")) |> 
    mutate(estimate = as.numeric(estimate)) #|> 
    # print(n=35)

dat |> 
    rename(ESdomain = 1) |> 
    mutate(ESdomain = str_remove_all(ESdomain, "\\(harmonised units\\)") |> 
               str_trim("both")) |> 
    mutate(type = case_when(
        str_detect(ESdomain, "Access") ~ "Just",
        str_detect(ESdomain, "Safe") ~ "Safe"
    )) |> 
    group_by(domain) |> 
    #mutate(estimate = scales::rescale(estimate)) |> 
    ggplot(aes(type, estimate)) +
    #geom_col(position = "stack", aes(fill = ESdomain)) +
    geom_point() +
    geom_line(aes(color = type)) +
    facet_wrap(~domain, scales = "free")


#### conceptual figure ####
starfish <- tibble(
    min = rep(0, 7),
    max = rep(1, 7),
    safe = rnorm(7, mean = 0.75, sd = 0.05),
    just_access = rnorm(7, mean = 0.25, sd = 0.05),
    just_harm = rnorm(7, mean = 0.75, sd = 0.05),
    names = c("Climate", "Biodiversity", "Nitrogen", "Phosphorus", "NOx", "SO2", "Water")
        #dat$domain |> unique()
)

small_df <- starfish |> select(-min, -max) |> 
    pivot_longer(safe:just_harm, names_to = "class", values_to = "value") |> 
    arrange(names) |> 
    mutate(class = as_factor(class)) 


a <- starfish |> 
    mutate(
        present_unjust = just_access,
        safe_just = pmin(safe, just_harm) - just_access,
        future_unjust = max - pmin(just_harm, safe)
    ) |> 
    select(-min, -max, -safe, -just_access, -just_harm) |> 
    pivot_longer(present_unjust:future_unjust, names_to = "class", values_to = "value") |>
    mutate(class = as_factor(class),
           class = fct_relevel(class,"future_unjust", "safe_just", "present_unjust")) |> 
    ggplot(aes(names, value)) +
    geom_col(aes(fill = class), alpha = 0.4) +
    geom_path(data = small_df,
              aes(names, value, group = class, color = class), size = 0.5) +
    geom_point(data = small_df,
               aes(names, value, color = class)) +
    geom_errorbar(data = small_df,
                  aes(names, value, color = class, ymin = value-0.05, ymax = value+0.05),
                  size = 0.2, width = 0.1) +
    coord_polar() +
    scale_fill_manual(
        "Space", values = c("grey60", "green", "grey40"), labels = c("Unjust with future generations", "Safe and necessary but\n not sufficient for justice", "Unjust with present generations")) +
    scale_color_manual(
        "Boundaries", 
        labels = c("Safe", "Just: minimum universal access", "Just: no significant harm"), 
        values = c("blue", "orange", "purple")) +
    #labs(tag = "A") +
    theme_classic(base_size = 8) +
    theme(
        axis.text.y = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), axis.line = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 1)
    )

## Fig b
nocorridor <- tibble(
    min = rep(0, 7),
    max = rep(1, 7),
    safe = rnorm(7, mean = 0.25, sd = 0.05),
    just_access = rnorm(7, mean = 0.75, sd = 0.05),
    just_harm = rnorm(7, mean = 0.25, sd = 0.05),
    names = c("Climate change", "Biodiversity", "Nitrogen", "Phosphorus", "NOx", "SO2", "Water") #dat$domain |> unique()
)

small_df2 <- nocorridor |> select(-min, -max) |> 
    pivot_longer(safe:just_harm, names_to = "class", values_to = "value") |> 
    arrange(names) |> 
    mutate(class = as_factor(class)) 


b <- nocorridor |> 
    mutate(
        present_unjust = just_access,
        safe_just = 0,
        future_unjust = max - just_access
    ) |> 
    select(-min, -max, -safe, -just_access, -just_harm) |> 
    pivot_longer(present_unjust:future_unjust, names_to = "class", values_to = "value") |>
    mutate(class = as_factor(class),
           class = fct_relevel(class,"future_unjust", "safe_just", "present_unjust")) |> 
    ggplot(aes(names, value)) +
    geom_col(aes(fill = class), alpha = 0.4) +
    geom_path(data = small_df2,
              aes(names, value, group = class, color = class), size = 0.5) +
    geom_point(data = small_df2,
               aes(names, value, color = class)) +
    geom_errorbar(data = small_df2,
                  aes(names, value, color = class, ymin = value-0.05, ymax = value+0.05),
                  size = 0.2, width = 0.1) +
    coord_polar() +
    scale_fill_manual(
        "Space", values = c("grey60", "green", "grey40"), labels = c("Unjust with future generations", "Safe and necessary but\n not sufficient for justice", "Unjust with present generations")) +
    scale_color_manual(
        "Boundaries", 
        labels = c("Safe", "Just: minimum universal access", "Just: no significant harm"), 
        values = c("blue", "orange", "purple")) +
    #labs(tag = "B") + 
    theme_classic(base_size = 8) +
    theme(
        axis.text.y = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), axis.line = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 1)
    )

b + a + plot_layout(guide = "collect")

ggsave(
    filename = "conceptual_fig.png",
    plot = last_plot(),
    path = "figures/",
    device = "png",
    width = 7, height = 3
)
