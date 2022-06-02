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

## load processed datasets
load(file = "data/subnational_poverty_vars.RData") # 243Mb
load(file = "data/subnational_hdi_vars.RData") # 557Mb

#### Water ####
tic()
povsn <- povsn |> 
    mutate(mean_intrvs = mean_integrity * -1) 
toc()

povsn <- bi_class(
    povsn, x = GSAP2_poor, #x = GSAP2_poor, 
    y = N_surplus, dim = 3)

leg <- bi_legend(
    pal = "DkViolet", dim = 3, # "DkViolet"
    xlab = "Poverty", ylab = "Mean N surplus",
    flip_axes = FALSE, rotate_pal = FALSE) +
    bi_theme(base_family = "Helvetica", base_size = 4)
leg

## It is faster if I plot directly to the file, skip the screen viz
tic()
ggsave(
    plot = (povsn |> 
    ggplot() +
    geom_sf(aes(fill = bi_class), size = 0.01, color = "white", show.legend = FALSE) +
    bi_scale_fill(pal = "DkViolet", dim = 3) +
    annotation_custom(
        grob = ggplotGrob(leg), 
        xmin = -180, ymin = -50, xmax = -90, ymax = 0) +
    theme_void(base_size = 6)) ,
    filename = "N_surplus_poverty.png", path = "figures/", 
    device = "png", width = 6, height = 4, dpi = 400, bg = "white")
toc() #20s

### Tests: Do not assign the plots to objects, it duplicates the dataset by ggplot2
### the plot below are examples to include in the inset.
povsn |> 
    ggplot(aes(GSAP2_poor, risk_volinv)) +
    geom_density2d(color = "orange", size = 0.25) +
    geom_hline(
        yintercept = quantile(povsn$risk_volinv,c(0.3,0.6), na.rm = TRUE),
        color = "grey50", linetype = 2) +
    geom_vline(
        xintercept = quantile(povsn$GSAP2_poor,c(0.3,0.6), na.rm = TRUE),
        color = "grey50", linetype = 2) +
    labs(x = "Poverty rate at 1.9$US PPP", y = "Risk volume") +
    theme_classic()

ggsave(
    plot = last_plot(), 
    filename = "water_poverty_SM.png", path = "figures/", 
    device = "png", width = 3, height = 3, dpi = 400, bg = "white"
)

# povsn |> ggplot(aes(GSAP2_poor)) + 
#     geom_density() +
#     geom_vline(
#         xintercept = quantile(povsn$GSAP2_poor,c(0.3,0.6), na.rm = TRUE),
#         color = "grey50", linetype = 2) +
#     labs(x = "Poverty rate") +
#     theme_classic()
# 
# povsn |> ggplot(aes(risk_volinv)) + 
#     geom_density() +
#     geom_vline(
#         xintercept = quantile(povsn$risk_volume,c(0.3,0.6), na.rm = TRUE),
#         color = "grey50", linetype = 2) +
#     labs(x = "Risk volume") +
#     theme_classic()


tic()
hdi_shp <- hdi_shp |>
    mutate(#risk_volinv = risk_volume * -1,
           shdinv = shdi * -1)
toc()

hdi_shp <- bi_class(hdi_shp, x = shdinv, y = P.concentration, dim = 3)

# bi_pal(pal = "GrPink", dim = 3, preview = T, rotate_pal = F, flip_axes = F) 

leg <- bi_legend(
    pal = "GrPink", dim = 3, 
    xlab = "Human Development Index", ylab = "Mean P concentration",
    flip_axes = F, rotate_pal = F) +
    ## I need the legend without the arrows
    labs(x = "Human Development Index", y = "Mean Integrity") +
    bi_theme(base_family = "Helvetica", base_size = 4)
leg

## It is faster if I plot directly to the file, skip the screen viz
tic()
ggsave(
    plot = (hdi_shp |> 
                ggplot() +
                geom_sf(aes(fill = bi_class), size = 0.01, color = "white", show.legend = FALSE) +
                bi_scale_fill(pal = "GrPink", dim = 3) +
                annotation_custom(
                    grob = ggplotGrob(leg), 
                    xmin = -180, ymin = -50, xmax = -90, ymax = 0) +
                theme_void(base_size = 6)),
    filename = "P_concentration_hdi.png", path = "figures/", 
    device = "png", width = 6, height = 4, dpi = 400, bg = "white")
toc() #49s

hdi_shp |> 
    ggplot(aes(shdi, risk_volinv)) +
    geom_density2d(color = "orange", size = 0.25) +
    geom_hline(
        yintercept = quantile(hdi_shp$risk_volinv,c(0.3,0.6), na.rm = TRUE),
        color = "grey50", linetype = 2) +
    geom_vline(
        xintercept = quantile(hdi_shp$shdi,c(0.3,0.6), na.rm = TRUE),
        color = "grey50", linetype = 2) +
    labs(x = "Human Development Index", y = "Risk volume") +
    theme_classic()

ggsave(
    plot = last_plot(), 
    filename = "water_hdi_SM.png", path = "figures/", 
    device = "png", width = 3, height = 3, dpi = 400, bg = "white"
)

#### Population

