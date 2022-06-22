#load(file= "~/Documents/Projects/DATA/EC_additional_datasets/P_concn_plot.RData")
load(file= "~/Documents/Projects/DATA/EC_additional_datasets/P_load.RData")

P_load_df <- as.data.frame(P_load, xy = TRUE) #turning P_load raster in to dataframe for plotting
colnames(P_load_df) <- c("long","lat","P")
P_load_df[P_load_df[,3] < 0 & !is.na(P_load_df[,3]), 3] <- NA #get rid of any -ve values (shouldn't have any anyway)
P_load_df[is.infinite(P_load_df[,3]), 3] <- NA #get rid of any inf values (shouldn't have any anyway)
P_load_df[(P_load_df[,3])>100000 & !is.na(P_load_df[,3]), 3] <- NA #gets rid of anomalously high values
library(tidyverse)
world <- map_data("world")
#
P_concn_plot <- ggplot() +
    geom_map(data = world, map = world,aes(long, lat, map_id = region),color = NA, fill = "lightgrey", size = 0.1) +
    geom_raster(data=P_load_df,aes(long,lat, fill=cut(P,breaks=c(0,50,100,200,500,10000000)))) + 
    coord_quickmap() + #for quick projection
    #coord_map('rectangular',lat(0=30) + #for proper projection, but takes ages
    scale_fill_manual(name=bquote("P concentration (mgP"~m^-3~")"),values=c('green','orange','red','purple','black'),labels=c('0-50','50-100','100-200','200-500','>500')) +
    #theme_map() +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme(legend.title=element_text(colour="black",size=15,face="bold")) + #element_blank()
    theme(legend.text=element_text(colour="black",size=15)) + #element_blank()
    theme(legend.position=c(0,0)) + #"bottom" #c(0.5, 0) #legend.box = "horizontal"
    theme(legend.key.width=unit(3,"cm"))
#

## Juan's version:
library(tidyterra)

p <- P_load |> 
    rast() 

ggplot() +
    geom_spatraster(data = p) +
    scale_fill_continuous(
        "P concentration",type = "viridis", trans = "log1p",  na.value = "white",
        breaks = c(0, 100, 1000, 10000, 50000),
        guide = guide_colorbar(title.position = "top", barwidth = unit(2,"mm"), 
                               barheight = unit(20,"mm"))) +
    geom_sf(data = st_as_sf(coastsCoarse), size = 0.1, color = "gray25") +
    labs(tag = "A") + lims(y = c(-55.9, 83.2)) + # to make it the same extend as povsn
    theme_void(base_size = 6) +
    theme(legend.position = c(0.1, 0.3),
          legend.direction = "vertical")


ggsave(plot = last_plot(),
       filename = "p_draft.png", path = "figures/", 
       device = "png", width = 4, height = 2, dpi = 400, bg = "white")

