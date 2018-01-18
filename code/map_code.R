# load ----
library(PBSmapping)
library(tidyverse)
library(ggrepel)
library(rgdal)
library(extrafont)
#font_import() only do this one time - it takes a while
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

library(tidyverse)
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

# data ----
areas <- readOGR("data/shpfiles", layer = "NMFS_GOA_WGS1984")
areas@data$id = rownames(areas@data)
areas.points = fortify(areas, region="id")
areas.df = left_join(areas.points, areas@data, by="id")


data('nepacLLhigh') #load PBSmapping data set for N Pacific - much better resolution than world highres...
nepacLLhigh %>% 
  dplyr::select(group=PID, POS=POS,long=X,lat=Y) -> ak1

ports <- data.frame(long = c(-152.433333, -160.493333, -162.318056, -165.775), 
                    lat = c(57.787, 55.336667, 55.072222, 54.1325), 
                    port = c("Kodiak", "Sand Point", "King Cove", "Akutan"))


# map ----

ggplot() +
  geom_polygon(data=ak1,aes(long,lat,group=group), fill="lightgray") + 
  coord_map("gilbert", xlim = c(-170, -139), ylim = c(49, 62))+
  xlab(expression(paste(Longitude^o,~'W'))) +
  ylab(expression(paste(Latitude^o,~'W'))) +
  geom_path(data = areas.df, aes(long, lat, group = group)) +
  annotate("text", x = -165, y = 51.5, label = "Area \n 610") +
  annotate("text", x = -157, y = 53, label = "Area \n 620") +
  annotate("text", x = -151, y = 55, label = "Area \n 630") +
  annotate("text", x = -145, y = 57.2, label = "Area \n 640") +
  annotate("text", x = -167, y = 57.5, label = "Bering Sea") +
  annotate("text", x = -159, y = 60.5, label = "Alaska") +
  annotate("text", x = -150, y = 51, label = "Gulf of Alaska") +
  geom_point(data = ports, aes(long, lat), size = 3, color = 4)+
  geom_text_repel(data = ports, aes(long, lat, label = port, group = port), point.padding = 0.75, direction = "y")
