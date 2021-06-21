
# Mapping

##################################################################################################################################################

library(tidyverse)
library(ggmap)
  # register_google(key = "AIzaSyBZtlOvI_D2-aMTG8WpBIRNfNyIk0gHv0A", write = TRUE)
  # Adding key to C:\Users\davidsonka\Documents/.Renviron
library(rgdal)
library(rgeos)
library(sf)


#rivers.raw.sf <- st_read("~/Data/Spatial/Basemaps/BCGW_rivers/FWA_RIVERS_POLY/FWRVRSPL_polygon.shp")

rivers.poly.ogr <- readOGR(dsn = "~/Data/Spatial/Basemaps/BCGW_rivers/FWA_RIVERS_POLY/FWRVRSPL_polygon.shp", verbose=T)
water.poly2.ogr <- readOGR(dsn = "~/Data/Spatial/Basemaps/BC_RIV_LAKE_WET/BC_RIV_LAKE_WET_POLYS_500M/WATER_2M_polygon.shp", verbose=T)
water.poly6.ogr <- readOGR(dsn = "~/Data/Spatial/Basemaps/BCGW_waterpoly/BC_WATER_POLYS_5KM/WATER_6M_polygon.shp", verbose=T)
water.line.ogr <- readOGR(dsn="~/Data/Spatial/Basemaps/BC_WATER_LINES/BC_WATER_LINES_500M/WAT_LIN_2M_line.shp", verbose=T)

fraser.river.kml <- readOGR("~/Data/Spatial/Fraser_River.kml")
stellako.river.kml <- readOGR("~/Data/Spatial/Stellako_River.kml")
nadina.river.kml <- readOGR("~/Data/Spatial/Nadina_River.kml")
nechako.kml <- readOGR("~/Data/Spatial/Nechako.kml")
upper.fraser.kml <- readOGR("~/Data/Spatial/Upper_Fraser.kml")
nadleh.kml <- readOGR("~/Data/Spatial/Nadleh.kml")


##################################################################################################################################################

rivers.poly.df <- broom::tidy(rivers.poly.ogr)
water.poly2.df <- broom::tidy(water.poly2.ogr)
water.poly6.df <- broom::tidy(water.poly6.ogr)
water.line.df <- broom::tidy(water.line.ogr)

fraser.df <- broom::tidy(fraser.river.kml)
stellako.df <- broom::tidy(stellako.river.kml)
nadina.df <- broom::tidy(nadina.river.kml)
nechako.df <- broom::tidy(nechako.kml)
nadleh.df <- broom::tidy(nadleh.kml)
upper.fraser.df <- broom::tidy(upper.fraser.kml)


##################################################################################################################################################

#                                                            DEVELOP MAP


#-------- EXTRACT LAKE NAMES FROM BC WATER LINES SHP
# Zoom in to Francois-Fraser to extract polygon ID names of lakes for use in larger map
ggmap(get_googlemap(center=c(lon=-124.94, lat= 54.01), maptype = "satellite", zoom=11)) + 
  geom_path(data=water.line.df, aes(x=long, y=lat, group=id, colour=id), size=1)+
  geom_label(data=water.line.df, aes(x=long, y=lat, label=id)) +
  theme(legend.position="none")


#-------- PLOT FULL MAP
# Plot basemap and lines 
ggmap(get_googlemap(center=c(lon=-122.39, lat=51.84), maptype="satellite", zoom=6)) + 
  geom_path(data=fraser.df, aes(x=long, y=lat), colour="#5de2ff", size=0.4) +
  geom_path(data=upper.fraser.df, aes(x=long, y=lat), colour="#5de2ff", size=0.4) +
  geom_path(data=nechako.df, aes(x=long, y=lat), colour="#5de2ff", size=0.4) +
  geom_path(data=stellako.df, aes(x=long, y=lat), colour="#5de2ff", size=0.4) +
  geom_path(data=nadina.df, aes(x=long, y=lat), colour="#5de2ff", size=0.4) +
  geom_path(data=nadleh.df, aes(x=long, y=lat), colour="#5de2ff", size=0.4) +
  geom_path(data=water.line.df%>%filter(id%in%c(2181, 2174)), aes(x=long, y=lat, group=id), colour="#1E88FF", size=0.6) +
  labs(y="", x="") +
  theme(legend.position="none",
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Plot zoomed basemap and lines 
ggmap(get_googlemap(center=c(lon=-124.60, lat=54.08), maptype = "satellite", zoom=12)) + 
  geom_path(data=fraser.df, aes(x=long, y=lat), colour="#5de2ff", size=0.5) +
  geom_path(data=upper.fraser.df, aes(x=long, y=lat), colour="#5de2ff", size=0.5) +
  geom_path(data=nechako.df, aes(x=long, y=lat), colour="#5de2ff", size=0.5) +
  geom_path(data=stellako.df, aes(x=long, y=lat), colour="#ffb933", size=0.5) +
  geom_path(data=nadina.df, aes(x=long, y=lat), colour="#b933ff", size=0.5) +
  geom_path(data=nadleh.df, aes(x=long, y=lat), colour="red", size=0.5) +
  #geom_path(data=water.line.df%>%filter(id%in%c(2181, 2174)), aes(x=long, y=lat, colour=id, group=id), size=1) +
  scale_colour_manual(values=c("#3379ff", "#3379ff")) +
  labs(y="", x="") +
  theme(legend.position="none",
        axis.text = element_blank(),
        axis.ticks = element_blank())





