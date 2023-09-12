# Purpose: Map mean 2019-2021 soybean production and yield in Europe using data from Faostat and Eurostat
# Author: M. Marteau-Bazouni
# Date: 18/07/2023


# Packages ----------------------------------------------------------------

library(mapview);library(giscoR);library(sf);library(dplyr);library(tidyr)
mapviewOptions(fgb = FALSE, na.color = NA, homebutton = FALSE, basemaps = "CartoDB.VoyagerNoLabels") # Basemap choice : https://leaflet-extras.github.io/leaflet-providers/preview/


# Input data --------------------------------------------------------------

stat <- read.csv2("../Input/Eurostat/Eurostat_soybean.csv")
fao <- read.csv2("../Input/Eurostat/FAOSTAT_soybean.csv")
fao$Value <- as.numeric(fao$Value)


# Yield map - data from Faostat -------------------------------------------

yield <- fao %>% filter(Year<2022 & Year >=2019 & Element == "Yield") %>% group_by(Area) %>% mutate(Mean=mean(Value)/10000) %>% distinct(Area,Mean)

shpe <- gisco_get_countries(region="Europe",epsg=4326) %>% filter(!FID %in% c("RU","IS","SJ")) #Get country shapes
shpe <- st_crop(shpe,xmin=-12,xmax=40,ymin=32,ymax=70)

yield <- merge(shpe,yield,by.y="Area",by.x="NAME_ENGL",all.x = TRUE)

pal <- c("white","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B") #Color palette
map <- mapview(yield,zcol="Mean",col.regions=pal,layer.name="Yield </br> (t.ha <sup>-1</sup>)")
mapshot(map,file = "../Output/Soybean_production_Eurostat/Soybean_yield_faostat.jpg",remove_controls = c("zoomControl", "layersControl", "homeButton","drawToolbar", "easyButton"),zoom=1.2,vwidth=650,vheight=780)


# Production map - data from Eurostat -------------------------------------

prod <- stat %>% filter(Var=="Production (1000t)") %>% group_by(Geo) %>% mutate(Mean=mean(c(X2019,X2020,X2021),na.rm=TRUE)) %>% distinct(Geo,Mean)

eu <- c("AL","AT","BA","BE","BG","BY","CH","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU","IE","IT","LT","LU","LV","MD","ME","MK","NL","NO","PL","PT","RO","RS","SE","SI","SK","UA","UK")
nuts <- subset(gisco_get_nuts(country=eu,nuts_level=2),select=c(FID,NAME_LATN,geometry)) #Get nuts shapes
countries <- subset(gisco_get_countries(country=c("Bosnia and Herzegovina","Ukraine")),select=c(FID,NAME_ENGL,geometry)) #Get country shapes for countries where no nuts shapes are available
colnames(countries) <- c("FID","NAME_LATN","geometry") 
full <- st_crop(rbind(nuts,countries),xmin=-12,xmax=40,ymin=32,ymax=70)

prod <- merge(full,prod,by.y="Geo",by.x="NAME_LATN",all.x = TRUE)

map <- mapview(prod,zcol="Mean",col.regions=pal,layer.name ="Soybean </br> prod. (1000t)")
mapshot(map,file = "../Output/Soybean_production_Eurostat/Soybean_prod_eurostat.jpg",remove_controls = c("zoomControl", "layersControl", "homeButton","drawToolbar", "easyButton"),zoom=1.2,vwidth=650,vheight=780)

